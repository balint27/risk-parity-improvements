#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - COMPREHENSIVE VERSION (Z4.4.R)
# PART 1: DATA HANDLING, VOLATILITY ESTIMATION, AND COVARIANCE CALCULATION
#=============================================================================

# Load required packages with reliable error handling
required_packages <- c("tidyverse", "quantmod", "xts", "PerformanceAnalytics",
                       "TTR", "zoo", "tidyquant", "ggplot2", "reshape2", 
                       "rugarch", "nloptr")  # Added rugarch and nloptr

# Ensure all packages are installed
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("Installing package: ", pkg, "\n"))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  
  # Use require with error handling
  if (!require(pkg, character.only = TRUE)) {
    warning(paste0("Package not available: ", pkg))
  } else {
    cat(paste0("Loaded package: ", pkg, "\n"))
  }
}

# Log system information
cat(sprintf("\nCurrent Date and Time (UTC): %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat(sprintf("R Version: %s\n", R.version.string))

# Global safety settings
options(warn = 1)  # Show warnings immediately
options(stringsAsFactors = FALSE)  # Never convert strings to factors by default

#=============================================================================
# HELPER FUNCTIONS
#=============================================================================

# Helper function to ensure we have proper Date objects
ensure_date_index <- function(x) {
  if (!is.null(x) && is.xts(x)) {
    if (!inherits(index(x), "Date")) {
      cat("Converting index to Date objects\n")
      index(x) <- as.Date(index(x))
    }
  }
  return(x)
}

# Helper function to ensure weights are in proper numeric format
ensure_numeric_weights <- function(weights) {
  # If it's a list, unlist it
  if (is.list(weights) && !is.xts(weights)) {
    weights <- unlist(weights)
  }
  
  # Ensure all weights are numeric and have names
  if (!is.null(weights)) {
    weight_names <- names(weights)
    weights <- as.numeric(weights)
    names(weights) <- weight_names
  }
  
  return(weights)
}

#=============================================================================
# DATA HANDLING - REAL DATA ONLY
#=============================================================================

# Load market data with robust error handling - NO SYNTHETIC DATA
load_market_data <- function(tickers, start_date, end_date = Sys.Date(), source = "yahoo",
                             include_inverse = TRUE) {
  # Ensure start and end dates are Date objects
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Define inverse ETF mapping 
  inverse_etf_map <- list(
    "SPY" = "SH",    # Inverse S&P 500
    "QQQ" = "PSQ",   # Inverse Nasdaq
    "IWM" = "RWM",   # Inverse Russell 2000
    "EEM" = "EUM",   # Inverse Emerging Markets
    "EFA" = "EFZ",   # Inverse EAFE (International)
    "IEF" = "TBF",   # Inverse 7-10 Year Treasury
    "HYG" = "SJB",   # Inverse High Yield Bond
    "GLD" = "DGZ",   # Inverse Gold
    "VNQ" = "DRV"    # Inverse Real Estate (3x leveraged)
  )
  
  # Add inverse ETFs if requested
  if (include_inverse) {
    # Find which standard ETFs in our list have inverse versions
    inverse_tickers <- c()
    for (ticker in tickers) {
      if (ticker %in% names(inverse_etf_map)) {
        inverse_tickers <- c(inverse_tickers, inverse_etf_map[[ticker]])
      }
    }
    
    # Add inverse ETFs to the ticker list
    if (length(inverse_tickers) > 0) {
      cat(sprintf("Adding %d inverse ETFs to data loading: %s\n", 
                  length(inverse_tickers),
                  paste(inverse_tickers, collapse=", ")))
      
      tickers <- c(tickers, inverse_tickers)
    }
  }
  
  # Initialize an empty xts object for prices
  all_prices <- NULL
  
  # Keep track of successfully loaded tickers
  loaded_tickers <- c()
  failed_tickers <- c()
  
  cat(sprintf("Loading data for %d tickers from %s to %s...\n", 
              length(tickers), start_date, as.character(end_date)))
  
  # Try to get data from Yahoo Finance
  for (ticker in tickers) {
    tryCatch({
      # Fetch data
      price_data <- getSymbols(ticker, from = start_date, to = end_date, 
                               src = source, auto.assign = FALSE)
      
      # Verify we have non-empty data
      if (is.null(price_data) || nrow(price_data) < 5) {
        cat(sprintf("  Error: %s returned insufficient data\n", ticker))
        failed_tickers <- c(failed_tickers, ticker)
        next
      }
      
      # Extract adjusted closing prices
      close_data <- price_data[, 6, drop = FALSE]
      
      # Verify we have numeric data
      if (!is.numeric(coredata(close_data))) {
        cat(sprintf("  Error: Non-numeric data for %s\n", ticker))
        failed_tickers <- c(failed_tickers, ticker)
        next
      }
      
      colnames(close_data) <- ticker
      
      # Merge with existing data
      if (is.null(all_prices)) {
        all_prices <- close_data
      } else {
        all_prices <- merge(all_prices, close_data)
      }
      
      loaded_tickers <- c(loaded_tickers, ticker)
      cat(sprintf("  Successfully loaded %s - data range %s to %s\n", 
                  ticker, 
                  format(index(close_data)[1], "%Y-%m-%d"),
                  format(index(close_data)[nrow(close_data)], "%Y-%m-%d")))
    }, error = function(e) {
      cat(sprintf("  Error loading %s: %s\n", ticker, e$message))
      failed_tickers <- c(failed_tickers, ticker)
    })
  }
  
  # Report loading status
  cat(sprintf("\nLoaded %d of %d tickers. Failed to load %d tickers.\n", 
              length(loaded_tickers), length(tickers), length(failed_tickers)))
  
  if (length(failed_tickers) > 0) {
    cat("Failed tickers:", paste(failed_tickers, collapse=", "), "\n")
  }
  
  # Final check if we have data
  if (is.null(all_prices) || ncol(all_prices) == 0) {
    stop("Failed to load any ticker data. Please check internet connection and ticker symbols.")
  }
  
  # Fill missing values using last observation carried forward
  all_prices <- na.locf(all_prices, na.rm = FALSE)
  
  # Then fill any remaining NAs with backward fill
  all_prices <- na.locf(all_prices, fromLast = TRUE, na.rm = FALSE)
  
  # Check for any remaining NAs and use interpolation
  if (any(is.na(all_prices))) {
    cat("WARNING: Some NA values remain after forward/backward filling.\n")
    cat("Using linear interpolation for remaining gaps...\n")
    
    # For each column with NAs
    for (col in colnames(all_prices)) {
      if (any(is.na(all_prices[, col]))) {
        all_prices[, col] <- na.approx(all_prices[, col], na.rm = FALSE)
      }
    }
  }
  
  # Ensure we have a valid date index
  all_prices <- ensure_date_index(all_prices)
  
  # Add inverse ETF map as attribute
  attr(all_prices, "inverse_etf_map") <- inverse_etf_map
  
  # Final report
  cat(sprintf("Final price data: %d days × %d tickers\n", 
              nrow(all_prices), ncol(all_prices)))
  
  return(all_prices)
}

#=============================================================================
# GARCH VOLATILITY FORECASTING - FROM Z2.1.R
#=============================================================================

# Implementation of GARCH volatility forecasting
forecast_garch_volatility <- function(returns, forecast_horizon = 22) {
  # Initialize results dataframe
  forecasted_vols <- data.frame(
    asset = colnames(returns),
    forecasted_vol = rep(0, ncol(returns)),
    method_used = rep("", ncol(returns)),
    row.names = colnames(returns)
  )
  
  # For each asset
  for (col in colnames(returns)) {
    tryCatch({
      # Extract return series and remove NAs
      return_series <- returns[, col]
      return_series <- return_series[!is.na(return_series)]
      
      # Skip if not enough data
      if (length(return_series) < 30) {
        forecasted_vols[col, "forecasted_vol"] <- sd(return_series, na.rm = TRUE) * sqrt(252)
        forecasted_vols[col, "method_used"] <- "historical"
        next
      }
      
      # Try standard GARCH(1,1) first
      garch_spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
        distribution.model = "std"  # Student-t for robustness
      )
      
      garch_fit <- ugarchfit(garch_spec, return_series, solver = "hybrid")
      
      # Check convergence
      if (!garch_fit@fit$convergence) {
        # If not converged, try a different solver
        garch_fit <- ugarchfit(garch_spec, return_series, solver = "solnp")
      }
      
      # Forecast volatility
      garch_forecast <- ugarchforecast(garch_fit, n.ahead = forecast_horizon)
      forecast_sigma <- as.numeric(sigma(garch_forecast)[forecast_horizon])
      
      # Annualize the volatility forecast
      forecasted_vols[col, "forecasted_vol"] <- forecast_sigma * sqrt(252)
      forecasted_vols[col, "method_used"] <- "GARCH"
      
    }, error = function(e) {
      # Try EWMA if GARCH fails
      tryCatch({
        cat(sprintf("GARCH failed for %s: %s, trying EWMA\n", col, e$message))
        
        # Extract return series
        return_series <- returns[, col]
        return_series <- return_series[!is.na(return_series)]
        
        # Calculate EWMA variance
        lambda <- 0.94  # RiskMetrics standard
        
        if (length(return_series) < 10) {
          # Just use historical volatility if too little data
          forecast_vol <- sd(return_series, na.rm = TRUE) * sqrt(252)
          forecasted_vols[col, "forecasted_vol"] <- forecast_vol
          forecasted_vols[col, "method_used"] <- "historical"
        } else {
          # Use exponentially weighted moving average
          weights <- lambda^(0:(length(return_series)-1))
          weights <- rev(weights / sum(weights))  # Normalize and reverse
          
          # Calculate weighted variance
          ewma_var <- sum(weights * return_series^2, na.rm = TRUE)
          forecast_vol <- sqrt(ewma_var) * sqrt(252)
          
          forecasted_vols[col, "forecasted_vol"] <- forecast_vol
          forecasted_vols[col, "method_used"] <- "EWMA"
        }
      }, error = function(e2) {
        # If all else fails, use historical volatility
        cat(sprintf("All volatility methods failed for %s: %s, using historical\n", col, e2$message))
        hist_vol <- sd(returns[, col], na.rm = TRUE) * sqrt(252)
        forecasted_vols[col, "forecasted_vol"] <- hist_vol
        forecasted_vols[col, "method_used"] <- "historical"
      })
    })
  }
  
  # Ensure all forecasts are reasonable
  min_vol <- 0.05  # 5% annualized minimum
  max_vol <- 0.80  # 80% annualized maximum
  
  for (col in colnames(returns)) {
    # Check if forecast is reasonable
    if (is.na(forecasted_vols[col, "forecasted_vol"]) || 
        forecasted_vols[col, "forecasted_vol"] < min_vol || 
        forecasted_vols[col, "forecasted_vol"] > max_vol) {
      
      # Calculate historical vol as fallback
      hist_vol <- sd(returns[, col], na.rm = TRUE) * sqrt(252)
      hist_vol <- min(max(hist_vol, min_vol), max_vol)  # Bound it
      
      cat(sprintf("Unreasonable vol forecast for %s (%.2f), using historical (%.2f)\n", 
                  col, forecasted_vols[col, "forecasted_vol"], hist_vol))
      
      forecasted_vols[col, "forecasted_vol"] <- hist_vol
      forecasted_vols[col, "method_used"] <- "historical (fallback)"
    }
  }
  
  # Log methods used
  method_counts <- table(forecasted_vols$method_used)
  cat("Volatility forecasting methods used:\n")
  for (method in names(method_counts)) {
    cat(sprintf("  %s: %d assets\n", method, method_counts[method]))
  }
  
  return(forecasted_vols)
}

#=============================================================================
# IMPROVED EWMA COVARIANCE ESTIMATION
#=============================================================================

# Enhanced EWMA covariance estimation
estimate_ewma_covariance <- function(returns, lambda = 0.94, min_obs = 60) {
  cat("\nEstimating covariance matrix using EWMA\n")
  
  # Safety check for input
  if (is.null(returns) || nrow(returns) < 10 || ncol(returns) < 2) {
    warning("Insufficient data for EWMA covariance - using sample covariance")
    return(cov(returns))
  }
  
  # Remove NAs and ensure we have enough data
  returns <- na.omit(returns)
  n_obs <- nrow(returns)
  n_assets <- ncol(returns)
  
  # Print diagnostics
  cat(sprintf("EWMA: Processing %d observations for %d assets with lambda=%.2f\n", 
              n_obs, n_assets, lambda))
  
  # Need at least min_obs observations for a stable estimate
  if (n_obs < min_obs) {
    warning(sprintf("EWMA needs at least %d observations - using robust sample covariance", 
                    min_obs))
    return(cov(returns))
  }
  
  # Calculate EWMA directly
  
  # Calculate weights - exponential decay (oldest observations get lowest weight)
  weights <- lambda^(0:(n_obs-1))
  weights <- rev(weights / sum(weights))  # Normalize and reverse (recent = higher weight)
  
  # Initialize covariance matrix
  cov_matrix <- matrix(0, nrow = n_assets, ncol = n_assets)
  colnames(cov_matrix) <- colnames(returns)
  rownames(cov_matrix) <- colnames(returns)
  
  # Calculate the weighted covariance
  # First standardize return data to improve numerical stability
  returns_std <- returns
  for (i in 1:n_assets) {
    returns_std[,i] <- returns[,i] / sd(returns[,i], na.rm = TRUE)
  }
  
  # For each asset pair, calculate weighted covariance
  for (i in 1:n_assets) {
    for (j in i:n_assets) {
      asset_i_returns <- as.numeric(returns_std[, i])
      asset_j_returns <- as.numeric(returns_std[, j])
      
      # Calculate product series
      cross_products <- asset_i_returns * asset_j_returns
      
      # Apply weights
      weighted_sum <- sum(weights * cross_products, na.rm = TRUE)
      
      # Store in covariance matrix (maintain symmetry)
      cov_matrix[i, j] <- weighted_sum
      cov_matrix[j, i] <- weighted_sum
    }
  }
  
  # Restore scaling
  for (i in 1:n_assets) {
    for (j in 1:n_assets) {
      cov_matrix[i,j] <- cov_matrix[i,j] * sd(returns[,i], na.rm = TRUE) * 
        sd(returns[,j], na.rm = TRUE)
    }
  }
  
  # Check for positive definiteness
  eigen_values <- eigen(cov_matrix, only.values = TRUE)$values
  
  if (min(eigen_values) <= 0) {
    cat("EWMA matrix not positive definite, applying regularization\n")
    
    # Shrinkage toward diagonal
    diag_target <- diag(diag(cov_matrix))
    shrink_factor <- 0.1  # 10% shrinkage
    cov_matrix <- (1 - shrink_factor) * cov_matrix + shrink_factor * diag_target
    
    # Check again
    eigen_values <- eigen(cov_matrix, only.values = TRUE)$values
    
    # If still not positive definite, add small constant to diagonal
    if (min(eigen_values) <= 0) {
      epsilon <- 1e-5 * mean(diag(cov_matrix))
      diag(cov_matrix) <- diag(cov_matrix) + epsilon
    }
  }
  
  # Print diagnostics about the resulting matrix
  eigen_values <- eigen(cov_matrix, only.values = TRUE)$values
  cat(sprintf("EWMA covariance matrix: min eigenvalue = %.6g, max eigenvalue = %.6g\n", 
              min(eigen_values), max(eigen_values)))
  
  # Check for NaN or NA values in the covariance matrix
  if (any(is.na(cov_matrix)) || any(is.nan(cov_matrix))) {
    cat("WARNING: NA/NaN values in covariance matrix, replacing with zeros\n")
    cov_matrix[is.na(cov_matrix) | is.nan(cov_matrix)] <- 0
    
    # Add small positive value to diagonal for stability
    diag(cov_matrix) <- pmax(diag(cov_matrix), 1e-6)
  }
  
  return(cov_matrix)
}

#=============================================================================
# ECONOMIC INDICATORS FROM ETF DATA - ENHANCED WITH COMMODITY TREND
#=============================================================================

# FIXED VERSION: Properly prepares VIX data with guaranteed alignment
get_vix_aligned <- function(price_dates, from = min(price_dates) - 30, to = max(price_dates) + 5) {
  cat("Loading VIX data...\n")
  
  # Ensure dates are proper Date objects
  price_dates <- as.Date(price_dates)
  from <- as.Date(from)
  to <- as.Date(to)
  
  # Create a template with all target dates - this ensures exact dimension match
  vix_aligned <- xts(rep(NA, length(price_dates)), order.by = price_dates)
  colnames(vix_aligned) <- "VIX"
  
  # Try to get VIX data from Yahoo Finance
  tryCatch({
    # Try direct VIX ticker
    cat("Trying to load VIX data from Yahoo Finance (^VIX)...\n")
    vix_data <- getSymbols("^VIX", src = "yahoo", from = from - 30, to = to + 5, auto.assign = FALSE)
    
    if (!is.null(vix_data) && nrow(vix_data) > 10) {
      vix_close <- Cl(vix_data)
      colnames(vix_close) <- "VIX"
      cat(sprintf("Successfully loaded VIX data: %d observations\n", nrow(vix_close)))
      
      # Find common dates
      common_dates <- intersect(index(vix_close), price_dates)
      
      if (length(common_dates) > 0) {
        # Assign values for matching dates
        vix_aligned[common_dates] <- vix_close[common_dates]
        
        # Fill NAs with last observation carried forward
        vix_aligned <- na.locf(vix_aligned, na.rm = FALSE)
        
        # Fill any remaining NAs at the beginning using LOCB
        vix_aligned <- na.locf(vix_aligned, fromLast = TRUE, na.rm = FALSE)
        
        # If still have NAs, fill with default value
        if (any(is.na(vix_aligned))) {
          vix_aligned[is.na(vix_aligned)] <- 20  # Default value
        }
      } else {
        cat("WARNING: No common dates between VIX and price data\n")
        vix_aligned[] <- 20  # Use default value
      }
    } else {
      cat("Failed to load VIX, using default values\n")
      vix_aligned[] <- 20  # Use default value
    }
  }, error = function(e) {
    cat("Error loading VIX data:", e$message, "\n")
    cat("Using default values\n")
    vix_aligned[] <- 20  # Use default value
  })
  
  cat(sprintf("Final aligned VIX data: %d rows, NA count: %d\n", 
              nrow(vix_aligned), sum(is.na(vix_aligned))))
  
  return(vix_aligned)
}

# Enhanced market data creation with bond-equity correlation and commodity trend
create_market_data <- function(prices, returns = NULL, enhanced_indicators = TRUE) {
  cat("\nCreating market data with guaranteed alignment...\n")
  
  # Safety checks
  if (is.null(prices) || nrow(prices) == 0) {
    stop("Cannot create market data - price data is empty")
  }
  
  # Calculate returns if not provided
  if (is.null(returns)) {
    returns <- ROC(prices, type = "discrete")
    returns <- na.omit(returns)
  }
  
  # Ensure prices has proper dates
  prices <- ensure_date_index(prices)
  price_dates <- index(prices)
  
  # Get VIX data aligned with price dates
  vix_aligned <- get_vix_aligned(price_dates)
  
  # Initialize output matrix with correct number of rows
  n_dates <- length(price_dates)
  indicators_list <- list()
  
  # Add VIX
  indicators_list[["VIX"]] <- as.numeric(vix_aligned)
  
  # 1. Growth indicator (SPY/IEF ratio - stocks vs bonds)
  if (all(c("SPY", "IEF") %in% colnames(prices))) {
    cat("Creating growth indicator (SPY/IEF ratio)...\n")
    indicators_list[["GROWTH"]] <- as.numeric(prices[, "SPY"] / prices[, "IEF"])
  }
  
  # 2. Inflation indicator (TIP/IEF ratio - TIPS vs nominal bonds)
  if (all(c("TIP", "IEF") %in% colnames(prices))) {
    cat("Creating inflation indicator (TIP/IEF ratio)...\n")
    indicators_list[["INFLATION"]] <- as.numeric(prices[, "TIP"] / prices[, "IEF"])
  } else if (all(c("GLD", "IEF") %in% colnames(prices))) {
    # Alternative inflation indicator using gold
    cat("Creating alternative inflation indicator (GLD/IEF ratio)...\n")
    indicators_list[["INFLATION"]] <- as.numeric(prices[, "GLD"] / prices[, "IEF"])
  }
  
  # 3. Credit risk indicator (LQD/IEF ratio - credit vs treasury)
  if (all(c("LQD", "IEF") %in% colnames(prices))) {
    cat("Creating credit risk indicator (LQD/IEF ratio)...\n")
    indicators_list[["CREDIT"]] <- as.numeric(prices[, "LQD"] / prices[, "IEF"])
  }
  
  # 4. Global growth (EFA/IEF ratio - international stocks vs bonds)
  if (all(c("EFA", "IEF") %in% colnames(prices))) {
    cat("Creating global growth indicator (EFA/IEF ratio)...\n")
    indicators_list[["GLOBAL_GROWTH"]] <- as.numeric(prices[, "EFA"] / prices[, "IEF"])
  }
  
  # 5. Risk appetite (SPY/LQD ratio - stocks vs corporate bonds)
  if (all(c("SPY", "LQD") %in% colnames(prices))) {
    cat("Creating risk appetite indicator (SPY/LQD ratio)...\n")
    indicators_list[["RISK_APPETITE"]] <- as.numeric(prices[, "SPY"] / prices[, "LQD"])
  }
  
  # 6. Bond-equity correlation - NEW FROM Z2.1.R
  if (all(c("IEF", "SPY") %in% colnames(returns))) {
    cat("Creating bond-equity correlation indicator...\n")
    bond_returns <- returns[, "IEF"]
    equity_returns <- returns[, "SPY"]
    
    # Combine and calculate rolling correlation
    combined_returns <- merge(bond_returns, equity_returns)
    
    # Calculate rolling correlation with 60-day window
    roll_corr <- rollapply(combined_returns, width = 60, 
                           function(x) cor(x[,1], x[,2], use = "pairwise.complete.obs"), 
                           by.column = FALSE, align = "right")
    
    # Handle NAs at beginning
    roll_corr <- na.locf(roll_corr, fromLast = TRUE, na.rm = FALSE)
    
    indicators_list[["BOND_EQUITY_CORR"]] <- as.numeric(roll_corr)
  }
  
  # 7. Commodity trend indicator - NEW FROM Z2.1.R
  if ("DBC" %in% colnames(prices)) {
    cat("Creating commodity trend indicator...\n")
    # 60-day momentum
    dbc_mom <- ROC(prices[, "DBC"], n = 60, type = "discrete")
    dbc_mom <- na.locf(dbc_mom, fromLast = TRUE, na.rm = FALSE)
    indicators_list[["COMMODITY_TREND"]] <- as.numeric(dbc_mom)
  }
  
  # Convert list to matrix - SAFELY with correct dimensions
  indicators_matrix <- matrix(0, nrow=n_dates, ncol=length(indicators_list))
  colnames(indicators_matrix) <- names(indicators_list)
  
  for (i in 1:length(indicators_list)) {
    indicator_name <- names(indicators_list)[i]
    indicator_values <- indicators_list[[i]]
    
    # Ensure length matches
    if (length(indicator_values) == n_dates) {
      indicators_matrix[, i] <- indicator_values
    } else {
      cat(sprintf("WARNING: Length mismatch for %s. Expected %d, got %d\n", 
                  indicator_name, n_dates, length(indicator_values)))
      # Pad or truncate as needed
      if (length(indicator_values) > n_dates) {
        indicators_matrix[, i] <- indicator_values[1:n_dates]
      } else {
        indicators_matrix[, i] <- c(indicator_values, rep(NA, n_dates - length(indicator_values)))
      }
    }
  }
  
  # Create XTS object with calculated indicators
  indicators_xts <- xts(indicators_matrix, order.by = price_dates)
  
  # Verify indicator data
  cat(sprintf("Created %d indicators: %s\n", 
              ncol(indicators_xts), 
              paste(colnames(indicators_xts), collapse=", ")))
  
  # Calculate Z-scores of indicators for regime detection
  z_score_matrix <- matrix(NA, nrow = n_dates, ncol = ncol(indicators_xts))
  colnames(z_score_matrix) <- paste0(colnames(indicators_xts), "_Z")
  
  for (i in 1:ncol(indicators_xts)) {
    # Get the indicator
    indicator <- as.numeric(indicators_xts[, i])
    
    # Calculate rolling means and standard deviations
    # Use 252 trading days (1 year) as lookback window
    roll_mean <- zoo::rollapply(indicator, width = 252, FUN = mean, 
                                align = "right", fill = NA)
    roll_sd <- zoo::rollapply(indicator, width = 252, FUN = sd, 
                              align = "right", fill = NA)
    
    # Calculate Z-scores
    z_scores <- (indicator - roll_mean) / roll_sd
    
    # Fill NA values at the beginning with 0
    z_scores[is.na(z_scores)] <- 0
    
    # Cap extreme values
    z_scores <- pmin(pmax(z_scores, -3), 3)
    
    # Store in matrix
    z_score_matrix[, i] <- z_scores
  }
  
  # Create z-score XTS object
  z_scores_xts <- xts(z_score_matrix, order.by = price_dates)
  
  # Combine price data, indicators, and z-scores
  market_data <- merge(prices, indicators_xts, z_scores_xts)
  
  # Verify alignment
  cat(sprintf("Final market data: %d rows × %d columns\n", 
              nrow(market_data), ncol(market_data)))
  
  # Add attribute to identify inverse ETFs if any exist
  if (!is.null(attr(prices, "inverse_etf_map"))) {
    attr(market_data, "inverse_etf_map") <- attr(prices, "inverse_etf_map")
  }
  
  return(market_data)
}

# Enhanced transaction costs calculation
get_transaction_costs <- function(tickers) {
  # Base transaction costs by ticker (in basis points)
  base_costs <- list(
    # US Equity ETFs - Large and liquid
    "SPY" = 0.8,    # S&P 500 - extremely liquid
    "IVV" = 1.0,    # S&P 500 alternative
    "VOO" = 1.0,    # S&P 500 alternative
    "QQQ" = 1.2,    # Nasdaq 100
    
    # US Equity ETFs - Less liquid segments
    "IWM" = 1.7,    # Russell 2000 - small caps (ADDED)
    "MDY" = 2.0,    # S&P Midcap
    "IJH" = 2.0,    # S&P Midcap alternative
    
    # International Equity ETFs
    "EFA" = 2.5,    # International Developed Markets
    "VEA" = 2.5,    # International Developed Markets alternative
    "EEM" = 3.0,    # Emerging Markets
    "VWO" = 3.0,    # Emerging Markets alternative
    
    # Fixed Income ETFs - Government
    "IEF" = 1.5,    # 7-10 Year Treasury
    "TLT" = 2.0,    # 20+ Year Treasury
    "SHY" = 1.0,    # 1-3 Year Treasury
    "TIP" = 2.2,    # TIPS (ADDED)
    "VGSH" = 1.2,   # Short-term Treasury
    
    # Fixed Income ETFs - Credit
    "LQD" = 2.5,    # Investment Grade Corporate Bonds
    "VCSH" = 2.0,   # Short-term corporate
    "VCIT" = 2.2,   # Intermediate corporate
    "HYG" = 4.0,    # High Yield Corporate Bonds
    "JNK" = 4.0,    # High Yield alternative
    
    # Commodities
    "GLD" = 1.5,    # Gold
    "IAU" = 1.8,    # Gold alternative
    "SLV" = 2.5,    # Silver
    "USO" = 3.5,    # Oil
    "DBC" = 4.0,    # Diversified Commodities (ADDED)
    "PDBC" = 4.2,   # Commodities alternative
    
    # Real Estate
    "VNQ" = 2.5,    # US Real Estate
    "IYR" = 3.0,    # US Real Estate alternative
    "VNQI" = 4.0,   # International Real Estate
    
    # Inverse ETFs - Always higher costs
    "SH" = 3.5,     # Inverse S&P 500
    "PSQ" = 4.0,    # Inverse QQQ
    "RWM" = 4.5,    # Inverse Russell 2000
    "EUM" = 5.0,    # Inverse Emerging Markets
    "EFZ" = 5.0,    # Inverse EAFE
    "TBF" = 4.0,    # Inverse 7-10 Year Treasury
    "SJB" = 7.0,    # Inverse High Yield
    "DGZ" = 5.5,    # Inverse Gold
    "DRV" = 8.0     # Inverse Real Estate (3x)
  )
  
  # For unknown tickers, estimate based on asset class pattern
  default_costs <- list(
    "Equity_US_Large" = 2.0,
    "Equity_US_Small" = 3.0,
    "Equity_International" = 3.5,
    "Equity_Emerging" = 4.5,
    "Bond_Government" = 2.5,
    "Bond_Corporate" = 3.5,
    "Bond_HighYield" = 5.0,
    "Commodity" = 4.5,
    "Real_Estate" = 4.0,
    "Inverse" = 6.0,
    "Other" = 5.0
  )
  
  # Determine asset class for unknown tickers
  get_default_cost <- function(ticker) {
    # US Large Cap Equity
    if (grepl("^(SPY|VOO|IVV|DIA|QQQ|SPLG|VTI)", ticker)) {
      return(default_costs[["Equity_US_Large"]])
    }
    # US Small/Mid Cap Equity
    else if (grepl("^(IWM|MDY|IJH|IJS|IJR|VO|VB)", ticker)) {
      return(default_costs[["Equity_US_Small"]])
    }
    # International Developed Equity
    else if (grepl("^(EFA|VEA|IEFA|VGK|EWJ|HEDJ|EWU)", ticker)) {
      return(default_costs[["Equity_International"]])
    }
    # Emerging Markets Equity
    else if (grepl("^(EEM|VWO|IEMG|SCHE|FM|FEM)", ticker)) {
      return(default_costs[["Equity_Emerging"]])
    }
    # Government Bonds
    else if (grepl("^(IEF|TLT|SHY|VGSH|VGIT|VGLT|BIL|SCHO|SCHR|TIP|VTIP)", ticker)) {
      return(default_costs[["Bond_Government"]])
    }
    # Corporate Bonds
    else if (grepl("^(LQD|VCSH|VCIT|VCLT|SPIB|IGIB|AGG|BND)", ticker)) {
      return(default_costs[["Bond_Corporate"]])
    }
    # High Yield Bonds
    else if (grepl("^(HYG|JNK|SJNK|USHY|HYLB|BKLN)", ticker)) {
      return(default_costs[["Bond_HighYield"]])
    }
    # Commodities
    else if (grepl("^(GLD|IAU|SLV|USO|UNG|DBC|PDBC|GSG|BCI)", ticker)) {
      return(default_costs[["Commodity"]])
    }
    # Real Estate
    else if (grepl("^(VNQ|IYR|SCHH|RWR|VNQI|RWX)", ticker)) {
      return(default_costs[["Real_Estate"]])
    }
    # Inverse ETFs
    else if (grepl("^(SH|PSQ|DOG|RWM|EUM|EFZ|TBF|SJB|DGZ|DRV)", ticker) || 
             grepl("(SHORT|BEAR|INV|INVERSE)", ticker, ignore.case = TRUE)) {
      return(default_costs[["Inverse"]])
    }
    else {
      return(default_costs[["Other"]])
    }
  }
  
  # Create result vector for all tickers with proper names
  costs <- numeric(length(tickers))
  names(costs) <- tickers
  
  # Assign costs with error handling
  for (i in 1:length(tickers)) {
    ticker <- tickers[i]
    
    # Skip empty/NA tickers
    if (is.na(ticker) || ticker == "") {
      costs[i] <- default_costs[["Other"]]
      next
    }
    
    # Known ticker - use predefined cost
    if (ticker %in% names(base_costs)) {
      costs[i] <- base_costs[[ticker]]
    } 
    # Unknown ticker - estimate based on pattern
    else {
      costs[i] <- get_default_cost(ticker)
    }
  }
  
  # Convert basis points to percentage
  costs <- costs / 10000
  
  # Log the results
  cat("Transaction costs (basis points):\n")
  for (ticker in tickers) {
    cat(sprintf("  %s: %.1f bps\n", ticker, costs[ticker] * 10000))
  }
  
  return(costs)
}

# Calculate cash allocation based on volatility/drawdown
calculate_cash_allocation <- function(current_drawdown, max_drawdown, vol_zscore, max_cash_pct = 0.25) {
  # Safety checks
  if (is.na(current_drawdown) || !is.finite(current_drawdown)) {
    current_drawdown <- 0
  }
  
  if (is.na(vol_zscore) || !is.finite(vol_zscore)) {
    vol_zscore <- 0
  }
  
  # Default - no cash
  cash_pct <- 0
  
  # If drawdown exceeds threshold, increase cash
  if (current_drawdown > 0.6 * max_drawdown) {  # Start at 60% of max drawdown
    # Scale cash linearly from 0 to max as drawdown approaches max
    dd_ratio <- current_drawdown / max_drawdown
    dd_cash_pct <- min(max_cash_pct, (dd_ratio - 0.6) * 2 * max_cash_pct)  # Scale factor of 2
    cash_pct <- max(cash_pct, dd_cash_pct)
  }
  
  # If volatility is extreme, also increase cash
  if (vol_zscore > 1.0) {  # High volatility
    vol_cash_pct <- min(max_cash_pct, (vol_zscore - 1.0) * 0.1)  # 10% per z-score unit above 1.0
    cash_pct <- max(cash_pct, vol_cash_pct)
  }
  
  return(cash_pct)
}

cat("\nPart 1 loaded: Data handling, volatility forecasting, and EWMA covariance\n")
#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - COMPREHENSIVE VERSION (Z4.4.R)
# PART 2: REGIME DETECTION, RISK PARITY OPTIMIZATION, PORTFOLIO CONSTRUCTION
#=============================================================================

# Log system information
cat("\n========================================================\n")
cat(sprintf("Current Date and Time (UTC): %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("========================================================\n")

#=============================================================================
# ENHANCED REGIME DETECTION WITH Z-SCORE SMOOTHING
#=============================================================================

# Calculate Z-scores with improved smoothing
calculate_safe_zscore <- function(current_value, history, smoothing_window = 5) {
  # Safety checks
  if (is.null(current_value) || is.null(history) || length(history) < 10) {
    return(0)  # Default to neutral if insufficient data
  }
  
  # Remove any NA values
  history <- history[!is.na(history)]
  
  # Calculate mean and SD with safety checks
  hist_mean <- mean(history, na.rm = TRUE)
  hist_sd <- sd(history, na.rm = TRUE)
  
  # If SD is too small or zero, return 0
  if (is.na(hist_sd) || hist_sd < 1e-8) {
    return(0)
  }
  
  # Calculate raw Z-score
  raw_zscore <- (current_value - hist_mean) / hist_sd
  raw_zscore <- min(max(raw_zscore, -4), 4)  # Cap at 4 standard deviations
  
  # Apply smoothing with recent Z-scores if possible
  if (smoothing_window > 1 && length(history) >= smoothing_window) {
    # Calculate recent Z-scores
    recent_values <- tail(history, smoothing_window)
    recent_zscores <- (recent_values - hist_mean) / hist_sd
    
    # Cap recent Z-scores too
    recent_zscores <- pmin(pmax(recent_zscores, -4), 4)
    
    # Average with recent Z-scores
    smoothed_zscore <- mean(c(raw_zscore, recent_zscores), na.rm = TRUE)
    return(smoothed_zscore)
  } else {
    # Just return the raw Z-score if smoothing not possible
    return(raw_zscore)
  }
}

# Enhanced regime detection with recalibrated deflation detection
detect_market_regime <- function(market_data, lookback = 252) {
  cat("\n---- ENHANCED REGIME DETECTION ----\n")
  
  # Safety check
  if (is.null(market_data) || nrow(market_data) < lookback) {
    warning("Insufficient market data for regime detection. Using default growth regime.")
    return(list(
      regime = "growth",
      confidence = 0.5,
      z_scores = list(volatility = 0, growth = 0, inflation = 0),
      regime_probabilities = data.frame(
        growth = 1.0,
        reflation = 0.0,
        deflation = 0.0,
        stagflation = 0.0,
        risk_off = 0.0
      )
    ))
  }
  
  # Extract the most recent data point
  latest_data <- tail(market_data, 1)
  
  # Get historical data for z-score calculation
  if (nrow(market_data) <= lookback) {
    # If we don't have enough data, use all but the latest point
    historical_data <- head(market_data, nrow(market_data) - 1)
  } else {
    # Otherwise use the lookback window
    historical_data <- head(tail(market_data, lookback), lookback - 1)
  }
  
  # Initialize z-score container
  z_scores <- list()
  
  # 1. Calculate Volatility Z-score (VIX)
  if ("VIX" %in% colnames(market_data)) {
    vix_history <- as.numeric(historical_data[, "VIX"])
    vix_current <- as.numeric(latest_data[, "VIX"])
    z_scores$volatility <- calculate_safe_zscore(vix_current, vix_history)
    cat(sprintf("VIX Z-score: %.2f (Current: %.1f)\n", 
                z_scores$volatility, vix_current))
  } else {
    z_scores$volatility <- 0
    cat("VIX data not available, using neutral volatility signal\n")
  }
  
  # 2. Calculate Growth Z-score (using stock/bond ratio or direct Z-score if available)
  if ("GROWTH_Z" %in% colnames(market_data)) {
    z_scores$growth <- as.numeric(latest_data[, "GROWTH_Z"])
    cat(sprintf("Growth Z-score: %.2f\n", z_scores$growth))
  } else if ("GROWTH" %in% colnames(market_data)) {
    growth_history <- as.numeric(historical_data[, "GROWTH"])
    growth_current <- as.numeric(latest_data[, "GROWTH"])
    z_scores$growth <- calculate_safe_zscore(growth_current, growth_history)
    cat(sprintf("Growth Z-score: %.2f (Current ratio: %.2f)\n", 
                z_scores$growth, growth_current))
  } else {
    z_scores$growth <- 0
    cat("Growth data not available, using neutral growth signal\n")
  }
  
  # 3. Calculate Inflation Z-score (using TIPS/Treasury ratio or direct Z-score)
  if ("INFLATION_Z" %in% colnames(market_data)) {
    z_scores$inflation <- as.numeric(latest_data[, "INFLATION_Z"])
    cat(sprintf("Inflation Z-score: %.2f\n", z_scores$inflation))
  } else if ("INFLATION" %in% colnames(market_data)) {
    infl_history <- as.numeric(historical_data[, "INFLATION"])
    infl_current <- as.numeric(latest_data[, "INFLATION"])
    z_scores$inflation <- calculate_safe_zscore(infl_current, infl_history)
    cat(sprintf("Inflation Z-score: %.2f (Current ratio: %.2f)\n", 
                z_scores$inflation, infl_current))
  } else {
    z_scores$inflation <- 0
    cat("Inflation data not available, using neutral inflation signal\n")
  }
  
  # 4. Calculate Credit Z-score (using credit spread indicator)
  if ("CREDIT_Z" %in% colnames(market_data)) {
    z_scores$credit <- as.numeric(latest_data[, "CREDIT_Z"])
    cat(sprintf("Credit Z-score: %.2f\n", z_scores$credit))
  } else if ("CREDIT" %in% colnames(market_data)) {
    credit_history <- as.numeric(historical_data[, "CREDIT"])
    credit_current <- as.numeric(latest_data[, "CREDIT"])
    z_scores$credit <- calculate_safe_zscore(credit_current, credit_history)
    cat(sprintf("Credit Z-score: %.2f (Current spread: %.2f)\n", 
                z_scores$credit, credit_current))
  } else {
    z_scores$credit <- 0
    cat("Credit spread data not available, using neutral credit signal\n")
  }
  
  # 5. Calculate Bond-Equity Correlation Z-score - NEW
  if ("BOND_EQUITY_CORR_Z" %in% colnames(market_data)) {
    z_scores$bond_equity_corr <- as.numeric(latest_data[, "BOND_EQUITY_CORR_Z"])
    cat(sprintf("Bond-Equity Correlation Z-score: %.2f\n", z_scores$bond_equity_corr))
  } else if ("BOND_EQUITY_CORR" %in% colnames(market_data)) {
    corr_history <- as.numeric(historical_data[, "BOND_EQUITY_CORR"])
    corr_current <- as.numeric(latest_data[, "BOND_EQUITY_CORR"])
    z_scores$bond_equity_corr <- calculate_safe_zscore(corr_current, corr_history)
    cat(sprintf("Bond-Equity Correlation Z-score: %.2f (Current: %.2f)\n", 
                z_scores$bond_equity_corr, corr_current))
  } else {
    z_scores$bond_equity_corr <- 0
    cat("Bond-Equity Correlation data not available, using neutral signal\n")
  }
  
  # 6. Calculate Commodity Trend Z-score - NEW
  if ("COMMODITY_TREND_Z" %in% colnames(market_data)) {
    z_scores$commodity <- as.numeric(latest_data[, "COMMODITY_TREND_Z"])
    cat(sprintf("Commodity Trend Z-score: %.2f\n", z_scores$commodity))
  } else if ("COMMODITY_TREND" %in% colnames(market_data)) {
    comm_history <- as.numeric(historical_data[, "COMMODITY_TREND"])
    comm_current <- as.numeric(latest_data[, "COMMODITY_TREND"])
    z_scores$commodity <- calculate_safe_zscore(comm_current, comm_history)
    cat(sprintf("Commodity Trend Z-score: %.2f (Current: %.2f)\n", 
                z_scores$commodity, comm_current))
  } else {
    z_scores$commodity <- 0
    cat("Commodity Trend data not available, using neutral signal\n")
  }
  
  # Initialize regime probabilities
  regime_probs <- list(
    growth = 0,
    reflation = 0,
    deflation = 0,
    stagflation = 0,
    risk_off = 0
  )
  
  # Define threshold for binary classification
  threshold <- 0.5
  
  # Volatility signal (high = risk_off)
  vol_signal <- z_scores$volatility > threshold
  
  # Growth signal (positive = growth, negative = contraction)
  growth_signal <- z_scores$growth > threshold
  growth_neg_signal <- z_scores$growth < -threshold
  
  # Inflation signal (positive = high inflation, negative = low inflation)
  inflation_signal <- z_scores$inflation > threshold
  inflation_neg_signal <- z_scores$inflation < -threshold
  
  # Bond-equity correlation signal (positive = bonds don't diversify stocks)
  corr_signal <- !is.null(z_scores$bond_equity_corr) && z_scores$bond_equity_corr > threshold
  
  # Commodity trend signal (negative = deflationary pressure)
  commodity_neg_signal <- !is.null(z_scores$commodity) && z_scores$commodity < -threshold
  
  # RECALIBRATED: Calculate regime probabilities with enhanced deflation detection
  
  # 1. Risk-off regime (high volatility dominates)
  if (vol_signal && z_scores$volatility > 1.0) {
    regime_probs$risk_off = 0.6 + min((z_scores$volatility - 1.0) * 0.1, 0.3)
    
    # Add bond-equity correlation impact - positive correlation increases risk-off probability
    if (corr_signal) {
      regime_probs$risk_off = min(0.95, regime_probs$risk_off + 0.15)
    }
  } else {
    regime_probs$risk_off = max(0, (z_scores$volatility - 0.5) * 0.2)
  }
  
  # 2. Growth regime (positive growth, controlled inflation)
  if (growth_signal && !inflation_signal && !vol_signal) {
    regime_probs$growth = 0.6 + min(z_scores$growth * 0.1, 0.3)
  } else if (z_scores$growth > 0) {
    regime_probs$growth = max(0.1, z_scores$growth * 0.3)
  } else {
    regime_probs$growth = max(0, 0.1 - abs(z_scores$growth) * 0.1)
  }
  
  # 3. Reflation regime (positive growth AND rising inflation)
  if (growth_signal && inflation_signal && !vol_signal) {
    regime_probs$reflation = 0.6 + 
      min((z_scores$growth + z_scores$inflation) * 0.05, 0.3)
  } else if (z_scores$growth > 0 && z_scores$inflation > 0) {
    regime_probs$reflation = max(0, z_scores$growth * 0.2 + z_scores$inflation * 0.2)
  } else {
    regime_probs$reflation = 0
  }
  
  # 4. ENHANCED: Deflation regime (negative growth AND negative inflation)
  # Now considers both commodity trends and credit spreads
  if (growth_neg_signal && inflation_neg_signal && !vol_signal) {
    # Base probability
    regime_probs$deflation = 0.6 + 
      min((abs(z_scores$growth) + abs(z_scores$inflation)) * 0.05, 0.3)
    
    # Enhance with negative commodity trends
    if (commodity_neg_signal) {
      regime_probs$deflation = min(0.95, regime_probs$deflation + 0.15)
    }
    
    # Credit deterioration can also indicate deflation
    if (!is.null(z_scores$credit) && z_scores$credit < -0.7) {
      regime_probs$deflation = min(0.95, regime_probs$deflation + 0.1)
    }
    
  } else if (z_scores$growth < 0 && z_scores$inflation < 0) {
    # Base probability with continuous scaling
    regime_probs$deflation = max(0, abs(z_scores$growth) * 0.2 + abs(z_scores$inflation) * 0.2)
    
    # Enhance with commodity trends and credit signals
    if (commodity_neg_signal) {
      regime_probs$deflation = min(0.9, regime_probs$deflation + 0.1)
    }
    
    if (!is.null(z_scores$credit) && z_scores$credit < -0.5) {
      regime_probs$deflation = min(0.9, regime_probs$deflation + 0.1)
    }
  } else {
    regime_probs$deflation = 0
  }
  
  # 5. Stagflation regime (negative growth AND positive inflation)
  if (growth_neg_signal && inflation_signal && !vol_signal) {
    regime_probs$stagflation = 0.6 + 
      min((abs(z_scores$growth) + z_scores$inflation) * 0.05, 0.3)
  } else if (z_scores$growth < 0 && z_scores$inflation > 0) {
    regime_probs$stagflation = max(0, abs(z_scores$growth) * 0.2 + z_scores$inflation * 0.2)
  } else {
    regime_probs$stagflation = 0
  }
  
  # Ensure minimum probability for each regime
  for (regime in names(regime_probs)) {
    regime_probs[[regime]] = max(0.01, regime_probs[[regime]])
  }
  
  # Normalize probabilities to sum to 1
  total_prob <- sum(unlist(regime_probs))
  for (regime in names(regime_probs)) {
    regime_probs[[regime]] <- regime_probs[[regime]] / total_prob
  }
  
  # Determine the dominant regime
  dominant_regime <- names(which.max(unlist(regime_probs)))
  regime_confidence <- regime_probs[[dominant_regime]]
  
  # Print regime probabilities
  cat("\nREGIME PROBABILITIES:\n")
  for (regime in names(regime_probs)) {
    cat(sprintf("  %s: %.1f%%\n", regime, regime_probs[[regime]] * 100))
  }
  
  cat(sprintf("\nDominant Regime: %s (%.1f%% confidence)\n", 
              dominant_regime, regime_confidence * 100))
  
  # Convert to data frame for easier handling
  regime_probs_df <- as.data.frame(regime_probs)
  
  # Return results
  return(list(
    regime = dominant_regime,
    confidence = regime_confidence,
    z_scores = z_scores,
    regime_probabilities = regime_probs_df
  ))
}

#=============================================================================
# REGIME-BASED ASSET ALLOCATION
#=============================================================================

# Get target asset allocation for each regime
get_regime_weights <- function(regime, asset_classes = NULL) {
  # Define base weights for different regimes
  regime_weights <- list(
    growth = list(
      US_EQUITY = 0.30,
      INTL_DEVELOPED = 0.15,
      EMERGING_MARKETS = 0.10,
      US_TREASURY = 0.10,
      TIPS = 0.05,           # Added TIPS
      CREDIT_IG = 0.15,
      GOLD = 0.05,
      COMMODITIES = 0.00,    # Added COMMODITIES
      REIT = 0.10,
      US_SMALL_CAP = 0.00,   # Added US_SMALL_CAP
      CASH = 0.00
    ),
    
    reflation = list(
      US_EQUITY = 0.20,
      INTL_DEVELOPED = 0.10,
      EMERGING_MARKETS = 0.15,
      US_TREASURY = 0.05,
      TIPS = 0.10,           # Increased TIPS for inflation
      CREDIT_IG = 0.10,
      GOLD = 0.10,
      COMMODITIES = 0.10,    # Increased COMMODITIES for inflation
      REIT = 0.10,
      US_SMALL_CAP = 0.00,
      CASH = 0.00
    ),
    
    deflation = list(
      US_EQUITY = 0.10,
      INTL_DEVELOPED = 0.05,
      EMERGING_MARKETS = 0.00,
      US_TREASURY = 0.40,    # More treasuries in deflation
      TIPS = 0.05,
      CREDIT_IG = 0.15,
      GOLD = 0.10,
      COMMODITIES = 0.00,
      REIT = 0.05,
      US_SMALL_CAP = 0.00,
      CASH = 0.10            # Hold more cash in deflation
    ),
    
    stagflation = list(
      US_EQUITY = 0.10,
      INTL_DEVELOPED = 0.05,
      EMERGING_MARKETS = 0.05,
      US_TREASURY = 0.10,
      TIPS = 0.15,           # More TIPS in stagflation
      CREDIT_IG = 0.10,
      GOLD = 0.20,           # More gold in stagflation
      COMMODITIES = 0.10,    # More commodities in stagflation
      REIT = 0.10,
      US_SMALL_CAP = 0.00,
      CASH = 0.05
    ),
    
    risk_off = list(
      US_EQUITY = 0.05,
      INTL_DEVELOPED = 0.00,
      EMERGING_MARKETS = 0.00,
      US_TREASURY = 0.45,    # Treasury safe haven in risk-off
      TIPS = 0.05,
      CREDIT_IG = 0.10,
      GOLD = 0.15,
      COMMODITIES = 0.00,
      REIT = 0.00,
      US_SMALL_CAP = 0.00,
      CASH = 0.20            # Increased cash in risk-off
    )
  )
  
  # If regime not found, use growth as default
  if (!regime %in% names(regime_weights)) {
    warning(sprintf("Unknown regime '%s', using growth weights", regime))
    regime <- "growth"
  }
  
  # Get weights for the specified regime
  weights <- regime_weights[[regime]]
  
  # If asset_classes provided, return only those weights
  if (!is.null(asset_classes)) {
    requested_weights <- list()
    for (asset in asset_classes) {
      if (asset %in% names(weights)) {
        requested_weights[[asset]] <- weights[[asset]]
      } else {
        requested_weights[[asset]] <- 0  # Default weight of 0 for unknown assets
      }
    }
    return(requested_weights)
  }
  
  return(weights)
}

# Map ETFs to asset classes - EXPANDED TO INCLUDE TIP, IWM, DBC
create_asset_mapping <- function(tickers) {
  mapping <- list()
  
  for (ticker in tickers) {
    # US Equity
    if (ticker %in% c("SPY", "IVV", "VOO", "SPLG", "VTI", "ITOT")) {
      mapping[[ticker]] <- "US_EQUITY"
    }
    # US Small Cap - NEW CATEGORY
    else if (ticker %in% c("IWM", "SCHA", "VB", "IJR")) {
      mapping[[ticker]] <- "US_SMALL_CAP"
    }
    
    # International Developed
    else if (ticker %in% c("EFA", "VEA", "IEFA", "SCHF")) {
      mapping[[ticker]] <- "INTL_DEVELOPED"
    }
    # Emerging Markets
    else if (ticker %in% c("EEM", "VWO", "IEMG", "SCHE")) {
      mapping[[ticker]] <- "EMERGING_MARKETS"
    }
    # US Treasury
    else if (ticker %in% c("IEF", "TLT", "SHY", "VGSH", "BIL", "SCHO", "SCHR")) {
      mapping[[ticker]] <- "US_TREASURY"
    }
    # TIPS - NEW CATEGORY
    else if (ticker %in% c("TIP", "VTIP", "SCHP", "STIP")) {
      mapping[[ticker]] <- "TIPS"
    }
    # Investment Grade Credit
    else if (ticker %in% c("LQD", "VCSH", "VCIT", "IGIB", "IGSB")) {
      mapping[[ticker]] <- "CREDIT_IG"
    }
    # Gold
    else if (ticker %in% c("GLD", "IAU", "SGOL", "GLDM")) {
      mapping[[ticker]] <- "GOLD"
    }
    # Commodities - NEW CATEGORY
    else if (ticker %in% c("DBC", "PDBC", "GSG", "BCI", "USCI")) {
      mapping[[ticker]] <- "COMMODITIES"
    }
    # REITs
    else if (ticker %in% c("VNQ", "IYR", "SCHH", "RWR")) {
      mapping[[ticker]] <- "REIT"
    }
    # High Yield (not in base weights but may be used)
    else if (ticker %in% c("HYG", "JNK", "SJNK", "USHY", "HYLB")) {
      mapping[[ticker]] <- "CREDIT_HY"
    }
    # Inverse ETFs - map to their underlying asset class but mark as inverse
    else if (ticker %in% c("SH", "DOG", "PSQ")) {
      mapping[[ticker]] <- "US_EQUITY_INV"
    }
    else if (ticker %in% c("RWM")) {
      mapping[[ticker]] <- "US_SMALL_CAP_INV" 
    }
    else if (ticker %in% c("EUM", "EFZ")) {
      mapping[[ticker]] <- "INTL_DEVELOPED_INV"
    }
    else if (ticker %in% c("TBF")) {
      mapping[[ticker]] <- "US_TREASURY_INV"
    }
    else if (ticker %in% c("DGZ")) {
      mapping[[ticker]] <- "GOLD_INV"
    }
    else if (ticker %in% c("SJB")) {
      mapping[[ticker]] <- "CREDIT_HY_INV"
    }
    else if (ticker %in% c("DRV")) {
      mapping[[ticker]] <- "REIT_INV"
    }
    # Unknown ticker - mark as OTHER
    else {
      mapping[[ticker]] <- "OTHER"
    }
  }
  
  # Return list mapping tickers to asset classes
  return(mapping)
}

# FIXED: Map asset class weights to ETF weights
map_weights_to_etfs <- function(asset_weights, etf_mapping, preferred_etfs = NULL) {
  # Initialize ETF weights
  etf_weights <- list()
  for (ticker in names(etf_mapping)) {
    etf_weights[[ticker]] <- 0
  }
  
  # Get unique asset classes with non-zero weights
  asset_classes <- unique(unlist(etf_mapping))
  asset_classes <- asset_classes[asset_classes %in% names(asset_weights)]
  
  # For each asset class with weight
  for (asset_class in asset_classes) {
    # Skip if weight is zero
    if (asset_weights[[asset_class]] == 0) {
      next
    }
    
    # Find all ETFs in this asset class
    class_tickers <- names(etf_mapping)[sapply(etf_mapping, function(x) x == asset_class)]
    
    # If we have no ETFs for this asset class, skip
    if (length(class_tickers) == 0) {
      cat(sprintf("Warning: No ETFs found for asset class: %s\n", asset_class))
      next
    }
    
    # If preferred ETFs provided, use them first
    if (!is.null(preferred_etfs) && any(preferred_etfs %in% class_tickers)) {
      preferred_class_tickers <- preferred_etfs[preferred_etfs %in% class_tickers]
      
      # Divide weight evenly among preferred ETFs
      weight_per_etf <- asset_weights[[asset_class]] / length(preferred_class_tickers)
      
      for (ticker in preferred_class_tickers) {
        etf_weights[[ticker]] <- weight_per_etf
      }
    } else {
      # No preferred ETFs for this asset class, use all of them
      
      # If more than 2 ETFs in class, just pick 1-2 to avoid over-diversification
      if (length(class_tickers) > 2) {
        # Just use the first 1-2 ETFs in the list
        class_tickers <- class_tickers[1:min(2, length(class_tickers))]
      }
      
      # Divide weight evenly among all ETFs in class
      weight_per_etf <- asset_weights[[asset_class]] / length(class_tickers)
      
      for (ticker in class_tickers) {
        etf_weights[[ticker]] <- weight_per_etf
      }
    }
  }
  
  # Remove ETFs with zero weight
  etf_weights <- etf_weights[sapply(etf_weights, function(x) x > 0)]
  
  # Normalize weights to ensure they sum to 1
  total_weight <- sum(unlist(etf_weights))
  
  if (total_weight > 0) {
    for (ticker in names(etf_weights)) {
      etf_weights[[ticker]] <- etf_weights[[ticker]] / total_weight
    }
  } else {
    warning("Total ETF weight is zero - using equal weight instead")
    equal_weight <- 1 / length(etf_weights)
    for (ticker in names(etf_weights)) {
      etf_weights[[ticker]] <- equal_weight
    }
  }
  
  # Convert to numeric vector with names before returning
  etf_weights_vector <- unlist(etf_weights)
  names(etf_weights_vector) <- names(etf_weights)
  
  return(etf_weights_vector)  # Return as vector instead of list
}

#=============================================================================
# RISK PARITY OPTIMIZATION WITH ENHANCED FALLBACKS
#=============================================================================

# Risk parity objective function - calculating risk contribution deviation
risk_parity_objective <- function(w, cov_matrix) {
  # Safety check
  if (any(is.na(w)) || any(is.na(cov_matrix))) {
    return(1e10)  # Return a very large number
  }
  
  # Calculate portfolio variance
  port_var <- t(w) %*% cov_matrix %*% w
  
  # Calculate marginal contribution to risk (MCR)
  mcr <- (cov_matrix %*% w) / sqrt(port_var)
  
  # Calculate risk contribution (RC)
  rc <- w * mcr
  
  # Target risk contribution (equal for all assets)
  target_rc <- sqrt(port_var) / length(w)
  
  # Sum squared deviation from target
  rc_deviation <- sum((rc - target_rc)^2)
  
  # Also add penalty for non-normalized weights
  normalization_penalty <- (sum(w) - 1)^2 * 1000
  
  return(rc_deviation + normalization_penalty)
}

# SLSQP optimization method
try_slsqp_optimization <- function(initial_weights, cov_matrix, min_weight, max_weight) {
  cat("Attempting optimization with SLSQP method...\n")
  
  n <- length(initial_weights)
  lower_bounds <- rep(min_weight, n)
  upper_bounds <- rep(max_weight, n)
  
  result <- tryCatch({
    opt_result <- slsqp(
      x0 = initial_weights,
      fn = function(w) risk_parity_objective(w, cov_matrix),
      lower = lower_bounds,
      upper = upper_bounds,
      hin = function(w) -(sum(w) - 1),
      control = list(maxeval = 500, xtol_rel = 1e-4)
    )
    
    if (opt_result$convergence != 0) {
      stop("SLSQP optimizer failed to converge")
    }
    
    opt_result$par
  }, error = function(e) {
    cat("SLSQP optimization failed:", e$message, "\n")
    NULL
  })
  
  return(result)
}

# BFGS as alternative optimization method
try_bfgs_optimization <- function(initial_weights, cov_matrix, min_weight, max_weight) {
  cat("Trying alternate optimization with BFGS...\n")
  
  n <- length(initial_weights)
  
  # Define objective that includes constraint penalty
  penalized_objective <- function(w) {
    # Apply bounds
    w <- pmin(pmax(w, rep(min_weight, n)), rep(max_weight, n))
    
    # Normalize to sum to 1
    w <- w / sum(w)
    
    # Calculate risk parity objective
    return(risk_parity_objective(w, cov_matrix))
  }
  
  # Run optimization
  result <- tryCatch({
    opt_result <- optim(
      par = initial_weights,
      fn = penalized_objective,
      method = "BFGS",
      control = list(maxit = 1000)
    )
    
    # Check convergence
    if (opt_result$convergence != 0) {
      stop("BFGS optimizer failed to converge")
    }
    
    # Normalize result to ensure sum to 1
    result <- opt_result$par / sum(opt_result$par)
    
    # Apply bounds
    result <- pmin(pmax(result, rep(min_weight, n)), rep(max_weight, n))
    result <- result / sum(result)
    
    result
  }, error = function(e) {
    cat("BFGS optimization failed:", e$message, "\n")
    NULL
  })
  
  return(result)
}

# Implement turnover reduction for high turnover scenarios
optimize_for_transaction_costs <- function(prev_weights, current_weights, transaction_costs, turnover_threshold = 0.3) {
  # Skip if either weight vector is missing
  if (is.null(prev_weights) || is.null(current_weights)) {
    return(current_weights)
  }
  
  # Find common tickers
  common_tickers <- intersect(names(prev_weights), names(current_weights))
  
  if (length(common_tickers) > 0) {
    # Calculate turnover and identify big changes
    big_changes <- c()
    total_turnover <- 0
    
    for (ticker in common_tickers) {
      prev_weight <- prev_weights[ticker]
      curr_weight <- current_weights[ticker]
      
      # Skip if either weight is NA
      if (is.na(prev_weight) || is.na(curr_weight)) {
        next
      }
      
      # Calculate change size
      change_size <- abs(curr_weight - prev_weight)
      total_turnover <- total_turnover + change_size
      
      # Identify large changes (>5%)
      if (change_size > 0.05) {
        big_changes <- c(big_changes, ticker)
      }
    }
    
    # If turnover is high, moderate changes
    if (total_turnover > turnover_threshold && length(big_changes) > 0) {
      cat(sprintf("High turnover detected (%.1f%%), moderating changes\n", total_turnover * 100))
      
      # Moderate large position changes
      for (ticker in big_changes) {
        prev_weight <- prev_weights[ticker]
        curr_weight <- current_weights[ticker]
        
        # Blend weights to reduce turnover (70% new, 30% previous)
        blended_weight <- 0.7 * curr_weight + 0.3 * prev_weight
        current_weights[ticker] <- blended_weight
      }
      
      # Re-normalize weights
      current_weights <- current_weights / sum(current_weights)
      
      # Recalculate turnover after blending
      new_turnover <- 0
      for (ticker in common_tickers) {
        prev_weight <- prev_weights[ticker]
        curr_weight <- current_weights[ticker]
        
        if (!is.na(prev_weight) && !is.na(curr_weight)) {
          change_size <- abs(curr_weight - prev_weight)
          new_turnover <- new_turnover + change_size
        }
      }
      
      cat(sprintf("Turnover reduced from %.1f%% to %.1f%%\n", 
                  total_turnover * 100, new_turnover * 100))
    }
  }
  
  return(current_weights)
}

# Enhanced Risk Parity Optimizer with multiple fallback methods
optimize_risk_parity <- function(
    target_weights, cov_matrix, 
    min_weight = 0.01, max_weight = 0.30, 
    max_attempts = 5) {
  
  cat("\n---- RISK PARITY OPTIMIZATION ----\n")
  
  # Verify inputs
  if (is.null(target_weights) || is.null(cov_matrix)) {
    stop("Target weights or covariance matrix is NULL")
  }
  
  # Get tickers from target weights
  tickers <- names(target_weights)
  
  # Convert target weights to vector format
  initial_weights <- unlist(target_weights)
  names(initial_weights) <- tickers
  
  # Extract subset of covariance matrix matching our tickers
  cov_subset <- cov_matrix[tickers, tickers, drop = FALSE]
  
  # Verify covariance matrix
  if (any(is.na(cov_subset))) {
    stop("Covariance matrix contains NA values")
  }
  
  # Ensure covariance matrix is positive definite
  eigen_values <- eigen(cov_subset, symmetric = TRUE, only.values = TRUE)$values
  
  if (min(eigen_values) <= 0 || any(is.na(eigen_values))) {
    cat("Covariance matrix is not positive definite, applying shrinkage\n")
    
    # Shrink to identity matrix
    shrinkage_factor <- 0.1
    n <- nrow(cov_subset)
    shrinkage_target <- diag(diag(cov_subset))
    cov_subset <- (1 - shrinkage_factor) * cov_subset + shrinkage_factor * shrinkage_target
  }
  
  cat(sprintf("Optimizing weights for %d assets with min=%.2f, max=%.2f\n", 
              length(tickers), min_weight, max_weight))
  
  # Attempt different optimization methods until one works
  attempt <- 1
  opt_weights <- NULL
  
  while (is.null(opt_weights) && attempt <= max_attempts) {
    cat(sprintf("Optimization attempt %d of %d\n", attempt, max_attempts))
    
    if (attempt == 1) {
      # First try: SLSQP optimization
      opt_weights <- try_slsqp_optimization(initial_weights, cov_subset, min_weight, max_weight)
      
    } else if (attempt == 2) {
      # Second try: BFGS optimization
      opt_weights <- try_bfgs_optimization(initial_weights, cov_subset, min_weight, max_weight)
      
    } else if (attempt == 3) {
      # Third try: Inverse volatility weighting (simpler but robust)
      cat("Falling back to inverse volatility weighting\n")
      
      asset_vols <- sqrt(diag(cov_subset))
      inv_vols <- 1 / asset_vols
      
      # Handle any NaN or Inf
      inv_vols[is.na(inv_vols) | !is.finite(inv_vols)] <- 0
      
      if (sum(inv_vols) > 0) {
        opt_weights <- inv_vols / sum(inv_vols)
        names(opt_weights) <- tickers
      }
      
    } else if (attempt == 4) {
      # Fourth try: Equal weighting with constraints
      cat("Falling back to constrained equal weighting\n")
      
      n <- length(initial_weights)
      equal_weights <- rep(1/n, n)
      names(equal_weights) <- tickers
      
      # Apply min/max constraints
      equal_weights <- pmin(pmax(equal_weights, min_weight), max_weight)
      
      # Re-normalize to sum to 1
      equal_weights <- equal_weights / sum(equal_weights)
      
      opt_weights <- equal_weights
      
    } else if (attempt == 5) {
      # Last try: Use target weights directly
      cat("Using target weights directly (last resort)\n")
      opt_weights <- initial_weights
      opt_weights <- pmin(pmax(opt_weights, min_weight), max_weight)
      opt_weights <- opt_weights / sum(opt_weights)
    }
    
    # Move to next attempt if current one failed
    attempt <- attempt + 1
  }
  
  # Check if optimization succeeded
  if (is.null(opt_weights) || length(opt_weights) == 0) {
    cat("All optimization methods failed, using equal weights\n")
    opt_weights <- rep(1/length(tickers), length(tickers))
    names(opt_weights) <- tickers
  }
  
  # Post-process to ensure constraints are met
  # Apply min/max constraints
  opt_weights <- pmin(pmax(opt_weights, min_weight), max_weight)
  
  # Re-normalize to sum to 1
  opt_weights <- opt_weights / sum(opt_weights)
  
  # Verify results
  cat("\nOptimized weights:\n")
  for (ticker in names(opt_weights)) {
    cat(sprintf("  %s: %.1f%%\n", ticker, 100 * opt_weights[ticker]))
  }
  
  return(opt_weights)
}

cat("\nPart 2 loaded: Regime detection and risk parity optimization\n")
#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - COMPREHENSIVE VERSION (Z4.4.R)
# PART 3: BACKTESTING, PERFORMANCE EVALUATION, AND CASH MANAGEMENT
#=============================================================================

# Log system information
cat("\n========================================================\n")
cat(sprintf("Current Date and Time (UTC): %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("========================================================\n")

#=============================================================================
# PERFORMANCE CALCULATION AND CASH MANAGEMENT
#=============================================================================

# Calculate portfolio performance with enhanced cash management
calculate_portfolio_performance <- function(
    prices, weights, rebalance_dates = NULL, 
    lookback_window = 252, frequency = "monthly",
    track_regimes = FALSE, market_data = NULL,
    transaction_costs = NULL, max_cash_pct = 0.25) {
  
  cat("\n---- PORTFOLIO PERFORMANCE CALCULATION ----\n")
  
  # Ensure we have valid input data
  if (is.null(prices) || ncol(prices) == 0) {
    stop("No price data provided")
  }
  
  # Convert any named list to named vector
  if (is.list(weights) && !is.xts(weights)) {
    weights <- unlist(weights)
  }
  
  # Determine tickers to use (intersection of weights and prices)
  tickers <- intersect(names(weights), colnames(prices))
  
  if (length(tickers) == 0) {
    stop("No matching tickers between weights and price data")
  }
  
  # Use only the common tickers and renormalize weights
  subset_weights <- weights[tickers]
  subset_weights <- subset_weights / sum(subset_weights)
  
  # Verify normalized weights
  cat(sprintf("Using %d assets for performance calculation\n", length(tickers)))
  
  # Generate rebalance dates if not provided
  if (is.null(rebalance_dates)) {
    # Get all dates from the price data
    all_dates <- index(prices)
    
    # Default to monthly rebalancing (end of month)
    if (frequency == "monthly") {
      months <- format(all_dates, "%Y-%m")
      month_ends <- tapply(all_dates, months, max)
      rebalance_dates <- as.Date(month_ends)
    } else if (frequency == "quarterly") {
      quarters <- format(all_dates, "%Y-Q%q")
      quarter_ends <- tapply(all_dates, quarters, max)
      rebalance_dates <- as.Date(quarter_ends)
    } else if (frequency == "yearly") {
      years <- format(all_dates, "%Y")
      year_ends <- tapply(all_dates, years, max)
      rebalance_dates <- as.Date(year_ends)
    } else {
      # Default to monthly if invalid frequency
      months <- format(all_dates, "%Y-%m")
      month_ends <- tapply(all_dates, months, max)
      rebalance_dates <- as.Date(month_ends)
    }
    
    # Ensure chronological order
    rebalance_dates <- sort(rebalance_dates)
    
    cat(sprintf("Generated %d rebalance dates using %s frequency\n", 
                length(rebalance_dates), frequency))
  } else {
    cat(sprintf("Using %d provided rebalance dates\n", length(rebalance_dates)))
  }
  
  # Calculate returns from price data
  returns <- ROC(prices, type = "discrete")
  
  # Initialize portfolio values, weights, and regime tracking
  port_values <- xts(rep(1, nrow(prices)), order.by = index(prices))
  colnames(port_values) <- "PORT_VALUE"
  
  # For tracking weights over time
  weight_matrix <- matrix(0, nrow = nrow(prices), ncol = length(tickers))
  colnames(weight_matrix) <- tickers
  weight_history <- xts(weight_matrix, order.by = index(prices))
  
  # Track cash position separately
  cash_weights <- xts(rep(0, nrow(prices)), order.by = index(prices))
  colnames(cash_weights) <- "CASH"
  
  # For tracking regimes over time (if requested)
  if (track_regimes && !is.null(market_data)) {
    regime_history <- xts(
      matrix("", nrow = nrow(prices), ncol = 1), 
      order.by = index(prices)
    )
    colnames(regime_history) <- "REGIME"
  }
  
  # Get transaction costs if not provided
  if (is.null(transaction_costs)) {
    transaction_costs <- get_transaction_costs(tickers)
  }
  
  # Tracking variables
  current_weights <- subset_weights
  prev_weights <- NULL
  current_cash <- 0
  total_turnover <- 0
  total_cost <- 0
  
  # Track portfolio drawdown for cash management
  equity_curve <- cumprod(1 + c(0, rep(0, nrow(prices) - 1)))
  max_equity <- equity_curve[1]
  max_drawdown <- 0
  current_drawdown <- 0
  
  # For volatility tracking
  return_history <- c()
  vol_zscore <- 0
  port_vols <- rep(NA, nrow(prices))
  
  # Loop through the dates
  for (i in 1:nrow(prices)) {
    date <- index(prices)[i]
    
    # If this is a rebalance date or the first date, update weights
    if (date %in% rebalance_dates || i == 1) {
      cat(sprintf("Rebalancing on %s\n", as.character(date)))
      
      # Store previous weights for turnover calculation
      prev_weights <- current_weights
      
      # If tracking regimes, detect current regime and update weights
      if (track_regimes && !is.null(market_data)) {
        # Find relevant market data (up to current date)
        market_subset <- market_data[index(market_data) <= date, ]
        
        if (nrow(market_subset) > lookback_window) {
          # Detect market regime
          regime_result <- detect_market_regime(market_subset, lookback = lookback_window)
          current_regime <- regime_result$regime
          
          # Get target weights for this regime
          regime_weights <- get_regime_weights(current_regime)
          
          # Map asset class weights to ETFs
          etf_mapping <- create_asset_mapping(tickers)
          mapped_weights <- map_weights_to_etfs(regime_weights, etf_mapping)
          
          # FIXED: Ensure mapped weights are in the correct format
          mapped_weights <- ensure_numeric_weights(mapped_weights)
          
          # Store updated weights
          current_weights <- mapped_weights
          
          # Store regime
          regime_history[date] <- current_regime
          
          cat(sprintf("  Detected regime: %s\n", current_regime))
        }
      }
      
      # Calculate turnover
      if (!is.null(prev_weights)) {
        # Ensure both weight vectors are properly formatted
        prev_weights <- ensure_numeric_weights(prev_weights)
        current_weights <- ensure_numeric_weights(current_weights)
        
        # Find common tickers
        common_tickers <- intersect(names(prev_weights), names(current_weights))
        
        turnover <- 0
        if (length(common_tickers) > 0) {
          for (ticker in common_tickers) {
            # Additional type safety
            prev_w <- as.numeric(prev_weights[ticker])
            curr_w <- as.numeric(current_weights[ticker])
            
            # Check for NA/NaN
            if (is.na(prev_w) || is.na(curr_w)) next
            
            turnover <- turnover + abs(curr_w - prev_w)
          }
          
          # Add turnover from tickers that were removed
          removed_tickers <- setdiff(names(prev_weights), names(current_weights))
          if (length(removed_tickers) > 0) {
            removed_weights <- prev_weights[removed_tickers]
            removed_weights <- removed_weights[!is.na(removed_weights)]
            turnover <- turnover + sum(removed_weights)
          }
          
          # Add turnover from tickers that were added
          added_tickers <- setdiff(names(current_weights), names(prev_weights))
          if (length(added_tickers) > 0) {
            added_weights <- current_weights[added_tickers]
            added_weights <- added_weights[!is.na(added_weights)]
            turnover <- turnover + sum(added_weights)
          }
          
          total_turnover <- total_turnover + turnover
          
          # Calculate transaction cost
          rebalance_cost <- 0
          for (ticker in union(names(prev_weights), names(current_weights))) {
            prev_w <- ifelse(ticker %in% names(prev_weights), as.numeric(prev_weights[ticker]), 0)
            curr_w <- ifelse(ticker %in% names(current_weights), as.numeric(current_weights[ticker]), 0)
            
            # Skip if either is NA
            if (is.na(prev_w) || is.na(curr_w)) next
            
            # Cost is proportional to weight change
            if (ticker %in% names(transaction_costs)) {
              rebalance_cost <- rebalance_cost + abs(curr_w - prev_w) * transaction_costs[ticker]
            } else {
              # Default cost if ticker not found
              rebalance_cost <- rebalance_cost + abs(curr_w - prev_w) * 0.0005
            }
          }
          
          total_cost <- total_cost + rebalance_cost
          
          cat(sprintf("  Turnover: %.2f%%, Transaction cost: %.2f%%\n", 
                      100 * turnover, 100 * rebalance_cost))
        }
        
        # Update weights using transaction cost optimization
        current_weights <- optimize_for_transaction_costs(prev_weights, current_weights, 
                                                          transaction_costs, 0.30)
      }
      
      # Calculate portfolio volatility over recent history
      if (length(return_history) > 60) {
        recent_vol <- sd(tail(return_history, 60), na.rm = TRUE) * sqrt(252)
        
        # Get volatility Z-score for cash management
        if (length(return_history) > 120) {
          vol_history <- rollapply(
            c(return_history, 0), 
            width = 60, 
            FUN = function(x) sd(x, na.rm = TRUE) * sqrt(252),
            by = 1, 
            align = "right"
          )
          
          vol_mean <- mean(vol_history, na.rm = TRUE)
          vol_sd <- sd(vol_history, na.rm = TRUE)
          
          if (vol_sd > 0) {
            vol_zscore <- (recent_vol - vol_mean) / vol_sd
          } else {
            vol_zscore <- 0
          }
        }
      } else {
        recent_vol <- NA
      }
      
      # Store portfolio volatility
      port_vols[i] <- ifelse(!is.na(recent_vol), recent_vol, 0)
      
      # ENHANCED CASH MANAGEMENT - from Z2.1.R
      # Calculate current drawdown for cash management
      if (i > 1) {
        current_drawdown <- 1 - equity_curve[i-1] / max_equity
        max_drawdown <- max(max_drawdown, current_drawdown)
        
        # Calculate cash allocation based on drawdown and volatility
        current_cash <- calculate_cash_allocation(
          current_drawdown = current_drawdown,
          max_drawdown = max_drawdown,
          vol_zscore = vol_zscore,
          max_cash_pct = max_cash_pct
        )
        
        if (current_cash > 0) {
          cat(sprintf("  Cash allocation: %.1f%% (Drawdown: %.1f%%, Vol Z-score: %.1f)\n", 
                      100 * current_cash, 100 * current_drawdown, vol_zscore))
          
          # Adjust weights for cash allocation
          for (ticker in names(current_weights)) {
            current_weights[ticker] <- current_weights[ticker] * (1 - current_cash)
          }
        }
      }
    }
    
    # Update weight history (using weight vector with correct tickers)
    for (ticker in tickers) {
      if (ticker %in% names(current_weights)) {
        weight_history[date, ticker] <- current_weights[ticker]
      }
    }
    
    # Update cash weight history
    cash_weights[date] <- current_cash
    
    # Calculate portfolio return for this date
    if (i > 1) {
      daily_return <- 0
      
      for (ticker in names(current_weights)) {
        # Skip if ticker not in returns or weight is zero
        if (!(ticker %in% colnames(returns)) || current_weights[ticker] == 0) {
          next
        }
        
        ticker_return <- as.numeric(returns[date, ticker])
        
        if (!is.na(ticker_return)) {
          daily_return <- daily_return + current_weights[ticker] * ticker_return
        }
      }
      
      # Cash portion earns 0% (could be changed to short-term rate)
      # daily_return = daily_return * (1 - current_cash) + current_cash * 0
      
      # Update portfolio value
      port_values[date] <- as.numeric(port_values[i-1]) * (1 + daily_return)
      equity_curve[i] <- equity_curve[i-1] * (1 + daily_return)
      
      # Update maximum equity and drawdown tracking
      if (equity_curve[i] > max_equity) {
        max_equity <- equity_curve[i]
      }
      
      # Add return to history for volatility calculation
      return_history <- c(return_history, daily_return)
    }
  }
  
  # Calculate portfolio statistics
  port_returns <- ROC(port_values, type = "discrete")
  
  stats <- list(
    volatility = sd(port_returns, na.rm = TRUE) * sqrt(252),
    sharpe = mean(port_returns, na.rm = TRUE) / sd(port_returns, na.rm = TRUE) * sqrt(252),
    mean_return = mean(port_returns, na.rm = TRUE) * 252,
    max_drawdown = maxDrawdown(port_returns),
    total_return = as.numeric(tail(port_values, 1)) / as.numeric(port_values[1]) - 1,
    turnover_per_rebalance = total_turnover / length(rebalance_dates),
    total_cost = total_cost
  )
  
  # Report results
  cat("\nPORTFOLIO PERFORMANCE SUMMARY:\n")
  cat(sprintf("  Total Return: %.2f%%\n", stats$total_return * 100))
  cat(sprintf("  Annualized Return: %.2f%%\n", stats$mean_return * 100))
  cat(sprintf("  Annualized Volatility: %.2f%%\n", stats$volatility * 100))
  cat(sprintf("  Sharpe Ratio: %.2f\n", stats$sharpe))
  cat(sprintf("  Maximum Drawdown: %.2f%%\n", stats$max_drawdown * 100))
  cat(sprintf("  Avg. Turnover per Rebalance: %.2f%%\n", stats$turnover_per_rebalance * 100))
  cat(sprintf("  Total Transaction Cost: %.2f%%\n", stats$total_cost * 100))
  
  # Create results list
  results <- list(
    portfolio_values = port_values,
    portfolio_returns = port_returns,
    weight_history = weight_history,
    cash_history = cash_weights,
    stats = stats,
    volatility_history = xts(port_vols, order.by = index(prices))
  )
  
  # Add regime history if tracked
  if (track_regimes && !is.null(market_data)) {
    results$regime_history <- regime_history
  }
  
  return(results)
}

#=============================================================================
# BACKTESTING PROCESS AND PERFORMANCE EVALUATION
#=============================================================================

# Run comprehensive backtest with enhanced features
run_enhanced_backtest <- function(
    tickers, 
    start_date, 
    end_date = Sys.Date(), 
    rebalance_frequency = "monthly",
    lookback_window = 252,
    min_weight = 0.01,
    max_weight = 0.30,
    max_cash = 0.25,
    include_inverse = TRUE) {
  
  cat("\n========== ENHANCED RISK PARITY BACKTEST ==========\n")
  cat("Starting comprehensive backtest with the following settings:\n")
  cat(sprintf("  Assets: %s\n", paste(tickers, collapse=", ")))
  cat(sprintf("  Period: %s to %s\n", start_date, end_date))
  cat(sprintf("  Rebalance frequency: %s\n", rebalance_frequency))
  cat(sprintf("  Asset constraints: Min %.1f%%, Max %.1f%%\n", min_weight*100, max_weight*100))
  cat(sprintf("  Maximum cash allocation: %.1f%%\n", max_cash*100))
  
  # Step 1: Load market data with proper error handling
  prices <- load_market_data(
    tickers = tickers,
    start_date = start_date,
    end_date = end_date,
    include_inverse = include_inverse
  )
  
  # Calculate returns for analysis
  returns <- ROC(prices, type = "discrete")
  returns <- returns[-1, ]  # Remove first NA row
  
  # Step 2: Create market regime indicators
  market_data <- create_market_data(prices, returns)
  
  # Step 3: Create rebalance dates
  all_dates <- index(prices)
  
  if (rebalance_frequency == "monthly") {
    months <- format(all_dates, "%Y-%m")
    month_ends <- tapply(all_dates, months, max)
    rebalance_dates <- as.Date(month_ends)
  } else if (rebalance_frequency == "quarterly") {
    quarters <- format(all_dates, "%Y-Q%q")
    quarter_ends <- tapply(all_dates, quarters, max)
    rebalance_dates <- as.Date(quarter_ends)
  } else if (rebalance_frequency == "yearly") {
    years <- format(all_dates, "%Y")
    year_ends <- tapply(all_dates, years, max)
    rebalance_dates <- as.Date(year_ends)
  } else {
    # Default to monthly if invalid frequency
    months <- format(all_dates, "%Y-%m")
    month_ends <- tapply(all_dates, months, max)
    rebalance_dates <- as.Date(month_ends)
  }
  
  # Remove future rebalance dates
  rebalance_dates <- sort(rebalance_dates)
  
  # Step 4: Calculate transaction costs
  transaction_costs <- get_transaction_costs(colnames(prices))
  
  # Step 5: Compute covariance matrix once for initial weights
  cat("\nCalculating initial covariance matrix...\n")
  
  # Use first year of data for initial covariance
  initial_returns <- returns[1:min(252, nrow(returns)), ]
  initial_cov <- estimate_ewma_covariance(initial_returns, lambda = 0.94)
  
  # Step 6: Set up initial weights - equal risk contribution
  n_assets <- ncol(prices)
  initial_weights <- rep(1/n_assets, n_assets)
  names(initial_weights) <- colnames(prices)
  
  # Step 7: Run portfolio simulation with dynamic regime detection
  cat("\nRunning portfolio simulation with dynamic regime detection...\n")
  
  results <- calculate_portfolio_performance(
    prices = prices, 
    weights = initial_weights, 
    rebalance_dates = rebalance_dates,
    lookback_window = lookback_window,
    frequency = rebalance_frequency,
    track_regimes = TRUE,
    market_data = market_data,
    transaction_costs = transaction_costs,
    max_cash_pct = max_cash
  )
  
  # Step 8: Create benchmark for comparison
  cat("\nCalculating benchmark performance...\n")
  
  # Create equal-weight benchmark
  equal_weights <- rep(1/length(tickers), length(tickers))
  names(equal_weights) <- tickers
  
  benchmark_results <- calculate_portfolio_performance(
    prices = prices[, tickers], 
    weights = equal_weights,
    rebalance_dates = rebalance_dates,
    frequency = rebalance_frequency,
    track_regimes = FALSE,
    transaction_costs = transaction_costs[tickers]
  )
  
  # Try to create SPY benchmark if available
  spy_results <- NULL
  if ("SPY" %in% colnames(prices)) {
    spy_weights <- c(1)
    names(spy_weights) <- "SPY"
    
    spy_results <- calculate_portfolio_performance(
      prices = prices[, "SPY", drop=FALSE], 
      weights = spy_weights,
      track_regimes = FALSE
    )
  }
  
  # Step 9: Calculate comparison metrics
  cat("\nCALCULATING COMPARATIVE METRICS\n")
  
  # Compare strategy to equal-weight
  relative_sharpe <- results$stats$sharpe / benchmark_results$stats$sharpe
  relative_return <- results$stats$mean_return / benchmark_results$stats$mean_return
  relative_vol <- results$stats$volatility / benchmark_results$stats$volatility
  relative_dd <- results$stats$max_drawdown / benchmark_results$stats$max_drawdown
  
  cat("\nCOMPARISON TO EQUAL-WEIGHT BENCHMARK:\n")
  cat(sprintf("  Relative Sharpe Ratio: %.2fx\n", relative_sharpe))
  cat(sprintf("  Relative Return: %.2fx\n", relative_return))
  cat(sprintf("  Relative Volatility: %.2fx\n", relative_vol))
  cat(sprintf("  Relative Max Drawdown: %.2fx\n", relative_dd))
  
  # Compare to SPY if available
  if (!is.null(spy_results)) {
    spy_relative_sharpe <- results$stats$sharpe / spy_results$stats$sharpe
    spy_relative_return <- results$stats$mean_return / spy_results$stats$mean_return
    spy_relative_vol <- results$stats$volatility / spy_results$stats$volatility
    spy_relative_dd <- results$stats$max_drawdown / spy_results$stats$max_drawdown
    
    cat("\nCOMPARISON TO SPY BENCHMARK:\n")
    cat(sprintf("  Relative Sharpe Ratio: %.2fx\n", spy_relative_sharpe))
    cat(sprintf("  Relative Return: %.2fx\n", spy_relative_return))
    cat(sprintf("  Relative Volatility: %.2fx\n", spy_relative_vol))
    cat(sprintf("  Relative Max Drawdown: %.2fx\n", spy_relative_dd))
  }
  
  # Step 10: Calculate drawdown statistics
  strategy_dd <- PerformanceAnalytics::Drawdowns(results$portfolio_returns)
  equal_dd <- PerformanceAnalytics::Drawdowns(benchmark_results$portfolio_returns)
  
  cat("\nDRAWDOWN ANALYSIS:\n")
  cat(sprintf("  Strategy Max Drawdown: %.2f%%\n", min(strategy_dd) * 100))
  cat(sprintf("  Equal-Weight Max Drawdown: %.2f%%\n", min(equal_dd) * 100))
  
  # Calculate drawdown recovery statistics
  drawdown_table <- table.Drawdowns(results$portfolio_returns, 5)
  cat("\nTOP 5 DRAWDOWN PERIODS:\n")
  print(drawdown_table)
  
  # Step 11: Return comprehensive results
  return(list(
    strategy = results,
    equal_weight = benchmark_results,
    spy = spy_results,
    prices = prices,
    market_data = market_data,
    comparison = list(
      vs_equal = list(
        sharpe = relative_sharpe,
        return = relative_return,
        volatility = relative_vol,
        drawdown = relative_dd
      ),
      vs_spy = if(!is.null(spy_results)) list(
        sharpe = spy_relative_sharpe,
        return = spy_relative_return,
        volatility = spy_relative_vol,
        drawdown = spy_relative_dd
      ) else NULL
    )
  ))
}

#=============================================================================
# POSITION SIZE CALCULATION AND RECOMMENDATION OUTPUT
#=============================================================================

# Generate current position recommendations with better formatting
generate_position_recommendations <- function(backtest_results, portfolio_value = 100000) {
  cat("\n========== CURRENT PORTFOLIO RECOMMENDATIONS ==========\n")
  
  # Get the most recent weights
  weight_history <- backtest_results$strategy$weight_history
  cash_history <- backtest_results$strategy$cash_history
  
  # Extract the most recent date
  most_recent_date <- tail(index(weight_history), 1)
  
  # Get weights for that date
  current_weights <- as.numeric(tail(weight_history, 1))
  names(current_weights) <- colnames(weight_history)
  
  # Get cash allocation
  current_cash <- as.numeric(tail(cash_history, 1))
  
  # Remove zero weights
  current_weights <- current_weights[current_weights > 0.001]
  
  # Get current prices
  prices <- backtest_results$prices
  current_prices <- as.numeric(tail(prices[, names(current_weights)], 1))
  names(current_prices) <- names(current_weights)
  
  # Calculate position sizes
  positions <- list()
  
  # Get regime if available
  if (!is.null(backtest_results$strategy$regime_history)) {
    current_regime <- as.character(tail(backtest_results$strategy$regime_history, 1))
  } else {
    current_regime <- "unknown"
  }
  
  # Format the date
  formatted_date <- format(most_recent_date, "%B %d, %Y")
  
  cat(sprintf("\nAs of %s (Regime: %s)\n", formatted_date, current_regime))
  cat(sprintf("Portfolio Value: $%s\n", formatC(portfolio_value, format="f", big.mark=",", digits=0)))
  
  if (current_cash > 0.001) {
    cat(sprintf("Cash Allocation: %.1f%% ($%s)\n", 
                current_cash * 100, 
                formatC(portfolio_value * current_cash, format="f", big.mark=",", digits=0)))
    
    # Adjust portfolio value for cash
    invested_value <- portfolio_value * (1 - current_cash)
  } else {
    invested_value <- portfolio_value
  }
  
  # Create a table of positions
  position_table <- data.frame(
    Ticker = names(current_weights),
    Allocation = current_weights * 100,
    Price = current_prices,
    Shares = floor((invested_value * current_weights) / current_prices),
    Value = 0,
    stringsAsFactors = FALSE
  )
  
  # Calculate values
  position_table$Value <- position_table$Shares * position_table$Price
  
  # Sort by allocation (descending)
  position_table <- position_table[order(-position_table$Allocation), ]
  
  # Format for display
  position_table$Allocation <- sprintf("%.1f%%", position_table$Allocation)
  position_table$Price <- sprintf("$%.2f", position_table$Price)
  position_table$Value <- sprintf("$%s", formatC(position_table$Value, format="f", big.mark=",", digits=0))
  
  # Print the table
  cat("\nRECOMMENDED POSITIONS:\n")
  print(position_table, row.names=FALSE)
  
  # Calculate total invested
  total_invested <- sum(as.numeric(gsub("[$,]", "", position_table$Value)))
  cash_value <- portfolio_value - total_invested
  
  cat(sprintf("\nTotal Invested: $%s", formatC(total_invested, format="f", big.mark=",", digits=0)))
  cat(sprintf("\nRemaining Cash: $%s", formatC(cash_value, format="f", big.mark=",", digits=0)))
  
  return(position_table)
}

# Plot performance comparison
plot_performance_comparison <- function(backtest_results, title = "Performance Comparison") {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    # Extract portfolio values
    strategy_values <- backtest_results$strategy$portfolio_values
    equal_values <- backtest_results$equal_weight$portfolio_values
    
    # Add SPY if available
    if (!is.null(backtest_results$spy)) {
      spy_values <- backtest_results$spy$portfolio_values
      
      # Combine into one data frame
      combined_values <- merge(strategy_values, equal_values, spy_values)
      colnames(combined_values) <- c("Strategy", "Equal Weight", "S&P 500")
    } else {
      # Just strategy and equal weight
      combined_values <- merge(strategy_values, equal_values)
      colnames(combined_values) <- c("Strategy", "Equal Weight")
    }
    
    # Convert to long format for ggplot
    combined_df <- data.frame(
      Date = index(combined_values),
      combined_values
    )
    
    df_long <- reshape2::melt(combined_df, id.vars = "Date", variable.name = "Portfolio", value.name = "Value")
    
    # Create the plot
    p <- ggplot(df_long, aes(x = Date, y = Value, color = Portfolio)) +
      geom_line() +
      theme_minimal() +
      labs(title = title, y = "Portfolio Value", x = "") +
      scale_y_log10() +
      theme(legend.position = "bottom")
    
    print(p)
    
    # Also create a drawdown chart
    strategy_dd <- PerformanceAnalytics::Drawdowns(backtest_results$strategy$portfolio_returns)
    equal_dd <- PerformanceAnalytics::Drawdowns(backtest_results$equal_weight$portfolio_returns)
    
    if (!is.null(backtest_results$spy)) {
      spy_dd <- PerformanceAnalytics::Drawdowns(backtest_results$spy$portfolio_returns)
      combined_dd <- merge(strategy_dd, equal_dd, spy_dd)
      colnames(combined_dd) <- c("Strategy", "Equal Weight", "S&P 500")
    } else {
      combined_dd <- merge(strategy_dd, equal_dd)
      colnames(combined_dd) <- c("Strategy", "Equal Weight")
    }
    
    # Convert to long format for drawdown plot
    dd_df <- data.frame(
      Date = index(combined_dd),
      combined_dd
    )
    
    dd_long <- reshape2::melt(dd_df, id.vars = "Date", variable.name = "Portfolio", value.name = "Drawdown")
    
    # Create the drawdown plot
    p_dd <- ggplot(dd_long, aes(x = Date, y = Drawdown, color = Portfolio)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Drawdown Comparison", y = "Drawdown", x = "") +
      scale_y_continuous(labels = scales::percent) +
      theme(legend.position = "bottom")
    
    print(p_dd)
    
    return(list(performance_plot = p, drawdown_plot = p_dd))
  } else {
    cat("ggplot2 package not available for plotting\n")
    return(NULL)
  }
}

#=============================================================================
# MAIN EXECUTION
#=============================================================================

# Define the asset universe (expanded to include TIP, IWM, DBC)
tickers <- c(
  # US Equity
  "SPY",   # S&P 500
  "QQQ",   # Nasdaq 100
  "IWM",   # Russell 2000 Small Cap (NEW)
  
  # International Equity
  "EFA",   # Developed Markets
  "EEM",   # Emerging Markets
  
  # Fixed Income
  "IEF",   # 7-10 Year Treasury
  "TLT",   # 20+ Year Treasury
  "LQD",   # Investment Grade Corporate Bonds
  "HYG",   # High Yield Corporate Bonds
  "TIP",   # Treasury Inflation-Protected Securities (NEW)
  
  # Alternatives
  "GLD",   # Gold
  "DBC",   # Commodities (NEW)
  "VNQ"    # Real Estate
)

# Define backtest parameters
start_date <- "2010-01-01"
end_date <- Sys.Date()
rebalance_frequency <- "monthly"
min_weight <- 0.01
max_weight <- 0.30
max_cash <- 0.25  # Maximum cash allocation (25%)

# Run the backtest
backtest_results <- run_enhanced_backtest(
  tickers = tickers,
  start_date = start_date,
  end_date = end_date,
  rebalance_frequency = rebalance_frequency,
  min_weight = min_weight,
  max_weight = max_weight,
  max_cash = max_cash,
  include_inverse = TRUE
)

# Generate current recommendations
recommendations <- generate_position_recommendations(backtest_results)

# Plot performance comparison
plots <- plot_performance_comparison(backtest_results)

# Display regime history analysis if available
if (!is.null(backtest_results$strategy$regime_history)) {
  regime_history <- backtest_results$strategy$regime_history
  regime_counts <- table(as.character(regime_history))
  regime_pct <- prop.table(regime_counts) * 100
  
  cat("\n========== REGIME ANALYSIS ==========\n")
  cat("Distribution of market regimes during backtest period:\n")
  
  regime_table <- data.frame(
    Regime = names(regime_counts),
    Days = as.numeric(regime_counts),
    Percentage = sprintf("%.1f%%", regime_pct)
  )
  
  print(regime_table)
  
  # Analysis of returns by regime
  portfolio_returns <- backtest_results$strategy$portfolio_returns
  
  if (nrow(portfolio_returns) == nrow(regime_history)) {
    cat("\nPerformance by regime:\n")
    
    for (regime in names(regime_counts)) {
      regime_returns <- portfolio_returns[regime_history == regime]
      
      if (length(regime_returns) > 0) {
        annualized_return <- mean(regime_returns, na.rm=TRUE) * 252 * 100
        annualized_vol <- sd(regime_returns, na.rm=TRUE) * sqrt(252) * 100
        sharpe <- annualized_return / annualized_vol
        
        cat(sprintf("  %s: Return = %.2f%%, Vol = %.2f%%, Sharpe = %.2f\n",
                    regime, annualized_return, annualized_vol, sharpe))
      }
    }
  }
}

# Analyze weight changes over time
weight_history <- backtest_results$strategy$weight_history
cash_history <- backtest_results$strategy$cash_history

# Calculate average weights
avg_weights <- colMeans(weight_history, na.rm=TRUE)
avg_weights <- sort(avg_weights[avg_weights > 0.01], decreasing=TRUE)

cat("\n========== ALLOCATION ANALYSIS ==========\n")
cat("Average asset allocations during backtest period:\n")

for (ticker in names(avg_weights)) {
  cat(sprintf("  %s: %.1f%%\n", ticker, avg_weights[ticker] * 100))
}

cat(sprintf("\nAverage cash allocation: %.1f%%\n", 
            mean(as.numeric(cash_history), na.rm=TRUE) * 100))
cat(sprintf("Maximum cash allocation: %.1f%%\n", 
            max(as.numeric(cash_history), na.rm=TRUE) * 100))

cat("\n========== ENHANCED RISK PARITY STRATEGY COMPLETE ==========\n")
cat("Thank you for using the Enhanced Risk Parity System Z4.4.R\n")