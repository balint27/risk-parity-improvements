#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - VERSION Z6
# PART 1: DATA HANDLING, VOLATILITY ESTIMATION, AND COVARIANCE CALCULATION
#=============================================================================

# Load required packages with reliable error handling
required_packages <- c("tidyverse", "quantmod", "xts", "PerformanceAnalytics",
                       "TTR", "zoo", "tidyquant", "ggplot2", "reshape2", 
                       "rugarch", "nloptr", "gridExtra", "grid")

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
cat("\n========================================================\n")
cat(sprintf("Current Date and Time (UTC): %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat(sprintf("Current User's Login: %s\n", Sys.info()["user"]))
cat(sprintf("R Version: %s\n", R.version.string))
cat(sprintf("System: %s\n", Sys.info()["sysname"]))
cat("========================================================\n")

# Global safety settings
options(warn = 1)  # Show warnings immediately
options(stringsAsFactors = FALSE)  # Never convert strings to factors by default

#=============================================================================
# HELPER FUNCTIONS
#=============================================================================
# Helper function to ensure proper time series date formats
fix_portfolio_calculation <- function(portfolio, returns) {
  # Convert any date-related objects to proper Date format
  if (!is.null(portfolio$date)) {
    portfolio$date <- as.Date(format(as.Date(portfolio$date), "%Y-%m-%d"))
  }
  
  # Ensure returns has proper date index
  if (is.xts(returns)) {
    index(returns) <- as.Date(format(index(returns), "%Y-%m-%d"))
  }
  
  return(portfolio)
}

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

# Debug helper function - Enhanced for better visibility
debug_print <- function(message, value = NULL, important = FALSE) {
  if (important) {
    cat("\n===== DEBUG [IMPORTANT] =====\n")
  } else {
    cat("\n----- DEBUG -----\n")
  }
  
  cat(message, "\n")
  
  if (!is.null(value)) {
    if (is.data.frame(value) || is.matrix(value)) {
      print(head(value, 5))
      cat(sprintf("Dimensions: %d rows × %d columns\n", nrow(value), ncol(value)))
    } else if (is.vector(value)) {
      if (length(value) > 10) {
        print(head(value, 10))
        cat(sprintf("... (length: %d)\n", length(value)))
      } else {
        print(value)
      }
    } else {
      print(value)
    }
  }
  
  if (important) {
    cat("===========================\n\n")
  } else {
    cat("-------------------\n\n")
  }
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
  
  cat(sprintf("\nLoading market data from %s to %s\n", start_date, end_date))
  
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
  
  cat(sprintf("Loading data for %d tickers...\n", length(tickers)))
  
  # Try to get data from Yahoo Finance
  for (ticker in tickers) {
    tryCatch({
      # Fetch data with expanded date range to ensure we have enough data
      price_data <- getSymbols(ticker, 
                               from = start_date - 30,  # Add buffer days  
                               to = end_date + 5,       # Add buffer days
                               src = source, 
                               auto.assign = FALSE)
      
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
  
  # Trim to the requested date range
  date_range <- paste0(start_date, "/", end_date)
  all_prices <- all_prices[date_range]
  
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
  
  # Final check for NAs - replace any remaining with last valid value
  if (any(is.na(all_prices))) {
    cat("WARNING: Still have NA values. Replacing with last valid values.\n")
    all_prices <- na.locf(all_prices, na.rm = FALSE)
    all_prices <- na.locf(all_prices, fromLast = TRUE, na.rm = FALSE)
  }
  
  # Ensure we have a valid date index - CRITICAL FIX
  all_prices <- ensure_date_index(all_prices)
  
  # Add inverse ETF map as attribute
  attr(all_prices, "inverse_etf_map") <- inverse_etf_map
  
  # Final report
  cat(sprintf("Final price data: %d days × %d tickers (%s to %s)\n", 
              nrow(all_prices), ncol(all_prices),
              min(index(all_prices)), max(index(all_prices))))
  
  return(all_prices)
}

#=============================================================================
# GARCH VOLATILITY FORECASTING - IMPROVED
#=============================================================================

# Implementation of GARCH volatility forecasting with better error handling
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

# Enhanced EWMA covariance estimation with better robustness
estimate_ewma_covariance <- function(returns, lambda = 0.94, min_obs = 30) {
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
# ENHANCED ECONOMIC INDICATORS FROM ETF DATA
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

# Enhanced market data creation with expanded indicators
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
  
  debug_print("Price dates range", c(min(price_dates), max(price_dates)))
  
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
  
  # 6. Bond-equity correlation
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
    
    # Ensure we have the correct length (handle off-by-one errors)
    if (length(roll_corr) != n_dates) {
      if (length(roll_corr) > n_dates) {
        roll_corr <- tail(roll_corr, n_dates)
      } else {
        # Pad with the first value if needed
        first_value <- roll_corr[1]
        roll_corr <- c(rep(first_value, n_dates - length(roll_corr)), roll_corr)
      }
    }
    
    indicators_list[["BOND_EQUITY_CORR"]] <- as.numeric(roll_corr)
  }
  
  # 7. Commodity trend indicator
  if ("DBC" %in% colnames(prices)) {
    cat("Creating commodity trend indicator...\n")
    # 60-day momentum
    dbc_mom <- ROC(prices[, "DBC"], n = 60, type = "discrete")
    dbc_mom <- na.locf(dbc_mom, fromLast = TRUE, na.rm = FALSE)
    indicators_list[["COMMODITY_TREND"]] <- as.numeric(dbc_mom)
  } else if ("GLD" %in% colnames(prices)) {
    # Alternative using gold if DBC not available
    cat("Creating commodity trend indicator using GLD...\n")
    gld_mom <- ROC(prices[, "GLD"], n = 60, type = "discrete")
    gld_mom <- na.locf(gld_mom, fromLast = TRUE, na.rm = FALSE)
    indicators_list[["COMMODITY_TREND"]] <- as.numeric(gld_mom)
  }
  
  # 8. Yield curve indicator (if available)
  if (all(c("SHY", "TLT") %in% colnames(prices))) {
    cat("Creating yield curve indicator (SHY/TLT)...\n")
    # Approximate the yield curve slope using short vs long bond ETFs
    # SHY (1-3yr) vs TLT (20+yr)
    yield_curve <- as.numeric(prices[, "SHY"] / prices[, "TLT"])
    indicators_list[["YIELD_CURVE"]] <- yield_curve
  } else if (all(c("SHY", "IEF") %in% colnames(prices))) {
    # Alternative using SHY and IEF
    cat("Creating yield curve indicator (SHY/IEF)...\n")
    yield_curve <- as.numeric(prices[, "SHY"] / prices[, "IEF"])
    indicators_list[["YIELD_CURVE"]] <- yield_curve
  }
  
  # 9. Market breadth indicator (if available with high/low data)
  if ("SPY" %in% colnames(prices)) {
    tryCatch({
      cat("Creating market breadth indicator using SPY...\n")
      # Try to get SPY high-low data
      spy_data <- getSymbols("SPY", src = "yahoo", 
                             from = min(price_dates) - 30,
                             to = max(price_dates) + 5,
                             auto.assign = FALSE)
      
      if (!is.null(spy_data) && ncol(spy_data) >= 4) {
        # Calculate high-low range relative to close
        spy_hl_range <- (Hi(spy_data) - Lo(spy_data)) / Cl(spy_data)
        
        # Calculate 20-day average range
        avg_range <- SMA(spy_hl_range, n = 20)
        
        # Find common dates
        common_dates <- intersect(index(avg_range), price_dates)
        
        if (length(common_dates) > 10) {
          # Create aligned series
          breadth_aligned <- xts(rep(NA, length(price_dates)), order.by = price_dates)
          breadth_aligned[common_dates] <- avg_range[common_dates]
          
          # Fill NAs
          breadth_aligned <- na.locf(breadth_aligned, na.rm = FALSE)
          breadth_aligned <- na.locf(breadth_aligned, fromLast = TRUE, na.rm = FALSE)
          
          indicators_list[["MARKET_BREADTH"]] <- as.numeric(breadth_aligned)
          cat("Successfully created market breadth indicator\n")
        }
      }
    }, error = function(e) {
      cat("Could not create market breadth indicator:", e$message, "\n")
    })
  }
  
  # 10. NEW: Volatility indicator directly from returns
  cat("Creating volatility indicator from returns...\n")
  # Calculate 20-day rolling volatility of SPY if available, otherwise use portfolio vol
  if ("SPY" %in% colnames(returns)) {
    spy_returns <- returns[, "SPY"]
    rolling_vol <- rollapply(spy_returns, width = 20, 
                             FUN = function(x) sd(x, na.rm = TRUE) * sqrt(252),
                             by.column = FALSE, align = "right")
    
    # Ensure we have the correct length
    if (length(rolling_vol) != n_dates) {
      if (length(rolling_vol) > n_dates) {
        rolling_vol <- tail(rolling_vol, n_dates)
      } else {
        # Fill with the first value
        first_vol <- rolling_vol[1]
        rolling_vol <- c(rep(first_vol, n_dates - length(rolling_vol)), rolling_vol)
      }
    }
    
    rolling_vol <- na.locf(rolling_vol, fromLast = TRUE, na.rm = FALSE)
    indicators_list[["REALIZED_VOL"]] <- as.numeric(rolling_vol)
  } else {
    # Use average volatility of all assets
    all_vols <- rollapply(returns, width = 20, 
                          FUN = function(x) mean(apply(x, 2, sd, na.rm = TRUE)) * sqrt(252),
                          by.column = FALSE, align = "right")
    
    # Ensure we have the correct length
    if (length(all_vols) != n_dates) {
      if (length(all_vols) > n_dates) {
        all_vols <- tail(all_vols, n_dates)
      } else {
        # Fill with the first value
        first_vol <- all_vols[1]
        all_vols <- c(rep(first_vol, n_dates - length(all_vols)), all_vols)
      }
    }
    
    all_vols <- na.locf(all_vols, fromLast = TRUE, na.rm = FALSE)
    indicators_list[["REALIZED_VOL"]] <- as.numeric(all_vols)
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
  
  # Use shorter window for z-scores (126 days instead of 252)
  z_score_window <- 126  # ~6 months instead of 1 year
  
  for (i in 1:ncol(indicators_xts)) {
    # Get the indicator
    indicator <- as.numeric(indicators_xts[, i])
    
    # Calculate rolling means and standard deviations
    roll_mean <- zoo::rollapply(indicator, width = z_score_window, FUN = mean, 
                                align = "right", fill = NA)
    roll_sd <- zoo::rollapply(indicator, width = z_score_window, FUN = sd, 
                              align = "right", fill = NA)
    
    # Calculate Z-scores
    z_scores <- (indicator - roll_mean) / roll_sd
    
    # For early periods without enough data for z-score calculation
    if (sum(is.na(z_scores)) > 0) {
      # Use simple z-score calculation for early periods
      early_data <- indicator[is.na(z_scores)]
      if (length(early_data) > 0) {
        early_mean <- mean(early_data, na.rm = TRUE)
        early_sd <- sd(early_data, na.rm = TRUE)
        if (!is.na(early_sd) && early_sd > 0) {
          early_z <- (early_data - early_mean) / early_sd
          z_scores[is.na(z_scores)] <- early_z
        }
      }
    }
    
    # Fill any remaining NA values with 0
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
    "IWM" = 1.7,    # Russell 2000 - small caps
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
    "TIP" = 2.2,    # TIPS
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
    "DBC" = 4.0,    # Diversified Commodities
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

# ENHANCED: More responsive cash allocation with regime awareness
calculate_cash_allocation <- function(current_drawdown, max_drawdown, vol_zscore, regime, 
                                      max_cash_pct = 0.50, cash_ramp_factor = 2.0) {
  # Start with zero cash
  cash_pct <- 0
  
  # Smoother, more gradual cash allocation - starts at 30% of max_drawdown
  if (current_drawdown > 0.3 * max_drawdown) {
    dd_ratio <- current_drawdown / max_drawdown
    # More progressive scaling with new parameter
    dd_cash_pct <- min(max_cash_pct, (dd_ratio - 0.3) * cash_ramp_factor * max_cash_pct)
    cash_pct <- max(cash_pct, dd_cash_pct)
    debug_print(sprintf("Drawdown-based cash: %.1f%% (DD: %.1f%% of max)", 
                        dd_cash_pct * 100, dd_ratio * 100))
  }
  
  # More gradual vol-based cash allocation
  if (vol_zscore > 0.5) {  # Lower threshold
    vol_cash_pct <- min(max_cash_pct, (vol_zscore - 0.5) * 0.2)
    old_cash <- cash_pct
    cash_pct <- max(cash_pct, vol_cash_pct)
    
    if (cash_pct > old_cash) {
      debug_print(sprintf("Volatility-based cash increase: %.1f%% → %.1f%% (Z-score: %.2f)", 
                          old_cash * 100, cash_pct * 100, vol_zscore))
    }
  }
  
  # More nuanced regime-based cash allocation
  if (!is.null(regime) && !is.na(regime) && regime != "") {
    regime_cash <- switch(regime,
                          risk_off = 0.30,
                          deflation = 0.20,
                          stagflation = 0.15,
                          reflation = 0.05,
                          growth = 0.00,
                          0.10)  # Default for unknown regimes
    
    old_cash <- cash_pct
    cash_pct <- max(cash_pct, regime_cash)
    
    if (cash_pct > old_cash) {
      debug_print(sprintf("Regime-based cash increase: %.1f%% → %.1f%% (Regime: %s)", 
                          old_cash * 100, cash_pct * 100, regime))
    }
  }
  
  # Ensure we never have more than max_cash_pct
  cash_pct <- min(cash_pct, max_cash_pct)
  
  return(cash_pct)
}

# NEW: Function to check for recent regime changes
detect_regime_change <- function(regime_history, lookback = 3) {
  if (nrow(regime_history) < lookback + 1) {
    return(FALSE)  # Not enough history
  }
  
  # Get the most recent regime
  current_regime <- as.character(tail(regime_history, 1)[1, 1])
  
  # Get the previous regimes
  previous_regimes <- as.character(tail(head(regime_history, nrow(regime_history)-1), lookback)[, 1])
  
  # Check if current is different from any previous
  changed <- !all(previous_regimes == current_regime, na.rm = TRUE)
  
  if (changed) {
    debug_print(sprintf("Regime change detected: %s → %s", 
                        paste(unique(previous_regimes), collapse="/"), 
                        current_regime), important = TRUE)
  }
  return(changed)
}
#=============================================================================
# PART 2: REGIME DETECTION AND CLASSIFICATION FUNCTIONS
#=============================================================================

# Enhanced regime classification with multiple methods
detect_regime <- function(market_data, lookback_periods = list(short = 21, medium = 63, long = 126)) {
  cat("\nDetecting economic regime from market data...\n")
  
  # Ensure we have at least the minimum required data
  if (is.null(market_data) || nrow(market_data) < lookback_periods$medium) {
    warning("Insufficient data for regime detection")
    return("unknown")
  }
  
  # Extract most recent data point
  current_data <- tail(market_data, 1)
  
  # Variables to store regime signals
  growth_signal <- 0
  inflation_signal <- 0
  volatility_signal <- 0
  
  # CHECK 1: GROWTH INDICATORS
  # Growth regime detection - use equity/bond ratio if available
  if ("GROWTH_Z" %in% colnames(current_data)) {
    growth_z <- as.numeric(current_data[, "GROWTH_Z"])
    
    if (is.na(growth_z)) {
      growth_z <- 0
      cat("WARNING: Growth Z-score is NA\n")
    }
    
    if (growth_z > 0.75) {
      growth_signal <- 1  # Strong growth
    } else if (growth_z < -0.75) {
      growth_signal <- -1  # Contraction
    } else {
      growth_signal <- 0  # Neutral
    }
    
    cat(sprintf("Growth signal: %d (z-score: %.2f)\n", growth_signal, growth_z))
  }
  # Fallback to SPY momentum if equity/bond ratio not available
  else if ("SPY" %in% colnames(market_data)) {
    # Calculate momentum using 3-month return
    spy_recent <- tail(market_data[, "SPY"], lookback_periods$medium + 1)
    spy_momentum <- as.numeric(tail(spy_recent, 1) / as.numeric(spy_recent[1]) - 1)
    
    if (spy_momentum > 0.05) {
      growth_signal <- 1  # Strong growth
    } else if (spy_momentum < -0.05) {
      growth_signal <- -1  # Contraction
    } else {
      growth_signal <- 0  # Neutral
    }
    
    cat(sprintf("Growth signal (fallback): %d (SPY 3m return: %.2f%%)\n", 
                growth_signal, spy_momentum * 100))
  }
  
  # CHECK 2: INFLATION INDICATORS
  # Inflation regime detection
  if ("INFLATION_Z" %in% colnames(current_data)) {
    inflation_z <- as.numeric(current_data[, "INFLATION_Z"])
    
    if (is.na(inflation_z)) {
      inflation_z <- 0
      cat("WARNING: Inflation Z-score is NA\n")
    }
    
    if (inflation_z > 0.75) {
      inflation_signal <- 1  # High inflation
    } else if (inflation_z < -0.75) {
      inflation_signal <- -1  # Deflation
    } else {
      inflation_signal <- 0  # Normal inflation
    }
    
    cat(sprintf("Inflation signal: %d (z-score: %.2f)\n", inflation_signal, inflation_z))
  } 
  # Gold as alternative inflation indicator
  else if (all(c("GLD", "IEF") %in% colnames(market_data))) {
    # Calculate GLD vs bonds ratio over time
    gld_recent <- tail(market_data[, "GLD"], lookback_periods$medium + 1)
    ief_recent <- tail(market_data[, "IEF"], lookback_periods$medium + 1)
    
    gld_ief_ratio <- as.numeric(tail(gld_recent, 1) / tail(ief_recent, 1)) / 
      as.numeric(gld_recent[1] / ief_recent[1]) - 1
    
    if (gld_ief_ratio > 0.05) {
      inflation_signal <- 1  # High inflation
    } else if (gld_ief_ratio < -0.05) {
      inflation_signal <- -1  # Deflation
    } else {
      inflation_signal <- 0  # Normal inflation
    }
    
    cat(sprintf("Inflation signal (fallback): %d (GLD/IEF ratio change: %.2f%%)\n", 
                inflation_signal, gld_ief_ratio * 100))
  }
  
  # CHECK 3: VOLATILITY/RISK ENVIRONMENT
  # Volatility regime detection
  if ("VIX" %in% colnames(current_data)) {
    vix_value <- as.numeric(current_data[, "VIX"])
    vix_z <- 0
    
    # Also use VIX Z-score if available
    if ("VIX_Z" %in% colnames(current_data)) {
      vix_z <- as.numeric(current_data[, "VIX_Z"])
      if (is.na(vix_z)) vix_z <- 0
    }
    
    # Determine volatility signal using both absolute VIX and Z-score
    if (vix_value > 30 || vix_z > 1.5) {
      volatility_signal <- -1  # High volatility
    } else if (vix_value < 15 || vix_z < -1) {
      volatility_signal <- 1  # Low volatility
    } else {
      volatility_signal <- 0  # Normal volatility
    }
    
    cat(sprintf("Volatility signal: %d (VIX: %.1f, Z-score: %.2f)\n", 
                volatility_signal, vix_value, vix_z))
  }
  # Use realized volatility if VIX not available
  else if ("REALIZED_VOL" %in% colnames(current_data)) {
    realized_vol <- as.numeric(current_data[, "REALIZED_VOL"])
    
    # Determine volatility signal using realized volatility
    if (realized_vol > 0.25) {  # 25% annualized
      volatility_signal <- -1  # High volatility
    } else if (realized_vol < 0.10) {  # 10% annualized
      volatility_signal <- 1  # Low volatility
    } else {
      volatility_signal <- 0  # Normal volatility
    }
    
    cat(sprintf("Volatility signal (fallback): %d (Realized vol: %.1f%%)\n", 
                volatility_signal, realized_vol * 100))
  }
  
  # COMBINE SIGNALS TO DETERMINE REGIME
  
  # Economic regime classification based on growth and inflation signals
  # Growth signal:    1 (growth),   0 (neutral),  -1 (contraction)
  # Inflation signal: 1 (inflation), 0 (neutral),  -1 (deflation)
  # Volatility modifies the baseline regime
  
  regime <- "unknown"
  
  # Define the four main economic regimes
  if (growth_signal == 1 && inflation_signal >= 0) {
    regime <- "growth"  # High growth, stable-to-high inflation
  } 
  else if (growth_signal == 1 && inflation_signal < 0) {
    regime <- "reflation"  # High growth, low inflation
  }
  else if (growth_signal <= 0 && inflation_signal == 1) {
    regime <- "stagflation"  # Low growth, high inflation
  }
  else if (growth_signal < 0 && inflation_signal <= 0) {
    regime <- "deflation"  # Low growth, low inflation
  }
  else if (growth_signal == 0 && inflation_signal == 0) {
    regime <- "neutral"  # Neither signal is strong
  }
  
  # Volatility can override the economic regime in extreme cases
  if (volatility_signal == -1 && (regime != "stagflation")) {
    # High volatility overrides the regime to risk-off
    # (except stagflation, which already indicates caution)
    cat("High volatility detected - overriding to risk_off regime\n")
    regime <- "risk_off"
  }
  
  debug_print(sprintf("Detected economic regime: %s", regime), important = TRUE)
  
  return(regime)
}

# Function to get appropriate risk targets for current regime
get_regime_risk_targets <- function(regime, base_vol_target = 0.10, 
                                    regime_adjustments = list(
                                      growth = 1.2,
                                      reflation = 1.1, 
                                      neutral = 1.0,
                                      stagflation = 0.8,
                                      deflation = 0.7,
                                      risk_off = 0.5)) {
  
  # Default adjustment factor if regime is unknown
  adjustment <- 1.0
  
  # Get adjustment factor for the current regime
  if (!is.null(regime) && regime %in% names(regime_adjustments)) {
    adjustment <- regime_adjustments[[regime]]
  }
  
  # Calculate adjusted volatility target
  vol_target <- base_vol_target * adjustment
  
  return(vol_target)
}

# Function to track regime history
update_regime_history <- function(regime_history, current_regime, current_date) {
  # Create new entry
  new_entry <- data.frame(
    regime = current_regime, 
    date = as.Date(current_date), 
    stringsAsFactors = FALSE
  )
  
  # Append to history
  updated_history <- rbind(regime_history, new_entry)
  
  return(updated_history)
}

#=============================================================================
# PART 3: RISK PARITY OPTIMIZATION FUNCTIONS - FIXED VERSION
#=============================================================================

# Improved risk parity objective function
risk_parity_objective <- function(w, cov_matrix, lambda = 2, target_risk_contrib = NULL) {
  # Normalize weights to sum to 1
  w <- abs(w) / sum(abs(w))
  
  n <- nrow(cov_matrix)
  portfolio_variance <- t(w) %*% cov_matrix %*% w
  portfolio_vol <- sqrt(portfolio_variance)[1,1]
  
  # Calculate each asset's risk contribution
  marginal_contrib <- (cov_matrix %*% w) / portfolio_vol
  risk_contrib <- w * marginal_contrib
  
  # If target contributions provided, use them
  if (is.null(target_risk_contrib)) {
    target_risk_contrib <- rep(portfolio_vol/n, n)  # Equal risk contribution
  }
  
  # Sum of squared differences between actual and target risk contributions
  risk_diff <- sum((risk_contrib - target_risk_contrib)^2)
  
  # Add L2 regularization to improve numerical stability
  reg_term <- lambda * sum(w^2)
  
  return(risk_diff + reg_term)
}

# FIXED VERSION: Risk parity optimization with proper constraint handling
optimize_risk_parity <- function(cov_matrix, custom_risk_targets = NULL, 
                                 lambda = 2, max_iterations = 10000) {
  n <- nrow(cov_matrix)
  
  # Initial equal weights
  equal_weights <- rep(1/n, n)
  
  # Safety check
  if (any(is.na(cov_matrix)) || any(is.infinite(cov_matrix))) {
    warning("Invalid covariance matrix with NA or Inf values")
    return(equal_weights)
  }
  
  # Ensure covariance matrix is positive definite
  eigen_values <- eigen(cov_matrix, only.values = TRUE)$values
  
  if (min(eigen_values) <= 0 || max(eigen_values) / min(eigen_values) > 1e6) {
    cat("Covariance matrix needs regularization\n")
    
    # Add small diagonal element for numerical stability
    epsilon <- max(1e-6, 0.01 * mean(diag(cov_matrix)))
    diag(cov_matrix) <- diag(cov_matrix) + epsilon
  }
  
  # Set target risk contributions
  if (is.null(custom_risk_targets)) {
    # Equal risk contribution targets
    port_vol <- sqrt(t(equal_weights) %*% cov_matrix %*% equal_weights)[1,1]
    target_risk <- rep(port_vol/n, n)
  } else {
    # Normalize custom risk targets
    custom_risk_targets <- abs(custom_risk_targets)
    target_risk <- custom_risk_targets / sum(custom_risk_targets) * 
      sqrt(t(equal_weights) %*% cov_matrix %*% equal_weights)[1,1]
  }
  
  # Set names
  if (!is.null(colnames(cov_matrix))) {
    names(equal_weights) <- colnames(cov_matrix)
    names(target_risk) <- colnames(cov_matrix)
  }
  
  tryCatch({
    cat("Starting risk parity optimization...\n")
    
    # FIXED VERSION: Use correct interface for nloptr::slsqp with constraint <= 0
    # The issue was that hin >= 0 is deprecated, and should be rewritten as hin <= 0
    # We need to negate the constraint function
    
    result <- nloptr::slsqp(
      x0 = equal_weights,
      fn = risk_parity_objective,
      cov_matrix = cov_matrix,
      lambda = lambda,
      target_risk_contrib = target_risk,
      lower = rep(0.001, n),
      upper = rep(0.999, n),
      hin = function(w) 1 - sum(w),  # Sum of weights <= 1 constraint (negated form)
      heq = NULL,  # No equality constraints
      control = list(
        maxeval = max_iterations,
        xtol_rel = 1e-6
      )
    )
    
    # Get optimized weights
    weights <- abs(result$par) / sum(abs(result$par))
    
    if (!is.null(colnames(cov_matrix))) {
      names(weights) <- colnames(cov_matrix)
    }
    
    cat(sprintf("Risk parity optimization completed with objective value: %.8f\n", result$value))
    
    return(weights)
    
  }, error = function(e) {
    warning(paste("Risk parity optimization failed:", e$message))
    cat("Falling back to equal weights\n")
    
    # Fall back to equal weights
    return(equal_weights)
  })
}

# Custom risk budgeting based on regimes
assign_risk_budgets <- function(assets, regime, factor_tilts = NULL) {
  # Set default risk budget (equal risk)
  n_assets <- length(assets)
  risk_budget <- rep(1, n_assets)
  names(risk_budget) <- assets
  
  # Define regime-specific risk budgets
  regime_tilts <- list(
    growth = list(
      "SPY" = 1.3, "QQQ" = 1.4, "IWM" = 1.3, "EFA" = 1.2, "EEM" = 1.3,
      "IEF" = 0.6, "TLT" = 0.5, "LQD" = 0.8, "HYG" = 1.0,
      "GLD" = 0.8, "DBC" = 1.1, "VNQ" = 1.2,
      # Inverse ETFs get reduced allocation in growth regime
      "SH" = 0.2, "PSQ" = 0.2, "RWM" = 0.2, "EUM" = 0.2, "EFZ" = 0.2,
      "TBF" = 0.3, "SJB" = 0.3, "DGZ" = 0.3, "DRV" = 0.2
    ),
    
    reflation = list(
      "SPY" = 1.2, "QQQ" = 1.3, "IWM" = 1.2, "EFA" = 1.1, "EEM" = 1.2,
      "IEF" = 0.7, "TLT" = 0.6, "LQD" = 0.9, "HYG" = 1.1,
      "GLD" = 1.1, "DBC" = 1.3, "VNQ" = 1.1,
      # Inverse ETFs get reduced allocation in reflation
      "SH" = 0.3, "PSQ" = 0.3, "RWM" = 0.3, "EUM" = 0.3, "EFZ" = 0.3,
      "TBF" = 0.4, "SJB" = 0.4, "DGZ" = 0.5, "DRV" = 0.3
    ),
    
    neutral = list(
      "SPY" = 1.0, "QQQ" = 1.0, "IWM" = 1.0, "EFA" = 1.0, "EEM" = 1.0,
      "IEF" = 1.0, "TLT" = 1.0, "LQD" = 1.0, "HYG" = 1.0,
      "GLD" = 1.0, "DBC" = 1.0, "VNQ" = 1.0,
      # Inverse ETFs get neutral allocation
      "SH" = 0.5, "PSQ" = 0.5, "RWM" = 0.5, "EUM" = 0.5, "EFZ" = 0.5,
      "TBF" = 0.5, "SJB" = 0.5, "DGZ" = 0.5, "DRV" = 0.5
    ),
    
    stagflation = list(
      "SPY" = 0.7, "QQQ" = 0.6, "IWM" = 0.7, "EFA" = 0.7, "EEM" = 0.7,
      "IEF" = 0.8, "TLT" = 0.8, "LQD" = 0.7, "HYG" = 0.6,
      "GLD" = 1.5, "DBC" = 1.4, "VNQ" = 0.8,
      # Inverse ETFs get increased allocation in stagflation
      "SH" = 0.9, "PSQ" = 0.9, "RWM" = 0.8, "EUM" = 0.8, "EFZ" = 0.8,
      "TBF" = 0.7, "SJB" = 0.7, "DGZ" = 0.3, "DRV" = 0.8
    ),
    
    deflation = list(
      "SPY" = 0.6, "QQQ" = 0.5, "IWM" = 0.5, "EFA" = 0.5, "EEM" = 0.4,
      "IEF" = 1.4, "TLT" = 1.6, "LQD" = 1.0, "HYG" = 0.5,
      "GLD" = 1.1, "DBC" = 0.6, "VNQ" = 0.5,
      # Inverse ETFs get increased allocation in deflation
      "SH" = 1.2, "PSQ" = 1.2, "RWM" = 1.2, "EUM" = 1.1, "EFZ" = 1.1,
      "TBF" = 0.4, "SJB" = 0.8, "DGZ" = 0.5, "DRV" = 1.0
    ),
    
    risk_off = list(
      "SPY" = 0.4, "QQQ" = 0.3, "IWM" = 0.3, "EFA" = 0.3, "EEM" = 0.2,
      "IEF" = 1.6, "TLT" = 1.8, "LQD" = 0.8, "HYG" = 0.3,
      "GLD" = 1.3, "DBC" = 0.5, "VNQ" = 0.3,
      # Inverse ETFs get increased allocation in risk-off
      "SH" = 1.5, "PSQ" = 1.5, "RWM" = 1.4, "EUM" = 1.4, "EFZ" = 1.4,
      "TBF" = 0.3, "SJB" = 1.0, "DGZ" = 0.4, "DRV" = 1.3
    )
  )
  
  # Apply regime-specific risk budget
  if (!is.null(regime) && regime %in% names(regime_tilts)) {
    # Get the appropriate risk budget for this regime
    regime_budget <- regime_tilts[[regime]]
    
    # Apply to each asset if it exists in our list
    for (asset in assets) {
      if (asset %in% names(regime_budget)) {
        risk_budget[asset] <- regime_budget[[asset]]
      }
    }
    
    cat(sprintf("Applied risk budget for '%s' regime\n", regime))
  }
  
  # Apply additional factor tilts if specified
  if (!is.null(factor_tilts)) {
    for (asset in assets) {
      if (asset %in% names(factor_tilts)) {
        risk_budget[asset] <- risk_budget[asset] * factor_tilts[[asset]]
      }
    }
    cat("Applied custom factor tilts to risk budget\n")
  }
  
  # Ensure risk budget is positive
  risk_budget <- pmax(risk_budget, 0.1)
  
  # Normalize risk budget to improve readability
  risk_budget <- risk_budget / mean(risk_budget)
  
  # Print final risk budget
  cat("Risk budget multipliers:\n")
  sorted_budget <- sort(risk_budget, decreasing = TRUE)
  for (asset in names(sorted_budget)) {
    cat(sprintf("  %s: %.2f\n", asset, sorted_budget[asset]))
  }
  
  return(risk_budget)
}

#=============================================================================
# PART 4: PORTFOLIO CONSTRUCTION AND MANAGEMENT FUNCTIONS
#=============================================================================

# ENHANCED: More responsive cash allocation with regime awareness
calculate_cash_allocation <- function(current_drawdown, max_drawdown, vol_zscore, regime, 
                                      max_cash_pct = 0.50, cash_ramp_factor = 2.0) {
  # Start with zero cash
  cash_pct <- 0
  
  # Smoother, more gradual cash allocation - starts at 30% of max_drawdown
  if (current_drawdown > 0.3 * max_drawdown) {
    dd_ratio <- current_drawdown / max_drawdown
    # More progressive scaling with new parameter
    dd_cash_pct <- min(max_cash_pct, (dd_ratio - 0.3) * cash_ramp_factor * max_cash_pct)
    cash_pct <- max(cash_pct, dd_cash_pct)
    debug_print(sprintf("Drawdown-based cash: %.1f%% (DD: %.1f%% of max)", 
                        dd_cash_pct * 100, dd_ratio * 100))
  }
  
  # More gradual vol-based cash allocation
  if (vol_zscore > 0.5) {  # Lower threshold
    vol_cash_pct <- min(max_cash_pct, (vol_zscore - 0.5) * 0.2)
    old_cash <- cash_pct
    cash_pct <- max(cash_pct, vol_cash_pct)
    
    if (cash_pct > old_cash) {
      debug_print(sprintf("Volatility-based cash increase: %.1f%% → %.1f%% (Z-score: %.2f)", 
                          old_cash * 100, cash_pct * 100, vol_zscore))
    }
  }
  
  # More nuanced regime-based cash allocation
  if (!is.null(regime) && !is.na(regime) && regime != "") {
    regime_cash <- switch(regime,
                          risk_off = 0.30,
                          deflation = 0.20,
                          stagflation = 0.15,
                          reflation = 0.05,
                          growth = 0.00,
                          0.10)  # Default for unknown regimes
    
    old_cash <- cash_pct
    cash_pct <- max(cash_pct, regime_cash)
    
    if (cash_pct > old_cash) {
      debug_print(sprintf("Regime-based cash increase: %.1f%% → %.1f%% (Regime: %s)", 
                          old_cash * 100, cash_pct * 100, regime))
    }
  }
  
  # Ensure we never have more than max_cash_pct
  cash_pct <- min(cash_pct, max_cash_pct)
  
  return(cash_pct)
}

# NEW: Function to check for recent regime changes
detect_regime_change <- function(regime_history, lookback = 3) {
  if (nrow(regime_history) < lookback + 1) {
    return(FALSE)  # Not enough history
  }
  
  # Get the most recent regime
  current_regime <- as.character(tail(regime_history, 1)[1, 1])
  
  # Get the previous regimes
  previous_regimes <- as.character(tail(head(regime_history, nrow(regime_history)-1), lookback)[, 1])
  
  # Check if current is different from any previous
  changed <- !all(previous_regimes == current_regime, na.rm = TRUE)
  
  if (changed) {
    debug_print(sprintf("Regime change detected: %s → %s", 
                        paste(unique(previous_regimes), collapse="/"), 
                        current_regime), important = TRUE)
  }
  return(changed)
}

# Function to construct portfolio from asset weights
construct_portfolio <- function(weights, prices, target_volatility = 0.10, 
                                cash_weight = 0, leverage_limit = 1.5, 
                                max_single_weight = 0.30) {
  cat("\nConstructing portfolio...\n")
  
  # Extract last row of prices
  if (is.xts(prices)) {
    latest_prices <- as.numeric(tail(prices, 1))
    names(latest_prices) <- colnames(prices)
    
    # FIX: Ensure proper Date format
    latest_date <- as.Date(format(index(tail(prices, 1)), "%Y-%m-%d"))
  } else {
    stop("Prices must be an xts object")
  }
  
  # Create standard output format
  portfolio <- list(
    date = latest_date,  # This will now be a properly formatted Date object
    weights = weights,
    cash_weight = cash_weight,
    prices = latest_prices,
    target_volatility = target_volatility,
    leverage = 0,
    effective_weights = NULL,
    n_assets = length(weights)
  )
  # Normalize weights to account for cash
  if (cash_weight > 0) {
    # Reduce all asset weights proportionally to make room for cash
    portfolio$weights <- portfolio$weights * (1 - cash_weight)
  }
  
  # Cap individual weights at the maximum
  if (max(portfolio$weights) > max_single_weight) {
    cat(sprintf("Capping weights at %.1f%%\n", max_single_weight * 100))
    excess_idx <- which(portfolio$weights > max_single_weight)
    excess_total <- sum(portfolio$weights[excess_idx] - max_single_weight)
    
    # Redistribute excess weight proportionally
    portfolio$weights[excess_idx] <- max_single_weight
    remaining_idx <- which(portfolio$weights < max_single_weight)
    
    if (length(remaining_idx) > 0) {
      # Redistribute excess weight proportionally among other assets
      remaining_total <- sum(portfolio$weights[remaining_idx])
      if (remaining_total > 0) {
        # Calculate scaling factor for redistribution
        scale_factor <- (remaining_total + excess_total) / remaining_total
        # Apply scaling, ensuring no weight exceeds max_single_weight
        portfolio$weights[remaining_idx] <- pmin(
          portfolio$weights[remaining_idx] * scale_factor,
          max_single_weight
        )
      }
    }
  }
  
  # Ensure weights sum to (1 - cash_weight)
  target_sum <- 1 - cash_weight
  current_sum <- sum(portfolio$weights)
  
  if (abs(current_sum - target_sum) > 0.0001) {
    cat(sprintf("Adjusting weights to sum to %.4f (currently %.4f)\n", 
                target_sum, current_sum))
    portfolio$weights <- portfolio$weights * (target_sum / current_sum)
  }
  
  # Store the effective weights (including cash)
  portfolio$effective_weights <- portfolio$weights
  if (cash_weight > 0) {
    portfolio$effective_weights <- c(portfolio$weights, "CASH" = cash_weight)
  }
  
  return(portfolio)
}
#=============================================================================
# PART 3: PERFORMANCE ANALYSIS, PORTFOLIO STATISTICS & TRADING SIGNALS
#=============================================================================

# Fix the specific issue in calculate_portfolio_stats function 
# where matrix multiplication drops the xts class
calculate_portfolio_stats <- function(portfolio, returns, cov_matrix, lookback = 252) {
  cat("\nCalculating portfolio statistics...\n")
  
  # Force proper date formatting for returns
  if (is.xts(returns)) {
    index(returns) <- as.Date(format(index(returns), "%Y-%m-%d"))
  }
  
  # Ensure portfolio date is proper Date format
  if (!is.null(portfolio$date)) {
    portfolio$date <- as.Date(format(portfolio$date, "%Y-%m-%d"))
  }
  
  # Extract portfolio weights (excluding cash)
  weights <- portfolio$weights
  
  # Safety check - ensure we have returns and covariance
  if (is.null(returns) || is.null(cov_matrix)) {
    warning("Missing returns or covariance matrix for statistics calculation")
    return(portfolio)
  }
  
  # Ensure we have enough return history
  if (nrow(returns) < 20) {
    warning("Insufficient return history for statistics calculation")
    return(portfolio)
  }
  
  # Calculate portfolio variance/volatility
  if (length(weights) != ncol(cov_matrix)) {
    warning("Dimension mismatch between weights and covariance matrix")
    return(portfolio)
  }
  
  # Calculate portfolio variance
  port_var <- t(weights) %*% cov_matrix %*% weights
  
  # Extract single value
  if (is.matrix(port_var) && nrow(port_var) == 1 && ncol(port_var) == 1) {
    port_var <- as.numeric(port_var)
  }
  
  # Calculate annualized volatility
  if (port_var > 0) {
    portfolio$volatility <- sqrt(port_var) * sqrt(252)
    cat(sprintf("Portfolio volatility: %.2f%%\n", portfolio$volatility * 100))
  } else {
    warning("Non-positive portfolio variance calculated")
    portfolio$volatility <- NA
  }
  
  # Calculate asset contribution to risk
  if (!is.na(portfolio$volatility) && portfolio$volatility > 0) {
    # Marginal contribution to risk
    mcr <- (cov_matrix %*% weights) / portfolio$volatility
    # Total contribution to risk (element-wise multiplication)
    portfolio$risk_contribution <- weights * mcr
    
    # Print top risk contributors
    top_contrib <- sort(portfolio$risk_contribution, decreasing = TRUE)
    if (length(top_contrib) > 0) {
      cat("Top risk contributors:\n")
      for (i in 1:min(5, length(top_contrib))) {
        asset <- names(top_contrib)[i]
        contrib_pct <- top_contrib[i] / sum(portfolio$risk_contribution) * 100
        cat(sprintf("  %s: %.1f%% (weight: %.1f%%)\n", 
                    asset, contrib_pct, weights[asset] * 100))
      }
    }
  }
  
  # Calculate recent performance if enough history
  if (nrow(returns) >= lookback) {
    # Filter returns to include only assets in our portfolio
    common_assets <- intersect(names(weights), colnames(returns))
    
    if (length(common_assets) > 0) {
      # CRITICAL FIX: Properly create portfolio returns as xts object
      # 1. Calculate weighted returns for each asset
      weighted_returns <- returns[, common_assets] * matrix(weights[common_assets], 
                                                            nrow = nrow(returns),
                                                            ncol = length(common_assets), 
                                                            byrow = TRUE)
      
      # 2. Sum across assets to get portfolio returns (preserving xts structure)
      port_returns <- xts(rowSums(weighted_returns), order.by = index(returns))
      colnames(port_returns) <- "portfolio_return"
      
      # Last month return (21 days)
      portfolio$return_1m <- as.numeric(prod(1 + tail(port_returns, 21)) - 1)
      
      # Last quarter return (63 days)
      portfolio$return_3m <- as.numeric(prod(1 + tail(port_returns, 63)) - 1)
      
      # Last 6 months (126 days)
      portfolio$return_6m <- as.numeric(prod(1 + tail(port_returns, 126)) - 1)
      
      # Year to date
      start_of_year <- as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01"))
      ytd_range <- paste0(start_of_year, "/", Sys.Date())
      
      # Use tryCatch to handle case where the range might be invalid
      ytd_returns <- tryCatch({
        port_returns[ytd_range]
      }, error = function(e) {
        cat("Warning: Could not calculate YTD returns - using all available data\n")
        port_returns
      })
      
      portfolio$return_ytd <- as.numeric(prod(1 + ytd_returns) - 1)
      
      # Maximum drawdown - use PerformanceAnalytics safely
      tryCatch({
        portfolio$max_drawdown <- as.numeric(maxDrawdown(port_returns))
      }, error = function(e) {
        cat("Warning: Could not calculate maximum drawdown\n")
        portfolio$max_drawdown <- NA
      })
      
      # Print performance summary
      cat(sprintf("Performance summary:\n"))
      cat(sprintf("  1-month return: %.2f%%\n", portfolio$return_1m * 100))
      cat(sprintf("  3-month return: %.2f%%\n", portfolio$return_3m * 100))
      cat(sprintf("  6-month return: %.2f%%\n", portfolio$return_6m * 100))
      cat(sprintf("  YTD return: %.2f%%\n", portfolio$return_ytd * 100))
      if (!is.na(portfolio$max_drawdown)) {
        cat(sprintf("  Max drawdown: %.2f%%\n", portfolio$max_drawdown * 100))
      }
    }
  }
  
  return(portfolio)
}

# Function to analyze portfolio exposure
analyze_portfolio_exposures <- function(weights, market_data, 
                                        asset_classes = NULL, 
                                        regions = NULL) {
  cat("\nAnalyzing portfolio exposures...\n")
  
  # Initialize results
  exposure <- list(
    asset_class = list(),
    region = list(),
    factor = list()
  )
  
  # Define default asset classes if not provided
  if (is.null(asset_classes)) {
    asset_classes <- list(
      "Equity" = c("SPY", "IVV", "VTI", "QQQ", "IWM", "MDY", "EFA", "EEM", 
                   "VGK", "EWJ", "VPL", "VWO", "IEMG", "SH", "PSQ", "RWM", "EUM", "EFZ"),
      "Fixed Income" = c("IEF", "TLT", "SHY", "TIP", "LQD", "VCSH", "VCIT", 
                         "HYG", "JNK", "MBB", "TBF", "SJB"),
      "Commodities" = c("GLD", "IAU", "SLV", "DBC", "USO", "UNG", "DGZ"),
      "Real Estate" = c("VNQ", "IYR", "SCHH", "RWR", "VNQI", "DRV")
    )
  }
  
  # Define default regions if not provided
  if (is.null(regions)) {
    regions <- list(
      "US" = c("SPY", "IVV", "VTI", "QQQ", "IWM", "MDY", "SH", "PSQ", "RWM",
               "IEF", "TLT", "SHY", "TIP", "LQD", "VCSH", "VCIT", "HYG", "JNK", "MBB", "TBF", "SJB",
               "VNQ", "IYR", "SCHH", "RWR", "DRV"),
      "Developed ex-US" = c("EFA", "VGK", "EWJ", "VPL", "EFZ", "VNQI"),
      "Emerging Markets" = c("EEM", "VWO", "IEMG", "EUM"),
      "Global" = c("GLD", "IAU", "SLV", "DBC", "USO", "UNG", "DGZ")
    )
  }
  
  # Calculate asset class exposures
  for (class_name in names(asset_classes)) {
    class_tickers <- asset_classes[[class_name]]
    class_exposure <- 0
    
    # Find tickers in this class that we own
    common_tickers <- intersect(names(weights), class_tickers)
    
    if (length(common_tickers) > 0) {
      class_exposure <- sum(weights[common_tickers])
      exposure$asset_class[[class_name]] <- class_exposure
    }
  }
  
  # Calculate regional exposures
  for (region_name in names(regions)) {
    region_tickers <- regions[[region_name]]
    region_exposure <- 0
    
    # Find tickers in this region that we own
    common_tickers <- intersect(names(weights), region_tickers)
    
    if (length(common_tickers) > 0) {
      region_exposure <- sum(weights[common_tickers])
      exposure$region[[region_name]] <- region_exposure
    }
  }
  
  # Function to calculate exposure to a specific factor
  calc_factor_exposure <- function(factor_name, positive_tickers, negative_tickers = NULL) {
    pos_exposure <- 0
    neg_exposure <- 0
    
    pos_common <- intersect(names(weights), positive_tickers)
    if (length(pos_common) > 0) {
      pos_exposure <- sum(weights[pos_common])
    }
    
    if (!is.null(negative_tickers)) {
      neg_common <- intersect(names(weights), negative_tickers)
      if (length(neg_common) > 0) {
        neg_exposure <- -sum(weights[neg_common])
      }
    }
    
    return(pos_exposure + neg_exposure)
  }
  
  # Calculate factor exposures
  exposure$factor$growth <- calc_factor_exposure(
    "Growth", 
    c("SPY", "QQQ", "IWM", "VUG", "VBK"),
    c("SH", "PSQ", "RWM")
  )
  
  exposure$factor$value <- calc_factor_exposure(
    "Value", 
    c("VTV", "IWD", "VBR", "IWN")
  )
  
  exposure$factor$momentum <- calc_factor_exposure(
    "Momentum", 
    c("MTUM", "PDP", "QQQ", "VUG")
  )
  
  exposure$factor$quality <- calc_factor_exposure(
    "Quality", 
    c("QUAL", "SPHQ", "LQD", "VCIT")
  )
  
  exposure$factor$duration <- calc_factor_exposure(
    "Duration", 
    c("TLT", "EDV"),
    c("SHY", "VGSH", "TBF")
  )
  
  exposure$factor$inflation <- calc_factor_exposure(
    "Inflation", 
    c("TIP", "GLD", "DBC"),
    c("DGZ")
  )
  
  # Print summary of exposures
  cat("Asset Class Exposures:\n")
  for (class_name in names(exposure$asset_class)) {
    cat(sprintf("  %s: %.2f%%\n", class_name, 
                exposure$asset_class[[class_name]] * 100))
  }
  
  cat("\nRegional Exposures:\n")
  for (region_name in names(exposure$region)) {
    cat(sprintf("  %s: %.2f%%\n", region_name, 
                exposure$region[[region_name]] * 100))
  }
  
  cat("\nFactor Exposures:\n")
  for (factor_name in names(exposure$factor)) {
    cat(sprintf("  %s: %.2f%%\n", factor_name, 
                exposure$factor[[factor_name]] * 100))
  }
  
  return(exposure)
}

# Function to simulate portfolio rebalancing with transaction cost analysis
rebalance_portfolio <- function(current_portfolio, new_weights, 
                                prices, txn_costs, cash_weight = 0,
                                min_trade_size = 0.001) {
  cat("\nSimulating portfolio rebalancing...\n")
  
  # Extract current portfolio information
  old_weights <- current_portfolio$weights
  old_cash <- current_portfolio$cash_weight
  tickers <- names(new_weights)
  
  # Ensure we have all weights properly named
  if (is.null(names(old_weights)) || is.null(names(new_weights))) {
    warning("Weights must have names (tickers)")
    return(NULL)
  }
  
  # For new assets in portfolio, assume zero current weight
  for (ticker in tickers) {
    if (!(ticker %in% names(old_weights))) {
      old_weights[ticker] <- 0
    }
  }
  
  # Calculate trade sizes (as percentage of portfolio)
  trades <- new_weights - old_weights[tickers]
  
  # Apply minimum trade size threshold
  small_trade_idx <- which(abs(trades) < min_trade_size)
  if (length(small_trade_idx) > 0) {
    trades[small_trade_idx] <- 0
    new_weights[small_trade_idx] <- old_weights[names(trades)[small_trade_idx]]
  }
  
  # Calculate transaction costs
  ticker_costs <- txn_costs[tickers]
  
  # For any missing transaction costs, use a default
  missing_cost <- which(is.na(ticker_costs))
  if (length(missing_cost) > 0) {
    ticker_costs[missing_cost] <- 0.0025  # Default 25 bps
  }
  
  # Calculate total transaction cost
  total_cost <- sum(abs(trades) * ticker_costs)
  
  # Calculate turnover
  turnover <- sum(abs(trades)) / 2  # One-way turnover
  
  # Create rebalance summary
  rebalance <- list(
    date = Sys.Date(),
    old_weights = old_weights,
    new_weights = new_weights,
    trades = trades,
    old_cash = old_cash,
    new_cash = cash_weight,
    turnover = turnover,
    transaction_cost = total_cost
  )
  
  # Print rebalance summary
  cat(sprintf("Rebalance summary:\n"))
  cat(sprintf("  Turnover: %.2f%%\n", turnover * 100))
  cat(sprintf("  Transaction cost: %.2f bps\n", total_cost * 10000))
  cat(sprintf("  Cash allocation: %.2f%% → %.2f%%\n", 
              old_cash * 100, cash_weight * 100))
  
  # Print largest trades
  if (length(trades) > 0) {
    top_trades <- sort(abs(trades), decreasing = TRUE)
    cat("Largest trades:\n")
    for (i in 1:min(5, length(top_trades))) {
      ticker <- names(top_trades)[i]
      trade_pct <- trades[ticker] * 100
      direction <- ifelse(trade_pct > 0, "BUY", "SELL")
      cat(sprintf("  %s %s: %+.2f%% (new weight: %.2f%%)\n", 
                  direction, ticker, trade_pct, new_weights[ticker] * 100))
    }
  }
  
  return(rebalance)
}

# Function to create trading signals
generate_trading_signals <- function(weights, portfolio = NULL, threshold = 0.01) {
  # Initialize an empty data frame for signals
  signals <- data.frame(
    ticker = names(weights),
    weight = as.numeric(weights),
    action = rep("HOLD", length(weights)),
    size = rep(0, length(weights)),
    stringsAsFactors = FALSE
  )
  
  # If we have a current portfolio, compare weights for trading decision
  if (!is.null(portfolio) && !is.null(portfolio$weights)) {
    current_weights <- portfolio$weights
    
    # For each asset in the new weights
    for (i in 1:nrow(signals)) {
      ticker <- signals$ticker[i]
      new_weight <- signals$weight[i]
      
      # Get current weight (0 if not in current portfolio)
      current_weight <- ifelse(ticker %in% names(current_weights),
                               current_weights[ticker], 0)
      
      # Calculate size difference
      weight_diff <- new_weight - current_weight
      signals$size[i] <- abs(weight_diff)
      
      # Determine action based on weight difference
      if (weight_diff > threshold) {
        signals$action[i] <- "BUY"
      } else if (weight_diff < -threshold) {
        signals$action[i] <- "SELL"
      } else {
        signals$action[i] <- "HOLD"
      }
    }
    
    # Check for assets to completely exit
    for (ticker in names(current_weights)) {
      if (!(ticker %in% signals$ticker) && current_weights[ticker] > threshold) {
        # Need to sell this asset not in new allocation
        new_row <- data.frame(
          ticker = ticker,
          weight = 0,
          action = "SELL",
          size = current_weights[ticker],
          stringsAsFactors = FALSE
        )
        signals <- rbind(signals, new_row)
      }
    }
  } else {
    # Without existing portfolio, all non-zero weights are buys
    signals$action <- ifelse(signals$weight > threshold, "BUY", "HOLD")
    signals$size <- signals$weight
  }
  
  # Sort by size descending (largest trades first)
  signals <- signals[order(-signals$size), ]
  
  return(signals)
}

# Fix portfolio date formats
fix_portfolio_calculation <- function(portfolio, returns) {
  # Convert any date-related objects to proper Date format
  if (!is.null(portfolio$date)) {
    portfolio$date <- as.Date(portfolio$date)
  }
  
  # Ensure returns has proper date index
  if (is.xts(returns)) {
    index(returns) <- as.Date(index(returns))
  } else {
    warning("Returns data is not an xts object")
  }
  
  # Return fixed portfolio
  return(portfolio)
}
#=============================================================================
# PART 4: COMPREHENSIVE PERFORMANCE ANALYSIS & YEARLY BREAKDOWN
#=============================================================================

# ENHANCED: Comprehensive performance analysis with yearly breakdown
generate_comprehensive_performance_report <- function(strategy_results, returns, prices, 
                                                      benchmark_ticker = "SPY",
                                                      calendar_analysis = TRUE) {
  cat("\n========================================================\n")
  cat("COMPREHENSIVE PORTFOLIO PERFORMANCE ANALYSIS\n")
  cat("========================================================\n")
  
  # Check if we have valid results
  if (is.null(strategy_results) || strategy_results$status != "success" || is.null(strategy_results$portfolio)) {
    cat("Cannot generate performance report - strategy did not complete successfully\n")
    return(NULL)
  }
  
  # Extract portfolio information
  portfolio <- strategy_results$portfolio
  weights <- portfolio$weights
  
  # Ensure we have at least one period of return history
  if (is.null(returns) || nrow(returns) < 22) {
    cat("Insufficient return history for performance analysis\n")
    return(NULL)
  }
  
  # Ensure the returns have proper Date index
  index(returns) <- as.Date(index(returns))
  
  # Calculate historical portfolio returns
  port_returns <- xts(rep(NA, nrow(returns)), order.by = index(returns))
  colnames(port_returns) <- "portfolio"
  
  # Only use assets that exist in our returns data
  common_assets <- intersect(names(weights), colnames(returns))
  
  if (length(common_assets) > 0) {
    # Filter weights to only use common assets
    weights_subset <- weights[common_assets]
    # Normalize remaining weights
    weights_subset <- weights_subset / sum(weights_subset)
    
    # Calculate portfolio returns using rowSums to maintain xts structure
    weighted_returns <- returns[, common_assets] * matrix(weights_subset, 
                                                          nrow = nrow(returns),
                                                          ncol = length(common_assets), 
                                                          byrow = TRUE)
    port_returns[, 1] <- rowSums(weighted_returns)
    
    # Calculate benchmark returns
    if (benchmark_ticker %in% colnames(returns)) {
      benchmark_returns <- returns[, benchmark_ticker]
      benchmark_name <- benchmark_ticker
    } else {
      benchmark_returns <- rowMeans(returns)
      benchmark_name <- "Equal-weighted"
    }
    
    # Add benchmark to the return series
    port_returns <- merge(port_returns, benchmark_returns)
    colnames(port_returns)[2] <- benchmark_name
    
    # ----- RECENT PERIOD RETURNS -----
    periods <- c("1M" = 21, "3M" = 63, "6M" = 126, "1Y" = 252)
    
    cat("\n----- RECENT PERIOD RETURNS -----\n")
    for (p_name in names(periods)) {
      p_length <- periods[p_name]
      if (nrow(port_returns) >= p_length) {
        period_data <- tail(port_returns, p_length)
        port_return <- prod(1 + period_data[, 1]) - 1
        bench_return <- prod(1 + period_data[, 2]) - 1
        diff_return <- port_return - bench_return
        
        cat(sprintf("  %s: Portfolio = %+.2f%%, %s = %+.2f%%, Difference = %+.2f%%\n", 
                    p_name, port_return * 100, benchmark_name, bench_return * 100, diff_return * 100))
      }
    }
    
    # ----- CALENDAR YEAR PERFORMANCE -----
    if (calendar_analysis) {
      cat("\n----- CALENDAR YEAR PERFORMANCE -----\n")
      
      # Get all years in the data
      years <- unique(format(index(port_returns), "%Y"))
      
      yearly_stats <- list()
      
      for (year in years) {
        # Extract data for this year
        year_start <- as.Date(paste0(year, "-01-01"))
        year_end <- as.Date(paste0(year, "-12-31"))
        year_range <- paste0(year_start, "/", year_end)
        
        year_data <- port_returns[year_range]
        
        # If we have data for this year
        if (nrow(year_data) > 0) {
          # Calculate returns
          port_return <- prod(1 + year_data[, 1]) - 1
          bench_return <- prod(1 + year_data[, 2]) - 1
          excess_return <- port_return - bench_return
          
          # Calculate volatility
          port_vol <- sd(year_data[, 1], na.rm = TRUE) * sqrt(252)
          bench_vol <- sd(year_data[, 2], na.rm = TRUE) * sqrt(252)
          
          # Calculate Sharpe ratio (assuming risk-free rate of 0 for simplicity)
          port_sharpe <- ifelse(port_vol > 0, port_return / port_vol, NA)
          bench_sharpe <- ifelse(bench_vol > 0, bench_return / bench_vol, NA)
          
          # Calculate maximum drawdown
          port_dd <- tryCatch(
            maxDrawdown(year_data[, 1]), 
            error = function(e) NA
          )
          bench_dd <- tryCatch(
            maxDrawdown(year_data[, 2]),
            error = function(e) NA
          )
          
          # Store statistics
          yearly_stats[[year]] <- list(
            port_return = port_return,
            bench_return = bench_return,
            excess_return = excess_return,
            port_vol = port_vol,
            bench_vol = bench_vol,
            port_sharpe = port_sharpe,
            bench_sharpe = bench_sharpe,
            port_dd = port_dd,
            bench_dd = bench_dd
          )
          
          # Print summary
          cat(sprintf("\n  Year %s:\n", year))
          cat(sprintf("    Portfolio Return: %+.2f%%, Volatility: %.2f%%, Sharpe: %.2f, Max DD: %.2f%%\n", 
                      port_return * 100, port_vol * 100, port_sharpe, port_dd * 100))
          cat(sprintf("    %s Return: %+.2f%%, Volatility: %.2f%%, Sharpe: %.2f, Max DD: %.2f%%\n", 
                      benchmark_name, bench_return * 100, bench_vol * 100, bench_sharpe, bench_dd * 100))
          cat(sprintf("    Excess Return: %+.2f%%\n", excess_return * 100))
        }
      }
    }
    
    # ----- FULL PERIOD ANALYSIS -----
    cat("\n----- FULL PERIOD ANALYSIS -----\n")
    
    # Calculate portfolio statistics
    total_port_return <- prod(1 + na.omit(port_returns[, 1])) - 1
    total_bench_return <- prod(1 + na.omit(port_returns[, 2])) - 1
    
    # Annualize returns
    years <- nrow(port_returns) / 252
    annual_port_return <- (1 + total_port_return)^(1/years) - 1
    annual_bench_return <- (1 + total_bench_return)^(1/years) - 1
    
    # Calculate volatilities
    port_vol <- sd(port_returns[, 1], na.rm = TRUE) * sqrt(252)
    bench_vol <- sd(port_returns[, 2], na.rm = TRUE) * sqrt(252)
    
    # Calculate risk-adjusted metrics
    port_sharpe <- annual_port_return / port_vol
    bench_sharpe <- annual_bench_return / bench_vol
    
    # Calculate downside deviation
    port_downside_dev <- PerformanceAnalytics::DownsideDeviation(port_returns[, 1], MAR = 0) * sqrt(252)
    bench_downside_dev <- PerformanceAnalytics::DownsideDeviation(port_returns[, 2], MAR = 0) * sqrt(252)
    
    # Calculate Sortino ratio
    port_sortino <- annual_port_return / port_downside_dev
    bench_sortino <- annual_bench_return / bench_downside_dev
    
    # Calculate maximum drawdown
    port_max_dd <- tryCatch(
      maxDrawdown(port_returns[, 1]),
      error = function(e) NA
    )
    bench_max_dd <- tryCatch(
      maxDrawdown(port_returns[, 2]),
      error = function(e) NA
    )
    
    # Calculate win rate (percentage of positive days)
    port_win_rate <- sum(port_returns[, 1] > 0, na.rm = TRUE) / sum(!is.na(port_returns[, 1]))
    bench_win_rate <- sum(port_returns[, 2] > 0, na.rm = TRUE) / sum(!is.na(port_returns[, 2]))
    
    # Calculate profit ratio (average gain / average loss)
    port_gains <- port_returns[port_returns[, 1] > 0, 1]
    port_losses <- port_returns[port_returns[, 1] < 0, 1]
    port_profit_ratio <- ifelse(length(port_losses) > 0, 
                                mean(port_gains, na.rm = TRUE) / abs(mean(port_losses, na.rm = TRUE)),
                                NA)
    
    bench_gains <- port_returns[port_returns[, 2] > 0, 2]
    bench_losses <- port_returns[port_returns[, 2] < 0, 2]
    bench_profit_ratio <- ifelse(length(bench_losses) > 0,
                                 mean(bench_gains, na.rm = TRUE) / abs(mean(bench_losses, na.rm = TRUE)),
                                 NA)
    
    # Calculate information ratio
    tracking_error <- sd(port_returns[, 1] - port_returns[, 2], na.rm = TRUE) * sqrt(252)
    info_ratio <- (annual_port_return - annual_bench_return) / tracking_error
    
    # Calculate alpha and beta
    model <- tryCatch({
      lm(port_returns[, 1] ~ port_returns[, 2])
    }, error = function(e) NULL)
    
    beta <- ifelse(!is.null(model), coef(model)[2], NA)
    alpha <- ifelse(!is.null(model), coef(model)[1] * 252, NA) # Annualize alpha
    correlation <- cor(port_returns[, 1], port_returns[, 2], use = "pairwise.complete.obs")
    
    # Print portfolio statistics
    cat(sprintf("\n  Portfolio Statistics (Full Period: %s to %s):\n", 
                format(index(port_returns)[1], "%Y-%m-%d"), 
                format(index(port_returns)[nrow(port_returns)], "%Y-%m-%d")))
    cat(sprintf("    Total Return: %+.2f%%\n", total_port_return * 100))
    cat(sprintf("    Annualized Return: %+.2f%%\n", annual_port_return * 100))
    cat(sprintf("    Annualized Volatility: %.2f%%\n", port_vol * 100))
    cat(sprintf("    Sharpe Ratio: %.2f\n", port_sharpe))
    cat(sprintf("    Sortino Ratio: %.2f\n", port_sortino))
    cat(sprintf("    Maximum Drawdown: %.2f%%\n", port_max_dd * 100))
    cat(sprintf("    Win Rate: %.2f%%\n", port_win_rate * 100))
    cat(sprintf("    Profit Ratio: %.2f\n", port_profit_ratio))
    
    # Print benchmark statistics
    cat(sprintf("\n  %s Statistics (Full Period):\n", benchmark_name))
    cat(sprintf("    Total Return: %+.2f%%\n", total_bench_return * 100))
    cat(sprintf("    Annualized Return: %+.2f%%\n", annual_bench_return * 100))
    cat(sprintf("    Annualized Volatility: %.2f%%\n", bench_vol * 100))
    cat(sprintf("    Sharpe Ratio: %.2f\n", bench_sharpe))
    cat(sprintf("    Sortino Ratio: %.2f\n", bench_sortino))
    cat(sprintf("    Maximum Drawdown: %.2f%%\n", bench_max_dd * 100))
    cat(sprintf("    Win Rate: %.2f%%\n", bench_win_rate * 100))
    cat(sprintf("    Profit Ratio: %.2f\n", bench_profit_ratio))
    
    # Print comparison statistics
    cat("\n  Comparison Statistics:\n")
    cat(sprintf("    Alpha (annualized): %+.2f%%\n", alpha * 100))
    cat(sprintf("    Beta: %.2f\n", beta))
    cat(sprintf("    Correlation: %.2f\n", correlation))
    cat(sprintf("    Information Ratio: %.2f\n", info_ratio))
    cat(sprintf("    Tracking Error: %.2f%%\n", tracking_error * 100))
    
    # ----- ROLLING ANALYSIS -----
    cat("\n----- ROLLING PERFORMANCE METRICS -----\n")
    
    # Define rolling periods
    rolling_periods <- c(21, 63, 126, 252)
    rolling_names <- c("1M", "3M", "6M", "1Y")
    
    for (i in 1:length(rolling_periods)) {
      period <- rolling_periods[i]
      name <- rolling_names[i]
      
      if (nrow(port_returns) >= period * 2) {  # Ensure enough data for meaningful analysis
        cat(sprintf("\n  Rolling %s Statistics:\n", name))
        
        # Calculate rolling returns
        roll_port <- rollapply(port_returns[, 1], width = period, FUN = function(x) prod(1 + x) - 1, 
                               by.column = FALSE, align = "right")
        roll_bench <- rollapply(port_returns[, 2], width = period, FUN = function(x) prod(1 + x) - 1, 
                                by.column = FALSE, align = "right")
        
        # Calculate rolling volatility
        roll_port_vol <- rollapply(port_returns[, 1], width = period, 
                                   FUN = function(x) sd(x) * sqrt(252), 
                                   by.column = FALSE, align = "right")
        roll_bench_vol <- rollapply(port_returns[, 2], width = period, 
                                    FUN = function(x) sd(x) * sqrt(252), 
                                    by.column = FALSE, align = "right")
        
        # Calculate rolling Sharpe ratio
        roll_port_sharpe <- roll_port / roll_port_vol
        roll_bench_sharpe <- roll_bench / roll_bench_vol
        
        # Calculate rolling correlation
        roll_corr <- rollapply(port_returns, width = period, 
                               FUN = function(x) cor(x[, 1], x[, 2], use = "pairwise.complete.obs"), 
                               by.column = FALSE, align = "right")
        
        # Calculate rolling outperformance
        roll_outperf <- roll_port - roll_bench
        
        # Print summary statistics
        port_avg <- mean(roll_port, na.rm = TRUE)
        bench_avg <- mean(roll_bench, na.rm = TRUE)
        outperf_avg <- mean(roll_outperf, na.rm = TRUE)
        
        port_worst <- min(roll_port, na.rm = TRUE)
        bench_worst <- min(roll_bench, na.rm = TRUE)
        outperf_worst <- min(roll_outperf, na.rm = TRUE)
        
        port_best <- max(roll_port, na.rm = TRUE)
        bench_best <- max(roll_bench, na.rm = TRUE)
        outperf_best <- max(roll_outperf, na.rm = TRUE)
        
        cat(sprintf("    Average Returns: Portfolio = %+.2f%%, %s = %+.2f%%, Difference = %+.2f%%\n", 
                    port_avg * 100, benchmark_name, bench_avg * 100, outperf_avg * 100))
        cat(sprintf("    Best Period: Portfolio = %+.2f%%, %s = %+.2f%%, Best Outperformance = %+.2f%%\n", 
                    port_best * 100, benchmark_name, bench_best * 100, outperf_best * 100))
        cat(sprintf("    Worst Period: Portfolio = %+.2f%%, %s = %+.2f%%, Worst Outperformance = %+.2f%%\n", 
                    port_worst * 100, benchmark_name, bench_worst * 100, outperf_worst * 100))
        
        # Calculate percentage of positive return periods
        pct_pos_port <- mean(roll_port > 0, na.rm = TRUE) * 100
        pct_pos_bench <- mean(roll_bench > 0, na.rm = TRUE) * 100
        pct_outperf <- mean(roll_outperf > 0, na.rm = TRUE) * 100
        
        cat(sprintf("    Positive Periods: Portfolio = %.1f%%, %s = %.1f%%, Outperformance = %.1f%%\n", 
                    pct_pos_port, benchmark_name, pct_pos_bench, pct_outperf))
      }
    }
    
    # Return the calculated performance metrics
    return(list(
      portfolio_returns = port_returns[, 1],
      benchmark_returns = port_returns[, 2],
      benchmark_name = benchmark_name,
      total_return = total_port_return,
      annual_return = annual_port_return,
      volatility = port_vol,
      sharpe = port_sharpe,
      sortino = port_sortino,
      max_drawdown = port_max_dd,
      alpha = alpha,
      beta = beta,
      correlation = correlation,
      info_ratio = info_ratio,
      tracking_error = tracking_error,
      yearly_stats = yearly_stats
    ))
  } else {
    cat("No common assets found between portfolio weights and return data\n")
    return(NULL)
  }
}

# NEW: Generate performance heatmap by year and month
generate_performance_heatmap <- function(portfolio_returns, benchmark_ticker = "SPY", 
                                         title = "Monthly Returns Heatmap") {
  if (!requireNamespace("ggplot2", quietly = TRUE) || 
      !requireNamespace("reshape2", quietly = TRUE)) {
    warning("Packages ggplot2 and reshape2 are required for heatmap generation")
    return(NULL)
  }
  
  # Ensure portfolio_returns is an xts object
  if (!is.xts(portfolio_returns)) {
    warning("Portfolio returns must be an xts object")
    return(NULL)
  }
  
  # Convert daily returns to monthly returns
  monthly_returns <- apply.monthly(portfolio_returns, function(x) prod(1 + x) - 1)
  
  # Create year and month columns
  monthly_returns_df <- data.frame(
    date = index(monthly_returns),
    returns = coredata(monthly_returns),
    stringsAsFactors = FALSE
  )
  
  monthly_returns_df$year <- format(monthly_returns_df$date, "%Y")
  monthly_returns_df$month <- format(monthly_returns_df$date, "%b")
  
  # Set factor levels for months to ensure correct order
  month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  monthly_returns_df$month <- factor(monthly_returns_df$month, levels = month_levels)
  
  # Create a wide format data frame with years as columns and months as rows
  heatmap_data <- reshape2::dcast(monthly_returns_df, month ~ year, value.var = "returns")
  
  # Convert to long format for ggplot2
  heatmap_data_long <- reshape2::melt(heatmap_data, id.vars = "month", 
                                      variable.name = "year", value.name = "returns")
  
  # Create color scale for returns
  color_scale <- scale_fill_gradient2(
    low = "firebrick3", 
    mid = "white", 
    high = "forestgreen", 
    midpoint = 0, 
    limits = c(-max(abs(heatmap_data_long$returns), na.rm = TRUE), 
               max(abs(heatmap_data_long$returns), na.rm = TRUE)),
    na.value = "gray90",
    labels = scales::percent_format()
  )
  
  # Create heatmap
  heatmap <- ggplot(heatmap_data_long, aes(x = year, y = month, fill = returns)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = ifelse(is.na(returns), "", 
                                 sprintf("%+.1f%%", returns * 100))), 
              size = 3) +
    color_scale +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    labs(title = title,
         subtitle = paste("Performance by Month and Year"))
  
  return(heatmap)
}

# Function to analyze yearly performance with seasonal patterns
analyze_yearly_performance <- function(portfolio_weights, returns, 
                                       benchmark_ticker = "SPY", 
                                       start_date = NULL, end_date = NULL) {
  # Validate inputs
  if (is.null(portfolio_weights) || is.null(returns)) {
    warning("Portfolio weights and returns data are required")
    return(NULL)
  }
  
  # Filter date range if specified
  if (!is.null(start_date)) {
    returns <- returns[paste0(start_date, "/")]
  }
  if (!is.null(end_date)) {
    returns <- returns[paste0("/", end_date)]
  }
  
  # Calculate portfolio returns
  common_assets <- intersect(names(portfolio_weights), colnames(returns))
  
  if (length(common_assets) == 0) {
    warning("No common assets between weights and returns data")
    return(NULL)
  }
  
  # Normalize weights to use only common assets
  weights_subset <- portfolio_weights[common_assets]
  weights_subset <- weights_subset / sum(weights_subset)
  
  # Calculate weighted returns
  weighted_returns <- returns[, common_assets] * matrix(
    weights_subset, 
    nrow = nrow(returns), 
    ncol = length(common_assets), 
    byrow = TRUE
  )
  portfolio_returns <- xts(rowSums(weighted_returns), order.by = index(returns))
  colnames(portfolio_returns) <- "portfolio"
  
  # Add benchmark if available
  if (benchmark_ticker %in% colnames(returns)) {
    benchmark_returns <- returns[, benchmark_ticker]
    colnames(benchmark_returns) <- benchmark_ticker
    all_returns <- merge(portfolio_returns, benchmark_returns)
  } else {
    all_returns <- portfolio_returns
    benchmark_ticker <- NULL
  }
  
  # Calculate monthly returns
  monthly_returns <- apply.monthly(all_returns, function(x) prod(1 + x) - 1)
  
  # Calculate yearly returns
  yearly_returns <- apply.yearly(all_returns, function(x) prod(1 + x) - 1)
  
  # Calculate quarterly returns
  quarterly_returns <- apply.quarterly(all_returns, function(x) prod(1 + x) - 1)
  
  # Calculate monthly statistics
  month_stats <- data.frame(
    month = month.abb,
    avg_return = rep(NA, 12),
    best_return = rep(NA, 12),
    worst_return = rep(NA, 12),
    positive_pct = rep(NA, 12),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:12) {
    month_data <- monthly_returns[format(index(monthly_returns), "%m") == sprintf("%02d", i), "portfolio"]
    if (length(month_data) > 0) {
      month_stats$avg_return[i] <- mean(month_data, na.rm = TRUE)
      month_stats$best_return[i] <- max(month_data, na.rm = TRUE)
      month_stats$worst_return[i] <- min(month_data, na.rm = TRUE)
      month_stats$positive_pct[i] <- mean(month_data > 0, na.rm = TRUE) * 100
    }
  }
  
  # Calculate quarterly statistics
  quarter_stats <- data.frame(
    quarter = paste0("Q", 1:4),
    avg_return = rep(NA, 4),
    best_return = rep(NA, 4),
    worst_return = rep(NA, 4),
    positive_pct = rep(NA, 4),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:4) {
    quarter_data <- quarterly_returns[format(index(quarterly_returns), "%q") == as.character(i), "portfolio"]
    if (length(quarter_data) > 0) {
      quarter_stats$avg_return[i] <- mean(quarter_data, na.rm = TRUE)
      quarter_stats$best_return[i] <- max(quarter_data, na.rm = TRUE)
      quarter_stats$worst_return[i] <- min(quarter_data, na.rm = TRUE)
      quarter_stats$positive_pct[i] <- mean(quarter_data > 0, na.rm = TRUE) * 100
    }
  }
  
  # Create a year-by-year performance summary
  years <- unique(format(index(yearly_returns), "%Y"))
  yearly_summary <- data.frame(
    year = years,
    portfolio_return = NA,
    benchmark_return = NA,
    excess_return = NA,
    portfolio_volatility = NA,
    benchmark_volatility = NA,
    portfolio_sharpe = NA,
    benchmark_sharpe = NA,
    portfolio_maxdd = NA,
    benchmark_maxdd = NA,
    stringsAsFactors = FALSE
  )
  
  for (i in 1:length(years)) {
    year <- years[i]
    year_start <- as.Date(paste0(year, "-01-01"))
    year_end <- as.Date(paste0(year, "-12-31"))
    
    # Get data for this year
    year_data <- all_returns[paste0(year_start, "/", year_end)]
    
    if (nrow(year_data) > 0) {
      # Calculate returns
      yearly_summary$portfolio_return[i] <- prod(1 + year_data[, "portfolio"]) - 1
      
      # Calculate volatility
      yearly_summary$portfolio_volatility[i] <- sd(year_data[, "portfolio"], na.rm = TRUE) * sqrt(252)
      
      # Calculate Sharpe ratio (assuming risk-free rate of 0 for simplicity)
      yearly_summary$portfolio_sharpe[i] <- yearly_summary$portfolio_return[i] / 
        yearly_summary$portfolio_volatility[i]
      
      # Calculate maximum drawdown
      yearly_summary$portfolio_maxdd[i] <- tryCatch(
        maxDrawdown(year_data[, "portfolio"]),
        error = function(e) NA
      )
      
      # Calculate benchmark metrics if available
      if (!is.null(benchmark_ticker) && benchmark_ticker %in% colnames(year_data)) {
        yearly_summary$benchmark_return[i] <- prod(1 + year_data[, benchmark_ticker]) - 1
        yearly_summary$benchmark_volatility[i] <- sd(year_data[, benchmark_ticker], na.rm = TRUE) * sqrt(252)
        yearly_summary$benchmark_sharpe[i] <- yearly_summary$benchmark_return[i] / 
          yearly_summary$benchmark_volatility[i]
        yearly_summary$benchmark_maxdd[i] <- tryCatch(
          maxDrawdown(year_data[, benchmark_ticker]),
          error = function(e) NA
        )
        
        # Calculate excess return
        yearly_summary$excess_return[i] <- yearly_summary$portfolio_return[i] - 
          yearly_summary$benchmark_return[i]
      }
    }
  }
  
  # Return results
  results <- list(
    yearly_summary = yearly_summary,
    monthly_stats = month_stats,
    quarterly_stats = quarter_stats,
    monthly_returns = monthly_returns,
    quarterly_returns = quarterly_returns,
    yearly_returns = yearly_returns
  )
  
  return(results)
}