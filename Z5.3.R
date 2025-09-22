#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - VERSION Z5.3.R
# PART 1: DATA HANDLING, VOLATILITY ESTIMATION, AND COVARIANCE CALCULATION
#=============================================================================

# Load required packages with reliable error handling
required_packages <- c("tidyverse", "quantmod", "xts", "PerformanceAnalytics",
                       "TTR", "zoo", "tidyquant", "ggplot2", "reshape2", 
                       "rugarch", "nloptr")

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

# Debug helper function - NEW in Z5.3.R
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
  
  # Ensure we have a valid date index
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
estimate_ewma_covariance <- function(returns, lambda = 0.94, min_obs = 30) {  # CHANGED: Reduced min_obs from 60 to 30
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
  
  # 10. NEW: Volatility indicator directly from returns - Z5.3.R ADDITION
  cat("Creating volatility indicator from returns...\n")
  # Calculate 20-day rolling volatility of SPY if available, otherwise use portfolio vol
  if ("SPY" %in% colnames(returns)) {
    spy_returns <- returns[, "SPY"]
    rolling_vol <- rollapply(spy_returns, width = 20, 
                             FUN = function(x) sd(x, na.rm = TRUE) * sqrt(252),
                             by.column = FALSE, align = "right")
    rolling_vol <- na.locf(rolling_vol, fromLast = TRUE, na.rm = FALSE)
    indicators_list[["REALIZED_VOL"]] <- as.numeric(rolling_vol)
  } else {
    # Use average volatility of all assets
    all_vols <- rollapply(returns, width = 20, 
                          FUN = function(x) mean(apply(x, 2, sd, na.rm = TRUE)) * sqrt(252),
                          by.column = FALSE, align = "right")
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
  
  # NEW in Z5.3.R: Use shorter window for z-scores (126 days instead of 252)
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

# ENHANCED: More responsive cash allocation with regime awareness - Z5.3.R IMPROVED VERSION
calculate_cash_allocation <- function(current_drawdown, max_drawdown, vol_zscore, regime, 
                                      max_cash_pct = 0.50, cash_ramp_factor = 2.0) {
  # Start with zero cash
  cash_pct <- 0
  
  # IMPROVED: Smoother, more gradual cash allocation - starts at 30% of max_drawdown
  if (current_drawdown > 0.3 * max_drawdown) {  # Start earlier at 30% (was 40%)
    dd_ratio <- current_drawdown / max_drawdown
    # More progressive scaling with new parameter
    dd_cash_pct <- min(max_cash_pct, (dd_ratio - 0.3) * cash_ramp_factor * max_cash_pct)
    cash_pct <- max(cash_pct, dd_cash_pct)
    debug_print(sprintf("Drawdown-based cash: %.1f%% (DD: %.1f%% of max)", 
                        dd_cash_pct * 100, dd_ratio * 100))
  }
  
  # IMPROVED: More gradual vol-based cash allocation
  if (vol_zscore > 0.5) {  # Even lower threshold (was 0.75)
    vol_cash_pct <- min(max_cash_pct, (vol_zscore - 0.5) * 0.2)
    old_cash <- cash_pct
    cash_pct <- max(cash_pct, vol_cash_pct)
    
    if (cash_pct > old_cash) {
      debug_print(sprintf("Volatility-based cash increase: %.1f%% → %.1f%% (Z-score: %.2f)", 
                          old_cash * 100, cash_pct * 100, vol_zscore))
    }
  }
  
  # ENHANCED: More nuanced regime-based cash allocation
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
  
  # NEW: Ensure we never have more than max_cash_pct
  cash_pct <- min(cash_pct, max_cash_pct)
  
  return(cash_pct)
}

# NEW: Function to check for recent regime changes - Z5.3.R ADDITION
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

cat("\nPart 1 of Z5.3.R loaded: Enhanced data handling, volatility forecasting, and market indicators\n")
#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - VERSION Z5.3.R
# PART 2: REGIME DETECTION, RISK PARITY OPTIMIZATION, PORTFOLIO CONSTRUCTION
#=============================================================================

# Log system information with user details
cat("\n========================================================\n")
cat(sprintf("Current Date and Time (UTC): %s\n", "2025-09-04 14:23:57"))
cat(sprintf("Current User's Login: %s\n", "balint27ni"))
cat("========================================================\n")

#=============================================================================
# COMPLETELY OVERHAULED REGIME DETECTION WITH GUARANTEED ASSIGNMENT
#=============================================================================

# Calculate Z-scores with improved smoothing
calculate_safe_zscore <- function(current_value, history, smoothing_window = 5) {
  # Safety checks
  if (is.null(current_value) || is.null(history) || length(history) < 5) {
    return(0)  # Default to neutral if insufficient data - REDUCED requirement
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

# COMPLETELY REVISED: Enhanced regime detection with more sensitive thresholds
# and guaranteed regime assignment
detect_market_regime <- function(market_data, lookback = 126, default_regime = "growth", 
                                 force_regime = TRUE) {
  cat("\n---- ENHANCED REGIME DETECTION (Z5.3.R) ----\n")
  
  # Safety check - REDUCED minimum requirement from 60 to 30 days
  if (is.null(market_data) || nrow(market_data) < 30) { 
    warning("Insufficient market data for regime detection. Using default growth regime.")
    debug_print("Using default growth regime due to insufficient data", important = TRUE)
    
    return(list(
      regime = default_regime,
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
  latest_date <- index(latest_data)[1]
  cat(sprintf("Detecting regime for date: %s\n", as.character(latest_date)))
  
  # Get historical data for z-score calculation
  # IMPORTANT FIX: Only require lookback/3 days instead of half lookback
  min_history_days <- min(30, floor(lookback/3))
  
  if (nrow(market_data) <= min_history_days) {
    # If we don't have enough data, use all but the latest point
    historical_data <- head(market_data, nrow(market_data) - 1)
    cat(sprintf("Limited history: using %d days for regime detection\n", nrow(historical_data)))
  } else {
    # Otherwise use at least min_history_days
    history_length <- min(lookback, max(min_history_days, nrow(market_data) - 1))
    historical_data <- tail(head(market_data, nrow(market_data) - 1), history_length)
    cat(sprintf("Using %d days of history for regime detection\n", nrow(historical_data)))
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
  } else if ("REALIZED_VOL" %in% colnames(market_data)) {
    # NEW: Use realized volatility if VIX not available
    vol_history <- as.numeric(historical_data[, "REALIZED_VOL"])
    vol_current <- as.numeric(latest_data[, "REALIZED_VOL"])
    z_scores$volatility <- calculate_safe_zscore(vol_current, vol_history)
    cat(sprintf("Realized Volatility Z-score: %.2f (Current: %.1f%%)\n", 
                z_scores$volatility, vol_current * 100))
  } else {
    z_scores$volatility <- 0
    cat("Volatility data not available, using neutral volatility signal\n")
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
  } else if ("SPY" %in% colnames(market_data) && "IEF" %in% colnames(market_data)) {
    # NEW: Direct calculation if indicator not available but components are
    cat("Calculating growth indicator directly from SPY/IEF\n")
    spy_prices <- market_data[, "SPY"]
    ief_prices <- market_data[, "IEF"]
    growth_ratio <- spy_prices / ief_prices
    
    growth_history <- as.numeric(growth_ratio[index(historical_data)])
    growth_current <- as.numeric(growth_ratio[index(latest_data)])
    
    z_scores$growth <- calculate_safe_zscore(growth_current, growth_history)
    cat(sprintf("Direct Growth Z-score: %.2f (Current ratio: %.2f)\n", 
                z_scores$growth, growth_current))
  } else {
    # NEW: Default to price momentum of equity if no other growth indicator
    if ("SPY" %in% colnames(market_data)) {
      spy_returns <- ROC(market_data[, "SPY"], n = 60, type = "discrete")
      mom_history <- as.numeric(spy_returns[index(historical_data)])
      mom_current <- as.numeric(spy_returns[index(latest_data)])
      
      z_scores$growth <- calculate_safe_zscore(mom_current, mom_history)
      cat(sprintf("Fallback Growth Z-score (SPY momentum): %.2f\n", z_scores$growth))
    } else {
      z_scores$growth <- 0
      cat("Growth data not available, using neutral growth signal\n")
    }
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
  } else if (all(c("TIP", "IEF") %in% colnames(market_data))) {
    # NEW: Direct calculation if indicator not available
    cat("Calculating inflation indicator directly from TIP/IEF\n")
    tip_prices <- market_data[, "TIP"]
    ief_prices <- market_data[, "IEF"]
    infl_ratio <- tip_prices / ief_prices
    
    infl_history <- as.numeric(infl_ratio[index(historical_data)])
    infl_current <- as.numeric(infl_ratio[index(latest_data)])
    
    z_scores$inflation <- calculate_safe_zscore(infl_current, infl_history)
    cat(sprintf("Direct Inflation Z-score: %.2f (Current ratio: %.2f)\n", 
                z_scores$inflation, infl_current))
  } else if (all(c("GLD", "IEF") %in% colnames(market_data))) {
    # Alternative using gold
    cat("Calculating inflation indicator using GLD/IEF\n")
    gld_prices <- market_data[, "GLD"]
    ief_prices <- market_data[, "IEF"]
    infl_ratio <- gld_prices / ief_prices
    
    infl_history <- as.numeric(infl_ratio[index(historical_data)])
    infl_current <- as.numeric(infl_ratio[index(latest_data)])
    
    z_scores$inflation <- calculate_safe_zscore(infl_current, infl_history)
    cat(sprintf("Gold-based Inflation Z-score: %.2f (Current ratio: %.2f)\n", 
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
  } else if (all(c("LQD", "IEF") %in% colnames(market_data))) {
    # Direct calculation
    cat("Calculating credit indicator directly from LQD/IEF\n")
    lqd_prices <- market_data[, "LQD"]
    ief_prices <- market_data[, "IEF"]
    credit_ratio <- lqd_prices / ief_prices
    
    credit_history <- as.numeric(credit_ratio[index(historical_data)])
    credit_current <- as.numeric(credit_ratio[index(latest_data)])
    
    z_scores$credit <- calculate_safe_zscore(credit_current, credit_history)
    cat(sprintf("Direct Credit Z-score: %.2f (Current ratio: %.2f)\n", 
                z_scores$credit, credit_current))
  } else {
    z_scores$credit <- 0
    cat("Credit spread data not available, using neutral credit signal\n")
  }
  
  # 5. Calculate Bond-Equity Correlation Z-score
  if ("BOND_EQUITY_CORR_Z" %in% colnames(market_data)) {
    z_scores$bond_equity_corr <- as.numeric(latest_data[, "BOND_EQUITY_CORR_Z"])
    cat(sprintf("Bond-Equity Correlation Z-score: %.2f\n", z_scores$bond_equity_corr))
  } else if ("BOND_EQUITY_CORR" %in% colnames(market_data)) {
    corr_history <- as.numeric(historical_data[, "BOND_EQUITY_CORR"])
    corr_current <- as.numeric(latest_data[, "BOND_EQUITY_CORR"])
    z_scores$bond_equity_corr <- calculate_safe_zscore(corr_current, corr_history)
    cat(sprintf("Bond-Equity Correlation Z-score: %.2f (Current: %.2f)\n", 
                z_scores$bond_equity_corr, corr_current))
  } else if (all(c("SPY", "IEF") %in% colnames(market_data))) {
    # Calculate rolling correlation directly
    cat("Calculating bond-equity correlation directly\n")
    spy_returns <- ROC(market_data[, "SPY"], type = "discrete")
    ief_returns <- ROC(market_data[, "IEF"], type = "discrete")
    
    # Create combined data
    combined_returns <- merge(spy_returns, ief_returns)
    combined_returns <- na.omit(combined_returns)
    
    # Need at least 20 days for a meaningful correlation
    if (nrow(combined_returns) >= 20) {
      # Use a 60-day rolling window if possible, otherwise as much as we have
      window_size <- min(60, nrow(combined_returns) - 1)
      
      roll_corr <- rollapply(combined_returns, 
                             width = window_size, 
                             function(x) cor(x[,1], x[,2], use = "pairwise.complete.obs"),
                             by.column = FALSE, 
                             align = "right")
      
      corr_history <- as.numeric(roll_corr[index(historical_data)])
      corr_current <- as.numeric(tail(roll_corr, 1))
      
      z_scores$bond_equity_corr <- calculate_safe_zscore(corr_current, corr_history)
      cat(sprintf("Direct Bond-Equity Correlation Z-score: %.2f (Current: %.2f)\n", 
                  z_scores$bond_equity_corr, corr_current))
    } else {
      z_scores$bond_equity_corr <- 0
    }
  } else {
    z_scores$bond_equity_corr <- 0
    cat("Bond-Equity Correlation data not available, using neutral signal\n")
  }
  
  # 6. Calculate Commodity Trend Z-score
  if ("COMMODITY_TREND_Z" %in% colnames(market_data)) {
    z_scores$commodity <- as.numeric(latest_data[, "COMMODITY_TREND_Z"])
    cat(sprintf("Commodity Trend Z-score: %.2f\n", z_scores$commodity))
  } else if ("COMMODITY_TREND" %in% colnames(market_data)) {
    comm_history <- as.numeric(historical_data[, "COMMODITY_TREND"])
    comm_current <- as.numeric(latest_data[, "COMMODITY_TREND"])
    z_scores$commodity <- calculate_safe_zscore(comm_current, comm_history)
    cat(sprintf("Commodity Trend Z-score: %.2f (Current: %.2f)\n", 
                z_scores$commodity, comm_current))
  } else if ("DBC" %in% colnames(market_data)) {
    # Calculate DBC momentum directly
    cat("Calculating commodity trend directly from DBC\n")
    dbc_mom <- ROC(market_data[, "DBC"], n = 60, type = "discrete")
    
    comm_history <- as.numeric(dbc_mom[index(historical_data)])
    comm_current <- as.numeric(dbc_mom[index(latest_data)])
    
    z_scores$commodity <- calculate_safe_zscore(comm_current, comm_history)
    cat(sprintf("Direct Commodity Trend Z-score: %.2f (Current: %.2f)\n", 
                z_scores$commodity, comm_current))
  } else if ("GLD" %in% colnames(market_data)) {
    # Use gold momentum as fallback
    cat("Using GLD momentum as fallback commodity trend\n")
    gld_mom <- ROC(market_data[, "GLD"], n = 60, type = "discrete")
    
    comm_history <- as.numeric(gld_mom[index(historical_data)])
    comm_current <- as.numeric(gld_mom[index(latest_data)])
    
    z_scores$commodity <- calculate_safe_zscore(comm_current, comm_history)
    cat(sprintf("Gold-based Commodity Trend Z-score: %.2f (Current: %.2f)\n", 
                z_scores$commodity, comm_current))
  } else {
    z_scores$commodity <- 0
    cat("Commodity Trend data not available, using neutral signal\n")
  }
  
  # 7. Calculate Yield Curve Z-score (if available)
  if ("YIELD_CURVE_Z" %in% colnames(market_data)) {
    z_scores$yield_curve <- as.numeric(latest_data[, "YIELD_CURVE_Z"])
    cat(sprintf("Yield Curve Z-score: %.2f\n", z_scores$yield_curve))
  } else if ("YIELD_CURVE" %in% colnames(market_data)) {
    yc_history <- as.numeric(historical_data[, "YIELD_CURVE"])
    yc_current <- as.numeric(latest_data[, "YIELD_CURVE"])
    z_scores$yield_curve <- calculate_safe_zscore(yc_current, yc_history)
    cat(sprintf("Yield Curve Z-score: %.2f (Current: %.2f)\n", 
                z_scores$yield_curve, yc_current))
  } else if (all(c("SHY", "TLT") %in% colnames(market_data))) {
    # Calculate directly
    cat("Calculating yield curve directly from SHY/TLT\n")
    shy_prices <- market_data[, "SHY"]
    tlt_prices <- market_data[, "TLT"]
    yc_ratio <- shy_prices / tlt_prices
    
    yc_history <- as.numeric(yc_ratio[index(historical_data)])
    yc_current <- as.numeric(yc_ratio[index(latest_data)])
    
    z_scores$yield_curve <- calculate_safe_zscore(yc_current, yc_history)
    cat(sprintf("Direct Yield Curve Z-score: %.2f (Current: %.2f)\n", 
                z_scores$yield_curve, yc_current))
  } else {
    z_scores$yield_curve <- 0
    cat("Yield Curve data not available, using neutral signal\n")
  }
  
  # 8. NEW: Added Market Breadth Z-score
  if ("MARKET_BREADTH_Z" %in% colnames(market_data)) {
    z_scores$market_breadth <- as.numeric(latest_data[, "MARKET_BREADTH_Z"])
    cat(sprintf("Market Breadth Z-score: %.2f\n", z_scores$market_breadth))
  } else if ("MARKET_BREADTH" %in% colnames(market_data)) {
    mb_history <- as.numeric(historical_data[, "MARKET_BREADTH"])
    mb_current <- as.numeric(latest_data[, "MARKET_BREADTH"])
    z_scores$market_breadth <- calculate_safe_zscore(mb_current, mb_history)
    cat(sprintf("Market Breadth Z-score: %.2f (Current: %.2f)\n", 
                z_scores$market_breadth, mb_current))
  } else {
    z_scores$market_breadth <- 0
  }
  
  # Initialize regime probabilities with minimum probabilities
  # INCREASED minimum probability floors to avoid empty regimes
  regime_probs <- list(
    growth = 0.10,       # Was 0.05
    reflation = 0.05,    # Was 0.01
    deflation = 0.05,    # Was 0.01
    stagflation = 0.05,  # Was 0.01
    risk_off = 0.05      # Was 0.01
  )
  
  # Define threshold for binary classification - FURTHER LOWERED from 0.3 to 0.25
  threshold <- 0.25
  
  # Secondary threshold for weak signals
  weak_threshold <- 0.15  # NEW: Added for catching more regime changes
  
  # Define regime signals with clear thresholds
  
  # Volatility signal (high = risk_off)
  vol_signal <- z_scores$volatility > threshold
  weak_vol_signal <- z_scores$volatility > weak_threshold
  
  # Growth signal (positive = growth, negative = contraction)
  growth_signal <- z_scores$growth > threshold
  growth_neg_signal <- z_scores$growth < -threshold
  weak_growth_signal <- z_scores$growth > weak_threshold
  weak_growth_neg_signal <- z_scores$growth < -weak_threshold
  
  # Inflation signal (positive = high inflation, negative = low inflation)
  inflation_signal <- z_scores$inflation > threshold
  inflation_neg_signal <- z_scores$inflation < -threshold
  weak_inflation_signal <- z_scores$inflation > weak_threshold
  weak_inflation_neg_signal <- z_scores$inflation < -weak_threshold
  
  # Bond-equity correlation signal (positive = bonds don't diversify stocks)
  corr_signal <- !is.null(z_scores$bond_equity_corr) && z_scores$bond_equity_corr > threshold
  
  # Commodity trend signal (negative = deflationary pressure)
  commodity_neg_signal <- !is.null(z_scores$commodity) && z_scores$commodity < -threshold
  
  # Yield curve signal (negative = recession risk)
  yield_curve_neg_signal <- !is.null(z_scores$yield_curve) && z_scores$yield_curve < -threshold
  
  # Market breadth signal (negative = narrow market, potential weakness)
  narrow_market_signal <- !is.null(z_scores$market_breadth) && z_scores$market_breadth < -threshold
  
  # RECALIBRATED: Calculate regime probabilities with enhanced detection
  
  # 1. Risk-off regime (high volatility dominates)
  if (vol_signal) {
    regime_probs$risk_off = 0.5 + min((z_scores$volatility - threshold) * 0.2, 0.4)
    
    # Add bond-equity correlation impact - positive correlation increases risk-off probability
    if (corr_signal) {
      regime_probs$risk_off = min(0.95, regime_probs$risk_off + 0.15)
    }
    
    # Add yield curve impact - inverted curve increases risk-off probability
    if (yield_curve_neg_signal) {
      regime_probs$risk_off = min(0.95, regime_probs$risk_off + 0.15)
    }
    
    # Narrow market breadth increases risk-off probability
    if (narrow_market_signal) {
      regime_probs$risk_off = min(0.95, regime_probs$risk_off + 0.10)
    }
    
    # Negative growth strengthens risk-off even more
    if (growth_neg_signal) {
      regime_probs$risk_off = min(0.95, regime_probs$risk_off + 0.15)
    }
  } else if (weak_vol_signal) {
    # Even with weak volatility signal, increase risk-off probability somewhat
    regime_probs$risk_off = 0.25 + (z_scores$volatility - weak_threshold) * 0.5
    
    # Additional factors can still push it toward risk-off
    if (narrow_market_signal || yield_curve_neg_signal || corr_signal) {
      regime_probs$risk_off = min(0.8, regime_probs$risk_off + 0.2)
    }
  } else {
    # Base level risk-off probability
    regime_probs$risk_off = max(0.05, (z_scores$volatility + 0.2) * 0.25)
  }
  
  # 2. Growth regime (positive growth, controlled inflation)
  if (growth_signal && !inflation_signal && !vol_signal) {
    regime_probs$growth = 0.5 + min(z_scores$growth * 0.15, 0.4)
    
    # Positive market breadth strengthens growth signal
    if (!is.null(z_scores$market_breadth) && z_scores$market_breadth > weak_threshold) {
      regime_probs$growth = min(0.95, regime_probs$growth + 0.1)
    }
  } else if (weak_growth_signal && !inflation_signal && !vol_signal) {
    # Even weak growth signal can point to growth regime
    regime_probs$growth = 0.3 + min(z_scores$growth * 0.15, 0.3)
  } else if (z_scores$growth > 0) {
    regime_probs$growth = max(0.15, z_scores$growth * 0.35)
  } else {
    regime_probs$growth = max(0.10, 0.15 - abs(z_scores$growth) * 0.05)
  }
  
  # 3. Reflation regime (positive growth AND rising inflation)
  if (growth_signal && inflation_signal && !vol_signal) {
    regime_probs$reflation = 0.5 + 
      min((z_scores$growth + z_scores$inflation) * 0.1, 0.4)
    
    # Commodity trend strengthens reflation signal
    if (!is.null(z_scores$commodity) && z_scores$commodity > weak_threshold) {
      regime_probs$reflation = min(0.95, regime_probs$reflation + 0.15)
    }
  } else if (weak_growth_signal && weak_inflation_signal && !vol_signal) {
    # Even weak signals can suggest reflation
    regime_probs$reflation = 0.3 + 
      min((z_scores$growth + z_scores$inflation) * 0.1, 0.3)
  } else if (z_scores$growth > 0 && z_scores$inflation > 0) {
    regime_probs$reflation = max(0.05, z_scores$growth * 0.2 + z_scores$inflation * 0.3)
  } else {
    regime_probs$reflation = 0.05
  }
  
  # 4. ENHANCED: Deflation regime (negative growth AND negative inflation)
  if (growth_neg_signal && inflation_neg_signal) {
    # Base probability - more sensitive
    regime_probs$deflation = 0.5 + 
      min((abs(z_scores$growth) + abs(z_scores$inflation)) * 0.1, 0.4)
    
    # Enhance with negative commodity trends
    if (commodity_neg_signal) {
      regime_probs$deflation = min(0.95, regime_probs$deflation + 0.15)
    }
    
    # Credit deterioration can also indicate deflation
    if (!is.null(z_scores$credit) && z_scores$credit < -threshold) {
      regime_probs$deflation = min(0.95, regime_probs$deflation + 0.10)
    }
    
    # Yield curve inversion is a strong deflation signal
    if (yield_curve_neg_signal) {
      regime_probs$deflation = min(0.95, regime_probs$deflation + 0.15)
    }
    
  } else if (weak_growth_neg_signal && weak_inflation_neg_signal) {
    # Even weak signals can suggest deflation
    regime_probs$deflation = 0.3 + 
      min((abs(z_scores$growth) + abs(z_scores$inflation)) * 0.1, 0.3)
    
    # Other factors can strengthen the signal
    if (commodity_neg_signal || yield_curve_neg_signal) {
      regime_probs$deflation = min(0.8, regime_probs$deflation + 0.2)
    }
  } else if (z_scores$growth < 0 && z_scores$inflation < 0) {
    # Base probability with continuous scaling - more sensitive
    regime_probs$deflation = max(0.10, abs(z_scores$growth) * 0.25 + abs(z_scores$inflation) * 0.25)
    
    # Enhance with commodity trends and credit signals
    if (commodity_neg_signal) {
      regime_probs$deflation = min(0.9, regime_probs$deflation + 0.15)
    }
    
    if (!is.null(z_scores$credit) && z_scores$credit < -0.25) { # More sensitive threshold
      regime_probs$deflation = min(0.9, regime_probs$deflation + 0.15)
    }
  } else {
    regime_probs$deflation = max(0.05, min(0.2, 
                                           (abs(z_scores$growth) * 0.1) + (abs(z_scores$inflation) * 0.1)))
  }
  
  # 5. Stagflation regime (negative growth AND positive inflation)
  if (growth_neg_signal && inflation_signal) {
    regime_probs$stagflation = 0.5 + 
      min((abs(z_scores$growth) + z_scores$inflation) * 0.1, 0.4)
    
    # Commodity trend strengthens stagflation signal
    if (!is.null(z_scores$commodity) && z_scores$commodity > weak_threshold) {
      regime_probs$stagflation = min(0.95, regime_probs$stagflation + 0.15)
    }
  } else if (weak_growth_neg_signal && weak_inflation_signal) {
    # Even weak signals can suggest stagflation
    regime_probs$stagflation = 0.3 + 
      min((abs(z_scores$growth) + z_scores$inflation) * 0.1, 0.3)
  } else if (z_scores$growth < 0 && z_scores$inflation > 0) {
    regime_probs$stagflation = max(0.05, abs(z_scores$growth) * 0.25 + z_scores$inflation * 0.25)
  } else {
    regime_probs$stagflation = 0.05
  }
  
  # CRITICAL FIX: If no clear regime detected, increase default regime probability
  # and ensure we never have empty/undefined regime
  if (max(unlist(regime_probs)) < 0.3 || force_regime) {
    # Determine best default regime based on current z-scores
    if (z_scores$volatility > 0.5) {
      chosen_default <- "risk_off"
    } else if (z_scores$growth > 0 && z_scores$inflation > 0) {
      chosen_default <- "reflation"
    } else if (z_scores$growth > 0) {
      chosen_default <- "growth"
    } else if (z_scores$growth < 0 && z_scores$inflation < 0) {
      chosen_default <- "deflation"
    } else if (z_scores$growth < 0 && z_scores$inflation > 0) {
      chosen_default <- "stagflation"
    } else {
      chosen_default <- default_regime
    }
    
    # Boost the chosen regime probability
    if (max(unlist(regime_probs)) < 0.3) {
      # Significant boost if no clear signal
      regime_probs[[chosen_default]] <- max(regime_probs[[chosen_default]], 0.4)
      cat(sprintf("No clear regime detected, boosting '%s' regime probability to %.1f%%\n", 
                  chosen_default, regime_probs[[chosen_default]] * 100))
    } else if (force_regime) {
      # Small boost if forcing a regime assignment
      regime_probs[[chosen_default]] <- regime_probs[[chosen_default]] * 1.2
    }
  }
  
  # Ensure minimum probability for each regime
  for (regime in names(regime_probs)) {
    regime_probs[[regime]] = max(0.05, regime_probs[[regime]])
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
# IMPROVED REGIME-BASED ASSET ALLOCATION
#=============================================================================

# Get target asset allocation for each regime - ENHANCED REFLATION ALLOCATION
get_regime_weights <- function(regime, asset_classes = NULL) {
  # Define base weights for different regimes
  regime_weights <- list(
    growth = list(
      US_EQUITY = 0.30,
      INTL_DEVELOPED = 0.15,
      EMERGING_MARKETS = 0.10,
      US_TREASURY = 0.10,
      TIPS = 0.05,
      CREDIT_IG = 0.15,
      GOLD = 0.05,
      COMMODITIES = 0.00,
      REIT = 0.10,
      US_SMALL_CAP = 0.00,
      CASH = 0.00
    ),
    
    # ENHANCED: Improved reflation weights with more inflation protection
    reflation = list(
      US_EQUITY = 0.15,             # Reduced from 0.20
      INTL_DEVELOPED = 0.10,
      EMERGING_MARKETS = 0.10,      # Reduced from 0.15
      US_TREASURY = 0.00,           # Eliminated (poor in inflation)
      TIPS = 0.20,                  # DOUBLED from 0.10
      CREDIT_IG = 0.05,             # Reduced from 0.10
      GOLD = 0.15,                  # Increased from 0.10
      COMMODITIES = 0.20,           # DOUBLED from 0.10
      REIT = 0.05,                  # Reduced from 0.10
      US_SMALL_CAP = 0.00,
      CASH = 0.00
    ),
    
    deflation = list(
      US_EQUITY = 0.10,
      INTL_DEVELOPED = 0.05,
      EMERGING_MARKETS = 0.00,
      US_TREASURY = 0.40,
      TIPS = 0.05,
      CREDIT_IG = 0.15,
      GOLD = 0.10,
      COMMODITIES = 0.00,
      REIT = 0.05,
      US_SMALL_CAP = 0.00,
      CASH = 0.10                  # Explicit cash allocation
    ),
    
    stagflation = list(
      US_EQUITY = 0.10,
      INTL_DEVELOPED = 0.05,
      EMERGING_MARKETS = 0.05,
      US_TREASURY = 0.10,
      TIPS = 0.15,
      CREDIT_IG = 0.10,
      GOLD = 0.20,
      COMMODITIES = 0.10,
      REIT = 0.10,
      US_SMALL_CAP = 0.00,
      CASH = 0.05                 # Explicit cash allocation
    ),
    
    risk_off = list(
      US_EQUITY = 0.05,
      INTL_DEVELOPED = 0.00,
      EMERGING_MARKETS = 0.00,
      US_TREASURY = 0.45,
      TIPS = 0.05,
      CREDIT_IG = 0.10,
      GOLD = 0.15,
      COMMODITIES = 0.00,
      REIT = 0.00,
      US_SMALL_CAP = 0.00,
      CASH = 0.20                 # Explicit cash allocation
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
    # US Small Cap
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
    # TIPS
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
    # Commodities
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
    # Cash equivalents
    else if (ticker %in% c("BIL", "SHV", "NEAR", "MINT", "GBIL")) {
      mapping[[ticker]] <- "CASH"
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

# ENHANCED: Map asset class weights to ETF weights with better error handling
map_weights_to_etfs <- function(asset_weights, etf_mapping, preferred_etfs = NULL) {
  # Debug the inputs
  debug_print("Mapping asset class weights to ETFs")
  
  # Ensure asset_weights is a properly formatted list/vector
  if (!is.list(asset_weights) && !is.numeric(asset_weights)) {
    stop("Asset weights must be a list or numeric vector")
  }
  
  # FIXED: Convert any dataframe to vector/list for consistency
  if (is.data.frame(asset_weights)) {
    asset_weights <- as.list(asset_weights[1,])
  }
  
  # Initialize ETF weights
  etf_weights <- list()
  for (ticker in names(etf_mapping)) {
    etf_weights[[ticker]] <- 0
  }
  
  # Get unique asset classes with non-zero weights
  asset_classes <- unique(unlist(etf_mapping))
  asset_classes <- asset_classes[asset_classes %in% names(asset_weights)]
  
  # Check if we have asset classes to map
  if (length(asset_classes) == 0) {
    warning("No matching asset classes found between weights and ETF mapping")
    
    # Fall back to equal weights for all ETFs
    equal_weight <- 1 / length(etf_mapping)
    for (ticker in names(etf_mapping)) {
      etf_weights[[ticker]] <- equal_weight
    }
    
    debug_print("Using equal weights due to no matching asset classes", etf_weights)
    
    return(unlist(etf_weights))
  }
  
  debug_print(sprintf("Found %d matching asset classes", length(asset_classes)))
  
  # For each asset class with weight
  for (asset_class in asset_classes) {
    # Skip if weight is zero
    if (asset_weights[[asset_class]] == 0) {
      debug_print(sprintf("Skipping asset class %s (weight = 0)", asset_class))
      next
    }
    
    # Find all ETFs in this asset class
    class_tickers <- names(etf_mapping)[sapply(etf_mapping, function(x) x == asset_class)]
    
    # If we have no ETFs for this asset class, skip
    if (length(class_tickers) == 0) {
      cat(sprintf("Warning: No ETFs found for asset class: %s\n", asset_class))
      next
    }
    
    debug_print(sprintf("Mapping asset class %s (weight %.2f%%) to %d ETFs", 
                        asset_class, asset_weights[[asset_class]] * 100, 
                        length(class_tickers)))
    
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
  
  # FIXED: Only include ETFs with non-zero weights
  etf_weights <- etf_weights[sapply(etf_weights, function(x) x > 0)]
  
  # Normalize weights to ensure they sum to 1
  total_weight <- sum(unlist(etf_weights))
  
  if (total_weight > 0) {
    for (ticker in names(etf_weights)) {
      etf_weights[[ticker]] <- etf_weights[[ticker]] / total_weight
    }
  } else {
    warning("Total ETF weight is zero - using equal weight instead")
    equal_weight <- 1 / length(etf_mapping)
    for (ticker in names(etf_mapping)) {
      etf_weights[[ticker]] <- equal_weight
    }
    debug_print("Using equal weights due to zero total weight", etf_weights)
  }
  
  # Debug the output
  debug_print("Final ETF weights", etf_weights)
  
  # Convert to numeric vector with names before returning
  etf_weights_vector <- unlist(etf_weights)
  names(etf_weights_vector) <- names(etf_weights)
  
  return(etf_weights_vector)
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

# IMPROVED: More nuanced turnover reduction - Z5.3.R ENHANCED VERSION
optimize_for_transaction_costs <- function(prev_weights, current_weights, transaction_costs,
                                           regime, vol_zscore,
                                           base_turnover_threshold = 0.15) { # REDUCED from 0.20 to 0.15
  # Skip if either weight vector is missing
  if (is.null(prev_weights) || is.null(current_weights) || length(prev_weights) == 0 || length(current_weights) == 0) {
    debug_print("Cannot optimize for transaction costs - missing weights")
    return(current_weights)
  }
  
  # Ensure weights are properly formatted
  prev_weights <- ensure_numeric_weights(prev_weights)
  current_weights <- ensure_numeric_weights(current_weights)
  
  # Adjust turnover threshold based on regime and volatility
  turnover_threshold <- base_turnover_threshold
  
  # During stress regimes, allow more turnover
  if (!is.null(regime) && !is.na(regime) && regime != "" && 
      regime %in% c("risk_off", "deflation")) {
    turnover_threshold <- base_turnover_threshold * 1.5
    debug_print(sprintf("Increased turnover threshold to %.1f%% for %s regime", 
                        turnover_threshold * 100, regime))
  }
  
  # During high volatility, allow more turnover
  if (!is.na(vol_zscore) && vol_zscore > 1.5) {
    turnover_threshold <- turnover_threshold * 1.25
    debug_print(sprintf("Increased turnover threshold to %.1f%% for high volatility (z=%.1f)", 
                        turnover_threshold * 100, vol_zscore))
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
    
    # Add turnover from tickers that were removed
    removed_tickers <- setdiff(names(prev_weights), names(current_weights))
    if (length(removed_tickers) > 0) {
      removed_weights <- prev_weights[removed_tickers]
      removed_weights <- removed_weights[!is.na(removed_weights)]
      total_turnover <- total_turnover + sum(removed_weights)
      
      # Treat removed tickers as big changes
      big_changes <- c(big_changes, removed_tickers)
    }
    
    # Add turnover from tickers that were added
    added_tickers <- setdiff(names(current_weights), names(prev_weights))
    if (length(added_tickers) > 0) {
      added_weights <- current_weights[added_tickers]
      added_weights <- added_weights[!is.na(added_weights)]
      total_turnover <- total_turnover + sum(added_weights)
      
      # Treat added tickers as big changes
      big_changes <- c(big_changes, added_tickers)
    }
    
    # If turnover is high, moderate changes
    if (total_turnover > turnover_threshold && length(big_changes) > 0) {
      debug_print(sprintf("High turnover detected (%.1f%%), moderating changes", 
                          total_turnover * 100), important = TRUE)
      
      # Adjust blending ratio based on regime
      new_weight_ratio <- 0.7  # Default
      if (!is.null(regime) && !is.na(regime) && regime != "") {
        if (regime == "risk_off") {
          new_weight_ratio <- 0.85  # More aggressive adaptation in risk-off
          debug_print("Using aggressive 85% weight for new allocation in risk-off regime")
        } else if (regime %in% c("growth", "reflation")) {
          new_weight_ratio <- 0.75  # Moderate adaptation in growth
          debug_print("Using moderate 75% weight for new allocation in growth/reflation regime")
        } else {
          new_weight_ratio <- 0.65  # More conservative in other regimes
          debug_print("Using conservative 65% weight for new allocation")
        }
      }
      
      # Moderate large position changes
      for (ticker in big_changes) {
        if (ticker %in% names(current_weights) && ticker %in% names(prev_weights)) {
          prev_weight <- prev_weights[ticker]
          curr_weight <- current_weights[ticker]
          
          # Blend weights to reduce turnover with regime-specific ratio
          blended_weight <- new_weight_ratio * curr_weight + (1 - new_weight_ratio) * prev_weight
          current_weights[ticker] <- blended_weight
        } else if (ticker %in% names(current_weights)) {
          # New position - scale back size
          current_weights[ticker] <- current_weights[ticker] * new_weight_ratio
        }
        # Removed positions are automatically handled (no longer in current_weights)
      }
      
      # Re-normalize weights
      current_weights <- current_weights / sum(current_weights)
      
      # Recalculate turnover after blending
      new_turnover <- calculate_portfolio_turnover(prev_weights, current_weights)
      
      debug_print(sprintf("Turnover reduced from %.1f%% to %.1f%%", 
                          total_turnover * 100, new_turnover * 100))
    } else {
      debug_print(sprintf("Current turnover (%.1f%%) below threshold (%.1f%%), no changes needed", 
                          total_turnover * 100, turnover_threshold * 100))
    }
  }
  
  return(current_weights)
}

# New helper function to calculate portfolio turnover - Z5.3.R ADDITION
calculate_portfolio_turnover <- function(old_weights, new_weights) {
  # Initialize
  turnover <- 0
  
  # Process common tickers
  common_tickers <- intersect(names(old_weights), names(new_weights))
  for (ticker in common_tickers) {
    old_w <- old_weights[ticker]
    new_w <- new_weights[ticker]
    if (!is.na(old_w) && !is.na(new_w)) {
      turnover <- turnover + abs(new_w - old_w)
    }
  }
  
  # Add turnover from removed tickers
  removed_tickers <- setdiff(names(old_weights), names(new_weights))
  if (length(removed_tickers) > 0) {
    removed_weights <- old_weights[removed_tickers]
    turnover <- turnover + sum(removed_weights, na.rm = TRUE)
  }
  
  # Add turnover from added tickers
  added_tickers <- setdiff(names(new_weights), names(old_weights))
  if (length(added_tickers) > 0) {
    added_weights <- new_weights[added_tickers]
    turnover <- turnover + sum(added_weights, na.rm = TRUE)
  }
  
  return(turnover)
}

# Enhanced Risk Parity Optimizer with multiple fallback methods - Z5.3.R ENHANCED VERSION
optimize_risk_parity <- function(
    target_weights, cov_matrix, 
    min_weight = 0.01, max_weight = 0.30, 
    max_attempts = 5,
    force_optimization = TRUE) {  # NEW parameter to force optimization
  
  cat("\n---- RISK PARITY OPTIMIZATION ----\n")
  
  # Verify inputs
  if (is.null(target_weights) || is.null(cov_matrix)) {
    stop("Target weights or covariance matrix is NULL")
  }
  
  # Convert target weights to vector format if needed
  if (is.list(target_weights) && !is.vector(target_weights)) {
    target_weights <- unlist(target_weights)
  }
  
  # Get tickers from target weights
  tickers <- names(target_weights)
  
  # Convert target weights to vector format
  initial_weights <- as.numeric(target_weights)
  names(initial_weights) <- tickers
  
  # Make sure all tickers in target weights are in covariance matrix
  missing_tickers <- setdiff(tickers, rownames(cov_matrix))
  if (length(missing_tickers) > 0) {
    cat("WARNING: Some tickers in target weights missing from covariance matrix:", 
        paste(missing_tickers, collapse=", "), "\n")
    
    # Use only tickers that are in both
    common_tickers <- intersect(tickers, rownames(cov_matrix))
    
    if (length(common_tickers) == 0) {
      stop("No common tickers between target weights and covariance matrix")
    }
    
    # Subset weights and normalize
    initial_weights <- initial_weights[common_tickers]
    initial_weights <- initial_weights / sum(initial_weights)
    
    # Update tickers
    tickers <- common_tickers
    
    debug_print(sprintf("Reduced to %d common tickers for optimization", length(tickers)))
  }
  
  # Extract subset of covariance matrix matching our tickers
  cov_subset <- cov_matrix[tickers, tickers, drop = FALSE]
  
  # Verify covariance matrix
  if (any(is.na(cov_subset))) {
    warning("Covariance matrix contains NA values, fixing...")
    
    # Replace NAs with zeros
    cov_subset[is.na(cov_subset)] <- 0
    
    # Ensure diagonal has positive values
    diag(cov_subset) <- pmax(diag(cov_subset), 1e-6)
    
    debug_print("Fixed NA values in covariance matrix")
  }
  
  # Ensure covariance matrix is positive definite
  eigen_values <- eigen(cov_subset, symmetric = TRUE, only.values = TRUE)$values
  
  if (min(eigen_values) <= 0 || any(is.na(eigen_values))) {
    cat("Covariance matrix is not positive definite, applying shrinkage\n")
    
    # Shrink to identity matrix
    shrinkage_factor <- 0.1
    shrinkage_target <- diag(diag(cov_subset))
    cov_subset <- (1 - shrinkage_factor) * cov_subset + shrinkage_factor * shrinkage_target
    
    # Verify result
    eigen_values <- eigen(cov_subset, symmetric = TRUE, only.values = TRUE)$values
    cat(sprintf("After shrinkage: min eigenvalue = %.6g\n", min(eigen_values)))
  }
  
  # NEW: Skip optimization if force_optimization is FALSE and we're only using a few assets
  if (!force_optimization && length(tickers) <= 3) {
    debug_print("Skipping optimization for small portfolio, using target weights directly")
    opt_weights <- initial_weights
    opt_weights <- pmin(pmax(opt_weights, min_weight), max_weight)
    opt_weights <- opt_weights / sum(opt_weights)
    
    cat("\nUsing target weights directly (optimization skipped):\n")
    for (ticker in names(opt_weights)) {
      cat(sprintf("  %s: %.1f%%\n", ticker, 100 * opt_weights[ticker]))
    }
    
    return(opt_weights)
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

#=============================================================================
# NEW: TREND FOLLOWING OVERLAY - ADDED IN Z5.3.R
#=============================================================================

# Calculate moving average crossovers for trend detection
calculate_trend_signals <- function(prices, short_ma = 50, long_ma = 200) {
  cat("\n---- CALCULATING TREND SIGNALS ----\n")
  
  # Initialize result matrix
  n_assets <- ncol(prices)
  n_dates <- nrow(prices)
  
  trend_matrix <- matrix(0, nrow = n_dates, ncol = n_assets)
  colnames(trend_matrix) <- colnames(prices)
  trend_signals <- xts(trend_matrix, order.by = index(prices))
  
  # Calculate for each asset
  for (i in 1:n_assets) {
    ticker <- colnames(prices)[i]
    price_series <- prices[, ticker]
    
    # Calculate moving averages
    # If we have limited data, use shorter windows
    if (nrow(prices) < long_ma) {
      adjusted_long_ma <- floor(nrow(prices) * 0.7)
      adjusted_short_ma <- floor(adjusted_long_ma * short_ma / long_ma)
      
      cat(sprintf("Limited data for %s, using %d/%d day MAs instead of %d/%d\n",
                  ticker, adjusted_short_ma, adjusted_long_ma, short_ma, long_ma))
      
      short_ma <- adjusted_short_ma
      long_ma <- adjusted_long_ma
    }
    
    # Calculate moving averages if we have enough data
    if (nrow(prices) >= long_ma) {
      ma_short <- SMA(price_series, n = short_ma)
      ma_long <- SMA(price_series, n = long_ma)
      
      # Calculate trend signal: 1 = uptrend, 0 = neutral, -1 = downtrend
      trend <- ifelse(ma_short > ma_long, 1, -1)
      
      # Fill initial NA values with 0 (neutral)
      trend[is.na(trend)] <- 0
      
      trend_signals[, ticker] <- trend
      
      # Calculate what percentage of time is spent in uptrend/downtrend
      uptrend_pct <- sum(trend == 1, na.rm = TRUE) / length(trend) * 100
      downtrend_pct <- sum(trend == -1, na.rm = TRUE) / length(trend) * 100
      
      cat(sprintf("%s trend: %.1f%% uptrend, %.1f%% downtrend\n",
                  ticker, uptrend_pct, downtrend_pct))
    } else {
      cat(sprintf("Insufficient data for %s trend calculation, using neutral\n", ticker))
      trend_signals[, ticker] <- 0
    }
  }
  
  return(trend_signals)
}

# Apply trend-following overlay to position sizes
apply_trend_overlay <- function(weights, trend_signals, scaling_factor = 0.5) {
  cat("\n---- APPLYING TREND FOLLOWING OVERLAY ----\n")
  
  # If no trend signals available, return original weights
  if (is.null(trend_signals) || ncol(trend_signals) == 0) {
    debug_print("No trend signals available, skipping overlay")
    return(weights)
  }
  
  # Get the most recent trend signals
  latest_trends <- tail(trend_signals, 1)
  
  # Initialize adjusted weights with original weights
  adjusted_weights <- weights
  
  # For each asset, adjust weight based on trend
  for (ticker in names(weights)) {
    if (ticker %in% colnames(latest_trends)) {
      trend <- as.numeric(latest_trends[1, ticker])
      orig_weight <- weights[ticker]
      
      if (trend < 0) {  # Downtrend
        # Reduce position by scaling factor
        new_weight <- orig_weight * (1 - scaling_factor)
        adjusted_weights[ticker] <- new_weight
        cat(sprintf("Reducing %s due to downtrend: %.1f%% → %.1f%%\n",
                    ticker, orig_weight * 100, new_weight * 100))
      } else if (trend > 0) {  # Uptrend
        # Could potentially increase weight, but we'll be conservative
        # and just maintain the original weight
        adjusted_weights[ticker] <- orig_weight
      }
    }
  }
  
  # Normalize weights to ensure they sum to 1
  adjusted_weights <- adjusted_weights / sum(adjusted_weights)
  
  # Calculate the total adjustment
  total_reduction <- sum(weights) - sum(weights[names(weights) %in% names(adjusted_weights)])
  
  cat(sprintf("Trend overlay applied: %.1f%% portfolio adjustment\n", 
              total_reduction * 100))
  
  return(adjusted_weights)
}

cat("\nPart 2 loaded: Enhanced regime detection with guaranteed assignment and improved weight optimization\n")

#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - VERSION Z5.3.R
# PART 3: BACKTESTING, PERFORMANCE EVALUATION, AND CASH MANAGEMENT
#=============================================================================

# Log system information
cat("\n========================================================\n")
cat(sprintf("Current Date and Time (UTC): %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat(sprintf("Current User's Login: %s\n", Sys.info()["user"]))
cat("========================================================\n")

#=============================================================================
# PERFORMANCE CALCULATION AND CASH MANAGEMENT
#=============================================================================

# ENHANCED: Dynamic volatility targeting based on regime
calculate_target_volatility <- function(regime, base_target = 0.08, current_drawdown = 0, max_drawdown = 0.075) {
  # Base volatility by regime
  regime_vol_factor <- switch(regime,
                              growth = 1.0,
                              reflation = 0.9,
                              deflation = 0.7,
                              stagflation = 0.6,
                              risk_off = 0.5,
                              0.8  # Default if regime is unknown
  )
  
  # Scale down volatility during drawdowns
  drawdown_factor <- max(0.5, 1.0 - (current_drawdown / max_drawdown))
  
  # Calculate target volatility
  target_vol <- base_target * regime_vol_factor * drawdown_factor
  
  debug_print(sprintf("Target volatility: %.2f%% (Regime: %s, DD: %.1f%%)", 
                      target_vol * 100, regime, current_drawdown * 100))
  
  return(target_vol)
}

# Calculate portfolio performance with enhanced cash management - COMPLETELY REVISED FOR Z5.3.R
calculate_portfolio_performance <- function(
    prices, weights, rebalance_dates = NULL, 
    lookback_window = 126,  # REDUCED from 252
    frequency = "monthly",
    track_regimes = TRUE,  # CHANGED default to TRUE
    market_data = NULL,
    transaction_costs = NULL, 
    max_cash_pct = 0.50,
    trend_following = TRUE) {  # NEW parameter to enable trend following
  
  cat("\n---- PORTFOLIO PERFORMANCE CALCULATION ----\n")
  
  # Ensure we have valid input data
  if (is.null(prices) || ncol(prices) == 0) {
    stop("No price data provided")
  }
  
  # Convert any named list to named vector
  if (is.list(weights) && !is.xts(weights)) {
    weights <- unlist(weights)
  }
  
  # Ensure proper format for weights
  weights <- ensure_numeric_weights(weights)
  
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
    } else if (frequency == "weekly") {
      # Add weekly rebalancing option
      weeks <- format(all_dates, "%Y-%U")
      week_ends <- tapply(all_dates, weeks, max)
      rebalance_dates <- as.Date(week_ends)
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
  
  # For tracking regimes over time
  regime_history <- xts(
    matrix("", nrow = nrow(prices), ncol = 1), 
    order.by = index(prices)
  )
  colnames(regime_history) <- "REGIME"
  
  # NEW: Calculate trend signals if trend following is enabled
  trend_signals <- NULL
  if (trend_following) {
    trend_signals <- calculate_trend_signals(prices)
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
  current_regime <- "growth"  # Default to growth at start
  
  # Track portfolio drawdown for cash management
  equity_curve <- rep(1, nrow(prices))
  max_equity <- 1
  max_drawdown <- 0.075  # Default starting max drawdown (7.5%)
  current_drawdown <- 0
  
  # For volatility tracking
  return_history <- c()
  vol_zscore <- 0
  port_vols <- rep(NA, nrow(prices))
  
  # NEW: Track number of regime changes
  regime_changes <- 0
  last_regime <- ""
  
  # Loop through the dates
  for (i in 1:nrow(prices)) {
    date <- index(prices)[i]
    
    # If this is a rebalance date or the first date, update weights
    if (date %in% rebalance_dates || i == 1) {
      cat(sprintf("\nRebalancing on %s\n", as.character(date)))
      
      # Store previous weights for turnover calculation
      prev_weights <- current_weights
      
      # CRITICAL FIX: Always detect regime if market_data is available
      if (!is.null(market_data)) {
        # Find relevant market data (up to current date)
        market_subset <- market_data[index(market_data) <= date, ]
        
        # MAJOR FIX: Reduce minimum data requirement to 30 days
        if (nrow(market_subset) >= 30) {
          # Detect market regime
          regime_result <- detect_market_regime(market_subset, lookback = lookback_window, 
                                                force_regime = TRUE)
          current_regime <- regime_result$regime
          
          # Get target weights for this regime
          regime_weights <- get_regime_weights(current_regime)
          
          # Map asset class weights to ETFs
          etf_mapping <- create_asset_mapping(tickers)
          mapped_weights <- map_weights_to_etfs(regime_weights, etf_mapping)
          
          # Ensure mapped weights are in the correct format
          mapped_weights <- ensure_numeric_weights(mapped_weights)
          
          # Store updated weights
          current_weights <- mapped_weights
          
          # Store regime
          regime_history[date] <- current_regime
          
          # Track regime changes
          if (last_regime != "" && last_regime != current_regime) {
            regime_changes <- regime_changes + 1
            debug_print(sprintf("REGIME CHANGE #%d: %s → %s", 
                                regime_changes, last_regime, current_regime),
                        important = TRUE)
          }
          last_regime <- current_regime
          
          cat(sprintf("  Detected regime: %s\n", current_regime))
        } else {
          # Not enough data, use default regime (growth)
          cat("  Insufficient data for regime detection, using default growth regime\n")
          current_regime <- "growth"
          regime_history[date] <- current_regime
          
          # Use growth regime weights
          regime_weights <- get_regime_weights(current_regime)
          etf_mapping <- create_asset_mapping(tickers)
          mapped_weights <- map_weights_to_etfs(regime_weights, etf_mapping)
          mapped_weights <- ensure_numeric_weights(mapped_weights)
          current_weights <- mapped_weights
        }
      } else if (track_regimes) {
        # We want to track regimes but don't have market_data
        warning("Regime tracking requested but no market_data provided")
      }
      
      # TREND FOLLOWING OVERLAY - NEW in Z5.3.R
      if (trend_following && !is.null(trend_signals)) {
        # Apply trend overlay to current weights
        current_weights <- apply_trend_overlay(current_weights, trend_signals)
      }
      
      # Calculate turnover and transaction costs
      if (!is.null(prev_weights) && length(prev_weights) > 0) {
        # Calculate portfolio turnover
        turnover <- calculate_portfolio_turnover(prev_weights, current_weights)
        
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
        
        total_turnover <- total_turnover + turnover
        total_cost <- total_cost + rebalance_cost
        
        cat(sprintf("  Turnover: %.2f%%, Transaction cost: %.2f%%\n", 
                    100 * turnover, 100 * rebalance_cost))
        
        # Update weights using transaction cost optimization with regime awareness
        current_weights <- optimize_for_transaction_costs(
          prev_weights, current_weights, transaction_costs, 
          current_regime, vol_zscore, 0.15  # REDUCED turnover threshold to 15%
        )
      } else {
        cat("  Initial allocation, no turnover\n")
      }
      
      # Calculate portfolio volatility over recent history
      if (length(return_history) >= 60) {
        recent_vol <- sd(tail(return_history, 60), na.rm = TRUE) * sqrt(252)
        
        # Get volatility Z-score for cash management
        if (length(return_history) >= 120) {
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
      } else if (length(return_history) >= 20) {
        # With limited data, still calculate volatility but don't compute z-score
        recent_vol <- sd(return_history, na.rm = TRUE) * sqrt(252)
        vol_zscore <- 0  # Neutral
      } else {
        recent_vol <- NA
        vol_zscore <- 0  # Neutral
      }
      
      # Store portfolio volatility
      port_vols[i] <- ifelse(!is.na(recent_vol), recent_vol, 0)
      
      # ENHANCED CASH MANAGEMENT - COMPLETELY REVISED
      if (i > 1) {
        # Calculate current drawdown precisely
        current_drawdown <- 1 - equity_curve[i-1] / max_equity
        
        # Update max drawdown observed
        if (current_drawdown > max_drawdown) {
          # If we see a new max drawdown, update our reference
          max_drawdown <- current_drawdown
          debug_print(sprintf("New maximum drawdown: %.2f%%", max_drawdown * 100),
                      important = TRUE)
        }
        
        # Calculate cash allocation based on drawdown, volatility, and regime
        cash_allocation <- calculate_cash_allocation(
          current_drawdown = current_drawdown,
          max_drawdown = max_drawdown,
          vol_zscore = vol_zscore,
          regime = current_regime,  
          max_cash_pct = max_cash_pct,
          cash_ramp_factor = 2.5  # More aggressive cash ramping
        )
        
        # Update current cash position if it changes significantly
        if (abs(cash_allocation - current_cash) > 0.05 || i == 1) {
          current_cash <- cash_allocation
          cat(sprintf("  Cash allocation: %.1f%% (Drawdown: %.1f%%, Vol Z-score: %.1f, Regime: %s)\n", 
                      100 * current_cash, 100 * current_drawdown, vol_zscore, current_regime))
        } else if (cash_allocation != current_cash) {
          # Small change, update without full report
          current_cash <- cash_allocation
          cat(sprintf("  Cash allocation updated to: %.1f%%\n", 100 * current_cash))
        }
        
        # Adjust weights for cash allocation
        if (current_cash > 0) {
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
    
    # Update regime history if not already set (for non-rebalance dates)
    if (as.character(regime_history[date]) == "") {
      regime_history[date] <- current_regime
    }
    
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
  
  # Annualized statistics
  ann_return <- mean(port_returns, na.rm = TRUE) * 252
  ann_vol <- sd(port_returns, na.rm = TRUE) * sqrt(252)
  sharpe <- ifelse(ann_vol > 0, ann_return / ann_vol, 0)
  
  # Maximum drawdown
  dd <- maxDrawdown(port_returns)
  if (is.na(dd)) {
    dd <- max_drawdown  # Use tracked drawdown if calculation fails
  }
  
  # More robust calculation of total return
  total_return <- as.numeric(tail(port_values, 1)[1]) / as.numeric(port_values[1][1]) - 1
  
  # Create stats summary
  stats <- list(
    volatility = ann_vol,
    sharpe = sharpe,
    mean_return = ann_return,
    max_drawdown = dd,
    total_return = total_return,
    turnover_per_rebalance = total_turnover / length(rebalance_dates),
    total_cost = total_cost,
    regime_changes = regime_changes
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
  cat(sprintf("  Regime Changes: %d\n", stats$regime_changes))
  
  # Create results list
  results <- list(
    portfolio_values = port_values,
    portfolio_returns = port_returns,
    weight_history = weight_history,
    cash_history = cash_weights,
    regime_history = regime_history,
    stats = stats,
    volatility_history = xts(port_vols, order.by = index(prices)),
    trend_signals = trend_signals
  )
  
  return(results)
}

#=============================================================================
# BACKTESTING PROCESS AND PERFORMANCE EVALUATION
#=============================================================================

# Run comprehensive backtest with enhanced features - IMPROVED FOR Z5.3.R
run_enhanced_backtest <- function(
    tickers, 
    start_date, 
    end_date = Sys.Date(), 
    rebalance_frequency = "monthly",
    lookback_window = 126,  # REDUCED from 252
    min_weight = 0.01,
    max_weight = 0.30,
    max_cash = 0.50,
    trend_following = TRUE,  # NEW parameter
    include_inverse = TRUE) {
  
  cat("\n========== ENHANCED RISK PARITY BACKTEST Z5.3.R ==========\n")
  cat("Starting comprehensive backtest with the following settings:\n")
  cat(sprintf("  Assets: %s\n", paste(tickers, collapse=", ")))
  cat(sprintf("  Period: %s to %s\n", start_date, end_date))
  cat(sprintf("  Rebalance frequency: %s\n", rebalance_frequency))
  cat(sprintf("  Asset constraints: Min %.1f%%, Max %.1f%%\n", min_weight*100, max_weight*100))
  cat(sprintf("  Maximum cash allocation: %.1f%%\n", max_cash*100))
  cat(sprintf("  Trend following overlay: %s\n", ifelse(trend_following, "Enabled", "Disabled")))
  
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
  
  # Step 2: Create market regime indicators with enhanced indicators
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
  } else if (rebalance_frequency == "weekly") {
    # NEW: Added weekly rebalancing option
    weeks <- format(all_dates, "%Y-%U")
    week_ends <- tapply(all_dates, weeks, max)
    rebalance_dates <- as.Date(week_ends)
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
  
  # CRITICAL FIX: Always enable regime tracking
  results <- calculate_portfolio_performance(
    prices = prices, 
    weights = initial_weights, 
    rebalance_dates = rebalance_dates,
    lookback_window = lookback_window,
    frequency = rebalance_frequency,
    track_regimes = TRUE,  
    market_data = market_data,
    transaction_costs = transaction_costs,
    max_cash_pct = max_cash,
    trend_following = trend_following
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
    transaction_costs = transaction_costs[tickers],
    trend_following = FALSE  # No trend following for benchmark
  )
  
  # Try to create SPY benchmark if available
  spy_results <- NULL
  if ("SPY" %in% colnames(prices)) {
    spy_weights <- c(1)
    names(spy_weights) <- "SPY"
    
    spy_results <- calculate_portfolio_performance(
      prices = prices[, "SPY", drop=FALSE], 
      weights = spy_weights,
      track_regimes = FALSE,
      trend_following = FALSE
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
  cat(sprintf("  Strategy Max Drawdown: %.2f%%\n", min(strategy_dd, na.rm=TRUE) * 100))
  cat(sprintf("  Equal-Weight Max Drawdown: %.2f%%\n", min(equal_dd, na.rm=TRUE) * 100))
  
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

# Generate current position recommendations with better formatting - ENHANCED Z5.3.R VERSION
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
  
  # Get regime if available
  if (!is.null(backtest_results$strategy$regime_history)) {
    current_regime <- as.character(tail(backtest_results$strategy$regime_history, 1))
  } else {
    current_regime <- "unknown"
  }
  
  # Get trend signals if available
  trend_signals <- NULL
  if (!is.null(backtest_results$strategy$trend_signals)) {
    trend_signals <- backtest_results$strategy$trend_signals
    latest_trends <- as.numeric(tail(trend_signals, 1))
    names(latest_trends) <- colnames(trend_signals)
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
    Trend = "",
    stringsAsFactors = FALSE
  )
  
  # Calculate values
  position_table$Value <- position_table$Shares * position_table$Price
  
  # Add trend signals if available
  if (!is.null(trend_signals)) {
    for (i in 1:nrow(position_table)) {
      ticker <- position_table$Ticker[i]
      if (ticker %in% names(latest_trends)) {
        trend_value <- latest_trends[ticker]
        position_table$Trend[i] <- ifelse(trend_value > 0, "↑", 
                                          ifelse(trend_value < 0, "↓", "→"))
      } else {
        position_table$Trend[i] <- "→"
      }
    }
  } else {
    position_table$Trend <- NULL  # Remove column if no trend data
  }
  
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
  total_invested <- sum(position_table$Shares * current_prices)
  cash_value <- portfolio_value - total_invested
  
  cat(sprintf("\nTotal Invested: $%s", formatC(total_invested, format="f", big.mark=",", digits=0)))
  cat(sprintf("\nRemaining Cash: $%s", formatC(cash_value, format="f", big.mark=",", digits=0)))
  
  # Add trend legend if we have trend data
  if (!is.null(trend_signals)) {
    cat("\n\nTrend Legend: ↑ = Uptrend, ↓ = Downtrend, → = Neutral\n")
  }
  
  return(position_table)
}

# Plot performance comparison - IMPROVED FOR Z5.3.R
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
      colnames(combined_values) <- c("Z5.3.R Strategy", "Equal Weight", "S&P 500")
    } else {
      # Just strategy and equal weight
      combined_values <- merge(strategy_values, equal_values)
      colnames(combined_values) <- c("Z5.3.R Strategy", "Equal Weight")
    }
    
    # Convert to long format for ggplot
    combined_df <- data.frame(
      Date = index(combined_values),
      combined_values
    )
    
    df_long <- reshape2::melt(combined_df, id.vars = "Date", variable.name = "Portfolio", value.name = "Value")
    
    # Create the plot
    p <- ggplot(df_long, aes(x = Date, y = Value, color = Portfolio)) +
      geom_line(linewidth = 1) +
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
      colnames(combined_dd) <- c("Z5.3.R Strategy", "Equal Weight", "S&P 500")
    } else {
      combined_dd <- merge(strategy_dd, equal_dd)
      colnames(combined_dd) <- c("Z5.3.R Strategy", "Equal Weight")
    }
    
    # Convert to long format for drawdown plot
    dd_df <- data.frame(
      Date = index(combined_dd),
      combined_dd
    )
    
    dd_long <- reshape2::melt(dd_df, id.vars = "Date", variable.name = "Portfolio", value.name = "Drawdown")
    
    # Create the drawdown plot
    p_dd <- ggplot(dd_long, aes(x = Date, y = Drawdown, color = Portfolio)) +
      geom_line(linewidth = 1) +
      theme_minimal() +
      labs(title = "Drawdown Comparison", y = "Drawdown", x = "") +
      scale_y_continuous(labels = scales::percent) +
      theme(legend.position = "bottom")
    
    print(p_dd)
    
    # NEW: Plot regime history if available
    if (!is.null(backtest_results$strategy$regime_history) && 
        any(backtest_results$strategy$regime_history != "")) {
      regime_history <- backtest_results$strategy$regime_history
      
      # Convert regime history to numeric values for plotting
      regime_values <- rep(NA, nrow(regime_history))
      regime_values[regime_history == "growth"] <- 1
      regime_values[regime_history == "reflation"] <- 2
      regime_values[regime_history == "deflation"] <- 3
      regime_values[regime_history == "stagflation"] <- 4
      regime_values[regime_history == "risk_off"] <- 5
      
      regime_numeric <- xts(regime_values, order.by = index(regime_history))
      colnames(regime_numeric) <- "REGIME"
      
      # Create regime plot df
      regime_df <- data.frame(
        Date = index(regime_numeric),
        Regime = as.numeric(regime_numeric)
      )
      
      # Create the regime plot
      p_regime <- ggplot(regime_df, aes(x = Date, y = Regime)) +
        geom_step(linewidth = 1) +
        theme_minimal() +
        labs(title = "Market Regime History", y = "Regime", x = "") +
        scale_y_continuous(breaks = 1:5, 
                           labels = c("Growth", "Reflation", "Deflation", "Stagflation", "Risk-Off")) +
        theme(legend.position = "none")
      
      print(p_regime)
      
      return(list(performance_plot = p, drawdown_plot = p_dd, regime_plot = p_regime))
    }
    
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
  "IWM",   # Russell 2000 Small Cap
  
  # International Equity
  "EFA",   # Developed Markets
  "EEM",   # Emerging Markets
  
  # Fixed Income
  "IEF",   # 7-10 Year Treasury
  "TLT",   # 20+ Year Treasury
  "LQD",   # Investment Grade Corporate Bonds
  "HYG",   # High Yield Corporate Bonds
  "TIP",   # Treasury Inflation-Protected Securities
  
  # Alternatives
  "GLD",   # Gold
  "DBC",   # Commodities
  "VNQ"    # Real Estate
)

# Define backtest parameters
start_date <- "2010-01-01"
end_date <- Sys.Date()
rebalance_frequency <- "monthly"
min_weight <- 0.01
max_weight <- 0.30
max_cash <- 0.50
trend_following <- TRUE  # Enable trend following

# Run the backtest
backtest_results <- run_enhanced_backtest(
  tickers = tickers,
  start_date = start_date,
  end_date = end_date,
  rebalance_frequency = rebalance_frequency,
  min_weight = min_weight,
  max_weight = max_weight,
  max_cash = max_cash,
  trend_following = trend_following,
  include_inverse = TRUE
)

# Generate current recommendations
recommendations <- generate_position_recommendations(backtest_results)

# Plot performance comparison
plots <- plot_performance_comparison(backtest_results, "Enhanced Risk Parity Z5.3.R Performance")

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
      if (regime == "") next  # Skip empty regime
      
      regime_returns <- portfolio_returns[regime_history == regime]
      
      if (length(regime_returns) > 0) {
        annualized_return <- mean(regime_returns, na.rm=TRUE) * 252 * 100
        annualized_vol <- sd(regime_returns, na.rm=TRUE) * sqrt(252) * 100
        sharpe <- ifelse(annualized_vol > 0, annualized_return / annualized_vol, 0)
        
        cat(sprintf("  %s: Return = %.2f%%, Vol = %.2f%%, Sharpe = %.2f, Days = %d\n",
                    regime, annualized_return, annualized_vol, sharpe, regime_counts[regime]))
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
cat("Thank you for using the Enhanced Risk Parity System Z5.3.R\n")