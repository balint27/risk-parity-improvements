#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - VERSION Z5.4.R
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
  
  # 10. NEW: Volatility indicator directly from returns
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
# PART 3: RISK PARITY OPTIMIZATION FUNCTIONS 
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

# Fixed and enhanced BFGS optimization function
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
    
    # Set up optimization problem
    result <- nloptr::slsqp(x0 = equal_weights, 
                            fn = risk_parity_objective,
                            cov_matrix = cov_matrix,
                            lambda = lambda,
                            target_risk_contrib = target_risk,
                            lower = rep(0.001, n),   # Minimum weight per asset
                            upper = rep(0.999, n),   # Maximum weight per asset
                            inequality = function(w) { sum(w) - 1 },
                            inequality.upper = 0,
                            control = list(
                              maxeval = max_iterations,
                              xtol_rel = 1e-6
                            ))
    
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

# Function to construct portfolio from asset weights
construct_portfolio <- function(weights, prices, target_volatility = 0.10, 
                                cash_weight = 0, leverage_limit = 1.5, 
                                max_single_weight = 0.30) {
  cat("\nConstructing portfolio...\n")
  
  # Extract last row of prices
  if (is.xts(prices)) {
    latest_prices <- as.numeric(tail(prices, 1))
    names(latest_prices) <- colnames(prices)
    latest_date <- index(tail(prices, 1))
  } else {
    stop("Prices must be an xts object")
  }
  
  # Create standard output format
  portfolio <- list(
    date = latest_date,
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

# Function to calculate portfolio statistics
calculate_portfolio_stats <- function(portfolio, returns, cov_matrix, lookback = 252) {
  cat("\nCalculating portfolio statistics...\n")
  
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
  if (portfolio$volatility > 0) {
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
      # Calculate portfolio returns
      port_returns <- returns[, common_assets] %*% weights[common_assets]
      
      # Last month return (21 days)
      portfolio$return_1m <- as.numeric(prod(1 + tail(port_returns, 21)) - 1)
      
      # Last quarter return (63 days)
      portfolio$return_3m <- as.numeric(prod(1 + tail(port_returns, 63)) - 1)
      
      # Last 6 months (126 days)
      portfolio$return_6m <- as.numeric(prod(1 + tail(port_returns, 126)) - 1)
      
      # Year to date
      start_of_year <- as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01"))
      ytd_returns <- port_returns[paste0(start_of_year, "/", Sys.Date())]
      portfolio$return_ytd <- as.numeric(prod(1 + ytd_returns) - 1)
      
      # Maximum drawdown
      port_cum_returns <- cumprod(1 + port_returns)
      portfolio$max_drawdown <- as.numeric(maxDrawdown(port_returns))
      
      # Print performance summary
      cat(sprintf("Performance summary:\n"))
      cat(sprintf("  1-month return: %.2f%%\n", portfolio$return_1m * 100))
      cat(sprintf("  3-month return: %.2f%%\n", portfolio$return_3m * 100))
      cat(sprintf("  6-month return: %.2f%%\n", portfolio$return_6m * 100))
      cat(sprintf("  YTD return: %.2f%%\n", portfolio$return_ytd * 100))
      cat(sprintf("  Max drawdown: %.2f%%\n", portfolio$max_drawdown * 100))
    }
  }
  
  return(portfolio)
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

#=============================================================================
# PART 5: TRADE EXECUTION AND PORTFOLIO MONITORING
#=============================================================================

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

#=============================================================================
# PART 6: MAIN EXECUTION FUNCTION WITH ERROR HANDLING
#=============================================================================

# Main execution function
run_risk_parity_strategy <- function(tickers = c("SPY", "QQQ", "IWM", "EFA", "EEM", 
                                                 "IEF", "TLT", "LQD", "HYG", 
                                                 "GLD", "DBC", "VNQ"),
                                     start_date = "2010-01-01",
                                     end_date = Sys.Date(),
                                     base_vol_target = 0.10,
                                     include_inverse = TRUE,
                                     rebalance_threshold = 0.05,
                                     max_single_weight = 0.30) {
  
  # Print header
  cat("\n========================================================\n")
  cat("ENHANCED RISK PARITY TRADING SYSTEM - VERSION Z5.4.R\n")
  cat("========================================================\n")
  
  # Initialize results
  results <- list(
    portfolio = NULL,
    regime_history = data.frame(regime = character(), 
                                date = as.Date(character()), 
                                stringsAsFactors = FALSE),
    status = "initialized"
  )
  
  # Execute with error handling
  tryCatch({
    # Step 1: Load market data
    prices <- load_market_data(tickers, start_date, end_date, include_inverse = include_inverse)
    
    # Print data summary
    cat(sprintf("\nLoaded price data: %d trading days from %s to %s\n", 
                nrow(prices), min(index(prices)), max(index(prices))))
    
    # Calculate returns
    returns <- ROC(prices, type = "discrete")
    returns <- na.omit(returns)
    
    # Step 2: Create enhanced market data with indicators
    market_data <- create_market_data(prices, returns)
    
    # Step 3: Estimate volatilities and correlations
    cat("\nForecasting volatilities...\n")
    vol_forecasts <- forecast_garch_volatility(returns, forecast_horizon = 22)
    
    # Print volatility summary
    vol_summary <- summary(vol_forecasts$forecasted_vol)
    cat(sprintf("Volatility forecasts: min=%.1f%%, median=%.1f%%, max=%.1f%%\n",
                vol_summary["Min."] * 100, vol_summary["Median"] * 100, 
                vol_summary["Max."] * 100))
    
    # Step 4: Estimate correlation/covariance matrix
    cat("\nEstimating correlation structure...\n")
    cov_matrix <- estimate_ewma_covariance(returns, lambda = 0.94)
    
    # Check correlation structure
    cor_matrix <- cov2cor(cov_matrix)
    cat(sprintf("Correlation matrix: min=%.2f, mean=%.2f, max=%.2f\n",
                min(cor_matrix[lower.tri(cor_matrix)]),
                mean(cor_matrix[lower.tri(cor_matrix)]),
                max(cor_matrix[lower.tri(cor_matrix)])))
    
    # Step 5: Detect current economic regime
    current_regime <- detect_regime(market_data)
    
    # Update regime history
    results$regime_history <- update_regime_history(results$regime_history,
                                                    current_regime,
                                                    tail(index(market_data), 1))
    
    # Check if regime changed recently
    regime_changed <- detect_regime_change(results$regime_history, lookback = 3)
    
    # Step 6: Adjust risk targets based on regime
    vol_target <- get_regime_risk_targets(current_regime, base_vol_target)
    cat(sprintf("\nRegime-adjusted volatility target: %.1f%%\n", vol_target * 100))
    
    # Step 7: Calculate cash allocation based on market conditions
    # Extract current drawdown - use SPY if available
    current_drawdown <- 0
    if ("SPY" %in% colnames(prices)) {
      spy_prices <- prices[, "SPY"]
      spy_peak <- cummax(spy_prices)
      current_drawdown <- as.numeric(1 - tail(spy_prices, 1) / tail(spy_peak, 1))
    } else {
      # Use equal-weighted portfolio
      eq_returns <- rowMeans(returns)
      eq_perf <- cumprod(1 + eq_returns)
      eq_peak <- cummax(eq_perf)
      current_drawdown <- as.numeric(1 - tail(eq_perf, 1) / tail(eq_peak, 1))
    }
    
    # Get VIX Z-score if available
    vol_zscore <- 0
    if ("VIX_Z" %in% colnames(market_data)) {
      vol_zscore <- as.numeric(tail(market_data[, "VIX_Z"], 1))
    }
    
    # Calculate cash allocation
    cash_weight <- calculate_cash_allocation(
      current_drawdown = current_drawdown,
      max_drawdown = 0.30,  # Assume 30% maximum drawdown
      vol_zscore = vol_zscore,
      regime = current_regime,
      max_cash_pct = 0.50
    )
    
    cat(sprintf("\nCalculated cash allocation: %.1f%%\n", cash_weight * 100))
    
    # Step 8: Assign risk budgets based on regime
    risk_budgets <- assign_risk_budgets(colnames(prices), current_regime)
    
    # Step 9: Optimize risk parity weights
    cat("\nOptimizing risk parity weights with custom risk budgets...\n")
    rp_weights <- optimize_risk_parity(cov_matrix, custom_risk_targets = risk_budgets)
    
    # Step 10: Construct portfolio
    cat("\nConstructing final portfolio...\n")
    portfolio <- construct_portfolio(
      weights = rp_weights,
      prices = prices,
      target_volatility = vol_target,
      cash_weight = cash_weight,
      max_single_weight = max_single_weight
    )
    
    # Step 11: Calculate portfolio statistics
    portfolio <- calculate_portfolio_stats(portfolio, returns, cov_matrix)
    
    # Step 12: Analyze portfolio exposures
    exposures <- analyze_portfolio_exposures(portfolio$weights, market_data)
    
    # Step 13: Generate trading signals
    signals <- generate_trading_signals(portfolio$weights, threshold = 0.01)
    
    # Calculate transaction costs
    txn_costs <- get_transaction_costs(names(portfolio$weights))
    
    # Store results
    results$portfolio <- portfolio
    results$signals <- signals
    results$regime <- current_regime
    results$regime_changed <- regime_changed
    results$vol_target <- vol_target
    results$cash_weight <- cash_weight
    results$exposures <- exposures
    results$txn_costs <- txn_costs
    results$status <- "success"
    
    # Print summary
    cat("\n========================================================\n")
    cat("RISK PARITY STRATEGY SUMMARY\n")
    cat("========================================================\n")
    cat(sprintf("Current date: %s\n", format(Sys.Date())))
    cat(sprintf("Economic regime: %s\n", current_regime))
    cat(sprintf("Portfolio assets: %d\n", length(portfolio$weights)))
    cat(sprintf("Target volatility: %.1f%%\n", vol_target * 100))
    cat(sprintf("Cash allocation: %.1f%%\n", cash_weight * 100))
    cat(sprintf("Current drawdown: %.1f%%\n", current_drawdown * 100))
    
    return(results)
    
  }, error = function(e) {
    # Handle errors
    error_msg <- paste("ERROR in risk parity strategy:", e$message)
    cat("\n", error_msg, "\n")
    results$status <- "error"
    results$error_message <- error_msg
    return(results)
  })
}

#=============================================================================
# EXECUTE STRATEGY WITH DEFAULT PARAMETERS
#=============================================================================

# Uncomment and run the following lines to execute the strategy

tickers <- c("SPY", "QQQ", "IWM", "EFA", "EEM", "IEF", "TLT", "LQD", "HYG", "GLD", "DBC", "VNQ")
start_date <- "2015-01-01"  # 10 years of data for stable estimates
end_date <- Sys.Date()
base_vol_target <- 0.10     # 10% annualized vol target
include_inverse <- TRUE     # Include inverse ETFs for risk-off regimes

# Run the strategy
strategy_results <- run_risk_parity_strategy(
  tickers = tickers,
  start_date = start_date,
  end_date = end_date,
  base_vol_target = base_vol_target,
  include_inverse = include_inverse
)

# Print portfolio weights
if (!is.null(strategy_results$portfolio)) {
  cat("\nFinal portfolio weights:\n")
  weights <- sort(strategy_results$portfolio$weights, decreasing = TRUE)
  for (ticker in names(weights)) {
    cat(sprintf("  %s: %.2f%%\n", ticker, weights[ticker] * 100))
  }
  
  # Print trading signals
  cat("\nTrading signals:\n")
  print(strategy_results$signals)
}

cat("\nStrategy execution complete.\n")

#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - VERSION Z5.4.R
# PERFORMANCE ANALYSIS AND VISUALIZATION FUNCTIONS
#=============================================================================

# Function to generate portfolio performance summary
generate_performance_report <- function(strategy_results, returns, prices, 
                                        lookback_periods = list(short = 63, medium = 126, long = 252)) {
  cat("\n========================================================\n")
  cat("PORTFOLIO PERFORMANCE ANALYSIS\n")
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
    
    # Calculate portfolio returns
    port_returns[, 1] <- returns[, common_assets] %*% weights_subset
    
    # Calculate benchmark returns (SPY if available, otherwise equal-weighted)
    if ("SPY" %in% colnames(returns)) {
      benchmark_returns <- returns[, "SPY"]
      benchmark_name <- "SPY"
    } else {
      benchmark_returns <- rowMeans(returns)
      benchmark_name <- "Equal-weighted"
    }
    
    # Add benchmark to the return series
    port_returns <- merge(port_returns, benchmark_returns)
    colnames(port_returns)[2] <- benchmark_name
    
    # Calculate performance metrics
    periods <- c("1M" = 21, "3M" = 63, "6M" = 126, "1Y" = 252)
    
    cat("Period returns:\n")
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
    
    # Calculate risk metrics
    if (nrow(port_returns) >= lookback_periods$medium) {
      analysis_period <- tail(port_returns, lookback_periods$medium)
      
      # Annualized returns
      annual_port <- prod(1 + analysis_period[, 1])^(252/nrow(analysis_period)) - 1
      annual_bench <- prod(1 + analysis_period[, 2])^(252/nrow(analysis_period)) - 1
      
      # Volatility
      vol_port <- sd(analysis_period[, 1]) * sqrt(252)
      vol_bench <- sd(analysis_period[, 2]) * sqrt(252)
      
      # Sharpe ratio (assuming 0% risk-free rate for simplicity)
      sharpe_port <- annual_port / vol_port
      sharpe_bench <- annual_bench / vol_bench
      
      # Maximum drawdown
      dd_port <- maxDrawdown(analysis_period[, 1])
      dd_bench <- maxDrawdown(analysis_period[, 2])
      
      cat("\nRisk metrics (trailing 6 months):\n")
      cat(sprintf("  Annualized Return: Portfolio = %.2f%%, %s = %.2f%%\n", 
                  annual_port * 100, benchmark_name, annual_bench * 100))
      cat(sprintf("  Annualized Volatility: Portfolio = %.2f%%, %s = %.2f%%\n", 
                  vol_port * 100, benchmark_name, vol_bench * 100))
      cat(sprintf("  Sharpe Ratio: Portfolio = %.2f, %s = %.2f\n", 
                  sharpe_port, benchmark_name, sharpe_bench))
      cat(sprintf("  Maximum Drawdown: Portfolio = %.2f%%, %s = %.2f%%\n", 
                  dd_port * 100, benchmark_name, dd_bench * 100))
    }
    
    # Current drawdown status
    port_cum <- cumprod(1 + port_returns[, 1])
    bench_cum <- cumprod(1 + port_returns[, 2])
    
    port_peak <- cummax(port_cum)
    bench_peak <- cummax(bench_cum)
    
    port_dd <- (port_cum / port_peak - 1) * 100
    bench_dd <- (bench_cum / bench_peak - 1) * 100
    
    cat(sprintf("\nCurrent Drawdown: Portfolio = %.2f%%, %s = %.2f%%\n", 
                as.numeric(tail(port_dd, 1)), benchmark_name, as.numeric(tail(bench_dd, 1))))
    
    # Return calculated metrics for later use
    metrics <- list(
      returns = port_returns,
      cumulative_returns = merge(port_cum, bench_cum),
      drawdowns = merge(port_dd, bench_dd),
      volatility = c(portfolio = vol_port, benchmark = vol_bench),
      sharpe = c(portfolio = sharpe_port, benchmark = sharpe_bench),
      max_drawdown = c(portfolio = dd_port, benchmark = dd_bench)
    )
    
    return(metrics)
  } else {
    cat("No common assets between portfolio and return history\n")
    return(NULL)
  }
}

# Function to plot portfolio performance
plot_performance <- function(metrics, regime_history = NULL, 
                             title = "Portfolio Performance vs Benchmark") {
  # Check if we have valid metrics
  if (is.null(metrics) || is.null(metrics$cumulative_returns)) {
    warning("Cannot generate plots - no valid performance metrics")
    return(invisible(NULL))
  }
  
  # Extract data for plotting
  cum_returns <- metrics$cumulative_returns
  colnames(cum_returns) <- c("Portfolio", "Benchmark")
  
  # Create a new plotting device
  par(mfrow = c(3, 1), mar = c(4, 4, 3, 2), oma = c(0, 0, 2, 0))
  
  # Plot 1: Cumulative returns
  main_color <- "#0072B2"  # Blue
  benchmark_color <- "#D55E00"  # Orange/red
  
  # Convert xts to regular data frames for ggplot
  plot_data <- data.frame(
    date = index(cum_returns),
    Portfolio = as.numeric(cum_returns[, 1]),
    Benchmark = as.numeric(cum_returns[, 2])
  )
  
  # Reshape to long format
  plot_data_long <- reshape2::melt(plot_data, id.vars = "date", 
                                   variable.name = "Series", value.name = "Value")
  
  # Create plot using ggplot2
  p1 <- ggplot(plot_data_long, aes(x = date, y = Value, color = Series)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Portfolio" = main_color, "Benchmark" = benchmark_color)) +
    labs(title = "Cumulative Returns", x = "Date", y = "Growth of $1") +
    theme_minimal() +
    theme(legend.position = "top")
  
  # Plot 2: Drawdowns
  drawdowns <- metrics$drawdowns
  colnames(drawdowns) <- c("Portfolio", "Benchmark")
  
  # Convert drawdowns to data frame
  dd_data <- data.frame(
    date = index(drawdowns),
    Portfolio = as.numeric(drawdowns[, 1]),
    Benchmark = as.numeric(drawdowns[, 2])
  )
  
  # Reshape to long format
  dd_data_long <- reshape2::melt(dd_data, id.vars = "date", 
                                 variable.name = "Series", value.name = "Drawdown")
  
  # Create drawdown plot
  p2 <- ggplot(dd_data_long, aes(x = date, y = Drawdown, color = Series)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Portfolio" = main_color, "Benchmark" = benchmark_color)) +
    labs(title = "Drawdowns", x = "Date", y = "Drawdown (%)") +
    theme_minimal() +
    theme(legend.position = "top")
  
  # Plot 3: Regime transitions
  if (!is.null(regime_history) && nrow(regime_history) > 0) {
    # Create regime transition plot
    
    # Define colors for regimes
    regime_colors <- c(
      "growth" = "#2ECC71",      # Green
      "reflation" = "#3498DB",   # Blue
      "neutral" = "#F1C40F",     # Yellow
      "stagflation" = "#E67E22", # Orange
      "deflation" = "#E74C3C",   # Red
      "risk_off" = "#7F8C8D"     # Gray
    )
    
    # Make sure regime is a factor with proper levels
    regime_history$regime <- factor(regime_history$regime, 
                                    levels = names(regime_colors))
    
    # Create regime plot
    p3 <- ggplot(regime_history, aes(x = date, y = 1, fill = regime)) +
      geom_tile() +
      scale_fill_manual(values = regime_colors, name = "Economic Regime") +
      labs(title = "Economic Regime Timeline", x = "Date", y = "") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "top")
  } else {
    # Create empty plot if no regime data
    p3 <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "No regime data available") +
      theme_void()
  }
  
  # Combine plots using gridExtra or patchwork
  grid_plot <- gridExtra::grid.arrange(p1, p2, p3, ncol = 1,
                                       top = grid::textGrob(title, gp = grid::gpar(fontsize = 14, font = 2)))
  
  return(grid_plot)
}

# Function to plot portfolio allocation and risk breakdown
plot_portfolio_allocation <- function(portfolio, exposures = NULL) {
  # Check if we have valid portfolio
  if (is.null(portfolio) || is.null(portfolio$weights)) {
    warning("Cannot generate allocation plot - no valid portfolio")
    return(invisible(NULL))
  }
  
  # Extract weights
  weights <- portfolio$effective_weights
  
  # Sort weights for better visualization
  weights <- sort(weights, decreasing = TRUE)
  
  # Create a new plotting device
  par(mfrow = c(2, 2), mar = c(8, 4, 3, 2), oma = c(0, 0, 2, 0))
  
  # Plot 1: Asset allocation
  weights_df <- data.frame(
    Asset = factor(names(weights), levels = names(weights)),
    Weight = weights
  )
  
  p1 <- ggplot(weights_df, aes(x = Asset, y = Weight, fill = Asset)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Portfolio Allocation", x = "", y = "Weight") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  # Plot 2: Risk contribution if available
  if (!is.null(portfolio$risk_contribution)) {
    risk_contrib <- portfolio$risk_contribution
    risk_contrib <- risk_contrib / sum(risk_contrib)  # Normalize
    risk_contrib <- sort(risk_contrib, decreasing = TRUE)
    
    risk_df <- data.frame(
      Asset = factor(names(risk_contrib), levels = names(risk_contrib)),
      Contribution = risk_contrib
    )
    
    p2 <- ggplot(risk_df, aes(x = Asset, y = Contribution, fill = Asset)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Risk Contribution", x = "", y = "Risk") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
  } else {
    # Empty plot if risk contribution not available
    p2 <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "No risk contribution data") +
      theme_void()
  }
  
  # Plot 3: Asset class exposure if available
  if (!is.null(exposures) && !is.null(exposures$asset_class)) {
    class_exposure <- unlist(exposures$asset_class)
    
    class_df <- data.frame(
      Class = names(class_exposure),
      Exposure = class_exposure
    )
    
    p3 <- ggplot(class_df, aes(x = "", y = Exposure, fill = Class)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Asset Class Exposure", x = NULL, y = NULL) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank())
  } else {
    # Empty plot if exposures not available
    p3 <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "No asset class data") +
      theme_void()
  }
  
  # Plot 4: Factor exposure if available
  if (!is.null(exposures) && !is.null(exposures$factor)) {
    factor_exposure <- unlist(exposures$factor)
    
    # Sort by absolute value for better visualization
    factor_exposure <- factor_exposure[order(abs(factor_exposure), decreasing = TRUE)]
    
    factor_df <- data.frame(
      Factor = factor(names(factor_exposure), levels = names(factor_exposure)),
      Exposure = factor_exposure
    )
    
    p4 <- ggplot(factor_df, aes(x = Factor, y = Exposure, fill = Exposure > 0)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("TRUE" = "#2ECC71", "FALSE" = "#E74C3C"), 
                        guide = "none") +
      labs(title = "Factor Exposure", x = "", y = "Exposure") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    # Empty plot if factor exposures not available
    p4 <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "No factor exposure data") +
      theme_void()
  }
  
  # Combine plots
  grid_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2,
                                       top = grid::textGrob("Portfolio Analysis", 
                                                            gp = grid::gpar(fontsize = 14, font = 2)))
  
  return(grid_plot)
}

# Function to fix the time series conversion error
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
# MAIN WRAPPER FUNCTION WITH PERFORMANCE ANALYSIS AND PLOTS
#=============================================================================

# Main function to run strategy with performance analysis and plots
run_complete_analysis <- function(tickers = c("SPY", "QQQ", "IWM", "EFA", "EEM", 
                                              "IEF", "TLT", "LQD", "HYG", 
                                              "GLD", "DBC", "VNQ"),
                                  start_date = "2015-01-01",
                                  end_date = Sys.Date(),
                                  base_vol_target = 0.10,
                                  include_inverse = TRUE,
                                  create_plots = TRUE) {
  
  # Run the base strategy
  strategy_results <- run_risk_parity_strategy(
    tickers = tickers,
    start_date = start_date,
    end_date = end_date,
    base_vol_target = base_vol_target,
    include_inverse = include_inverse
  )
  
  # If strategy failed, return results as is
  if (strategy_results$status != "success" || is.null(strategy_results$portfolio)) {
    cat("\nStrategy execution failed. Cannot generate performance analysis.\n")
    return(strategy_results)
  }
  
  # Load full price history for analysis
  cat("\nLoading price data for performance analysis...\n")
  prices <- load_market_data(tickers, start_date, end_date, include_inverse = include_inverse)
  returns <- ROC(prices, type = "discrete")
  returns <- na.omit(returns)
  
  # Fix potential time series issues in portfolio calculation
  strategy_results$portfolio <- fix_portfolio_calculation(strategy_results$portfolio, returns)
  
  # Generate performance report
  performance_metrics <- generate_performance_report(strategy_results, returns, prices)
  strategy_results$performance <- performance_metrics
  
  # Create plots if requested
  if (create_plots && !is.null(performance_metrics)) {
    cat("\nGenerating performance visualizations...\n")
    
    # Performance plots
    perf_plot <- plot_performance(performance_metrics, strategy_results$regime_history,
                                  title = "Risk Parity Strategy Performance")
    
    # Allocation plots
    alloc_plot <- plot_portfolio_allocation(strategy_results$portfolio, 
                                            strategy_results$exposures)
    
    # Save plots to results
    strategy_results$plots <- list(
      performance = perf_plot,
      allocation = alloc_plot
    )
    
    # Save plots to files
    tryCatch({
      ggsave("risk_parity_performance.png", perf_plot, width = 10, height = 8, dpi = 300)
      ggsave("risk_parity_allocation.png", alloc_plot, width = 10, height = 8, dpi = 300)
      cat("Performance visualizations saved to current directory.\n")
    }, error = function(e) {
      cat("Could not save plots to files:", e$message, "\n")
    })
  }
  
  # Print final portfolio weights
  if (!is.null(strategy_results$portfolio$weights)) {
    cat("\nFinal portfolio weights:\n")
    weights <- sort(strategy_results$portfolio$weights, decreasing = TRUE)
    for (ticker in names(weights)) {
      cat(sprintf("  %s: %.2f%%\n", ticker, weights[ticker] * 100))
    }
  }
  
  cat("\nComplete analysis finished.\n")
  return(strategy_results)
}

# Update main execution to use the complete analysis
#=============================================================================
# EXECUTE STRATEGY WITH COMPLETE PERFORMANCE ANALYSIS
#=============================================================================

# Set your parameters
tickers <- c("SPY", "QQQ", "IWM", "EFA", "EEM", "IEF", "TLT", "LQD", "HYG", "GLD", "DBC", "VNQ")
start_date <- "2015-01-01"
end_date <- Sys.Date()
base_vol_target <- 0.10
include_inverse <- TRUE

# Run the complete analysis with performance metrics and plots
complete_results <- run_complete_analysis(
  tickers = tickers,
  start_date = start_date,
  end_date = end_date,
  base_vol_target = base_vol_target,
  include_inverse = include_inverse,
  create_plots = TRUE
)

# Display final summary
if (!is.null(complete_results$portfolio)) {
  cat("\n========================================================\n")
  cat("RISK PARITY STRATEGY FINAL SUMMARY\n")
  cat("========================================================\n")
  cat(sprintf("Strategy successfully executed on: %s\n", format(Sys.Date())))
  cat(sprintf("Current economic regime: %s\n", complete_results$regime))
  cat(sprintf("Portfolio volatility target: %.2f%%\n", complete_results$vol_target * 100))
  cat(sprintf("Cash allocation: %.2f%%\n", complete_results$cash_weight * 100))
  
  if (!is.null(complete_results$performance)) {
    cat("\nPerformance visualizations saved as:\n")
    cat("  - risk_parity_performance.png\n")
    cat("  - risk_parity_allocation.png\n")
  }
}
  