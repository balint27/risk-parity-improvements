#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - STREAMLINED VERSION
# PART 1: DATA HANDLING, VOLATILITY ESTIMATION, AND COVARIANCE CALCULATION
#=============================================================================

# Load required packages with reliable error handling
required_packages <- c("tidyverse", "quantmod", "xts", "PerformanceAnalytics",
                       "TTR", "zoo", "tidyquant", "ggplot2", "reshape2")

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
cat(sprintf("Current User's Login: %s\n", Sys.info()["user"]))
cat(sprintf("R Version: %s\n", R.version.string))

# Global safety settings
options(warn = 1)  # Show warnings immediately
options(stringsAsFactors = FALSE)  # Never convert strings to factors by default

#=============================================================================
# DATA HANDLING - REAL DATA ONLY
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
# ROBUST VOLATILITY FORECASTING (REPLACING GARCH)
#=============================================================================

# New robust volatility forecasting function
forecast_volatility <- function(returns, lambda = 0.94, min_vol = 0.05, max_vol = 0.50) {
  cat("\nForecasting volatility with robust EWMA approach\n")
  
  # Safety checks
  if (is.null(returns) || ncol(returns) < 1) {
    stop("Invalid returns data provided")
  }
  
  n_assets <- ncol(returns)
  assets <- colnames(returns)
  
  # Initialize results dataframe
  forecasted_vols <- data.frame(
    asset = assets,
    forecasted_vol = rep(NA, n_assets),
    method_used = rep("", n_assets),
    row.names = assets
  )
  
  # For each asset
  for (i in 1:n_assets) {
    asset <- assets[i]
    returns_i <- as.numeric(returns[, i])
    
    # Remove any NA values
    returns_i <- returns_i[!is.na(returns_i)]
    
    if (length(returns_i) < 30) {
      cat(sprintf("  Warning: Insufficient data for %s, using historical volatility\n", asset))
      vol <- sd(returns_i, na.rm = TRUE) * sqrt(252)
      forecasted_vols[asset, "forecasted_vol"] <- vol
      forecasted_vols[asset, "method_used"] <- "historical"
    } else {
      # Try multiple methods and use most reliable one
      
      # 1. EWMA volatility (most responsive to recent changes)
      tryCatch({
        # Calculate EWMA variance with exponential decay
        sq_returns <- returns_i^2
        weights <- lambda^(0:(length(returns_i)-1))
        weights <- rev(weights / sum(weights))
        
        # Ensure lengths match before multiplying
        min_length <- min(length(weights), length(sq_returns))
        ewma_var <- sum(weights[1:min_length] * sq_returns[1:min_length])
        ewma_vol <- sqrt(ewma_var) * sqrt(252)
        
        forecasted_vols[asset, "forecasted_vol"] <- ewma_vol
        forecasted_vols[asset, "method_used"] <- "EWMA"
      }, error = function(e) {
        cat(sprintf("  EWMA failed for %s: %s\n", asset, e$message))
        
        # 2. Try Parkinson volatility estimator (high-low range based)
        tryCatch({
          # This approach uses the high-low range as volatility estimator
          # We'd normally need high-low data, but we simulate with return shifts
          range_proxy <- max(returns_i) - min(returns_i)
          parkinson_vol <- range_proxy / (4 * sqrt(log(2))) * sqrt(252)
          
          forecasted_vols[asset, "forecasted_vol"] <- parkinson_vol
          forecasted_vols[asset, "method_used"] <- "range"
        }, error = function(e2) {
          # 3. Fallback to standard historical volatility
          vol <- sd(returns_i, na.rm = TRUE) * sqrt(252)
          forecasted_vols[asset, "forecasted_vol"] <- vol
          forecasted_vols[asset, "method_used"] <- "historical"
        })
      })
    }
    
    # Apply reasonable bounds
    vol <- forecasted_vols[asset, "forecasted_vol"]
    
    if (is.na(vol) || vol < min_vol) {
      forecasted_vols[asset, "forecasted_vol"] <- min_vol
      forecasted_vols[asset, "method_used"] <- paste0(forecasted_vols[asset, "method_used"], " (floor)")
    }
    
    if (vol > max_vol) {
      forecasted_vols[asset, "forecasted_vol"] <- max_vol
      forecasted_vols[asset, "method_used"] <- paste0(forecasted_vols[asset, "method_used"], " (cap)")
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

# Enhanced EWMA covariance estimation (our only covariance method now)
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
  
  return(cov_matrix)
}

#=============================================================================
# ECONOMIC INDICATORS FROM ETF DATA
#=============================================================================

# Improved version that guarantees alignment with price dates
get_vix_aligned <- function(price_dates, from = min(price_dates) - 30, to = max(price_dates) + 5) {
  cat("Loading VIX data...\n")
  
  # Ensure dates are proper Date objects
  price_dates <- as.Date(price_dates)
  from <- as.Date(from)
  to <- as.Date(to)
  
  # Create a template with all target dates
  vix_aligned <- xts(rep(NA, length(price_dates)), order.by = price_dates)
  colnames(vix_aligned) <- "VIX"
  
  # Try to get VIX data from Yahoo Finance
  vix <- tryCatch({
    # Try direct VIX ticker
    cat("Trying to load VIX data from Yahoo Finance (^VIX)...\n")
    vix_data <- getSymbols("^VIX", src = "yahoo", from = from - 30, to = to + 5, auto.assign = FALSE)
    
    if (!is.null(vix_data) && nrow(vix_data) > 10) {
      vix_close <- Cl(vix_data)
      colnames(vix_close) <- "VIX"
      cat(sprintf("Successfully loaded VIX data: %d observations\n", nrow(vix_close)))
      
      # Immediately align with price dates here
      for (i in 1:length(price_dates)) {
        price_date <- price_dates[i]
        if (price_date %in% index(vix_close)) {
          vix_aligned[i] <- vix_close[price_date]
        }
      }
      return(vix_aligned)
    } else {
      
      # Try VIXY ETF as alternative
      cat("Failed to load VIX directly, trying VIXY ETF...\n")
      vixy_data <- getSymbols("VIXY", src = "yahoo", from = from - 30, to = to + 5, auto.assign = FALSE)
      
      if (!is.null(vixy_data) && nrow(vixy_data) > 10) {
        vixy_close <- Cl(vixy_data)
        colnames(vixy_close) <- "VIX"
        cat(sprintf("Successfully loaded VIXY data: %d observations\n", nrow(vixy_close)))
        return(vixy_close)
      } else {
        # Try VXX as another alternative
        cat("Failed to load VIXY, trying VXX ETN...\n")
        vxx_data <- getSymbols("VXX", src = "yahoo", from = from - 30, to = to + 5, auto.assign = FALSE)
        
        if (!is.null(vxx_data) && nrow(vxx_data) > 10) {
          vxx_close <- Cl(vxx_data)
          colnames(vxx_close) <- "VIX"
          cat(sprintf("Successfully loaded VXX data: %d observations\n", nrow(vxx_close)))
          return(vxx_close)
        }
      }
    }
    stop("Could not load VIX data from any source")
  }, error = function(e) {
    cat("Error loading VIX data:", e$message, "\n")
    return(NULL)
  })
  
  # If we failed to get data, return NULL
  if (is.null(vix)) {
    cat("CRITICAL: All VIX data sources failed!\n")
    return(NULL)
  }
  
  # Align VIX data with price dates
  aligned_vix <- xts(matrix(NA, nrow=length(price_dates), ncol=1), 
                     order.by=price_dates)
  colnames(aligned_vix) <- "VIX"
  
  # For each price date, find the closest VIX date
  for (i in 1:length(price_dates)) {
    # Get the price date
    price_date <- price_dates[i]
    
    # Check if this exact date exists in VIX data
    if (price_date %in% index(vix)) {
      # Use the exact matching date
      aligned_vix[i,1] <- as.numeric(vix[price_date, "VIX"])
    } else {
      # Find the closest earlier date
      earlier_dates <- index(vix)[index(vix) <= price_date]
      
      if (length(earlier_dates) > 0) {
        closest_earlier <- max(earlier_dates)
        aligned_vix[i,1] <- as.numeric(vix[closest_earlier, "VIX"])
      }
    }
  }
  
  # Fill any NA values using LOCF
  aligned_vix <- na.locf(aligned_vix, na.rm = FALSE)
  
  # Fill any remaining NAs at the beginning using LOCB
  aligned_vix <- na.locf(aligned_vix, fromLast = TRUE, na.rm = FALSE)
  
  cat(sprintf("Final aligned VIX data: %d rows, NA count: %d\n", 
              nrow(aligned_vix), sum(is.na(aligned_vix))))
  
  return(aligned_vix)
}

# Fixed version of create_market_data function with proper VIX alignment
create_market_data <- function(prices, enhanced_indicators = TRUE) {
  cat("\nCreating market data with guaranteed alignment...\n")
  
  # Safety checks
  if (is.null(prices) || nrow(prices) == 0) {
    stop("Cannot create market data - price data is empty")
  }
  
  # Ensure prices has proper dates
  prices <- ensure_date_index(prices)
  price_dates <- index(prices)
  start_date <- min(price_dates)
  end_date <- max(price_dates)
  
  # Get VIX data aligned with price dates
  vix_data <- get_vix_aligned(price_dates, 
                              from = start_date - 30, 
                              to = end_date + 5)
  
  # If VIX loading failed, create a placeholder
  if (is.null(vix_data)) {
    cat("Creating placeholder VIX data\n")
    vix_data <- xts(rep(20, length(price_dates)), order.by = price_dates)
    colnames(vix_data) <- "VIX"
  }
  
  # Ensure exact alignment between VIX and price dates
  vix_aligned <- xts(rep(NA, length(price_dates)), order.by = price_dates)
  colnames(vix_aligned) <- "VIX"
  
  # Match each price date with corresponding VIX value
  common_dates <- intersect(index(vix_data), price_dates)
  if (length(common_dates) > 0) {
    vix_aligned[common_dates] <- vix_data[common_dates, "VIX"]
  }
  
  # Fill NAs with last observation carried forward
  vix_aligned <- na.locf(vix_aligned, na.rm = FALSE)
  
  # Fill remaining NAs with first available value (for start of series)
  first_valid <- which(!is.na(vix_aligned))[1]
  if (!is.na(first_valid)) {
    first_value <- as.numeric(vix_aligned[first_valid])
    vix_aligned[is.na(vix_aligned)] <- first_value
  } else {
    # If all NA, use default value
    vix_aligned[] <- 20
  }
  
  # Calculate returns
  returns <- ROC(prices, type = "discrete")
  returns <- na.omit(returns)
  
  # Create indicators matrix with correct dimensions
  indicators_matrix <- matrix(NA, nrow = length(price_dates), ncol = 1)
  indicators_matrix[, 1] <- as.numeric(vix_aligned)
  
  # Create indicator column names
  indicator_names <- c("VIX")
  
  # Add economic indicators from ETF ratios
  # Only if we have the necessary ETFs
  
  # 1. Growth indicator (SPY/IEF ratio - stocks vs bonds)
  if (all(c("SPY", "IEF") %in% colnames(prices))) {
    cat("Creating growth indicator (SPY/IEF ratio)...\n")
    growth_ratio <- as.numeric(prices[, "SPY"] / prices[, "IEF"])
    
    # Add to matrix - with proper dimension handling
    indicators_matrix <- cbind(indicators_matrix, growth_ratio)
    indicator_names <- c(indicator_names, "GROWTH")
  }
  
  # Continue with other indicators as before...

  
  # 2. Inflation indicator (TIP/IEF ratio - TIPS vs nominal bonds)
  if (all(c("TIP", "IEF") %in% colnames(prices))) {
    cat("Creating inflation indicator (TIP/IEF ratio)...\n")
    inflation_ratio <- as.numeric(prices[, "TIP"] / prices[, "IEF"])
    
    # Add to matrix
    indicators_matrix <- cbind(indicators_matrix, inflation_ratio)
    indicator_names <- c(indicator_names, "INFLATION")
  } else if (all(c("GLD", "IEF") %in% colnames(prices))) {
    # Alternative inflation indicator using gold
    cat("Creating alternative inflation indicator (GLD/IEF ratio)...\n")
    inflation_alt <- as.numeric(prices[, "GLD"] / prices[, "IEF"])
    
    # Add to matrix
    indicators_matrix <- cbind(indicators_matrix, inflation_alt)
    indicator_names <- c(indicator_names, "INFLATION")
  }
  
  # 3. Credit risk indicator (LQD/IEF ratio - credit vs treasury)
  if (all(c("LQD", "IEF") %in% colnames(prices))) {
    cat("Creating credit risk indicator (LQD/IEF ratio)...\n")
    credit_ratio <- as.numeric(prices[, "LQD"] / prices[, "IEF"])
    
    # Add to matrix
    indicators_matrix <- cbind(indicators_matrix, credit_ratio)
    indicator_names <- c(indicator_names, "CREDIT")
  }
  
  # 4. Global growth (EFA/IEF ratio - international stocks vs bonds)
  if (all(c("EFA", "IEF") %in% colnames(prices))) {
    cat("Creating global growth indicator (EFA/IEF ratio)...\n")
    global_ratio <- as.numeric(prices[, "EFA"] / prices[, "IEF"])
    
    # Add to matrix
    indicators_matrix <- cbind(indicators_matrix, global_ratio)
    indicator_names <- c(indicator_names, "GLOBAL_GROWTH")
  }
  
  # 5. Risk appetite (SPY/LQD ratio - stocks vs corporate bonds)
  if (all(c("SPY", "LQD") %in% colnames(prices))) {
    cat("Creating risk appetite indicator (SPY/LQD ratio)...\n")
    risk_ratio <- as.numeric(prices[, "SPY"] / prices[, "LQD"])
    
    # Add to matrix
    indicators_matrix <- cbind(indicators_matrix, risk_ratio)
    indicator_names <- c(indicator_names, "RISK_APPETITE")
  }
  
  # Create XTS object with calculated indicators
  indicators_xts <- xts(indicators_matrix, order.by = price_dates)
  colnames(indicators_xts) <- indicator_names
  
  # Verify indicator data
  cat(sprintf("Created %d indicators: %s\n", 
              ncol(indicators_xts), 
              paste(colnames(indicators_xts), collapse=", ")))
  
  # Calculate Z-scores of indicators for regime detection
  z_score_matrix <- matrix(NA, nrow = length(price_dates), ncol = ncol(indicators_xts))
  
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
  colnames(z_scores_xts) <- paste0(indicator_names, "_Z")
  
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

# Calculate transaction costs
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

cat("\nPart 1 loaded: Data handling, volatility forecasting, and EWMA covariance\n")
#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - STREAMLINED VERSION
# PART 2: REGIME DETECTION, RISK PARITY OPTIMIZATION, PORTFOLIO CONSTRUCTION
#=============================================================================

# Note: This part depends on functions from Part 1

#=============================================================================
# SIMPLIFIED REGIME DETECTION
#=============================================================================

# Calculate Z-scores with robust handling of outliers
calculate_zscore <- function(current_value, history, cap = 3) {
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
  
  # Calculate Z-score
  z_score <- (current_value - hist_mean) / hist_sd
  
  # Cap extreme values
  z_score <- min(max(z_score, -cap), cap)
  
  return(z_score)
}

# Simplified regime detection focusing on key indicators only
detect_market_regime <- function(market_data, lookback = 252) {
  cat("\n---- SIMPLIFIED REGIME DETECTION ----\n")
  
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
  historical_data <- head(tail(market_data, lookback), lookback - 1)
  
  # Initialize z-score container
  z_scores <- list()
  
  # 1. Calculate Volatility Z-score (VIX)
  if ("VIX" %in% colnames(market_data)) {
    vix_history <- as.numeric(historical_data[, "VIX"])
    vix_current <- as.numeric(latest_data[, "VIX"])
    z_scores$volatility <- calculate_zscore(vix_current, vix_history)
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
    z_scores$growth <- calculate_zscore(growth_current, growth_history)
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
    z_scores$inflation <- calculate_zscore(infl_current, infl_history)
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
    z_scores$credit <- calculate_zscore(credit_current, credit_history)
    cat(sprintf("Credit Z-score: %.2f (Current spread: %.2f)\n", 
                z_scores$credit, credit_current))
  } else {
    z_scores$credit <- 0
    cat("Credit spread data not available, using neutral credit signal\n")
  }
  
  #=============================================================================
  # SIMPLIFIED REGIME CLASSIFICATION
  #=============================================================================
  
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
  
  # Calculate regime probabilities
  
  # 1. Risk-off regime (high volatility dominates)
  if (vol_signal && z_scores$volatility > 1.0) {
    regime_probs$risk_off = 0.6 + min((z_scores$volatility - 1.0) * 0.1, 0.3)
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
  
  # 4. Deflation regime (negative growth AND negative inflation)
  if (growth_neg_signal && inflation_neg_signal && !vol_signal) {
    regime_probs$deflation = 0.6 + 
      min((abs(z_scores$growth) + abs(z_scores$inflation)) * 0.05, 0.3)
  } else if (z_scores$growth < 0 && z_scores$inflation < 0) {
    regime_probs$deflation = max(0, abs(z_scores$growth) * 0.2 + abs(z_scores$inflation) * 0.2)
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
      CREDIT_IG = 0.15,
      GOLD = 0.05,
      REIT = 0.15,
      CASH = 0.00
    ),
    
    reflation = list(
      US_EQUITY = 0.25,
      INTL_DEVELOPED = 0.10,
      EMERGING_MARKETS = 0.15,
      US_TREASURY = 0.05,
      CREDIT_IG = 0.15,
      GOLD = 0.15,
      REIT = 0.15,
      CASH = 0.00
    ),
    
    deflation = list(
      US_EQUITY = 0.15,
      INTL_DEVELOPED = 0.05,
      EMERGING_MARKETS = 0.05,
      US_TREASURY = 0.40,
      CREDIT_IG = 0.15,
      GOLD = 0.10,
      REIT = 0.05,
      CASH = 0.05
    ),
    
    stagflation = list(
      US_EQUITY = 0.15,
      INTL_DEVELOPED = 0.05,
      EMERGING_MARKETS = 0.05,
      US_TREASURY = 0.10,
      CREDIT_IG = 0.10,
      GOLD = 0.30,
      REIT = 0.15,
      CASH = 0.10
    ),
    
    risk_off = list(
      US_EQUITY = 0.10,
      INTL_DEVELOPED = 0.00,
      EMERGING_MARKETS = 0.00,
      US_TREASURY = 0.45,
      CREDIT_IG = 0.10,
      GOLD = 0.15,
      REIT = 0.00,
      CASH = 0.20
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

# Map ETFs to asset classes
create_asset_mapping <- function(tickers) {
  mapping <- list()
  
  for (ticker in tickers) {
    # US Equity
    if (ticker %in% c("SPY", "IVV", "VOO", "SPLG", "VTI", "ITOT")) {
      mapping[[ticker]] <- "US_EQUITY"
    }
    # International Developed
    else if (ticker %in% c("EFA", "VEA", "IEFA", "SCHF")) {
      mapping[[ticker]] <- "INTL_DEVELOPED"
    }
    # Emerging Markets
    else if (ticker %in% c("EEM", "VWO", "IEMG", "SCHE")) {
      mapping[[ticker]] <- "EMERGING_MARKETS"
    }
    # US Treasuries
    else if (ticker %in% c("IEF", "VGIT", "SCHR", "TLT", "VGLT", "SHY", "VGSH", "BIL")) {
      mapping[[ticker]] <- "US_TREASURY"
    }
    # Investment Grade Credit
    else if (ticker %in% c("LQD", "VCIT", "IGIB", "VCSH", "SPSB")) {
      mapping[[ticker]] <- "CREDIT_IG"
    }
    # Gold
    else if (ticker %in% c("GLD", "IAU", "SGOL", "BAR")) {
      mapping[[ticker]] <- "GOLD"
    }
    # Real Estate
    else if (ticker %in% c("VNQ", "IYR", "SCHH", "RWR", "XLRE")) {
      mapping[[ticker]] <- "REIT"
    }
    # Handle inverse ETFs
    else if (ticker %in% c("SH", "PSQ", "DOG")) {
      mapping[[ticker]] <- "SHORT_US_EQUITY"
    }
    else if (ticker %in% c("EUM")) {
      mapping[[ticker]] <- "SHORT_EMERGING_MARKETS"
    }
    else if (ticker %in% c("EFZ")) {
      mapping[[ticker]] <- "SHORT_INTL_DEVELOPED"
    }
    else if (ticker %in% c("TBF")) {
      mapping[[ticker]] <- "SHORT_US_TREASURY"
    }
    else if (ticker %in% c("DGZ")) {
      mapping[[ticker]] <- "SHORT_GOLD"
    }
    else if (ticker %in% c("DRV", "REK")) {
      mapping[[ticker]] <- "SHORT_REIT"
    }
    else {
      # For unknown tickers, use the ticker as the asset class
      mapping[[ticker]] <- ticker
    }
  }
  
  return(mapping)
}

#=============================================================================
# IMPROVED RISK PARITY OPTIMIZATION
#=============================================================================

# Simplified risk parity optimization using only EWMA covariance
optimize_risk_parity <- function(returns, vol_forecasts, target_risk_weights = NULL,
                                 target_vol = 0.08, min_weight = 0, max_weight = 1.0,
                                 enable_shorts = FALSE) {
  # Safety checks
  if (is.null(returns) || nrow(returns) < 60 || ncol(returns) < 2) {
    stop("Insufficient returns data for risk parity optimization")
  }
  
  # Get asset names and count
  asset_names <- colnames(returns)
  n_assets <- length(asset_names)
  
  cat(sprintf("\nOptimizing risk parity allocation for %d assets\n", n_assets))
  
  # Check if inverse ETFs are present
  inverse_mask <- rep(FALSE, n_assets)
  if (any(grepl("^SHORT_", asset_names))) {
    inverse_mask <- grepl("^SHORT_", asset_names)
    cat(sprintf("Found %d inverse ETF positions\n", sum(inverse_mask)))
  }
  
  # If shorts are disabled, exclude inverse ETFs
  if (!enable_shorts && any(inverse_mask)) {
    cat("Shorts disabled - excluding inverse ETFs from allocation\n")
    active_assets <- !inverse_mask
    if (sum(active_assets) == 0) {
      stop("No assets remain after excluding inverse ETFs")
    }
    returns <- returns[, active_assets]
    asset_names <- colnames(returns)
    n_assets <- length(asset_names)
    inverse_mask <- rep(FALSE, n_assets)  # Reset mask to all FALSE
  }
  
  # Calculate EWMA covariance matrix
  cov_matrix <- estimate_ewma_covariance(returns)
  
  # Extract volatility forecasts 
  if (is.data.frame(vol_forecasts)) {
    vols <- vol_forecasts$forecasted_vol[asset_names]
  } else if (is.vector(vol_forecasts) && length(vol_forecasts) == n_assets) {
    vols <- vol_forecasts
    names(vols) <- asset_names
  } else {
    stop("Volatility forecasts must be a data frame with forecasted_vol column or a named vector")
  }
  
  # Check for invalid vols and replace with asset median if needed
  if (any(is.na(vols)) || any(vols <= 0)) {
    cat("WARNING: Invalid volatility values detected, replacing with median\n")
    median_vol <- median(vols[!is.na(vols) & vols > 0])
    vols[is.na(vols) | vols <= 0] <- median_vol
  }
  
  # Set target risk weights
  if (is.null(target_risk_weights)) {
    # Default to equal risk allocation
    target_risk <- rep(1/n_assets, n_assets)
    names(target_risk) <- asset_names
  } else {
    # Ensure we have weights for all assets
    target_risk <- rep(0, n_assets)
    names(target_risk) <- asset_names
    
    for (i in 1:n_assets) {
      asset <- asset_names[i]
      if (asset %in% names(target_risk_weights)) {
        target_risk[i] <- target_risk_weights[[asset]]
      }
    }
    
    # Normalize if sum is > 0
    if (sum(target_risk) > 0) {
      target_risk <- target_risk / sum(target_risk)
    } else {
      # If all weights are 0, revert to equal weighting
      target_risk <- rep(1/n_assets, n_assets)
      names(target_risk) <- asset_names
    }
  }
  
  # Print target risk allocation
  cat("Target risk allocation:\n")
  for (i in 1:n_assets) {
    if (target_risk[i] > 0.001) {
      cat(sprintf("  %s: %.2f%%\n", asset_names[i], 100 * target_risk[i]))
    }
  }
  
  #=============================================================================
  # NUMERICAL OPTIMIZATION APPROACH
  #=============================================================================
  
  # Function to minimize - sum of squared deviations from target risk allocation
  risk_deviation <- function(w) {
    # Check weight vector length
    if (length(w) != n_assets) {
      cat("WARNING: Weight vector length mismatch in optimization\n")
      return(1e10)
    }
    
    # Apply constraints
    w <- pmax(min_weight, pmin(max_weight, w))
    
    # Normalize to sum to 1
    if (sum(w) > 0) {
      w <- w / sum(w)
    } else {
      return(1e10)  # Invalid weights
    }
    
    # Calculate portfolio volatility
    port_vol <- sqrt(as.numeric(t(w) %*% cov_matrix %*% w))
    
    # Prevent division by zero
    if (port_vol < 1e-8) {
      return(1e10)
    }
    
    # Calculate risk contribution for each asset
    marginal_contrib <- as.vector(cov_matrix %*% w) / port_vol
    risk_contrib <- w * marginal_contrib
    
    # Calculate proportional risk contribution
    risk_proportion <- risk_contrib / sum(risk_contrib)
    
    # Calculate squared deviation from target
    sum_sq_dev <- sum((risk_proportion - target_risk)^2)
    
    return(sum_sq_dev)
  }
  
  # Starting point - inverse volatility weights
  inv_vol_weights <- 1 / vols
  w0 <- inv_vol_weights / sum(inv_vol_weights)
  
  # If shorts are disabled, set starting weights for inverse ETFs to 0
  if (!enable_shorts && any(inverse_mask)) {
    w0[inverse_mask] <- 0
    if (sum(w0) > 0) {
      w0 <- w0 / sum(w0)
    }
  }
  
  # Enforce bounds
  w0 <- pmax(min_weight, pmin(max_weight, w0))
  if (sum(w0) > 0) {
    w0 <- w0 / sum(w0)
  }
  
  # Perform optimization - with simple error handling
  opt_result <- tryCatch({
    optim(
      par = w0, 
      fn = risk_deviation, 
      method = "L-BFGS-B",
      lower = rep(min_weight, n_assets),
      upper = rep(max_weight, n_assets),
      control = list(maxit = 1000)
    )
  }, error = function(e) {
    cat("Optimization error:", e$message, "\n")
    cat("Falling back to inverse volatility weights\n")
    list(par = w0)
  })
  
  # Extract the optimized weights
  weights <- opt_result$par
  
  # Ensure weights satisfy constraints
  weights <- pmax(min_weight, pmin(max_weight, weights))
  
  # Normalize to sum to 1
  if (sum(weights) > 0) {
    weights <- weights / sum(weights)
  } else {
    # Fallback to inverse volatility weights
    cat("WARNING: Optimization returned invalid weights, using inverse volatility\n")
    weights <- w0
  }
  
  names(weights) <- asset_names
  
  # Calculate portfolio volatility
  port_vol <- sqrt(t(weights) %*% cov_matrix %*% weights)
  
  # Calculate scaling factor to reach target volatility
  vol_scalar <- target_vol / as.numeric(port_vol)
  scaled_weights <- weights * vol_scalar
  
  # Calculate actual risk contribution of each asset
  mrc <- as.vector(cov_matrix %*% weights) / as.numeric(port_vol)
  risk_contrib <- weights * mrc
  risk_pct <- 100 * risk_contrib / sum(risk_contrib)
  names(risk_pct) <- asset_names
  
  # Print final allocation and risk contributions
  cat(sprintf("\nUnscaled portfolio volatility: %.2f%%\n", 100 * as.numeric(port_vol)))
  cat(sprintf("Volatility scaling factor: %.2f\n", vol_scalar))
  cat("\nFinal risk contributions:\n")
  
  for (i in 1:n_assets) {
    if (weights[i] > 0.001) {
      cat(sprintf("  %s: %.2f%% risk (weight: %.2f%%)\n", 
                  asset_names[i], risk_pct[i], 100 * weights[i]))
    }
  }
  
  return(list(
    weights = weights,
    scaled_weights = scaled_weights,
    portfolio_vol = as.numeric(port_vol),
    vol_scalar = vol_scalar,
    risk_contributions = as.numeric(risk_contrib),
    risk_pct = as.numeric(risk_pct)
  ))
}

#=============================================================================
# PORTFOLIO GENERATION
#=============================================================================

# Function to generate portfolio weights based on regime and risk parity
generate_portfolio <- function(returns, vol_forecasts, 
                               regime = "growth", 
                               target_vol = 0.08,
                               asset_mapping = NULL,
                               enable_cash = TRUE,
                               enable_shorts = FALSE,
                               max_leverage = 1.0,
                               max_short = 0.2) {
  # Safety checks
  if (is.null(returns) || nrow(returns) < 30 || ncol(returns) < 2) {
    stop("Insufficient returns data for portfolio generation")
  }
  
  # Get asset names and count
  asset_names <- colnames(returns)
  n_assets <- length(asset_names)
  
  cat(sprintf("\nGenerating portfolio for %s regime with %d assets\n", 
              regime, n_assets))
  
  # Create asset mapping if not provided
  if (is.null(asset_mapping)) {
    asset_mapping <- create_asset_mapping(asset_names)
  }
  
  # Get regime-based target weights
  asset_classes <- unique(unlist(asset_mapping))
  regime_weights <- get_regime_weights(regime, asset_classes)
  
  # Map asset class weights to individual assets
  target_weights <- list()
  for (asset in asset_names) {
    asset_class <- asset_mapping[[asset]]
    if (asset_class %in% names(regime_weights)) {
      target_weights[[asset]] <- regime_weights[[asset_class]]
    } else {
      target_weights[[asset]] <- 0
    }
  }
  
  # Run risk parity optimization
  rp_result <- optimize_risk_parity(
    returns = returns,
    vol_forecasts = vol_forecasts,
    target_risk_weights = target_weights,
    target_vol = target_vol,
    min_weight = 0,
    max_weight = 1.0,
    enable_shorts = enable_shorts
  )
  
  # Extract weights and apply leverage constraint
  weights <- rp_result$scaled_weights
  
  # If max_leverage < 1.0, explicitly allocate to cash
  if (max_leverage < 1.0) {
    cat(sprintf("Applying leverage constraint: %.2f\n", max_leverage))
    weights <- weights * max_leverage
  }
  
  # Calculate positive and negative exposure
  pos_sum <- sum(weights[weights > 0])
  neg_sum <- abs(sum(weights[weights < 0]))
  total_exposure <- pos_sum + neg_sum
  net_exposure <- pos_sum - neg_sum
  
  # Check if we exceed leverage constraint
  if (total_exposure > max_leverage) {
    cat(sprintf("Total exposure (%.2f) exceeds max leverage (%.2f) - scaling down\n", 
                total_exposure, max_leverage))
    scale_factor <- max_leverage / total_exposure
    weights <- weights * scale_factor
    
    # Recalculate
    pos_sum <- sum(weights[weights > 0])
    neg_sum <- abs(sum(weights[weights < 0]))
    total_exposure <- pos_sum + neg_sum
    net_exposure <- pos_sum - neg_sum
  }
  
  # Handle short constraints
  if (enable_shorts && neg_sum > 0) {
    # Apply maximum short constraint
    if (neg_sum > max_short) {
      cat(sprintf("Short exposure (%.2f) exceeds max short (%.2f) - scaling down shorts\n", 
                  neg_sum, max_short))
      short_scale <- max_short / neg_sum
      
      # Only scale down the negative weights
      weights[weights < 0] <- weights[weights < 0] * short_scale
      
      # Recalculate
      neg_sum <- abs(sum(weights[weights < 0]))
      pos_sum <- sum(weights[weights > 0])
      total_exposure <- pos_sum + neg_sum
      net_exposure <- pos_sum - neg_sum
    }
  } else if (!enable_shorts) {
    # If shorts are disabled, eliminate any negative weights
    if (any(weights < 0)) {
      cat("Shorts disabled - removing negative weights\n")
      weights[weights < 0] <- 0
      
      # Recalculate
      pos_sum <- sum(weights)
      neg_sum <- 0
      total_exposure <- pos_sum
      net_exposure <- pos_sum
    }
  }
  
  # Calculate cash position
  cash_weight <- 1.0 - net_exposure
  
  # If cash weight is negative and cash is enabled, reduce leverage
  if (cash_weight < 0 && enable_cash) {
    cat(sprintf("Cash weight negative (%.2f) - reducing leverage\n", cash_weight))
    scale_factor <- pos_sum / (pos_sum - neg_sum)
    weights <- weights * scale_factor
    
    # Recalculate
    pos_sum <- sum(weights[weights > 0])
    neg_sum <- abs(sum(weights[weights < 0]))
    net_exposure <- pos_sum - neg_sum
    cash_weight <- 1.0 - net_exposure
  } else if (!enable_cash && cash_weight > 0) {
    # If cash is disabled, scale up weights to be fully invested
    cat("Cash disabled - scaling up to be fully invested\n")
    scale_factor <- 1.0 / (1.0 - cash_weight)
    weights <- weights * scale_factor
    cash_weight <- 0
    
    # Recalculate
    pos_sum <- sum(weights[weights > 0])
    neg_sum <- abs(sum(weights[weights < 0]))
    net_exposure <- pos_sum - neg_sum
  }
  
  # Print final portfolio summary
  cat("\nFinal portfolio summary:\n")
  cat(sprintf("  Gross exposure: %.2f%%\n", 100 * (pos_sum + neg_sum)))
  cat(sprintf("  Net exposure: %.2f%%\n", 100 * net_exposure))
  cat(sprintf("  Long exposure: %.2f%%\n", 100 * pos_sum))
  cat(sprintf("  Short exposure: %.2f%%\n", 100 * neg_sum))
  cat(sprintf("  Cash weight: %.2f%%\n", 100 * cash_weight))
  
  # Print significant positions
  cat("\nFinal weights:\n")
  for (asset in asset_names) {
    if (abs(weights[asset]) >= 0.005) {  # Show positions ≥ 0.5%
      cat(sprintf("  %s: %.2f%%\n", asset, 100 * weights[asset]))
    }
  }
  
  if (cash_weight >= 0.005) {
    cat(sprintf("  CASH: %.2f%%\n", 100 * cash_weight))
  }
  
  # Calculate expected volatility of final portfolio
  final_vol <- sqrt(t(weights) %*% rp_result$cov_matrix %*% weights)
  cat(sprintf("\n  Expected annualized volatility: %.2f%%\n", 
              100 * as.numeric(final_vol)))
  
  return(list(
    weights = weights,
    cash = cash_weight,
    expected_vol = as.numeric(final_vol),
    gross_exposure = pos_sum + neg_sum,
    net_exposure = net_exposure,
    long_exposure = pos_sum,
    short_exposure = neg_sum,
    target_vol = target_vol,
    regime = regime
  ))
}

cat("\nPart 2 loaded: Regime detection and portfolio construction\n")

#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - STREAMLINED VERSION
# PART 3: BACKTESTING, PERFORMANCE ANALYSIS, AND VISUALIZATION
#=============================================================================

# Note: This part depends on functions from Parts 1 & 2

#=============================================================================
# BACKTESTING FRAMEWORK
#=============================================================================

# Function to backtest the enhanced risk parity strategy
backtest_strategy <- function(prices, returns = NULL, market_data = NULL,
                              asset_mapping = NULL,
                              target_vol = 0.08,
                              max_drawdown = 0.10,
                              rebalance_freq = "M",  # "D" = Daily, "W" = Weekly, "M" = Monthly
                              lookback_window = 252, # Historical window for vol/cov estimation
                              enable_cash = TRUE,
                              enable_shorts = FALSE,
                              max_leverage = 1.0,
                              max_short = 0.2) {
  
  # Performance tracking
  start_time <- Sys.time()
  
  # Log start of backtest
  cat("\n========================================================\n")
  cat("Starting Enhanced Risk Parity Backtest\n")
  cat("========================================================\n\n")
  cat(sprintf("Target volatility: %.1f%%\n", target_vol * 100))
  cat(sprintf("Rebalance frequency: %s\n", rebalance_freq))
  cat(sprintf("Historical window: %d days\n", lookback_window))
  cat(sprintf("Cash enabled: %s\n", ifelse(enable_cash, "Yes", "No")))
  cat(sprintf("Shorts enabled: %s\n", ifelse(enable_shorts, "Yes", "No")))
  cat(sprintf("Max leverage: %.2f\n", max_leverage))
  
  # Safety checks
  if (is.null(prices) || nrow(prices) == 0) {
    stop("Missing or empty price data")
  }
  
  # Calculate returns if not provided
  if (is.null(returns)) {
    cat("Calculating returns from prices\n")
    returns <- ROC(prices, type = "discrete")
    returns <- na.omit(returns)
  }
  
  # Create market data if not provided
  if (is.null(market_data)) {
    cat("Creating market data from prices\n")
    market_data <- create_market_data(prices, enhanced_indicators = TRUE)
  }
  
  # Align all data to ensure consistent dates
  common_dates <- intersect(intersect(index(prices), index(returns)), index(market_data))
  
  if (length(common_dates) < lookback_window) {
    stop(sprintf("Insufficient data for backtest. Need at least %d common dates, but only have %d.",
                 lookback_window, length(common_dates)))
  }
  
  # Subset data to common dates
  prices <- prices[common_dates]
  returns <- returns[common_dates]
  market_data <- market_data[common_dates]
  
  # Create asset mapping if not provided
  if (is.null(asset_mapping)) {
    cat("Creating asset mapping from tickers\n")
    asset_mapping <- create_asset_mapping(colnames(prices))
  }
  
  # Get transaction costs
  transaction_costs <- get_transaction_costs(colnames(prices))
  
  # Determine rebalance dates based on frequency
  date_strings <- as.character(common_dates)
  
  if (rebalance_freq == "D") {
    # Daily rebalance (use all dates)
    rebalance_dates <- common_dates
  } else if (rebalance_freq == "W") {
    # Weekly rebalance (use Fridays)
    weekdays_vector <- weekdays(common_dates)
    rebalance_dates <- common_dates[weekdays_vector == "Friday"]
    
    # Ensure the last date is included
    last_date <- tail(common_dates, 1)
    if (!(last_date %in% rebalance_dates)) {
      rebalance_dates <- c(rebalance_dates, last_date)
    }
  } else if (rebalance_freq == "M") {
    # Monthly rebalance (end of each month)
    date_table <- data.frame(
      date = common_dates,
      year = format(common_dates, "%Y"),
      month = format(common_dates, "%m")
    )
    
    # Group by year/month and get last date
    month_ends <- tapply(as.character(date_table$date), 
                         list(date_table$year, date_table$month),
                         function(x) max(as.Date(x)))
    rebalance_dates <- sort(as.Date(month_ends))
    
    # Ensure the last date is included
    last_date <- tail(common_dates, 1)
    if (!(last_date %in% rebalance_dates)) {
      rebalance_dates <- c(rebalance_dates, last_date)
    }
  } else {
    stop("Invalid rebalance frequency. Use 'D', 'W', or 'M'")
  }
  
  rebalance_date_strings <- as.character(rebalance_dates)
  
  # Initialize containers for results
  n_dates <- length(common_dates)
  n_assets <- ncol(prices)
  asset_names <- colnames(prices)
  
  # Create matrix for weights history
  weights_matrix <- matrix(0, nrow = n_dates, ncol = n_assets)
  colnames(weights_matrix) <- asset_names
  rownames(weights_matrix) <- date_strings
  
  # Create XTS objects for other tracking variables
  portfolio_returns <- xts(rep(0, n_dates), order.by = common_dates)
  colnames(portfolio_returns) <- "PORTFOLIO"
  
  cash_history <- xts(rep(0, n_dates), order.by = common_dates)
  colnames(cash_history) <- "CASH"
  
  regime_history <- xts(rep("", n_dates), order.by = common_dates)
  colnames(regime_history) <- "REGIME"
  
  # Create containers for regime probabilities
  regime_prob_columns <- c("growth", "reflation", "deflation", "stagflation", "risk_off")
  regime_probs <- xts(matrix(0, nrow = n_dates, ncol = length(regime_prob_columns)),
                      order.by = common_dates)
  colnames(regime_probs) <- regime_prob_columns
  
  # Tracking for volatility and exposure
  vol_target_history <- xts(rep(target_vol, n_dates), order.by = common_dates)
  vol_realized_history <- xts(rep(0, n_dates), order.by = common_dates)
  gross_exposure_history <- xts(rep(0, n_dates), order.by = common_dates)
  net_exposure_history <- xts(rep(0, n_dates), order.by = common_dates)
  
  # Initialize with zero weights
  current_weights <- rep(0, n_assets)
  names(current_weights) <- asset_names
  
  # Dynamic volatility target
  current_target_vol <- target_vol
  
  # Track rebalance count
  rebalance_count <- 0
  
  cat(sprintf("\nProcessing %d rebalance dates from %s to %s\n",
              length(rebalance_dates),
              format(head(rebalance_dates, 1), "%Y-%m-%d"),
              format(tail(rebalance_dates, 1), "%Y-%m-%d")))
  
  # Loop through each rebalance date
  for (i in 1:length(rebalance_dates)) {
    rebalance_date <- rebalance_dates[i]
    rebalance_date_string <- as.character(rebalance_date)
    
    # Progress indicator
    if (i %% 10 == 1 || i == length(rebalance_dates)) {
      cat(sprintf("\nProcessing rebalance date %d of %d: %s\n",
                  i, length(rebalance_dates), rebalance_date_string))
    }
    
    # Get historical window for this rebalance date
    historical_end_idx <- which(common_dates == rebalance_date)
    
    if (historical_end_idx <= lookback_window) {
      # Not enough history, use all available data
      historical_start_idx <- 1
    } else {
      historical_start_idx <- historical_end_idx - lookback_window + 1
    }
    
    historical_dates <- common_dates[historical_start_idx:historical_end_idx]
    
    # Get historical returns and market data
    hist_returns <- returns[historical_dates]
    hist_market_data <- market_data[historical_dates]
    
    # Detect market regime
    regime_result <- detect_market_regime(
      market_data = hist_market_data, 
      lookback = min(nrow(hist_market_data) - 1, lookback_window)
    )
    
    current_regime <- regime_result$regime
    
    # Store regime and probabilities
    date_idx <- which(date_strings == rebalance_date_string)
    if (length(date_idx) > 0) {
      regime_history[date_idx] <- current_regime
      
      # Store regime probabilities
      for (regime in colnames(regime_probs)) {
        if (regime %in% names(regime_result$regime_probabilities)) {
          regime_probs[date_idx, regime] <- regime_result$regime_probabilities[[regime]]
        }
      }
    }
    
    # Calculate volatility forecasts
    vol_forecasts <- forecast_volatility(hist_returns)
    
    # Apply dynamic volatility targeting based on drawdown
    if (i > 1) {
      # Calculate current drawdown
      portfolio_return_series <- as.numeric(portfolio_returns[1:(historical_end_idx-1)])
      if (length(portfolio_return_series) > 0) {
        cum_returns <- cumprod(1 + portfolio_return_series)
        current_drawdown <- 1 - tail(cum_returns, 1) / max(cum_returns)
        
        # Scale target vol based on drawdown
        drawdown_scale <- 1 - min(1, current_drawdown / max_drawdown)
        current_target_vol <- target_vol * drawdown_scale
        
        cat(sprintf("Current drawdown: %.2f%%, Adjusted vol target: %.2f%%\n", 
                    100 * current_drawdown, 100 * current_target_vol))
      }
    }
    
    # Generate portfolio for current regime
    portfolio <- tryCatch({
      generate_portfolio(
        returns = hist_returns,
        vol_forecasts = vol_forecasts,
        regime = current_regime,
        target_vol = current_target_vol,
        asset_mapping = asset_mapping,
        enable_cash = enable_cash,
        enable_shorts = enable_shorts,
        max_leverage = max_leverage,
        max_short = max_short
      )
    }, error = function(e) {
      cat("ERROR generating portfolio:", e$message, "\n")
      # Return a default portfolio with all cash
      list(
        weights = rep(0, n_assets),
        cash = 1.0,
        expected_vol = 0,
        gross_exposure = 0,
        net_exposure = 0,
        long_exposure = 0,
        short_exposure = 0,
        target_vol = current_target_vol,
        regime = current_regime
      )
    })
    
    # Extract new weights
    new_weights <- portfolio$weights
    names(new_weights) <- asset_names
    current_cash <- portfolio$cash
    
    # Find dates to apply these weights
    if (i < length(rebalance_dates)) {
      next_rebalance <- rebalance_dates[i + 1]
      holding_dates <- common_dates[common_dates >= rebalance_date & 
                                      common_dates < next_rebalance]
    } else {
      # Last rebalance, apply to all remaining dates
      holding_dates <- common_dates[common_dates >= rebalance_date]
    }
    
    # Calculate turnover and transaction costs
    turnover <- sum(abs(new_weights - current_weights))
    transaction_cost <- sum(abs(new_weights - current_weights) * transaction_costs)
    
    # Increment rebalance counter only when weights actually change
    if (turnover > 0.001) {  # Only count meaningful rebalances
      rebalance_count <- rebalance_count + 1
      
      cat(sprintf("Portfolio turnover: %.2f%%, Transaction costs: %.2f%%\n", 
                  100 * turnover, 100 * transaction_cost))
    }
    
    # Apply transaction costs to first day's return
    if (length(holding_dates) > 0 && transaction_cost > 0) {
      first_day_idx <- which(common_dates == holding_dates[1])
      if (length(first_day_idx) > 0) {
        portfolio_returns[first_day_idx] <- portfolio_returns[first_day_idx] - transaction_cost
      }
    }
    
    # Store weights and metrics for holding period
    for (holding_date in holding_dates) {
      holding_date_string <- as.character(holding_date)
      holding_idx <- which(date_strings == holding_date_string)
      
      if (length(holding_idx) > 0) {
        # Store weights
        weights_matrix[holding_idx, ] <- new_weights
        
        # Store other metrics
        cash_history[holding_idx] <- current_cash
        vol_target_history[holding_idx] <- current_target_vol
        vol_realized_history[holding_idx] <- portfolio$expected_vol
        gross_exposure_history[holding_idx] <- portfolio$gross_exposure
        net_exposure_history[holding_idx] <- portfolio$net_exposure
        regime_history[holding_idx] <- current_regime
      }
    }
    
    # Update current weights for next iteration
    current_weights <- new_weights
  }
  
  # Convert weights matrix to XTS object
  weights_history <- xts(weights_matrix, order.by = common_dates)
  
  # Calculate daily portfolio returns
  cat("\nCalculating daily portfolio returns\n")
  for (i in 2:n_dates) {
    date <- common_dates[i]
    prev_date <- common_dates[i-1]
    
    # Get weights from previous day
    prev_weights <- weights_matrix[as.character(prev_date), ]
    
    # Calculate portfolio return (weight * asset return)
    daily_return <- sum(prev_weights * returns[date, ], na.rm = TRUE)
    
    # Store in portfolio returns series
    portfolio_returns[i] <- daily_return
  }
  
  # Calculate cumulative returns
  cumulative_returns <- cumprod(1 + portfolio_returns)
  
  # Calculate drawdowns
  drawdowns <- 1 - cumulative_returns / cummax(cumulative_returns)
  
  # Calculate performance metrics
  annualized_return <- (tail(cumulative_returns, 1)^(252/nrow(portfolio_returns)) - 1)
  annualized_vol <- sd(portfolio_returns, na.rm = TRUE) * sqrt(252)
  sharpe_ratio <- mean(portfolio_returns, na.rm = TRUE) / 
    sd(portfolio_returns, na.rm = TRUE) * sqrt(252)
  
  # Calculate downside deviation (using only negative returns)
  downside_returns <- portfolio_returns[portfolio_returns < 0]
  downside_deviation <- sqrt(mean(downside_returns^2, na.rm = TRUE)) * sqrt(252)
  
  # Calculate Sortino ratio
  sortino_ratio <- mean(portfolio_returns, na.rm = TRUE) / downside_deviation * sqrt(252)
  
  # Maximum drawdown
  max_dd <- max(drawdowns, na.rm = TRUE)
  
  # Calmar ratio
  calmar_ratio <- annualized_return / max_dd
  
  # Regime distribution
  regime_counts <- table(as.character(regime_history))
  regime_distribution <- regime_counts / sum(regime_counts)
  
  # Log elapsed time
  end_time <- Sys.time()
  elapsed <- difftime(end_time, start_time, units = "mins")
  
  # Print performance summary
  cat("\n========================================================\n")
  cat("BACKTEST RESULTS\n")
  cat("========================================================\n\n")
  cat(sprintf("Backtest period: %s to %s (%d trading days)\n",
              format(head(common_dates, 1), "%Y-%m-%d"),
              format(tail(common_dates, 1), "%Y-%m-%d"),
              n_dates))
  cat(sprintf("Rebalances performed: %d\n", rebalance_count))
  cat(sprintf("Elapsed time: %.2f minutes\n\n", as.numeric(elapsed)))
  
  cat("PERFORMANCE METRICS:\n")
  cat(sprintf("Annualized Return: %.2f%%\n", 100 * annualized_return))
  cat(sprintf("Annualized Volatility: %.2f%%\n", 100 * annualized_vol))
  cat(sprintf("Sharpe Ratio: %.2f\n", sharpe_ratio))
  cat(sprintf("Sortino Ratio: %.2f\n", sortino_ratio))
  cat(sprintf("Calmar Ratio: %.2f\n", calmar_ratio))
  cat(sprintf("Maximum Drawdown: %.2f%%\n\n", 100 * max_dd))
  
  cat("REGIME DISTRIBUTION:\n")
  for (regime in names(regime_distribution)) {
    cat(sprintf("  %s: %.1f%%\n", regime, 100 * regime_distribution[regime]))
  }
  
  # Return comprehensive results
  results <- list(
    portfolio_returns = portfolio_returns,
    cumulative_returns = cumulative_returns,
    weights_history = weights_history,
    cash_history = cash_history,
    regime_history = regime_history,
    regime_probabilities = regime_probs,
    vol_target_history = vol_target_history,
    vol_realized_history = vol_realized_history,
    gross_exposure_history = gross_exposure_history,
    net_exposure_history = net_exposure_history,
    drawdowns = drawdowns,
    annualized_return = annualized_return,
    annualized_vol = annualized_vol,
    sharpe_ratio = sharpe_ratio,
    sortino_ratio = sortino_ratio,
    calmar_ratio = calmar_ratio,
    max_drawdown = max_dd,
    regime_distribution = regime_distribution,
    asset_mapping = asset_mapping
  )
  
  return(results)
}

#=============================================================================
# PERFORMANCE ANALYTICS
#=============================================================================

# Function to analyze performance and generate report
analyze_performance <- function(results) {
  # Safety check
  if (is.null(results) || !is.list(results)) {
    stop("Invalid results object provided")
  }
  
  # Extract key time series
  returns <- results$portfolio_returns
  cum_returns <- results$cumulative_returns
  drawdowns <- results$drawdowns
  weights <- results$weights_history
  regime_history <- results$regime_history
  
  # Calculate monthly returns
  monthly_returns <- apply.monthly(returns, sum)
  
  # Calculate rolling metrics (if enough data)
  if (nrow(returns) >= 63) {  # At least 3 months of daily data
    # 63-day (quarterly) rolling Sharpe ratio
    rolling_sharpe <- rollapply(returns, width = 63, 
                                FUN = function(x) {
                                  mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE) * sqrt(252)
                                },
                                by.column = TRUE, align = "right")
    
    # 63-day rolling volatility (annualized)
    rolling_vol <- rollapply(returns, width = 63,
                             FUN = function(x) {
                               sd(x, na.rm = TRUE) * sqrt(252)
                             },
                             by.column = TRUE, align = "right")
    
    # 63-day rolling return (annualized)
    rolling_return <- rollapply(returns, width = 63,
                                FUN = function(x) {
                                  (prod(1 + x) ^ (252/63) - 1)
                                },
                                by.column = TRUE, align = "right")
  } else {
    rolling_sharpe <- NULL
    rolling_vol <- NULL
    rolling_return <- NULL
  }
  
  # Calculate return statistics by regime
  regimes <- unique(as.character(regime_history))
  regime_stats <- list()
  
  for (regime in regimes) {
    # Get returns for this regime
    regime_returns <- returns[regime_history == regime]
    
    if (length(regime_returns) > 0) {
      # Calculate statistics
      regime_stats[[regime]] <- list(
        days = length(regime_returns),
        mean_return = mean(regime_returns, na.rm = TRUE),
        volatility = sd(regime_returns, na.rm = TRUE) * sqrt(252),
        cumulative = prod(1 + regime_returns),
        sharpe = mean(regime_returns, na.rm = TRUE) / 
          sd(regime_returns, na.rm = TRUE) * sqrt(252),
        pct_positive = mean(regime_returns > 0, na.rm = TRUE) * 100
      )
    }
  }
  
  # Calculate drawdown statistics
  drawdown_episodes <- c()
  in_drawdown <- FALSE
  start_idx <- 1
  threshold <- 0.05  # 5% threshold for significant drawdowns
  
  for (i in 2:length(drawdowns)) {
    # Start of a drawdown
    if (!in_drawdown && drawdowns[i] > threshold) {
      in_drawdown <- TRUE
      start_idx <- i
    }
    
    # End of a drawdown
    if (in_drawdown && drawdowns[i] <= threshold) {
      in_drawdown <- FALSE
      
      # Calculate statistics for this drawdown episode
      max_dd <- max(drawdowns[start_idx:i])
      duration <- i - start_idx
      
      # Add to list if significant
      if (max_dd > threshold && duration > 5) {  # At least 5 days long
        drawdown_episodes <- c(drawdown_episodes, list(list(
          start_date = index(drawdowns)[start_idx],
          end_date = index(drawdowns)[i],
          max_drawdown = max_dd,
          duration = duration
        )))
      }
    }
  }
  
  # Check if we're still in a drawdown at the end of the data
  if (in_drawdown) {
    i <- length(drawdowns)
    max_dd <- max(drawdowns[start_idx:i])
    duration <- i - start_idx
    
    # Add to list if significant
    if (max_dd > threshold && duration > 5) {
      drawdown_episodes <- c(drawdown_episodes, list(list(
        start_date = index(drawdowns)[start_idx],
        end_date = index(drawdowns)[i],
        max_drawdown = max_dd,
        duration = duration
      )))
    }
  }
  
  # Create performance summary data frame
  performance_summary <- data.frame(
    Metric = c(
      "Annualized Return",
      "Annualized Volatility",
      "Sharpe Ratio",
      "Sortino Ratio",
      "Calmar Ratio",
      "Maximum Drawdown",
      "% Positive Months",
      "Best Month",
      "Worst Month"
    ),
    Value = c(
      sprintf("%.2f%%", 100 * results$annualized_return),
      sprintf("%.2f%%", 100 * results$annualized_vol),
      sprintf("%.2f", results$sharpe_ratio),
      sprintf("%.2f", results$sortino_ratio),
      sprintf("%.2f", results$calmar_ratio),
      sprintf("%.2f%%", 100 * results$max_drawdown),
      sprintf("%.1f%%", 100 * mean(monthly_returns > 0, na.rm = TRUE)),
      sprintf("%.2f%%", 100 * max(monthly_returns, na.rm = TRUE)),
      sprintf("%.2f%%", 100 * min(monthly_returns, na.rm = TRUE))
    )
  )
  
  # Return comprehensive analysis results
  return(list(
    summary = performance_summary,
    monthly_returns = monthly_returns,
    rolling_sharpe = rolling_sharpe,
    rolling_vol = rolling_vol,
    rolling_return = rolling_return,
    regime_stats = regime_stats,
    drawdown_episodes = drawdown_episodes
  ))
}

#=============================================================================
# VISUALIZATION FUNCTIONS
#=============================================================================

# Function to plot cumulative returns
plot_cumulative_returns <- function(results, benchmark_returns = NULL, 
                                    log_scale = FALSE, title = "Cumulative Returns") {
  # Extract cumulative returns
  cum_returns <- results$cumulative_returns
  
  # Create base data frame
  plot_data <- data.frame(
    Date = index(cum_returns),
    Strategy = as.numeric(cum_returns)
  )
  
  # Add benchmark if provided
  if (!is.null(benchmark_returns)) {
    # Calculate benchmark cumulative returns
    benchmark_cum <- cumprod(1 + benchmark_returns)
    
    # Align dates
    common_dates <- intersect(index(cum_returns), index(benchmark_cum))
    
    if (length(common_dates) > 0) {
      # Rescale to start at same value
      start_date <- min(common_dates)
      strategy_start <- as.numeric(cum_returns[start_date])
      benchmark_start <- as.numeric(benchmark_cum[start_date])
      
      # Add to plot data
      benchmark_aligned <- benchmark_cum[common_dates] * strategy_start / benchmark_start
      plot_data$Benchmark <- as.numeric(benchmark_aligned)
    }
  }
  
  # Melt data for ggplot
  plot_data_long <- reshape2::melt(plot_data, id.vars = "Date", 
                                   variable.name = "Series", 
                                   value.name = "Value")
  
  # Create the plot
  p <- ggplot(plot_data_long, aes(x = Date, y = Value, color = Series)) +
    geom_line() +
    theme_minimal() +
    labs(title = title,
         x = "Date",
         y = "Value ($)",
         color = "")
  
  # Add log scale if requested
  if (log_scale) {
    p <- p + scale_y_log10()
  }
  
  return(p)
}

# Function to plot drawdowns
plot_drawdowns <- function(results, title = "Portfolio Drawdowns") {
  # Extract drawdowns
  drawdowns <- results$drawdowns
  
  # Create data frame
  plot_data <- data.frame(
    Date = index(drawdowns),
    Drawdown = as.numeric(drawdowns)
  )
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = Date, y = -Drawdown)) +
    geom_area(fill = "#E53935", alpha = 0.7) +
    theme_minimal() +
    labs(title = title,
         x = "Date",
         y = "Drawdown (%)") +
    scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}

# Function to plot regime history
plot_regimes <- function(results, title = "Market Regimes Over Time") {
  # Extract regime history
  regimes <- results$regime_history
  
  # Create data frame
  plot_data <- data.frame(
    Date = index(regimes),
    Regime = as.character(regimes)
  )
  
  # Define color palette for regimes
  regime_colors <- c(
    "growth" = "#4CAF50",      # Green
    "reflation" = "#FF9800",   # Orange
    "deflation" = "#2196F3",   # Blue
    "stagflation" = "#F44336", # Red
    "risk_off" = "#9C27B0"     # Purple
  )
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = Date, y = 1, fill = Regime)) +
    geom_tile() +
    scale_fill_manual(values = regime_colors) +
    theme_minimal() +
    labs(title = title,
         x = "Date",
         fill = "Regime") +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  # If we have probabilities, create a second plot
  if (!is.null(results$regime_probabilities) && ncol(results$regime_probabilities) > 0) {
    prob_data <- data.frame(
      Date = index(results$regime_probabilities),
      results$regime_probabilities
    )
    
    # Melt for ggplot
    prob_data_long <- reshape2::melt(prob_data, id.vars = "Date",
                                     variable.name = "Regime",
                                     value.name = "Probability")
    
    # Create probability plot
    p2 <- ggplot(prob_data_long, aes(x = Date, y = Probability, fill = Regime)) +
      geom_area(position = "stack") +
      scale_fill_manual(values = regime_colors) +
      theme_minimal() +
      labs(title = "Regime Probabilities",
           x = "Date",
           y = "Probability",
           fill = "Regime") +
      theme(plot.title = element_text(hjust = 0.5))
    
    return(list(regime_plot = p, probability_plot = p2))
  }
  
  return(p)
}

# Function to plot asset allocation over time
plot_allocation <- function(results, top_n = 8, title = "Portfolio Allocation Over Time") {
  # Extract weights
  weights <- results$weights_history
  
  # Create data frame
  weights_df <- data.frame(Date = index(weights))
  for (col in colnames(weights)) {
    weights_df[[col]] <- as.numeric(weights[, col])
  }
  
  # Add cash if available
  if (!is.null(results$cash_history)) {
    weights_df[["CASH"]] <- as.numeric(results$cash_history)
  }
  
  # Identify top assets by average allocation
  avg_weights <- colMeans(abs(weights_df[, -1]), na.rm = TRUE)
  top_assets <- names(sort(avg_weights, decreasing = TRUE))[1:min(top_n, length(avg_weights))]
  
  # Combine all other assets into "Other"
  weights_df$Other <- 0
  for (col in colnames(weights_df)[-1]) {
    if (!(col %in% top_assets) && col != "Other") {
      weights_df$Other <- weights_df$Other + weights_df[[col]]
      weights_df[[col]] <- NULL
    }
  }
  
  # Melt for ggplot
  weights_long <- reshape2::melt(weights_df, id.vars = "Date",
                                 variable.name = "Asset",
                                 value.name = "Weight")
  
  # Create stacked area plot
  p <- ggplot(weights_long, aes(x = Date, y = Weight, fill = Asset)) +
    geom_area(position = "stack") +
    theme_minimal() +
    labs(title = title,
         x = "Date",
         y = "Allocation",
         fill = "Asset") +
    scale_y_continuous(labels = scales::percent_format()) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}

# Function to plot monthly returns heatmap
plot_monthly_heatmap <- function(results, title = "Monthly Returns") {
  # Calculate monthly returns
  monthly_returns <- apply.monthly(results$portfolio_returns, sum)
  
  # Create data frame
  monthly_df <- data.frame(
    Date = index(monthly_returns),
    Return = as.numeric(monthly_returns)
  )
  
  # Extract year and month
  monthly_df$Year <- format(monthly_df$Date, "%Y")
  monthly_df$Month <- factor(format(monthly_df$Date, "%b"),
                             levels = month.abb)
  
  # Create heatmap
  p <- ggplot(monthly_df, aes(x = Month, y = Year, fill = Return)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "red", mid = "white", high = "green", 
                         midpoint = 0, labels = scales::percent_format()) +
    theme_minimal() +
    labs(title = title,
         x = "",
         y = "",
         fill = "Return") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Function to create complete performance dashboard
create_dashboard <- function(results, benchmark_returns = NULL) {
  # Run performance analysis
  analysis <- analyze_performance(results)
  
  # Create all plots
  plots <- list(
    cumulative_returns = plot_cumulative_returns(results, benchmark_returns),
    drawdowns = plot_drawdowns(results),
    regimes = plot_regimes(results),
    allocation = plot_allocation(results),
    monthly_heatmap = plot_monthly_heatmap(results)
  )
  
  # Add rolling metrics plots if available
  if (!is.null(analysis$rolling_sharpe)) {
    rolling_sharpe_df <- data.frame(
      Date = index(analysis$rolling_sharpe),
      Sharpe = as.numeric(analysis$rolling_sharpe)
    )
    
    plots$rolling_sharpe <- ggplot(rolling_sharpe_df, aes(x = Date, y = Sharpe)) +
      geom_line(color = "#4CAF50") +
      theme_minimal() +
      labs(title = "3-Month Rolling Sharpe Ratio",
           x = "Date",
           y = "Sharpe Ratio") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  if (!is.null(analysis$rolling_vol)) {
    rolling_vol_df <- data.frame(
      Date = index(analysis$rolling_vol),
      Volatility = as.numeric(analysis$rolling_vol)
    )
    
    plots$rolling_vol <- ggplot(rolling_vol_df, aes(x = Date, y = Volatility)) +
      geom_line(color = "#2196F3") +
      theme_minimal() +
      labs(title = "3-Month Rolling Volatility",
           x = "Date",
           y = "Annualized Volatility") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  # Create regime performance table
  regime_perf <- data.frame(
    Regime = character(),
    Days = integer(),
    Return = numeric(),
    Volatility = numeric(),
    Sharpe = numeric(),
    "% Positive" = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (regime in names(analysis$regime_stats)) {
    stats <- analysis$regime_stats[[regime]]
    regime_perf <- rbind(regime_perf, data.frame(
      Regime = regime,
      Days = stats$days,
      Return = stats$mean_return * 252 * 100,  # Annualized percent
      Volatility = stats$volatility * 100,      # Annualized percent
      Sharpe = stats$sharpe,
      `% Positive` = stats$pct_positive,
      stringsAsFactors = FALSE
    ))
  }
  
  # Return comprehensive dashboard
  return(list(
    summary = analysis$summary,
    regime_performance = regime_perf,
    plots = plots,
    analysis = analysis
  ))
}

# Run a complete backtest with real data
run_risk_parity_backtest <- function(tickers, 
                                     start_date = "2010-01-01",
                                     end_date = Sys.Date(),
                                     target_vol = 0.08,
                                     rebalance_freq = "M",
                                     enable_shorts = FALSE) {
  # Step 1: Load market data
  cat("Loading market data for", length(tickers), "tickers\n")
  prices <- load_market_data(tickers, start_date = start_date, end_date = end_date,
                             include_inverse = enable_shorts)
  
  # Step 2: Calculate returns
  returns <- ROC(prices, type = "discrete")
  returns <- na.omit(returns)
  
  # Step 3: Create market data with economic indicators
  market_data <- create_market_data(prices)
  
  # Step 4: Create asset mapping
  asset_mapping <- create_asset_mapping(colnames(prices))
  
  # Step 5: Run backtest
  results <- backtest_strategy(
    prices = prices,
    returns = returns,
    market_data = market_data,
    asset_mapping = asset_mapping,
    target_vol = target_vol,
    rebalance_freq = rebalance_freq,
    lookback_window = 252,
    enable_cash = TRUE,
    enable_shorts = enable_shorts
  )
  
  # Step 6: Create dashboard
  dashboard <- create_dashboard(results)
  
  # Return both results and dashboard
  return(list(
    results = results,
    dashboard = dashboard
  ))
}

cat("\nPart 3 loaded: Backtesting and performance analysis\n")
cat("\nEnhanced Risk Parity Trading System - Streamlined Version is fully loaded!\n")

# Current system time and user info - properly commented
# Current Date and Time (UTC): 2025-09-03 09:42:45
# Current User's Login: balint27

# Print system information
cat("\n========================================================\n")
cat(sprintf("Current Date and Time (UTC): %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat(sprintf("R Version: %s\n", R.version.string))
cat("========================================================\n")

# Example usage
backtest_results <- run_risk_parity_backtest(
  tickers = c("SPY", "IEF", "GLD", "LQD", "EEM", "EFA", "VNQ"),
  start_date = "2015-01-01",
  target_vol = 0.08,
  enable_shorts = TRUE
)

# Access the dashboard
dashboard <- backtest_results$dashboard

# Display performance metrics
print(dashboard$summary)