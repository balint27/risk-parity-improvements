#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - BLOOMBERG VERSION
# PART 1: DATA HANDLING, VOLATILITY ESTIMATION, AND COVARIANCE CALCULATION
#=============================================================================

# Load required packages with reliable error handling
required_packages <- c("tidyverse", "xts", "PerformanceAnalytics",
                       "TTR", "zoo", "tidyquant", "ggplot2", "reshape2", 
                       "rugarch", "nloptr", "gridExtra", "grid", "Rblpapi")

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
# DATA HANDLING WITH BLOOMBERG API
#=============================================================================

# Initialize Bloomberg connection
initialize_bloomberg <- function() {
  cat("\nInitializing Bloomberg connection...\n")
  
  conn_success <- tryCatch({
    blpConnect()
    TRUE
  }, error = function(e) {
    cat(sprintf("Error connecting to Bloomberg: %s\n", e$message))
    cat("Make sure Bloomberg terminal is running and you have proper access rights.\n")
    FALSE
  })
  
  return(conn_success)
}

# Convert standard ticker to Bloomberg ticker format
convert_to_bloomberg_ticker <- function(ticker) {
  # Bloomberg typically uses "TICKER Equity" format for US equities
  # For ETFs, we'll use "TICKER US Equity"
  # This is a simplified conversion - adjust based on your needs
  
  # Default suffix
  suffix <- " US Equity"
  
  # Special cases
  if (grepl("^\\^", ticker)) {
    # Handle index tickers like ^VIX
    clean_ticker <- gsub("^\\^", "", ticker)
    return(paste0(clean_ticker, " Index"))
  } else if (ticker == "CASH") {
    # Handle cash placeholder
    return("US0001M Index")  # 1-month US LIBOR as cash proxy
  }
  
  return(paste0(ticker, suffix))
}

# Load market data using Bloomberg API
load_market_data_bloomberg <- function(tickers, start_date, end_date = Sys.Date(), 
                                       include_inverse = TRUE) {
  # Ensure start and end dates are Date objects
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  cat(sprintf("\nLoading Bloomberg market data from %s to %s\n", start_date, end_date))
  
  # Define inverse ETF mapping (same as original)
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
  
  # Convert tickers to Bloomberg format
  bloomberg_tickers <- sapply(tickers, convert_to_bloomberg_ticker)
  
  # Connect to Bloomberg if not already connected
  if (!initialize_bloomberg()) {
    stop("Failed to connect to Bloomberg. Cannot proceed with data loading.")
  }
  
  # Define Bloomberg query parameters
  fields <- c("PX_LAST")  # We want closing prices
  
  # Initialize an empty xts object for prices
  all_prices <- NULL
  
  # Keep track of successfully loaded tickers
  loaded_tickers <- c()
  failed_tickers <- c()
  
  cat(sprintf("Loading data from Bloomberg for %d tickers...\n", length(bloomberg_tickers)))
  
  # Use a single batch query for better efficiency
  tryCatch({
    # Load all tickers in a single batch query
    # Add some buffer days to ensure we have enough data
    batch_data <- bdh(
      securities = bloomberg_tickers,
      fields = fields,
      start.date = start_date - 30,  # Buffer days
      end.date = end_date + 5,       # Buffer days
      options = c("periodicitySelection" = "DAILY")
    )
    
    # Process the batch results - Bloomberg returns a list with one element per security
    for (i in 1:length(bloomberg_tickers)) {
      ticker <- tickers[i]
      bloomberg_ticker <- bloomberg_tickers[i]
      
      # Extract data for this ticker
      ticker_data <- batch_data[[bloomberg_ticker]]
      
      # Check if we got valid data
      if (!is.null(ticker_data) && nrow(ticker_data) >= 5) {
        # Extract date and price columns
        dates <- ticker_data$date
        prices <- ticker_data$PX_LAST
        
        # Create xts object
        price_xts <- xts(prices, order.by = as.Date(dates))
        colnames(price_xts) <- ticker
        
        # Merge with existing data
        if (is.null(all_prices)) {
          all_prices <- price_xts
        } else {
          all_prices <- merge(all_prices, price_xts)
        }
        
        loaded_tickers <- c(loaded_tickers, ticker)
        cat(sprintf("  Successfully loaded %s - data range %s to %s\n", 
                    ticker, 
                    format(min(dates), "%Y-%m-%d"),
                    format(max(dates), "%Y-%m-%d")))
      } else {
        failed_tickers <- c(failed_tickers, ticker)
        cat(sprintf("  Failed to load data for %s (insufficient data)\n", ticker))
      }
    }
  }, error = function(e) {
    cat(sprintf("Error in batch Bloomberg data query: %s\n", e$message))
    cat("Falling back to individual ticker queries...\n")
    
    # Fall back to individual queries if batch fails
    for (i in 1:length(bloomberg_tickers)) {
      ticker <- tickers[i]
      bloomberg_ticker <- bloomberg_tickers[i]
      
      tryCatch({
        # Query for a single ticker
        ticker_data <- bdh(
          securities = bloomberg_ticker,
          fields = fields,
          start.date = start_date - 30,
          end.date = end_date + 5,
          options = c("periodicitySelection" = "DAILY")
        )[[1]]  # Extract the first (and only) element from the list
        
        if (!is.null(ticker_data) && nrow(ticker_data) >= 5) {
          # Extract date and price columns
          dates <- ticker_data$date
          prices <- ticker_data$PX_LAST
          
          # Create xts object
          price_xts <- xts(prices, order.by = as.Date(dates))
          colnames(price_xts) <- ticker
          
          # Merge with existing data
          if (is.null(all_prices)) {
            all_prices <- price_xts
          } else {
            all_prices <- merge(all_prices, price_xts)
          }
          
          loaded_tickers <- c(loaded_tickers, ticker)
          cat(sprintf("  Successfully loaded %s - data range %s to %s\n", 
                      ticker, 
                      format(min(dates), "%Y-%m-%d"),
                      format(max(dates), "%Y-%m-%d")))
        } else {
          failed_tickers <- c(failed_tickers, ticker)
          cat(sprintf("  Failed to load data for %s (insufficient data)\n", ticker))
        }
      }, error = function(e) {
        cat(sprintf("  Error loading %s: %s\n", ticker, e$message))
        failed_tickers <- c(failed_tickers, ticker)
      })
    }
  })
  
  # Report loading status
  cat(sprintf("\nLoaded %d of %d tickers. Failed to load %d tickers.\n", 
              length(loaded_tickers), length(tickers), length(failed_tickers)))
  
  if (length(failed_tickers) > 0) {
    cat("Failed tickers:", paste(failed_tickers, collapse=", "), "\n")
  }
  
  # Final check if we have data
  if (is.null(all_prices) || ncol(all_prices) == 0) {
    stop("Failed to load any ticker data. Please check Bloomberg connection and ticker symbols.")
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

# VIX data loader specifically for Bloomberg
get_vix_bloomberg <- function(price_dates, from = min(price_dates) - 30, to = max(price_dates) + 5) {
  cat("Loading VIX data from Bloomberg...\n")
  
  # Ensure dates are proper Date objects
  price_dates <- as.Date(price_dates)
  from <- as.Date(from)
  to <- as.Date(to)
  
  # Create a template with all target dates - this ensures exact dimension match
  vix_aligned <- xts(rep(NA, length(price_dates)), order.by = price_dates)
  colnames(vix_aligned) <- "VIX"
  
  # Try to get VIX data from Bloomberg
  tryCatch({
    # VIX in Bloomberg is "VIX Index"
    vix_data <- bdh(
      securities = "VIX Index",
      fields = "PX_LAST",
      start.date = from - 30,
      end.date = to + 5,
      options = c("periodicitySelection" = "DAILY")
    )[[1]]  # Extract the first element from the list
    
    if (!is.null(vix_data) && nrow(vix_data) > 10) {
      # Create xts object
      vix_xts <- xts(vix_data$PX_LAST, order.by = as.Date(vix_data$date))
      colnames(vix_xts) <- "VIX"
      
      cat(sprintf("Successfully loaded VIX data: %d observations\n", nrow(vix_xts)))
      
      # Find common dates
      common_dates <- intersect(index(vix_xts), price_dates)
      
      if (length(common_dates) > 0) {
        # Assign values for matching dates
        vix_aligned[common_dates] <- vix_xts[common_dates]
        
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
      cat("Failed to load VIX from Bloomberg, using default values\n")
      vix_aligned[] <- 20  # Use default value
    }
  }, error = function(e) {
    cat("Error loading VIX data from Bloomberg:", e$message, "\n")
    cat("Using default values\n")
    vix_aligned[] <- 20  # Use default value
  })
  
  cat(sprintf("Final aligned VIX data: %d rows, NA count: %d\n", 
              nrow(vix_aligned), sum(is.na(vix_aligned))))
  
  return(vix_aligned)
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

# Modified version to use Bloomberg VIX
get_vix_aligned <- function(price_dates, from = min(price_dates) - 30, to = max(price_dates) + 5) {
  # Get VIX from Bloomberg
  return(get_vix_bloomberg(price_dates, from, to))
}

# Enhanced market data creation with expanded indicators - Bloomberg-compatible
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
  
  # Get VIX data aligned with price dates - using Bloomberg
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
  
  # 9. Market breadth indicator - Uses Bloomberg data
  if ("SPY" %in% colnames(prices)) {
    tryCatch({
      cat("Creating market breadth indicator using Bloomberg SPY data...\n")
      
      # Try to get SPY high-low-close data from Bloomberg
      spy_bloomberg_ticker <- "SPY US Equity"
      spy_hlc_data <- bdh(
        securities = spy_bloomberg_ticker,
        fields = c("PX_HIGH", "PX_LOW", "PX_LAST"),
        start.date = min(price_dates) - 30,
        end.date = max(price_dates) + 5
      )[[1]]
      
      if (!is.null(spy_hlc_data) && nrow(spy_hlc_data) > 10) {
        # Create xts object with high, low, close
        spy_data_xts <- xts(
          cbind(spy_hlc_data$PX_HIGH, spy_hlc_data$PX_LOW, spy_hlc_data$PX_LAST),
          order.by = as.Date(spy_hlc_data$date)
        )
        colnames(spy_data_xts) <- c("High", "Low", "Close")
        
        # Calculate high-low range relative to close
        spy_hl_range <- (spy_data_xts[,"High"] - spy_data_xts[,"Low"]) / spy_data_xts[,"Close"]
        
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
  
  # 10. Volatility indicator directly from returns
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

# Enhanced transaction costs calculation for Bloomberg tickers
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
    "DRV" = 8.0,    # Inverse Real Estate (3x)
    
    # Bloomberg-specific money market proxies
    "BIL" = 0.8,    # 1-3 Month T-Bill ETF
    "SGOV" = 0.8    # Short-term treasury ETF
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
    # Clean the ticker (remove Bloomberg suffixes)
    clean_ticker <- gsub(" US Equity$|\\sEquity$|\\sIndex$", "", ticker)
    
    # US Large Cap Equity
    if (grepl("^(SPY|VOO|IVV|DIA|QQQ|SPLG|VTI)", clean_ticker)) {
      return(default_costs[["Equity_US_Large"]])
    }
    # US Small/Mid Cap Equity
    else if (grepl("^(IWM|MDY|IJH|IJS|IJR|VO|VB)", clean_ticker)) {
      return(default_costs[["Equity_US_Small"]])
    }
    # International Developed Equity
    else if (grepl("^(EFA|VEA|IEFA|VGK|EWJ|HEDJ|EWU)", clean_ticker)) {
      return(default_costs[["Equity_International"]])
    }
    # Emerging Markets Equity
    else if (grepl("^(EEM|VWO|IEMG|SCHE|FM|FEM)", clean_ticker)) {
      return(default_costs[["Equity_Emerging"]])
    }
    # Government Bonds
    else if (grepl("^(IEF|TLT|SHY|VGSH|VGIT|VGLT|BIL|SCHO|SCHR|TIP|VTIP)", clean_ticker)) {
      return(default_costs[["Bond_Government"]])
    }
    # Corporate Bonds
    else if (grepl("^(LQD|VCSH|VCIT|VCLT|SPIB|IGIB|AGG|BND)", clean_ticker)) {
      return(default_costs[["Bond_Corporate"]])
    }
    # High Yield Bonds
    else if (grepl("^(HYG|JNK|SJNK|USHY|HYLB|BKLN)", clean_ticker)) {
      return(default_costs[["Bond_HighYield"]])
    }
    # Commodities
    else if (grepl("^(GLD|IAU|SLV|USO|UNG|DBC|PDBC|GSG|BCI)", clean_ticker)) {
      return(default_costs[["Commodity"]])
    }
    # Real Estate
    else if (grepl("^(VNQ|IYR|SCHH|RWR|VNQI|RWX)", clean_ticker)) {
      return(default_costs[["Real_Estate"]])
    }
    # Inverse ETFs
    else if (grepl("^(SH|PSQ|DOG|RWM|EUM|EFZ|TBF|SJB|DGZ|DRV)", clean_ticker) || 
             grepl("(SHORT|BEAR|INV|INVERSE)", clean_ticker, ignore.case = TRUE)) {
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
    
    # Clean Bloomberg suffix for lookup
    clean_ticker <- gsub(" US Equity$|\\sEquity$|\\sIndex$", "", ticker)
    
    # Known ticker - use predefined cost
    if (clean_ticker %in% names(base_costs)) {
      costs[i] <- base_costs[[clean_ticker]]
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

#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - VERSION Z7 BLOOMBERG
# PART 2: REGIME DETECTION AND RISK PARITY OPTIMIZATION
#=============================================================================

#=============================================================================
# REGIME DETECTION AND CLASSIFICATION FUNCTIONS - BLOOMBERG ADAPTED
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
  else if ("SPY US Equity" %in% colnames(market_data)) { # Bloomberg ticker format
    # Calculate momentum using 3-month return
    spy_recent <- tail(market_data[, "SPY US Equity"], lookback_periods$medium + 1)
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
  # Gold as alternative inflation indicator - using Bloomberg tickers
  else if (all(c("GLD US Equity", "IEF US Equity") %in% colnames(market_data))) {
    # Calculate GLD vs bonds ratio over time
    gld_recent <- tail(market_data[, "GLD US Equity"], lookback_periods$medium + 1)
    ief_recent <- tail(market_data[, "IEF US Equity"], lookback_periods$medium + 1)
    
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
  # Volatility regime detection - using VIX data
  if ("VIX Index" %in% colnames(current_data)) {  # Bloomberg VIX ticker
    vix_value <- as.numeric(current_data[, "VIX Index"])
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
# RISK PARITY OPTIMIZATION FUNCTIONS - BLOOMBERG ADAPTED
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

# Risk parity optimization with proper constraint handling
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

# Custom risk budgeting based on regimes - adapted for Bloomberg ticker format
assign_risk_budgets <- function(assets, regime, factor_tilts = NULL) {
  # Set default risk budget (equal risk)
  n_assets <- length(assets)
  risk_budget <- rep(1, n_assets)
  names(risk_budget) <- assets
  
  # Convert Bloomberg tickers to base symbols for mapping
  base_symbols <- gsub(" US Equity$| Index$", "", assets)
  names(base_symbols) <- assets
  
  # Define regime-specific risk budgets (using base symbols)
  regime_tilts <- list(
    growth = list(
      "SPY" = 1.3, "QQQ" = 1.4, "IWM" = 1.3, "EFA" = 1.2, "EEM" = 1.3,
      "IEF" = 0.6, "TLT" = 0.5, "LQD" = 0.8, "HYG" = 1.0,
      "GLD" = 0.8, "DBC" = 1.1, "VNQ" = 1.2,
      # Inverse ETFs
      "SH" = 0.2, "PSQ" = 0.2, "RWM" = 0.2, "EUM" = 0.2, "EFZ" = 0.2,
      "TBF" = 0.3, "SJB" = 0.3, "DGZ" = 0.3, "DRV" = 0.2
    ),
    
    reflation = list(
      "SPY" = 1.2, "QQQ" = 1.3, "IWM" = 1.2, "EFA" = 1.1, "EEM" = 1.2,
      "IEF" = 0.7, "TLT" = 0.6, "LQD" = 0.9, "HYG" = 1.1,
      "GLD" = 1.1, "DBC" = 1.3, "VNQ" = 1.1,
      # Inverse ETFs
      "SH" = 0.3, "PSQ" = 0.3, "RWM" = 0.3, "EUM" = 0.3, "EFZ" = 0.3,
      "TBF" = 0.4, "SJB" = 0.4, "DGZ" = 0.5, "DRV" = 0.3
    ),
    
    neutral = list(
      "SPY" = 1.0, "QQQ" = 1.0, "IWM" = 1.0, "EFA" = 1.0, "EEM" = 1.0,
      "IEF" = 1.0, "TLT" = 1.0, "LQD" = 1.0, "HYG" = 1.0,
      "GLD" = 1.0, "DBC" = 1.0, "VNQ" = 1.0,
      # Inverse ETFs
      "SH" = 0.5, "PSQ" = 0.5, "RWM" = 0.5, "EUM" = 0.5, "EFZ" = 0.5,
      "TBF" = 0.5, "SJB" = 0.5, "DGZ" = 0.5, "DRV" = 0.5
    ),
    
    stagflation = list(
      "SPY" = 0.7, "QQQ" = 0.6, "IWM" = 0.7, "EFA" = 0.7, "EEM" = 0.7,
      "IEF" = 0.8, "TLT" = 0.8, "LQD" = 0.7, "HYG" = 0.6,
      "GLD" = 1.5, "DBC" = 1.4, "VNQ" = 0.8,
      # Inverse ETFs
      "SH" = 0.9, "PSQ" = 0.9, "RWM" = 0.8, "EUM" = 0.8, "EFZ" = 0.8,
      "TBF" = 0.7, "SJB" = 0.7, "DGZ" = 0.3, "DRV" = 0.8
    ),
    
    deflation = list(
      "SPY" = 0.6, "QQQ" = 0.5, "IWM" = 0.5, "EFA" = 0.5, "EEM" = 0.4,
      "IEF" = 1.4, "TLT" = 1.6, "LQD" = 1.0, "HYG" = 0.5,
      "GLD" = 1.1, "DBC" = 0.6, "VNQ" = 0.5,
      # Inverse ETFs
      "SH" = 1.2, "PSQ" = 1.2, "RWM" = 1.2, "EUM" = 1.1, "EFZ" = 1.1,
      "TBF" = 0.4, "SJB" = 0.8, "DGZ" = 0.5, "DRV" = 1.0
    ),
    
    risk_off = list(
      "SPY" = 0.4, "QQQ" = 0.3, "IWM" = 0.3, "EFA" = 0.3, "EEM" = 0.2,
      "IEF" = 1.6, "TLT" = 1.8, "LQD" = 0.8, "HYG" = 0.3,
      "GLD" = 1.3, "DBC" = 0.5, "VNQ" = 0.3,
      # Inverse ETFs
      "SH" = 1.5, "PSQ" = 1.5, "RWM" = 1.4, "EUM" = 1.4, "EFZ" = 1.4,
      "TBF" = 0.3, "SJB" = 1.0, "DGZ" = 0.4, "DRV" = 1.3
    )
  )
  
  # Apply regime-specific risk budget
  if (!is.null(regime) && regime %in% names(regime_tilts)) {
    # Get the appropriate risk budget for this regime
    regime_budget <- regime_tilts[[regime]]
    
    # Apply to each asset if it exists in our list (matching base symbols)
    for (asset in assets) {
      base_symbol <- base_symbols[asset]
      if (base_symbol %in% names(regime_budget)) {
        risk_budget[asset] <- regime_budget[[base_symbol]]
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

# Map Bloomberg ticker to base symbol for internal functions
get_base_symbol <- function(bloomberg_ticker) {
  # Remove the " US Equity" or " Index" suffix from Bloomberg tickers
  base_symbol <- gsub(" US Equity$| Index$", "", bloomberg_ticker)
  return(base_symbol)
}

# Map base symbol to Bloomberg ticker format
get_bloomberg_ticker <- function(base_symbol, type = "equity") {
  if (type == "equity") {
    return(paste0(base_symbol, " US Equity"))
  } else if (type == "index") {
    return(paste0(base_symbol, " Index"))
  } else {
    return(base_symbol) # Return as is if type not recognized
  }
}

# Create mapping between Bloomberg tickers and base symbols
create_ticker_mapping <- function(tickers) {
  base_symbols <- gsub(" US Equity$| Index$", "", tickers)
  mapping <- data.frame(
    bloomberg_ticker = tickers,
    base_symbol = base_symbols,
    stringsAsFactors = FALSE
  )
  return(mapping)
}

#=============================================================================
# ENHANCED CASH ALLOCATION & REGIME CHANGE DETECTION
#=============================================================================

# Enhanced cash allocation with regime awareness
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

# Function to check for recent regime changes
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
    
    # Ensure proper Date format
    latest_date <- as.Date(format(index(tail(prices, 1)), "%Y-%m-%d"))
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