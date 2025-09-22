#=============================================================================
# COMPLETE OPTIMIZED RISK PARITY TRADING SYSTEM - PURE Z-SCORE APPROACH
# - Target volatility: 7.5%
# - Target maximum drawdown: 7.5% 
# - Pure Z-score based regime detection (6 regimes)
# - Real market data only - no synthetic data
# - GARCH applied to all methods
# - Fixed optimization for stability
#=============================================================================

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, quantmod, xts, PerformanceAnalytics, 
  rugarch, robustbase, nloptr, TTR, fGarch, 
  tseries, reshape2, corpcor, ggplot2, RColorBrewer,
  zoo  # Added for rollapply functions
)

# Log system information
cat(sprintf("Current Date and Time (UTC - YYYY-MM-DD HH:MM:SS formatted): %s\n", 
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat(sprintf("Current User's Login: %s\n", Sys.info()["user"]))

#=============================================================================
# DATA HANDLING - REAL DATA ONLY
#=============================================================================

load_market_data <- function(tickers, start_date, end_date = Sys.Date(), source = "yahoo") {
  # Initialize an empty xts object for prices
  all_prices <- NULL
  
  cat(sprintf("Loading data for %d tickers from %s to %s...\n", 
              length(tickers), start_date, as.character(end_date)))
  
  # Try to get data from Yahoo Finance
  success <- TRUE
  for (ticker in tickers) {
    tryCatch({
      # Fetch data
      price_data <- getSymbols(ticker, from = start_date, to = end_date, 
                               src = source, auto.assign = FALSE)
      # Extract adjusted closing prices
      close_data <- price_data[, 6]
      colnames(close_data) <- ticker
      
      # Merge with existing data
      if (is.null(all_prices)) {
        all_prices <- close_data
      } else {
        all_prices <- merge(all_prices, close_data)
      }
      
      cat(sprintf("  Successfully loaded %s - data range %s to %s\n", 
                  ticker, 
                  format(index(close_data)[1], "%Y-%m-%d"),
                  format(index(close_data)[nrow(close_data)], "%Y-%m-%d")))
    }, error = function(e) {
      cat(sprintf("  Error loading %s: %s\n", ticker, e$message))
      success <- FALSE
    })
  }
  
  # If any ticker failed to load, throw an error
  if (!success || is.null(all_prices) || ncol(all_prices) < length(tickers)) {
    stop("Some tickers failed to load. Please check ticker symbols and date range.")
  }
  
  # Fill missing values using last observation carried forward
  all_prices <- na.locf(all_prices, na.rm = FALSE)
  
  cat(sprintf("Successfully loaded data for %d tickers\n", ncol(all_prices)))
  return(all_prices)
}

# Get market-based economic indicators from ETF price relationships
get_economic_indicators <- function(start_date, end_date = Sys.Date(), price_dates = NULL) {
  cat("Creating market-based economic indicators from ETF price relationships...\n")
  
  # Use price_dates if provided (guarantees alignment)
  if (!is.null(price_dates)) {
    cat("Using exact price dates for perfect alignment\n")
    target_dates <- as.Date(price_dates)
  } else {
    # Generate business days date range
    target_dates <- seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day")
    target_dates <- target_dates[weekdays(target_dates) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
  }
  
  # Load VIX from Yahoo Finance
  cat("Loading VIX data...\n")
  vix_data <- tryCatch({
    getSymbols("^VIX", from = as.Date(start_date) - 30, to = as.Date(end_date) + 5,
               src = "yahoo", auto.assign = FALSE)[, 6]
  }, error = function(e) {
    cat("  Error loading VIX:", e$message, "\n")
    NULL
  })
  
  # Load key ETFs for creating economic indicators
  etf_tickers <- c("SPY", "IEF", "GLD", "LQD", "IWM", "XLI", "XLU", "TIP")
  etf_prices <- NULL
  
  cat("Loading ETFs for economic indicators...\n")
  for (ticker in etf_tickers) {
    tryCatch({
      price_data <- getSymbols(ticker, from = as.Date(start_date) - 60, 
                               to = as.Date(end_date) + 5, 
                               src = "yahoo", auto.assign = FALSE)[, 6]
      
      # Merge with existing data
      if (is.null(etf_prices)) {
        etf_prices <- price_data
      } else {
        etf_prices <- merge(etf_prices, price_data)
      }
      
      cat(sprintf("  Successfully loaded %s\n", ticker))
    }, error = function(e) {
      cat(sprintf("  Error loading %s: %s\n", ticker, e$message))
    })
  }
  
  # Initialize empty indicators dataframe
  indicators <- NULL
  
  # 1. Add VIX as volatility indicator
  if (!is.null(vix_data)) {
    colnames(vix_data) <- "VIX"
    indicators <- vix_data
    cat("Added VIX as volatility indicator\n")
  }
  
  # Make sure we have ETF prices to work with
  if (!is.null(etf_prices) && ncol(etf_prices) >= 2) {
    # Get returns for all ETFs
    etf_returns <- ROC(etf_prices, type = "discrete")
    etf_returns <- na.omit(etf_returns)
    
    # 2. Create inflation proxy - TIP/IEF or GLD/SPY ratio
    if (all(c("TIP", "IEF") %in% colnames(etf_prices))) {
      # TIPS/Treasury ratio (better proxy for inflation expectations)
      inflation_ratio <- etf_prices[, "TIP"] / etf_prices[, "IEF"]
      colnames(inflation_ratio) <- "CPI_YOY"
      indicators <- merge(indicators, inflation_ratio)
      cat("Added TIP/IEF ratio as inflation indicator (CPI_YOY)\n")
    } else if (all(c("GLD", "SPY") %in% colnames(etf_prices))) {
      # Gold/S&P ratio as inflation proxy (higher = more inflation concern)
      inflation_ratio <- etf_prices[, "GLD"] / etf_prices[, "SPY"] 
      colnames(inflation_ratio) <- "CPI_YOY"
      indicators <- merge(indicators, inflation_ratio)
      cat("Added GLD/SPY ratio as inflation indicator (CPI_YOY)\n")
    }
    
    # 3. Create growth proxy - XLI/XLU or IWM/IEF ratio
    if (all(c("XLI", "XLU") %in% colnames(etf_prices))) {
      # Industrials/Utilities ratio (growth proxy)
      growth_ratio <- etf_prices[, "XLI"] / etf_prices[, "XLU"]
      colnames(growth_ratio) <- "PMI"
      indicators <- merge(indicators, growth_ratio)
      cat("Added XLI/XLU ratio as growth indicator (PMI)\n")
    } else if (all(c("IWM", "IEF") %in% colnames(etf_prices))) {
      # Small caps vs bonds (growth proxy)
      growth_ratio <- etf_prices[, "IWM"] / etf_prices[, "IEF"]
      colnames(growth_ratio) <- "PMI"
      indicators <- merge(indicators, growth_ratio)
      cat("Added IWM/IEF ratio as growth indicator (PMI)\n")
    }
    
    # 4. Create credit spread proxy - LQD/IEF 
    if (all(c("LQD", "IEF") %in% colnames(etf_prices))) {
      # Credit vs Treasury spread
      credit_ratio <- etf_prices[, "LQD"] / etf_prices[, "IEF"]
      colnames(credit_ratio) <- "CREDIT_SPREAD"
      indicators <- merge(indicators, credit_ratio)
      cat("Added LQD/IEF ratio as credit spread indicator\n")
    }
    
    # 5. Add realized volatility calculation (alternative to VIX)
    if ("SPY" %in% colnames(etf_prices)) {
      spy_returns <- ROC(etf_prices[, "SPY"], type = "discrete")
      
      # Calculate 21-day rolling volatility
      realized_vol <- tryCatch({
        vol <- rollapply(spy_returns, 21, function(x) sd(x, na.rm = TRUE), align = "right") * sqrt(252) * 100
        vol_xts <- xts(vol, order.by = index(vol))
        colnames(vol_xts) <- "REALIZED_VOL"
        vol_xts
      }, error = function(e) {
        cat("  Error calculating realized volatility:", e$message, "\n")
        NULL
      })
      
      if (!is.null(realized_vol)) {
        indicators <- merge(indicators, realized_vol)
        cat("Added SPY realized volatility as volatility indicator\n")
      }
    }
    
    # 6. Calculate bond-equity correlation - important for regime detection
    if (all(c("IEF", "SPY") %in% colnames(etf_returns))) {
      # Calculate 63-day rolling correlation
      combined_returns <- merge(etf_returns[, "IEF"], etf_returns[, "SPY"])
      colnames(combined_returns) <- c("bond", "equity")
      
      corr_ts <- tryCatch({
        roll_cor <- rollapply(combined_returns, 63, function(x) {
          cor(x[,"bond"], x[,"equity"], use = "pairwise.complete.obs")
        }, by.column = FALSE, align = "right")
        
        corr_xts <- xts(roll_cor, order.by = index(roll_cor))
        colnames(corr_xts) <- "BOND_EQUITY_CORR"
        corr_xts
      }, error = function(e) {
        cat("  Error calculating bond-equity correlation:", e$message, "\n")
        NULL
      })
      
      if (!is.null(corr_ts)) {
        indicators <- merge(indicators, corr_ts)
        cat("Added bond-equity correlation indicator\n")
      }
    }
  }
  
  # If we have no indicators at all, issue a warning
  if (is.null(indicators)) {
    cat("WARNING: Couldn't create any economic indicators! Using simplified approach.\n")
    
    # Create minimal indicators from prices
    if (!is.null(etf_prices) && "SPY" %in% colnames(etf_prices)) {
      # Use SPY returns for minimal indicators
      spy_returns <- ROC(etf_prices[, "SPY"], type = "discrete")
      
      # Calculate volatility
      vol <- rollapply(spy_returns, 21, function(x) sd(x, na.rm = TRUE), 
                       align = "right") * sqrt(252) * 100
      indicators <- xts(vol, order.by = index(vol))
      colnames(indicators) <- "VIX"
      
      # Create dummy PMI (60-day returns)
      growth_indicator <- rollapply(spy_returns, 60, function(x) sum(x), align = "right")
      colnames(growth_indicator) <- "PMI"
      indicators <- merge(indicators, growth_indicator)
    } else {
      # Last resort - create empty indicators
      indicators <- xts(matrix(0, nrow = length(target_dates), ncol = 2),
                        order.by = target_dates)
      colnames(indicators) <- c("VIX", "PMI")
    }
  }
  
  # Align to target dates
  aligned_indicators <- xts(matrix(NA, nrow = length(target_dates), ncol = ncol(indicators)),
                            order.by = target_dates)
  colnames(aligned_indicators) <- colnames(indicators)
  
  # Merge indicators with alignment dates and fill values
  combined_data <- merge(indicators, aligned_indicators)
  
  # Fill forward and backward
  for (col in colnames(combined_data)) {
    combined_data[, col] <- na.locf(combined_data[, col], na.rm = FALSE)
    combined_data[, col] <- na.locf(combined_data[, col], na.rm = FALSE, fromLast = TRUE)
  }
  
  # Keep only target dates
  final_indicators <- combined_data[as.character(target_dates)]
  
  # Check for any remaining NAs
  na_count <- sum(is.na(final_indicators))
  if (na_count > 0) {
    cat(sprintf("WARNING: %d NA values remain in economic indicators. Filling with column means.\n", na_count))
    
    for (col in colnames(final_indicators)) {
      col_mean <- mean(final_indicators[, col], na.rm = TRUE)
      na_idx <- is.na(final_indicators[, col])
      if (any(na_idx)) {
        final_indicators[na_idx, col] <- col_mean
        cat(sprintf("  Filled %d NA values in %s with mean: %.2f\n", 
                    sum(na_idx), col, col_mean))
      }
    }
  }
  
  cat(sprintf("Final economic indicators: %d rows × %d columns\n", 
              nrow(final_indicators), ncol(final_indicators)))
  cat("Indicator columns:", paste(colnames(final_indicators), collapse=", "), "\n")
  
  return(final_indicators)
}

# Create perfectly aligned market data
create_market_data <- function(prices) {
  cat("Creating market data with perfect date alignment...\n")
  
  # Use the price dates to ensure perfect alignment
  price_dates <- as.Date(index(prices))
  
  # Generate economic data for these exact dates
  econ_indicators <- get_economic_indicators(
    start_date = min(price_dates),
    end_date = max(price_dates),
    price_dates = price_dates
  )
  
  # Merge price and economic data
  market_data <- merge(prices, econ_indicators)
  
  cat(sprintf("Created perfectly aligned market data: %d rows × %d columns\n", 
              nrow(market_data), ncol(market_data)))
  
  # Verify alignment
  if (nrow(market_data) != nrow(prices)) {
    cat("WARNING: Date alignment issue detected!\n")
    cat(sprintf("Prices: %d rows, Market data: %d rows\n", nrow(prices), nrow(market_data)))
    
    # Force alignment
    cat("Forcing alignment using price dates...\n")
    market_data <- prices
    for (col in colnames(econ_indicators)) {
      market_data[, col] <- econ_indicators[, col]
    }
    cat(sprintf("After forcing alignment: %d rows\n", nrow(market_data)))
  } else {
    cat("Perfect date alignment confirmed!\n")
  }
  
  return(market_data)
}

#=============================================================================
# VOLATILITY FORECASTING
#=============================================================================

forecast_garch_volatility <- function(returns, forecast_horizon = 22) {
  # Initialize results dataframe
  forecasted_vols <- data.frame(
    asset = colnames(returns),
    forecasted_vol = rep(0, ncol(returns)),
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
        next
      }
      
      # Fit GARCH(1,1) model
      garch_spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)
      )
      garch_fit <- ugarchfit(garch_spec, return_series, solver = "hybrid")
      
      # Forecast volatility
      garch_forecast <- ugarchforecast(garch_fit, n.ahead = forecast_horizon)
      forecast_sigma <- as.numeric(sigma(garch_forecast)[forecast_horizon])
      
      # Annualize the volatility forecast
      forecasted_vols[col, "forecasted_vol"] <- forecast_sigma * sqrt(252)
    }, error = function(e) {
      # Use historical volatility if GARCH fails
      cat(sprintf("GARCH failed for %s: %s, using historical volatility\n", col, e$message))
      forecasted_vols[col, "forecasted_vol"] <- sd(returns[, col], na.rm = TRUE) * sqrt(252)
    })
  }
  
  return(forecasted_vols)
}

#=============================================================================
# RISK ESTIMATION 
#=============================================================================

# EWMA covariance estimation with numerical stability improvements
estimate_ewma_covariance <- function(returns, lambda = 0.94) {
  # Remove NAs and ensure we have enough data
  returns <- na.omit(returns)
  n_obs <- nrow(returns)
  n_assets <- ncol(returns)
  
  # Print diagnostics
  cat(sprintf("EWMA: Processing %d observations for %d assets with lambda=%.2f\n", 
              n_obs, n_assets, lambda))
  
  # Need at least 60 observations for a stable estimate
  if (n_obs < 60) {
    warning("EWMA needs at least 60 observations - using sample covariance")
    return(cov(returns))
  }
  
  # Calculate historical sample variances and correlations
  hist_vars <- apply(returns, 2, var, na.rm = TRUE)
  sample_cov <- cov(returns)
  
  # Set minimum variance floor
  min_var <- max(min(hist_vars), 1e-8)
  
  # Create initial covariance matrix 
  cov_matrix <- sample_cov
  
  # Check if initialization is valid
  if (any(is.na(cov_matrix)) || any(!is.finite(cov_matrix))) {
    cat("EWMA initialization failed, using robust alternative\n")
    
    # Create diagonal matrix with sample variances
    cov_matrix <- diag(pmax(hist_vars, min_var))
    colnames(cov_matrix) <- colnames(returns)
    rownames(cov_matrix) <- colnames(returns)
    
    # Add modest correlations (0.3 between all assets)
    for (i in 1:n_assets) {
      for (j in 1:n_assets) {
        if (i != j) {
          cov_matrix[i,j] <- 0.3 * sqrt(cov_matrix[i,i] * cov_matrix[j,j])
        }
      }
    }
  }
  
  # Using direct EWMA formula on daily returns
  # Calculate the weighted sum directly for stability
  weights <- lambda^(0:(n_obs-1))  # Most recent observation gets highest weight
  weights <- weights / sum(weights)  # Normalize weights to sum to 1
  
  # For each pair of assets, calculate weighted covariance
  for (i in 1:n_assets) {
    for (j in i:n_assets) {  # Only upper triangle needed due to symmetry
      # Get return series for the two assets
      asset_i_returns <- as.numeric(returns[, i])
      asset_j_returns <- as.numeric(returns[, j])
      
      # Calculate products of returns
      products <- asset_i_returns * asset_j_returns
      
      # Apply EWMA weights in reverse (most recent gets highest weight)
      weighted_sum <- sum(rev(weights) * products, na.rm = TRUE)
      
      # Assign to covariance matrix (ensure symmetry)
      cov_matrix[i, j] <- weighted_sum
      cov_matrix[j, i] <- weighted_sum
    }
  }
  
  # Ensure the matrix is positive definite
  eigen_vals <- eigen(cov_matrix, only.values = TRUE)$values
  if (min(eigen_vals) <= 0) {
    cat("EWMA matrix not positive definite, applying regularization\n")
    
    # Apply shrinkage toward well-conditioned target
    target <- diag(diag(cov_matrix))
    shrink_factor <- 0.1  # 10% shrinkage
    cov_matrix <- (1 - shrink_factor) * cov_matrix + shrink_factor * target
    
    # Verify it worked
    eigen_vals2 <- eigen(cov_matrix, only.values = TRUE)$values
    if (min(eigen_vals2) <= 0) {
      cat("Additional regularization needed\n")
      diag(cov_matrix) <- diag(cov_matrix) + 1e-5 * mean(diag(cov_matrix))
    }
  }
  
  # Print diagnostics about the resulting matrix
  cat(sprintf("EWMA covariance matrix: min eigenvalue = %.6g, max eigenvalue = %.6g\n", 
              min(eigen(cov_matrix)$values), max(eigen(cov_matrix)$values)))
  
  return(cov_matrix)
}

# Ledoit-Wolf shrinkage
estimate_robust_covariance <- function(returns, method = "ledoit-wolf") {
  # Make sure we have sufficient non-NA data
  returns <- na.omit(returns)
  
  if (nrow(returns) < 10) {
    warning("Not enough data for covariance estimation, using sample covariance")
    return(cov(returns))
  }
  
  if (method == "ledoit-wolf") {
    # Extract matrix from xts object
    returns_matrix <- as.matrix(returns)
    
    # Apply Ledoit-Wolf shrinkage
    cov_matrix <- corpcor::cov.shrink(returns_matrix, verbose = FALSE)
    
    # Return as matrix with column and row names preserved
    cov_matrix <- as.matrix(cov_matrix)
    colnames(cov_matrix) <- colnames(returns)
    rownames(cov_matrix) <- colnames(returns)
    
    # Print eigenvalues to check matrix quality
    eigen_values <- eigen(cov_matrix)$values
    cat(sprintf("Ledoit-Wolf Covariance - Min eigenvalue: %.6f, Max eigenvalue: %.6f\n",
                min(eigen_values), max(eigen_values)))
    
  } else {
    # Default to sample covariance
    cov_matrix <- cov(returns)
  }
  
  return(cov_matrix)
}

#=============================================================================
# VISUALIZATION FUNCTIONS
#=============================================================================

# Function to plot correlation/covariance matrix as heatmap
plot_covariance_matrix <- function(cov_matrix, title = "Asset Correlation Matrix") {
  # Convert to correlation matrix if it's a covariance matrix
  if (mean(diag(cov_matrix)) > 0.1) {  # Heuristic to detect covariance vs correlation
    # Convert to correlation matrix
    diag_sqrt <- sqrt(diag(cov_matrix))
    corr_matrix <- cov_matrix / (diag_sqrt %*% t(diag_sqrt))
  } else {
    corr_matrix <- cov_matrix  # Already a correlation matrix
  }
  
  # Ensure the matrix is symmetric and has proper names
  colnames(corr_matrix) <- rownames(corr_matrix)
  
  # Melt the matrix for ggplot
  corr_df <- reshape2::melt(corr_matrix)
  names(corr_df) <- c("Asset1", "Asset2", "Correlation")
  
  # Create color palette
  col_palette <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                                    "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                                    "#4393C3", "#2166AC", "#053061"))(100)
  
  # Create the heatmap
  p <- ggplot(corr_df, aes(x = Asset1, y = Asset2, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradientn(colors = col_palette, limits = c(-1, 1)) +
    geom_text(aes(label = sprintf("%.2f", Correlation)), 
              color = ifelse(abs(corr_df$Correlation) > 0.5, "white", "black"),
              size = 3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = title, x = "", y = "")
  
  return(p)
}

# Function to plot cumulative returns of different strategies
plot_strategy_comparison <- function(results_list, 
                                     title = "Strategy Comparison",
                                     log_scale = FALSE) {
  # Create data frame for plotting
  plot_data <- data.frame(Date = index(results_list[[1]]$cumulative_returns))
  
  # Add returns for each method
  for (method_name in names(results_list)) {
    method_returns <- results_list[[method_name]]$cumulative_returns
    plot_data[[method_name]] <- as.numeric(method_returns)
  }
  
  # Convert to long format
  plot_data_long <- reshape2::melt(plot_data, id.vars = "Date", 
                                   variable.name = "Method", 
                                   value.name = "Cumulative_Return")
  
  # Create the plot
  p <- ggplot(plot_data_long, aes(x = Date, y = Cumulative_Return, 
                                  color = Method, group = Method)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = title,
         x = "Date",
         y = "Cumulative Return (1 = initial investment)",
         color = "Method") +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
  
  # Add log scale if requested
  if (log_scale) {
    p <- p + scale_y_log10()
  }
  
  return(p)
}

# Function to plot regime distribution over time
plot_regime_distribution <- function(regime_history, title = "Market Regimes Over Time") {
  # Convert to data frame for ggplot
  regime_df <- data.frame(
    Date = index(regime_history),
    Regime = as.character(regime_history)
  )
  
  # Define colors for different regimes
  regime_colors <- c(
    "growth" = "#4CAF50",         # Green
    "reflation" = "#FF9800",      # Orange
    "deflation" = "#2196F3",      # Blue
    "stagflation" = "#F44336",    # Red
    "risk_off" = "#9C27B0",       # Purple
    "inflation_shock" = "#E91E63" # Pink
  )
  
  # Create the plot
  p <- ggplot(regime_df, aes(x = Date, y = 1, fill = Regime)) +
    geom_tile() +
    scale_fill_manual(values = regime_colors) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(title = title, x = "Date", fill = "Regime")
  
  return(p)
}

#=============================================================================
# PURE Z-SCORE BASED REGIME DETECTION - COMPLETELY FIXED
#=============================================================================

# FIXED: Enhanced Z-score detection with improved diagnostic output
detect_market_regime <- function(data, lookback = 252) {
  # Log that we're using Z-score detection, not HMM
  cat("\n---- USING PURE Z-SCORE REGIME DETECTION (NO HMM) ----\n")
  
  # Get latest values
  latest <- tail(data, 1)
  
  # Check available indicators
  cat("Available indicators: ", paste(colnames(data), collapse=", "), "\n")
  
  # Make sure we have data
  if (is.null(data) || nrow(data) < 60) {
    cat("Warning: Insufficient data for Z-score regime detection. Using default growth regime.\n")
    return(list(
      regime = "growth", 
      metrics = list(
        vol_zscore = 0, 
        pmi_zscore = 0,
        cpi_zscore = 0,
        bond_equity_zscore = 0,
        lookback_window = lookback
      )
    ))
  }
  
  # Default indicators if columns not found
  vol_zscore <- 0
  pmi_zscore <- 0
  cpi_zscore <- 0
  bond_equity_zscore <- 0
  
  # Use available history for Z-score calculation
  history_length <- min(nrow(data) - 1, lookback)
  if (history_length > 0) {
    history <- tail(data, history_length + 1)[-nrow(data),]
    
    # ------ Volatility (VIX) ------
    if ("VIX" %in% colnames(data)) {
      vix_history <- as.numeric(history[, "VIX"])
      vix_mean <- mean(vix_history, na.rm = TRUE)
      vix_sd <- sd(vix_history, na.rm = TRUE)
      vix_value <- as.numeric(latest[, "VIX"])
      if (!is.na(vix_mean) && !is.na(vix_sd) && !is.na(vix_value) && vix_sd > 0) {
        vol_zscore <- (vix_value - vix_mean) / vix_sd
        cat(sprintf("VIX Z-score: %.2f (Value: %.2f, Mean: %.2f, SD: %.2f)\n", 
                    vol_zscore, vix_value, vix_mean, vix_sd))
      }
    } else if ("REALIZED_VOL" %in% colnames(data)) {
      # Fallback to realized volatility
      vol_history <- as.numeric(history[, "REALIZED_VOL"])
      vol_mean <- mean(vol_history, na.rm = TRUE)
      vol_sd <- sd(vol_history, na.rm = TRUE)
      vol_value <- as.numeric(latest[, "REALIZED_VOL"])
      if (!is.na(vol_mean) && !is.na(vol_sd) && !is.na(vol_value) && vol_sd > 0) {
        vol_zscore <- (vol_value - vol_mean) / vol_sd
        cat(sprintf("Realized Vol Z-score: %.2f (Value: %.2f, Mean: %.2f, SD: %.2f)\n", 
                    vol_zscore, vol_value, vol_mean, vol_sd))
      }
    } else if ("SPY" %in% colnames(data)) {
      # Last resort: Calculate from SPY returns
      try({
        spy_prices <- data[, "SPY"]
        spy_returns <- ROC(spy_prices, type = "discrete")
        spy_returns <- na.omit(spy_returns)
        
        # Only proceed if we have sufficient returns
        if (length(spy_returns) >= 21) {
          # Get recent returns (last 21 days)
          recent_returns <- tail(spy_returns, 21)
          recent_vol <- sd(recent_returns) * sqrt(252)
          
          # Get historical returns (prior to recent)
          hist_returns <- head(spy_returns, -21)
          if (length(hist_returns) >= 60) {
            # Calculate rolling 21-day vols for historical returns
            roll_vols <- rollapply(hist_returns, 21, function(x) sd(x) * sqrt(252), 
                                   align = "right")
            
            # Calculate Z-score of recent vol vs historical vols
            vol_mean <- mean(roll_vols)
            vol_sd <- sd(roll_vols)
            vol_zscore <- (recent_vol - vol_mean) / vol_sd
            
            cat(sprintf("SPY Volatility Z-score: %.2f (Recent Vol: %.2f%%, Mean: %.2f%%, SD: %.2f%%)\n", 
                        vol_zscore, recent_vol*100, vol_mean*100, vol_sd*100))
          }
        }
      }, silent = TRUE)
    }
    
    # ------ Growth (PMI) ------
    if ("PMI" %in% colnames(data)) {
      pmi_history <- as.numeric(history[, "PMI"])
      pmi_mean <- mean(pmi_history, na.rm = TRUE)
      pmi_sd <- sd(pmi_history, na.rm = TRUE)
      pmi_value <- as.numeric(latest[, "PMI"])
      if (!is.na(pmi_mean) && !is.na(pmi_sd) && !is.na(pmi_value) && pmi_sd > 0) {
        pmi_zscore <- (pmi_value - pmi_mean) / pmi_sd
        cat(sprintf("PMI Z-score: %.2f (Value: %.2f, Mean: %.2f, SD: %.2f)\n", 
                    pmi_zscore, pmi_value, pmi_mean, pmi_sd))
      }
    } else if (all(c("IWM", "IEF") %in% colnames(data))) {
      # Try to create growth indicator from IWM/IEF ratio
      try({
        iwm_ief_ratio <- data[, "IWM"] / data[, "IEF"]
        ratio_history <- as.numeric(tail(iwm_ief_ratio, lookback)[-lookback])
        ratio_mean <- mean(ratio_history, na.rm = TRUE)
        ratio_sd <- sd(ratio_history, na.rm = TRUE)
        ratio_value <- as.numeric(tail(iwm_ief_ratio, 1))
        
        if (!is.na(ratio_mean) && !is.na(ratio_sd) && !is.na(ratio_value) && ratio_sd > 0) {
          pmi_zscore <- (ratio_value - ratio_mean) / ratio_sd
          cat(sprintf("Growth Proxy Z-score (IWM/IEF): %.2f\n", pmi_zscore))
        }
      }, silent = TRUE)
    }
    
    # ------ Inflation (CPI) ------
    if ("CPI_YOY" %in% colnames(data)) {
      cpi_history <- as.numeric(history[, "CPI_YOY"])
      cpi_mean <- mean(cpi_history, na.rm = TRUE)
      cpi_sd <- sd(cpi_history, na.rm = TRUE)
      cpi_value <- as.numeric(latest[, "CPI_YOY"])
      if (!is.na(cpi_mean) && !is.na(cpi_sd) && !is.na(cpi_value) && cpi_sd > 0) {
        cpi_zscore <- (cpi_value - cpi_mean) / cpi_sd
        cat(sprintf("CPI Z-score: %.2f (Value: %.2f, Mean: %.2f, SD: %.2f)\n", 
                    cpi_zscore, cpi_value, cpi_mean, cpi_sd))
      }
    } else if (all(c("GLD", "SPY") %in% colnames(data))) {
      # Try to create inflation proxy from GLD/SPY ratio
      try({
        gld_spy_ratio <- data[, "GLD"] / data[, "SPY"]
        ratio_history <- as.numeric(tail(gld_spy_ratio, lookback)[-lookback])
        ratio_mean <- mean(ratio_history, na.rm = TRUE)
        ratio_sd <- sd(ratio_history, na.rm = TRUE)
        ratio_value <- as.numeric(tail(gld_spy_ratio, 1))
        
        if (!is.na(ratio_mean) && !is.na(ratio_sd) && !is.na(ratio_value) && ratio_sd > 0) {
          cpi_zscore <- (ratio_value - ratio_mean) / ratio_sd
          cat(sprintf("Inflation Proxy Z-score (GLD/SPY): %.2f\n", cpi_zscore))
        }
      }, silent = TRUE)
    }
    
    # ------ Bond-equity correlation ------
    if ("BOND_EQUITY_CORR" %in% colnames(data)) {
      corr_history <- as.numeric(history[, "BOND_EQUITY_CORR"])
      corr_mean <- mean(corr_history, na.rm = TRUE)
      corr_sd <- sd(corr_history, na.rm = TRUE)
      corr_value <- as.numeric(latest[, "BOND_EQUITY_CORR"])
      if (!is.na(corr_mean) && !is.na(corr_sd) && !is.na(corr_value) && corr_sd > 0) {
        bond_equity_zscore <- (corr_value - corr_mean) / corr_sd
        cat(sprintf("Bond-Equity Correlation Z-score: %.2f (Value: %.2f, Mean: %.2f, SD: %.2f)\n", 
                    bond_equity_zscore, corr_value, corr_mean, corr_sd))
      }
    } else if (all(c("IEF", "SPY") %in% colnames(data))) {
      # Calculate bond-equity correlation if not directly available
      try({
        # Get returns for IEF and SPY
        ief_returns <- ROC(data[, "IEF"], type = "discrete")
        spy_returns <- ROC(data[, "SPY"], type = "discrete")
        
        # Combine returns
        combined_returns <- merge(ief_returns, spy_returns)
        colnames(combined_returns) <- c("bond", "equity")
        combined_returns <- na.omit(combined_returns)
        
        # Get historical and recent periods
        if (nrow(combined_returns) >= 60) {
          # Calculate 60-day rolling correlations
          roll_corr <- rollapply(combined_returns, 60, function(x) {
            cor(x[,"bond"], x[,"equity"], use = "complete.obs")
          }, by.column = FALSE, align = "right")
          
          # Get recent correlation and historical correlations
          recent_corr <- as.numeric(tail(roll_corr, 1))
          hist_corr <- as.numeric(head(roll_corr, -1))
          
          # Calculate Z-score
          corr_mean <- mean(hist_corr, na.rm = TRUE)
          corr_sd <- sd(hist_corr, na.rm = TRUE)
          
          if (!is.na(corr_mean) && !is.na(corr_sd) && corr_sd > 0) {
            bond_equity_zscore <- (recent_corr - corr_mean) / corr_sd
            cat(sprintf("Calculated Bond-Equity Corr Z-score: %.2f (Value: %.2f)\n", 
                        bond_equity_zscore, recent_corr))
          }
        }
      }, silent = TRUE)
    }
  }
  
  # Print Z-score summary
  cat("\nZ-SCORE SUMMARY:\n")
  cat(sprintf("  Volatility: %.2f\n", vol_zscore))
  cat(sprintf("  Growth: %.2f\n", pmi_zscore))
  cat(sprintf("  Inflation: %.2f\n", cpi_zscore))
  cat(sprintf("  Bond-Equity Correlation: %.2f\n", bond_equity_zscore))
  
  # Use more sensitive thresholds to ensure we detect regime changes
  # VOLATILITY REGIME - Takes precedence
  if (vol_zscore > 1.0) {  # Lower threshold from 1.5 to 1.0
    if (bond_equity_zscore > 0.5) {  # Lower threshold from 0.8 to 0.5
      regime <- "inflation_shock"  # Stocks and bonds falling together
      cat("Z-SCORE DETECTION: inflation_shock regime (high vol + positive bond-equity corr)\n")
    } else {
      regime <- "risk_off"  # Flight to quality
      cat("Z-SCORE DETECTION: risk_off regime (high volatility)\n")
    }
  } 
  # GROWTH & INFLATION REGIMES
  else if (pmi_zscore > 0.3) {  # Lower threshold from 0.5 to 0.3
    if (cpi_zscore > 0.3) {  # Lower threshold from 0.5 to 0.3
      regime <- "reflation"  # Growing with rising inflation
      cat("Z-SCORE DETECTION: reflation regime (strong growth + high inflation)\n")
    } else {
      regime <- "growth"  # Strong growth, controlled inflation
      cat("Z-SCORE DETECTION: growth regime (strong growth + controlled inflation)\n")
    }
  } else if (pmi_zscore < -0.3) {  # Lower threshold from -0.5 to -0.3
    if (cpi_zscore > 0.3) {  # Lower threshold from 0.5 to 0.3
      regime <- "stagflation"  # Weak growth with high inflation
      cat("Z-SCORE DETECTION: stagflation regime (weak growth + high inflation)\n")
    } else {
      regime <- "deflation"  # Weak growth, low inflation
      cat("Z-SCORE DETECTION: deflation regime (weak growth + low inflation)\n")
    }
  } 
  # NEUTRAL GROWTH BUT HIGH INFLATION
  else if (cpi_zscore > 0.5) {
    regime <- "stagflation"  # Neutral growth but high inflation
    cat("Z-SCORE DETECTION: stagflation regime (neutral growth + high inflation)\n")
  } 
  # DEFAULT TO GROWTH
  else {
    regime <- "growth"  # Default to growth regime
    cat("Z-SCORE DETECTION: default growth regime (neutral conditions)\n")
  }
  
  # Return both the regime and confidence metrics
  return(list(
    regime = regime,
    metrics = list(
      vol_zscore = vol_zscore,
      pmi_zscore = pmi_zscore,
      cpi_zscore = cpi_zscore,
      bond_equity_zscore = bond_equity_zscore,
      lookback_window = lookback
    )
  ))
}

# Define regime-based adjustments for portfolio weights
get_regime_adjustments <- function(regime) {
  # Define regime-based adjustments (multiplicative factors)
  regime_adjustments <- list(
    growth = list(
      US_EQUITY = 1.1,
      US_SMALL_CAP = 1.1,
      US_TREASURY = 0.9,
      CREDIT_IG = 1.0,
      GOLD = 0.9,
      REIT = 1.1
    ),
    reflation = list(
      US_EQUITY = 1.05,
      US_SMALL_CAP = 1.1,
      US_TREASURY = 0.8,
      CREDIT_IG = 0.95,
      GOLD = 1.1,
      REIT = 1.0
    ),
    deflation = list(
      US_EQUITY = 0.9,
      US_SMALL_CAP = 0.8,
      US_TREASURY = 1.3,
      CREDIT_IG = 1.2,
      GOLD = 1.2,
      REIT = 0.8
    ),
    stagflation = list(
      US_EQUITY = 0.8,
      US_SMALL_CAP = 0.7,
      US_TREASURY = 0.9,
      CREDIT_IG = 0.9,
      GOLD = 1.3,
      REIT = 0.7
    ),
    risk_off = list(
      US_EQUITY = 0.6,
      US_SMALL_CAP = 0.5,
      US_TREASURY = 1.5,
      CREDIT_IG = 1.1,
      GOLD = 1.5,
      REIT = 0.5
    ),
    inflation_shock = list(
      US_EQUITY = 0.6,
      US_SMALL_CAP = 0.5,
      US_TREASURY = 0.7,
      CREDIT_IG = 0.8,
      GOLD = 1.3,
      REIT = 0.5
    )
  )
  
  # Default to neutral if regime not recognized
  if (!regime %in% names(regime_adjustments)) {
    # Create a neutral adjustment (all factors = 1.0)
    neutral <- regime_adjustments$growth
    for (asset in names(neutral)) {
      neutral[[asset]] <- 1.0
    }
    return(neutral)
  }
  
  return(regime_adjustments[[regime]])
}
#=============================================================================
# RISK PARITY ALLOCATION - FIXED OPTIMIZATION
#=============================================================================

# Risk contribution calculation with proper error handling
risk_contribution <- function(weights, cov_matrix) {
  # Safety check for inputs
  if (is.null(weights) || is.null(cov_matrix)) {
    stop("Weights or covariance matrix is NULL")
  }
  
  if (length(weights) != nrow(cov_matrix)) {
    stop("Dimension mismatch: weights length ", length(weights), 
         " doesn't match covariance matrix rows ", nrow(cov_matrix))
  }
  
  # Make sure weights is a vector, not a one-column matrix
  weights <- as.numeric(weights)
  
  # Calculate portfolio volatility
  portfolio_vol <- sqrt(as.numeric(t(weights) %*% cov_matrix %*% weights))
  
  # Safety check for zero volatility
  if (portfolio_vol <= 1e-10) {
    warning("Near-zero portfolio volatility detected, using small positive value")
    portfolio_vol <- 1e-10
  }
  
  # Calculate marginal contribution to risk
  marginal_contrib <- (cov_matrix %*% weights) / portfolio_vol
  
  # Calculate risk contribution
  risk_contrib <- weights * marginal_contrib
  
  return(risk_contrib)
}

# Risk parity objective function with better error handling
risk_parity_objective <- function(weights, cov_matrix) {
  # Make sure weights is a vector
  weights <- as.numeric(weights)
  
  # Safety checks
  if (any(is.na(weights)) || any(is.na(cov_matrix))) {
    return(1e10)  # Return large value if inputs contain NAs
  }
  
  # Target risk contribution (equal for all assets)
  n <- length(weights)
  target_risk <- 1.0 / n
  
  # Calculate actual risk contributions
  risk_contrib <- tryCatch({
    risk_contribution(weights, cov_matrix)
  }, error = function(e) {
    warning("Error in risk contribution calculation: ", e$message)
    return(rep(1/n, n))  # Return equal contributions if calculation fails
  })
  
  # Sum of squared deviations from target (normalize by n for better scaling)
  objective_value <- sum((risk_contrib - target_risk)^2) / n
  
  return(objective_value)
}

# Calculate risk parity weights with robust optimization
calculate_risk_parity_weights <- function(returns, target_vol = 0.075, 
                                          cov_method = "ledoit-wolf", 
                                          use_garch = TRUE, ewma_lambda = 0.94) {
  # Remove NA values and ensure we have enough data
  returns <- na.omit(returns)
  if (nrow(returns) < 30 || ncol(returns) < 2) {
    cat("WARNING: Insufficient data for risk parity. Using equal weights.\n")
    equal_weights <- rep(1/ncol(returns), ncol(returns))
    names(equal_weights) <- colnames(returns)
    return(list(
      weights = equal_weights,
      cov_matrix = diag(ncol(returns)),
      method = "equal",
      port_vol = sd(rowSums(returns)) * sqrt(252)
    ))
  }
  
  # Number of assets
  n <- ncol(returns)
  
  # STEP 1: Choose covariance estimation method
  cat("Estimating covariance matrix using:", cov_method, "\n")
  
  if (cov_method == "ewma") {
    cov_matrix <- estimate_ewma_covariance(returns, lambda = ewma_lambda)
    cat("Using EWMA covariance estimation (λ =", ewma_lambda, ")\n")
  } else if (cov_method == "sample") {
    cov_matrix <- cov(returns)
    cat("Using sample covariance estimation\n")
  } else if (cov_method == "ledoit-wolf") {
    cov_matrix <- estimate_robust_covariance(returns, method = "ledoit-wolf")
    cat("Using Ledoit-Wolf shrinkage estimation\n")
  } else {
    # Default to sample if method not recognized
    cat("Warning: Unrecognized covariance method. Using sample covariance.\n")
    cov_matrix <- cov(returns)
  }
  
  # STEP 2: Apply GARCH adjustments if requested
  if (use_garch) {
    cat("Applying GARCH volatility forecasting adjustments\n")
    tryCatch({
      # Get GARCH volatility forecasts
      garch_vols <- forecast_garch_volatility(returns)
      
      # Calculate historical volatilities
      hist_vols <- apply(returns, 2, sd, na.rm = TRUE) * sqrt(252)
      
      # Loop through assets to adjust covariance matrix
      for (i in 1:n) {
        asset_i <- colnames(returns)[i]
        garch_vol_i <- garch_vols[asset_i, "forecasted_vol"]
        hist_vol_i <- hist_vols[i]
        
        # Only apply if we have valid volatilities
        if (hist_vol_i > 0 && !is.na(garch_vol_i)) {
          vol_ratio_i <- garch_vol_i / hist_vol_i
          
          # Update elements in covariance matrix
          for (j in 1:n) {
            asset_j <- colnames(returns)[j]
            if (j == i) {
              # Diagonal element - direct variance scaling
              cov_matrix[i, j] <- cov_matrix[i, j] * vol_ratio_i^2
            } else {
              # Off-diagonal - adjust by both asset volatility ratios
              garch_vol_j <- garch_vols[asset_j, "forecasted_vol"]
              hist_vol_j <- hist_vols[j]
              
              if (hist_vol_j > 0 && !is.na(garch_vol_j)) {
                vol_ratio_j <- garch_vol_j / hist_vol_j
                cov_matrix[i, j] <- cov_matrix[i, j] * vol_ratio_i * vol_ratio_j
                cov_matrix[j, i] <- cov_matrix[i, j]  # Maintain symmetry
              }
            }
          }
        }
      }
      cat("GARCH adjustments applied successfully\n")
    }, error = function(e) {
      cat("Failed to apply GARCH adjustments:", e$message, "\n")
    })
  }
  
  # STEP 3: Ensure covariance matrix is well-conditioned
  if (any(is.na(cov_matrix)) || any(!is.finite(cov_matrix))) {
    cat("Warning: Invalid values in covariance matrix. Using sample covariance.\n")
    cov_matrix <- cov(returns)
  }
  
  # Check positive definiteness
  eigen_values <- eigen(cov_matrix, only.values = TRUE)$values
  if (min(eigen_values) <= 0) {
    cat("Warning: Non-positive-definite covariance matrix. Adding regularization.\n")
    # Add small constant to diagonal to ensure positive definiteness
    diag(cov_matrix) <- diag(cov_matrix) + 1e-5 * mean(diag(cov_matrix))
  }
  
  # STEP 4: Solve risk parity optimization problem with FIXED optimization
  cat("Solving risk parity optimization...\n")
  
  # Initial guess - equal weights
  initial_weights <- rep(1/n, n)
  
  # Constraints
  lower_bounds <- rep(0.01, n)  # Min 1% per asset
  upper_bounds <- rep(0.30, n)  # Max 30% per asset
  
  # FIXED: Use inequality constraint correctly
  result <- tryCatch({
    # Fix the constraint to use <= format as required
    opt_result <- slsqp(
      x0 = initial_weights,
      fn = function(w) risk_parity_objective(w, cov_matrix),
      lower = lower_bounds,
      upper = upper_bounds,
      hin = function(w) -(sum(w) - 1),  # Sum to 1, <= 0 format (note the negative sign)
      control = list(maxeval = 500, xtol_rel = 1e-4)
    )
    
    # Check if optimization succeeded
    if (opt_result$convergence != 0) {
      cat("Optimization did not converge. Trying again with different parameters...\n")
      # Try again with looser constraints
      opt_result <- slsqp(
        x0 = initial_weights,
        fn = function(w) risk_parity_objective(w, cov_matrix),
        lower = rep(0.001, n),  # Looser lower bound
        upper = rep(0.5, n),    # Looser upper bound
        hin = function(w) -(sum(w) - 1),  # Corrected constraint format
        control = list(maxeval = 1000, xtol_rel = 1e-3)
      )
    }
    
    # Return optimized weights
    opt_result$par
  }, error = function(e) {
    cat("Optimization failed:", e$message, "\n")
    cat("Using equal weights instead.\n")
    initial_weights
  })
  
  # Check if result sums to approximately 1
  if (abs(sum(result) - 1) > 0.05) {
    cat("Warning: Optimization result doesn't sum to 1. Normalizing...\n")
    result <- result / sum(result)
  }
  
  # Name weights
  base_weights <- result
  names(base_weights) <- colnames(returns)
  
  # STEP 5: Calculate unscaled portfolio volatility
  port_vol <- tryCatch({
    sqrt(as.numeric(t(base_weights) %*% cov_matrix %*% base_weights)) * sqrt(252)
  }, error = function(e) {
    cat("Error calculating portfolio volatility:", e$message, "\n")
    # Fallback - use sample covariance for volatility
    sqrt(as.numeric(t(base_weights) %*% cov(returns) %*% base_weights)) * sqrt(252)
  })
  
  cat(sprintf("Unscaled portfolio volatility: %.2f%%\n", 100 * port_vol))
  
  # Return results as a list
  return(list(
    weights = base_weights,         # Base weights (sum to 1)
    cov_matrix = cov_matrix,        # Covariance matrix
    port_vol = port_vol,            # Portfolio volatility (annualized)
    leverage = target_vol / port_vol # Leverage needed for target vol
  ))
}

#=============================================================================
# PORTFOLIO CONSTRUCTION WITH FIXED EXPOSURE_SCALAR
#=============================================================================

# Helper function for calculating drawdowns
calculate_current_drawdown <- function(returns) {
  if (length(returns) < 2) {
    return(0)
  }
  
  # Calculate cumulative returns
  cumul_returns <- cumprod(1 + returns)
  
  # Calculate drawdown
  current_drawdown <- 1 - cumul_returns[length(cumul_returns)] / max(cumul_returns)
  
  return(current_drawdown)
}

# Portfolio construction with Z-score regime detection and robust error handling
construct_optimized_risk_parity <- function(returns, market_data, sleeve_mapping,
                                            portfolio_returns = NULL,
                                            target_vol = 0.075, 
                                            max_drawdown = 0.075,
                                            use_garch = TRUE,
                                            cov_method = "ledoit-wolf",
                                            min_weight = 0.02,
                                            debug = TRUE) {
  # Print debug header
  if (debug) {
    cat("\n===== CONSTRUCTING PORTFOLIO =====\n")
    cat("Method:", cov_method, ifelse(use_garch, "with GARCH", ""), "\n")
    cat("Target volatility:", sprintf("%.2f%%", 100 * target_vol), "\n")
    cat("Target max drawdown:", sprintf("%.2f%%", 100 * max_drawdown), "\n")
    cat("Assets:", ncol(returns), "\n")
  }
  
  #--------------------------------------------------------------------------
  # STEP 1: Detect market regime using Z-score approach
  #--------------------------------------------------------------------------
  # Get regime and metrics - use error handling
  regime_result <- tryCatch({
    detect_market_regime(market_data)
  }, error = function(e) {
    cat("Error in Z-score regime detection:", e$message, "\n")
    cat("Using default growth regime\n")
    list(
      regime = "growth",
      metrics = list(
        vol_zscore = 0,
        pmi_zscore = 0,
        cpi_zscore = 0,
        bond_equity_zscore = 0,
        lookback_window = 252
      )
    )
  })
  
  regime <- regime_result$regime
  regime_metrics <- regime_result$metrics
  
  if (debug) {
    cat("\n=== REGIME DETECTION (Z-SCORE BASED) ===\n")
    cat("Detected market regime:", regime, "\n")
    cat(sprintf("Volatility Z-score: %.2f\n", regime_metrics$vol_zscore))
    cat(sprintf("Growth Z-score: %.2f\n", regime_metrics$pmi_zscore))
    cat(sprintf("Inflation Z-score: %.2f\n", regime_metrics$cpi_zscore))
    cat(sprintf("Bond-Equity Corr Z-score: %.2f\n", regime_metrics$bond_equity_zscore))
  }
  
  #--------------------------------------------------------------------------
  # STEP 2: Calculate base risk parity weights
  #--------------------------------------------------------------------------
  rp_result <- calculate_risk_parity_weights(
    returns, 
    target_vol = target_vol, 
    cov_method = cov_method,
    use_garch = use_garch
  )
  
  # Extract components from result
  base_weights <- rp_result$weights        # Base weights (sum to 1)
  cov_matrix <- rp_result$cov_matrix       # Covariance matrix
  base_port_vol <- rp_result$port_vol      # Base portfolio volatility
  
  if (debug) {
    cat("\nBase Risk Parity Weights (before adjustments):\n")
    print(round(sort(base_weights, decreasing = TRUE) * 100, 2))
    cat("Base Portfolio Volatility:", sprintf("%.2f%%\n", 100 * base_port_vol))
  }
  
  #--------------------------------------------------------------------------
  # STEP 3: Apply regime-based adjustments
  #--------------------------------------------------------------------------
  regime_adjustments <- get_regime_adjustments(regime)
  
  # Create mapping from tickers to asset classes
  ticker_to_sleeve <- list()
  for (ticker in colnames(returns)) {
    if (ticker %in% names(sleeve_mapping)) {
      ticker_to_sleeve[[ticker]] <- sleeve_mapping[[ticker]]
    } else {
      ticker_to_sleeve[[ticker]] <- "UNKNOWN"
    }
  }
  
  # Apply regime adjustments
  adjusted_weights <- base_weights
  for (ticker in names(adjusted_weights)) {
    sleeve <- ticker_to_sleeve[[ticker]]
    if (sleeve %in% names(regime_adjustments)) {
      adjustment <- regime_adjustments[[sleeve]]
      adjusted_weights[ticker] <- adjusted_weights[ticker] * adjustment
      
      if (debug) {
        cat(sprintf("Adjusting %s (%s) by %.2fx due to %s regime\n", 
                    ticker, sleeve, adjustment, regime))
      }
    }
  }
  
  # Normalize adjusted weights to sum to 1
  adjusted_weights <- adjusted_weights / sum(adjusted_weights)
  
  #--------------------------------------------------------------------------
  # STEP 4: Apply minimum weight threshold and renormalize
  #--------------------------------------------------------------------------
  final_weights <- adjusted_weights
  final_weights[final_weights < min_weight] <- 0
  
  # Check if we have any non-zero weights left
  if (sum(final_weights > 0) == 0) {
    # All weights were below threshold, revert to adjusted_weights
    final_weights <- adjusted_weights
  }
  
  # Renormalize
  final_weights <- final_weights / sum(final_weights)
  
  # Calculate expected volatility after regime adjustments
  expected_vol <- tryCatch({
    sqrt(as.numeric(t(final_weights) %*% cov_matrix %*% final_weights)) * sqrt(252)
  }, error = function(e) {
    warning("Error calculating expected volatility after regime adjustments: ", e$message)
    base_port_vol  # Fallback to base portfolio volatility
  })
  
  if (debug) {
    cat("\nAfter Regime Adjustments:\n")
    cat("Expected Volatility:", sprintf("%.2f%%\n", 100 * expected_vol))
  }
  
  #--------------------------------------------------------------------------
  # STEP 5: Apply drawdown control - CRITICAL FIX: Initialize exposure_scalar FIRST
  #--------------------------------------------------------------------------
  # Initialize exposure_scalar FIRST to prevent errors
  exposure_scalar <- 1.0
  current_drawdown <- 0
  
  if (!is.null(portfolio_returns) && length(portfolio_returns) > 10) {
    current_drawdown <- calculate_current_drawdown(portfolio_returns)
    
    if (debug) {
      cat("\nCurrent Drawdown:", sprintf("%.2f%%\n", 100 * current_drawdown))
    }
    
    # Apply more aggressive drawdown-based exposure reduction
    if (current_drawdown > 0.5 * max_drawdown) {  # Lower threshold for earlier action
      # Calculate how close we are to max drawdown
      ratio <- current_drawdown / max_drawdown
      
      # Scale exposure progressively - more aggressive settings
      if (ratio >= 0.9) {
        exposure_scalar <- 0.5  # Very severe reduction
      } else if (ratio >= 0.8) {
        exposure_scalar <- 0.6  # Severe reduction
      } else if (ratio >= 0.7) {
        exposure_scalar <- 0.7  # Significant reduction
      } else if (ratio >= 0.6) {
        exposure_scalar <- 0.8  # Moderate reduction
      } else if (ratio >= 0.5) {
        exposure_scalar <- 0.9  # Mild reduction
      }
      
      if (debug) {
        cat("Applying drawdown control - exposure scalar:", exposure_scalar, "\n")
      }
      
      # Adjust weights - defensive assets increase, risky assets decrease
      for (ticker in names(final_weights)) {
        sleeve <- ticker_to_sleeve[[ticker]]
        
        # Defensive assets (increase)
        if (sleeve %in% c("US_TREASURY", "GOLD")) {
          final_weights[ticker] <- final_weights[ticker] * (1 + (1 - exposure_scalar))
        }
        # Risky assets (decrease)
        else if (sleeve %in% c("US_EQUITY", "US_SMALL_CAP", "REIT")) {
          final_weights[ticker] <- final_weights[ticker] * exposure_scalar
        }
      }
      
      # Renormalize to sum to 1
      final_weights <- final_weights / sum(final_weights)
      
      # Update expected volatility after drawdown adjustments
      expected_vol <- tryCatch({
        sqrt(as.numeric(t(final_weights) %*% cov_matrix %*% final_weights)) * sqrt(252)
      }, error = function(e) {
        warning("Error calculating expected volatility after drawdown control: ", e$message)
        expected_vol  # Keep previous estimate
      })
      
      if (debug) {
        cat("After Drawdown Control:\n")
        cat("Expected Volatility:", sprintf("%.2f%%\n", 100 * expected_vol))
      }
    }
  }
  
  #--------------------------------------------------------------------------
  # STEP 6: Apply volatility targeting
  #--------------------------------------------------------------------------
  # Calculate how much we need to scale the weights to hit target volatility
  vol_scalar <- target_vol / expected_vol * exposure_scalar
  
  # FIXED: Apply scalar to weights using as.vector() to fix deprecation warning
  final_scaled_weights <- final_weights * as.vector(vol_scalar)
  
  # Calculate final expected volatility
  final_vol <- tryCatch({
    sqrt(as.numeric(t(final_scaled_weights) %*% cov_matrix %*% final_scaled_weights)) * sqrt(252)
  }, error = function(e) {
    warning("Error calculating final volatility: ", e$message)
    target_vol  # Use target as fallback
  })
  
  if (debug) {
    cat("\nVOLATILITY TARGETING:\n")
    cat("Target Volatility:", sprintf("%.2f%%\n", 100 * target_vol))
    cat("Expected Volatility (pre-scaling):", sprintf("%.2f%%\n", 100 * expected_vol))
    cat("Volatility Scalar Applied:", sprintf("%.4f\n", vol_scalar))
    cat("Final Expected Volatility:", sprintf("%.2f%%\n", 100 * final_vol))
    cat("Sum of Final Weights:", sprintf("%.4f\n", sum(final_scaled_weights)))
  }
  
  #--------------------------------------------------------------------------
  # STEP 7: Return portfolio with all details
  #--------------------------------------------------------------------------
  portfolio <- list(
    weights = final_scaled_weights,  # These weights achieve target volatility
    regime = regime,
    regime_metrics = regime_metrics, # Store Z-scores for analysis
    expected_vol = final_vol,
    target_vol = target_vol,
    vol_scalar = vol_scalar,
    timestamp = index(tail(market_data, 1)),
    cov_method = cov_method,
    use_garch = use_garch,
    current_drawdown = current_drawdown,
    exposure_scalar = exposure_scalar,
    base_weights = base_weights,  # Original risk parity weights
    cov_matrix = cov_matrix      # Save the covariance matrix
  )
  
  return(portfolio)
}

#=============================================================================
# BACKTESTING - FIXED FOR Z-SCORE REGIME DETECTION
#=============================================================================

backtest_optimized_strategy <- function(prices, returns, market_data, sleeve_mapping, 
                                        target_vol = 0.075,      # Target volatility
                                        max_drawdown = 0.075,    # Maximum drawdown limit
                                        rebalance_freq = "M", 
                                        lookback_window = 252, 
                                        use_garch = TRUE,
                                        cov_method = "ledoit-wolf") {
  # Confirm we're using Z-score detection (not HMM)
  cat("USING PURE Z-SCORE REGIME DETECTION - NO HMM!\n")
  
  # Align indices - ensure we have matching dates
  common_idx <- as.Date(intersect(intersect(
    as.character(index(prices)), 
    as.character(index(returns))), 
    as.character(index(market_data))
  ))
  
  if (length(common_idx) == 0) {
    stop("No common dates found between prices, returns, and market data.")
  }
  
  # Convert back to POSIXct if necessary and subset the data
  prices <- prices[common_idx]
  returns <- returns[common_idx]
  market_data <- market_data[common_idx]
  
  # Check if we have enough data
  if (nrow(prices) < lookback_window) {
    stop(sprintf("Not enough data for backtest. Need at least %d rows, but only have %d.",
                 lookback_window, nrow(prices)))
  }
  
  # Determine rebalance dates based on frequency
  if (rebalance_freq == "D") {
    rebalance_dates <- index(prices)
  } else if (rebalance_freq == "W") {
    rebalance_dates <- unique(endpoints(prices, on = "weeks"))
    rebalance_dates <- index(prices)[rebalance_dates]
  } else if (rebalance_freq == "M") {
    rebalance_dates <- unique(endpoints(prices, on = "months"))
    rebalance_dates <- index(prices)[rebalance_dates]
  } else if (rebalance_freq == "Q") {
    rebalance_dates <- unique(endpoints(prices, on = "quarters"))
    rebalance_dates <- index(prices)[rebalance_dates]
  } else {
    stop("Invalid rebalance frequency. Use 'D', 'W', 'M', or 'Q'.")
  }
  
  # Initialize backtest variables
  weights <- xts(matrix(0, nrow = nrow(prices), ncol = ncol(returns)),
                 order.by = index(prices),
                 dimnames = list(NULL, colnames(returns)))
  
  portfolio_returns <- xts(rep(0, nrow(prices)), order.by = index(prices))
  portfolio_details <- list()
  
  # Start with equal weights (important: initialize with non-zero weights)
  current_weights <- rep(1/ncol(returns), ncol(returns))
  names(current_weights) <- colnames(returns)
  
  # FIXED: Initialize with "growth" default (not empty string) for proper regime tracking
  regime_history <- xts(rep("growth", nrow(prices)), order.by = index(prices))
  
  # Track Z-scores
  vol_zscore_history <- xts(rep(0, nrow(prices)), order.by = index(prices))
  pmi_zscore_history <- xts(rep(0, nrow(prices)), order.by = index(prices))
  cpi_zscore_history <- xts(rep(0, nrow(prices)), order.by = index(prices))
  corr_zscore_history <- xts(rep(0, nrow(prices)), order.by = index(prices))
  
  # Track drawdowns
  drawdown_history <- xts(rep(0, nrow(prices)), order.by = index(prices))
  
  # Run backtest
  cat(sprintf("Starting backtest with %s covariance method %s and PURE Z-SCORE regime detection...\n", 
              cov_method, ifelse(use_garch, "with GARCH", "without GARCH")))
  
  # Force first rebalance on day 1
  first_rebalance_done <- FALSE
  
  # Store last valid regime detection for filling between rebalances
  last_valid_regime <- "growth"
  
  for (i in 1:nrow(prices)) {
    date <- index(prices)[i]
    
    # We need an initial portfolio right away
    if (!first_rebalance_done && i >= lookback_window) {
      # Force first rebalance
      hist_start_idx <- max(1, i - lookback_window)
      hist_returns <- returns[hist_start_idx:i,]
      hist_market_data <- market_data[hist_start_idx:i,]
      
      # Construct initial portfolio
      tryCatch({
        portfolio <- construct_optimized_risk_parity(
          hist_returns,
          hist_market_data,
          sleeve_mapping,
          target_vol = target_vol,
          max_drawdown = max_drawdown,
          use_garch = use_garch,
          cov_method = cov_method
        )
        
        # Set initial weights
        current_weights <- portfolio$weights
        
        # Store portfolio details
        portfolio_details[[as.character(date)]] <- portfolio
        
        # FIXED: Track the current regime reliably
        if (!is.null(portfolio$regime) && portfolio$regime != "") {
          regime_history[i] <- portfolio$regime
          last_valid_regime <- portfolio$regime
        } else {
          regime_history[i] <- last_valid_regime
        }
        
        vol_zscore_history[i] <- portfolio$regime_metrics$vol_zscore
        pmi_zscore_history[i] <- portfolio$regime_metrics$pmi_zscore
        cpi_zscore_history[i] <- portfolio$regime_metrics$cpi_zscore
        corr_zscore_history[i] <- portfolio$regime_metrics$bond_equity_zscore
        
        cat(sprintf("Initial portfolio on %s - Regime: %s (Vol Z=%.2f) - Expected Vol: %.2f%%\n", 
                    format(date, "%Y-%m-%d"), portfolio$regime, 
                    portfolio$regime_metrics$vol_zscore,
                    100 * portfolio$expected_vol))
        
        first_rebalance_done <- TRUE
      }, error = function(e) {
        warning("Initial portfolio construction failed: ", e$message)
        # Use equal weights as fallback
        current_weights <- setNames(rep(1/ncol(returns), ncol(returns)), colnames(returns))
        first_rebalance_done <- TRUE
      })
    }
    
    # Update portfolio returns using current weights
    daily_returns <- returns[i,]
    if (all(is.na(daily_returns))) {
      portfolio_returns[i] <- 0  # Skip days with no return data
    } else {
      # FIX: Ensure daily returns are numeric and handle missing values
      daily_returns_vector <- as.numeric(daily_returns)
      
      # If any returns are NA, use 0 for those assets but keep track of valid weights
      valid_returns <- !is.na(daily_returns_vector)
      if (sum(valid_returns) == 0) {
        portfolio_returns[i] <- 0  # No valid returns at all
      } else {
        # If some weights are for assets with NA returns, normalize the valid weights
        valid_weights <- current_weights[valid_returns]
        valid_sum <- sum(valid_weights)
        
        if (valid_sum > 0) {
          # Normalize valid weights to sum to the original sum of all weights
          scaling_factor <- sum(current_weights) / valid_sum
          scaled_valid_weights <- valid_weights * scaling_factor
          
          # Multiply returns by weights
          portfolio_returns[i] <- sum(daily_returns_vector[valid_returns] * scaled_valid_weights)
        } else {
          portfolio_returns[i] <- 0
        }
      }
    }
    
    # Calculate current drawdown for tracking
    if (i > 1) {
      cumul_returns <- cumprod(1 + portfolio_returns[1:i])
      if (length(cumul_returns) > 0) {
        drawdown_history[i] <- 1 - cumul_returns[i] / max(cumul_returns)
      }
    }
    
    # Store current weights
    weights[i,] <- current_weights
    
    # Check if we need to rebalance
    if (date %in% rebalance_dates && i > lookback_window && first_rebalance_done) {
      # Get historical data for lookback window
      hist_start_idx <- max(1, i - lookback_window)
      hist_returns <- returns[hist_start_idx:i,]
      hist_market_data <- market_data[hist_start_idx:i,]
      
      # Get portfolio return history for drawdown control
      port_return_history <- portfolio_returns[1:i]
      
      # Construct new portfolio with specified covariance method
      tryCatch({
        portfolio <- construct_optimized_risk_parity(
          hist_returns,
          hist_market_data,
          sleeve_mapping,
          portfolio_returns = port_return_history,
          target_vol = target_vol,
          max_drawdown = max_drawdown,
          use_garch = use_garch,
          cov_method = cov_method,
          debug = FALSE  # Reduce verbosity during rebalance
        )
        
        # Update weights
        current_weights <- portfolio$weights
        
        # Store portfolio details
        portfolio_details[[as.character(date)]] <- portfolio
        
        # FIXED: Track regime reliably and update history
        if (!is.null(portfolio$regime) && portfolio$regime != "") {
          regime_history[i] <- portfolio$regime
          last_valid_regime <- portfolio$regime
          
          # Update previous days since last rebalance with current regime
          # This helps ensure we have proper regime tracking between rebalances
          if (i > 1) {
            # Find the last rebalance date
            prev_rebalance_idx <- max(which(index(regime_history)[1:(i-1)] %in% rebalance_dates), 0)
            if (prev_rebalance_idx > 0) {
              # Fill from last rebalance to current date with current regime
              update_range <- (prev_rebalance_idx+1):(i-1)
              if (length(update_range) > 0) {
                regime_history[update_range] <- portfolio$regime
              }
            }
          }
        } else {
          regime_history[i] <- last_valid_regime
        }
        
        vol_zscore_history[i] <- portfolio$regime_metrics$vol_zscore
        pmi_zscore_history[i] <- portfolio$regime_metrics$pmi_zscore
        cpi_zscore_history[i] <- portfolio$regime_metrics$cpi_zscore
        corr_zscore_history[i] <- portfolio$regime_metrics$bond_equity_zscore
        
        # Report drawdown status if significant
        if (portfolio$current_drawdown > max_drawdown * 0.5) {
          dd_msg <- sprintf(" - Drawdown: %.2f%% (%.0f%% of max)",
                            portfolio$current_drawdown * 100, 
                            (portfolio$current_drawdown/max_drawdown) * 100)
        } else {
          dd_msg <- ""
        }
        
        if (i %% 20 == 0) {  # Print status every 20 rebalances to reduce verbosity
          cat(sprintf("Rebalanced on %s - Regime: %s (Vol Z=%.2f) - Expected Vol: %.2f%% - Weight Sum: %.2f%s\n", 
                      format(date, "%Y-%m-%d"), 
                      portfolio$regime,
                      portfolio$regime_metrics$vol_zscore,
                      portfolio$expected_vol * 100,
                      sum(portfolio$weights) * 100,
                      dd_msg))
        }
      }, error = function(e) {
        warning("Portfolio construction failed on ", format(date, "%Y-%m-%d"), 
                ": ", e$message, ". Keeping current weights.")
      })
    } else {
      # On non-rebalance days, carry forward the last detected regime
      regime_history[i] <- last_valid_regime
    }
  }
  
  # FIXED: Make sure we don't have empty regimes
  regime_history[regime_history == ""] <- "growth"
  
  # Forward fill any remaining NAs
  regime_history <- na.locf(regime_history)
  vol_zscore_history <- na.locf(vol_zscore_history)
  pmi_zscore_history <- na.locf(pmi_zscore_history)
  cpi_zscore_history <- na.locf(cpi_zscore_history)
  corr_zscore_history <- na.locf(corr_zscore_history)
  
  # Combine Z-score histories
  zscore_history <- merge(vol_zscore_history, pmi_zscore_history, cpi_zscore_history, corr_zscore_history)
  colnames(zscore_history) <- c("vol", "pmi", "cpi", "corr")
  
  # Calculate cumulative returns
  cumulative_returns <- cumprod(1 + portfolio_returns)
  
  # CRITICAL FIX: Make sure portfolio_returns has no NA values
  portfolio_returns[is.na(portfolio_returns)] <- 0
  
  cat("Backtest completed\n")
  cat(sprintf("Final cumulative return: %.2f%%\n", 
              100 * (last(cumulative_returns) - 1)))
  
  # Display improved regime distribution
  regime_table <- table(as.character(regime_history))
  regime_pct <- round(100 * regime_table / sum(regime_table), 2)
  
  cat("\nRegime Distribution:\n")
  for (r in sort(names(regime_table))) {
    cat(sprintf("  %s: %d days (%.2f%%)\n", r, regime_table[r], regime_pct[r]))
  }
  
  # Return results including covariance method used
  return(list(
    returns = portfolio_returns,
    cumulative_returns = cumulative_returns,
    weights = weights,
    details = portfolio_details,
    regime_history = regime_history,
    zscore_history = zscore_history,
    drawdown_history = drawdown_history,
    sleeve_mapping = sleeve_mapping,
    cov_method = cov_method,
    use_garch = use_garch,
    target_vol = target_vol,
    max_drawdown = max_drawdown
  ))
}

#=============================================================================
# PERFORMANCE ANALYSIS
#=============================================================================

# Calculate performance metrics
calculate_performance_metrics <- function(returns) {
  # Ensure input is xts
  if (!is.xts(returns)) {
    returns <- try(as.xts(returns), silent = TRUE)
    if (inherits(returns, "try-error")) {
      warning("Failed to convert returns to xts format")
      return(list(
        total_return = NA, ann_return = NA, ann_vol = NA,
        sharpe = NA, sortino = NA, max_drawdown = NA, calmar = NA,
        win_rate = NA, profit_factor = NA
      ))
    }
  }
  
  # Make sure returns are numeric
  returns <- as.numeric(returns)
  
  # Remove NA values if any
  returns_data <- returns[!is.na(returns)]
  
  # Basic metrics calculation with error handling
  tryCatch({
    # Check if we have enough data
    if (length(returns_data) < 5) {
      cat("Warning: Not enough data for performance metrics (need at least 5 observations)\n")
      return(list(
        total_return = NA, ann_return = NA, ann_vol = NA,
        sharpe = NA, sortino = NA, max_drawdown = NA, calmar = NA,
        win_rate = NA, profit_factor = NA
      ))
    }
    
    # Calculate total return
    total_return <- prod(1 + returns_data) - 1
    
    # Calculate annualized return
    days <- length(returns_data)
    ann_factor <- 252  # Assuming daily returns
    ann_return <- (1 + total_return)^(ann_factor/days) - 1
    
    # Calculate volatility
    ann_vol <- sd(returns_data) * sqrt(ann_factor)
    
    # Calculate Sharpe ratio
    sharpe <- ifelse(ann_vol > 0, ann_return / ann_vol, NA)
    
    # Calculate Sortino ratio (downside risk)
    downside_returns <- returns_data[returns_data < 0]
    downside_dev <- ifelse(length(downside_returns) > 0, 
                           sd(downside_returns) * sqrt(ann_factor), 
                           NA)
    sortino <- ifelse(!is.na(downside_dev) && downside_dev > 0, 
                      ann_return / downside_dev, 
                      NA)
    
    # Calculate drawdowns
    equity_curve <- cumprod(1 + returns_data)
    running_max <- cummax(equity_curve)
    drawdowns <- 1 - equity_curve / running_max
    max_drawdown <- max(drawdowns)
    
    # Calculate Calmar ratio
    calmar <- ifelse(max_drawdown > 0, ann_return / max_drawdown, NA)
    
    # Calculate win rate
    win_rate <- sum(returns_data > 0) / length(returns_data)
    
    # Calculate profit factor
    gains <- sum(returns_data[returns_data > 0])
    losses <- -sum(returns_data[returns_data < 0])
    profit_factor <- ifelse(losses > 0, gains / losses, NA)
    
    # Return all metrics
    metrics <- list(
      total_return = total_return,
      ann_return = ann_return,
      ann_vol = ann_vol,
      sharpe = sharpe,
      sortino = sortino,
      max_drawdown = max_drawdown,
      calmar = calmar,
      win_rate = win_rate,
      profit_factor = profit_factor
    )
    
    return(metrics)
  }, error = function(e) {
    warning("Error calculating performance metrics: ", e$message)
    return(list(
      total_return = NA, ann_return = NA, ann_vol = NA,
      sharpe = NA, sortino = NA, max_drawdown = NA, calmar = NA,
      win_rate = NA, profit_factor = NA
    ))
  })
}

# Function to create a performance comparison table
create_performance_table <- function(results_list) {
  # Initialize result dataframe
  perf_table <- data.frame(
    Method = character(),
    Ann_Return = numeric(),
    Ann_Vol = numeric(),
    Sharpe = numeric(),
    MaxDD = numeric(),
    Calmar = numeric(),
    WinRate = numeric(),
    ProfitFactor = numeric(),
    stringsAsFactors = FALSE
  )
  
  # For each strategy, calculate and add metrics
  for (method_name in names(results_list)) {
    # Get returns for this method
    method_returns <- results_list[[method_name]]$returns
    
    # Calculate performance metrics
    metrics <- calculate_performance_metrics(method_returns)
    
    # Add to table
    perf_table <- rbind(perf_table, data.frame(
      Method = method_name,
      Ann_Return = metrics$ann_return * 100,  # Convert to percent
      Ann_Vol = metrics$ann_vol * 100,        # Convert to percent
      Sharpe = metrics$sharpe,
      MaxDD = metrics$max_drawdown * 100,     # Convert to percent
      Calmar = metrics$calmar,
      WinRate = metrics$win_rate * 100,       # Convert to percent
      ProfitFactor = metrics$profit_factor,
      stringsAsFactors = FALSE
    ))
  }
  
  # Format the table for display
  formatted_table <- perf_table
  formatted_table$Ann_Return <- sprintf("%.2f%%", perf_table$Ann_Return)
  formatted_table$Ann_Vol <- sprintf("%.2f%%", perf_table$Ann_Vol)
  formatted_table$Sharpe <- sprintf("%.2f", perf_table$Sharpe)
  formatted_table$MaxDD <- sprintf("%.2f%%", perf_table$MaxDD)
  formatted_table$Calmar <- sprintf("%.2f", perf_table$Calmar)
  formatted_table$WinRate <- sprintf("%.1f%%", perf_table$WinRate)
  formatted_table$ProfitFactor <- sprintf("%.2f", perf_table$ProfitFactor)
  
  return(formatted_table)
}

#=============================================================================
# YEARLY PERFORMANCE ANALYSIS
#=============================================================================

# Function to analyze yearly performance
analyze_yearly_performance <- function(results_list) {
  # Get unique years from the first result (assuming all have same date range)
  first_method <- names(results_list)[1]
  dates <- index(results_list[[first_method]]$returns)
  years <- unique(format(dates, "%Y"))
  
  # Initialize result data frame
  yearly_comparison <- data.frame(
    Year = years,
    stringsAsFactors = FALSE
  )
  
  # For each method, calculate yearly metrics
  for (method_name in names(results_list)) {
    # Get returns for this method
    method_returns <- results_list[[method_name]]$returns
    method_drawdowns <- results_list[[method_name]]$drawdown_history
    
    # Initialize columns for this method
    yearly_comparison[[paste0(method_name, "_Return")]] <- NA
    yearly_comparison[[paste0(method_name, "_Vol")]] <- NA
    yearly_comparison[[paste0(method_name, "_MaxDD")]] <- NA
    yearly_comparison[[paste0(method_name, "_Sharpe")]] <- NA
    
    # Calculate metrics for each year
    for (i in 1:length(years)) {
      year <- years[i]
      year_mask <- format(index(method_returns), "%Y") == year
      
      # Skip if no data for this year
      if (sum(year_mask) == 0) next
      
      # Extract returns for this year
      year_returns <- method_returns[year_mask]
      
      # Calculate metrics
      metrics <- calculate_performance_metrics(year_returns)
      
      # Find max drawdown for this year
      year_drawdowns <- method_drawdowns[year_mask]
      max_dd_for_year <- max(year_drawdowns, na.rm = TRUE)
      
      # Store results
      yearly_comparison[i, paste0(method_name, "_Return")] <- metrics$total_return * 100
      yearly_comparison[i, paste0(method_name, "_Vol")] <- metrics$ann_vol * 100
      yearly_comparison[i, paste0(method_name, "_MaxDD")] <- max_dd_for_year * 100
      yearly_comparison[i, paste0(method_name, "_Sharpe")] <- metrics$sharpe
    }
  }
  
  return(yearly_comparison)
}

# Function to display yearly performance in a formatted table
print_yearly_performance <- function(yearly_data) {
  # Format the data for display
  formatted_data <- yearly_data
  
  # Format numeric columns
  for (col in colnames(formatted_data)) {
    if (col == "Year") next
    
    # Format based on column type
    if (grepl("Return", col)) {
      formatted_data[[col]] <- sprintf("%+.2f%%", formatted_data[[col]])
    } else if (grepl("Vol|MaxDD", col)) {
      formatted_data[[col]] <- sprintf("%.2f%%", formatted_data[[col]])
    } else if (grepl("Sharpe", col)) {
      formatted_data[[col]] <- sprintf("%.2f", formatted_data[[col]])
    }
  }
  
  # Create sections for each metric type
  cat("\n=== YEARLY RETURNS ===\n")
  returns_cols <- c("Year", grep("Return", colnames(formatted_data), value = TRUE))
  print(formatted_data[, returns_cols], row.names = FALSE)
  
  cat("\n=== YEARLY MAXIMUM DRAWDOWNS ===\n")
  drawdown_cols <- c("Year", grep("MaxDD", colnames(formatted_data), value = TRUE))
  print(formatted_data[, drawdown_cols], row.names = FALSE)
  
  cat("\n=== YEARLY VOLATILITY ===\n")
  vol_cols <- c("Year", grep("Vol", colnames(formatted_data), value = TRUE))
  print(formatted_data[, vol_cols], row.names = FALSE)
  
  cat("\n=== YEARLY SHARPE RATIO ===\n")
  sharpe_cols <- c("Year", grep("Sharpe", colnames(formatted_data), value = TRUE))
  print(formatted_data[, sharpe_cols], row.names = FALSE)
}

# Function to identify years with high drawdowns
identify_drawdown_years <- function(yearly_data, threshold = 10) {
  # Find years with high drawdowns
  high_dd_years <- c()
  
  for (i in 1:nrow(yearly_data)) {
    year <- yearly_data$Year[i]
    
    # Check each method's drawdown columns
    dd_cols <- grep("MaxDD", colnames(yearly_data), value = TRUE)
    
    for (col in dd_cols) {
      if (yearly_data[i, col] > threshold) {
        high_dd_years <- c(high_dd_years, year)
        break  # Once we found a high drawdown for this year, no need to check other methods
      }
    }
  }
  
  if (length(high_dd_years) > 0) {
    cat("\n=== YEARS WITH HIGH DRAWDOWNS (>", threshold, "%) ===\n")
    cat(paste(high_dd_years, collapse = ", "), "\n")
  } else {
    cat("\nNo years with drawdowns exceeding", threshold, "%\n")
  }
  
  return(high_dd_years)
}

#=============================================================================
# EXECUTION BLOCK - USING REAL MARKET DATA WITH PURE Z-SCORE REGIMES
#=============================================================================

# Use a proper date range for real-world data
start_date <- as.Date("2015-01-01")  # We'll use a shorter history to ensure all data is available
end_date <- as.Date("2023-12-31")    # Using end of 2023 to ensure complete data

cat("\n\n==== RUNNING RISK PARITY WITH PURE Z-SCORE REGIME DETECTION (REAL DATA) ====\n")
cat(sprintf("Date Range: %s to %s\n", format(start_date, "%Y-%m-%d"), format(end_date, "%Y-%m-%d")))
cat(sprintf("Current System Date: %s\n", format(Sys.Date(), "%Y-%m-%d")))

# Define tickers and sleeve mapping
tickers <- c(
  "SPY",    # S&P 500 (Large Cap US Equity)
  "IWM",    # Russell 2000 (Small Cap US Equity)
  "IEF",    # 7-10 Year Treasury
  "LQD",    # Investment Grade Corporate Bonds
  "GLD",    # Gold
  "VNQ"     # US Real Estate
)

sleeve_mapping <- list(
  "SPY" = "US_EQUITY",
  "IWM" = "US_SMALL_CAP",
  "IEF" = "US_TREASURY",
  "LQD" = "CREDIT_IG", 
  "GLD" = "GOLD",
  "VNQ" = "REIT"
)

# Store results for all methods
all_results <- list()

# This is the corrected execution block that uses only real data and Z-score regimes
tryCatch({
  # Step 1: Load price data
  cat("Step 1: Loading price data...\n")
  prices <- tryCatch({
    load_market_data(tickers, start_date, end_date)
  }, error = function(e) {
    cat("Error loading prices:", e$message, "\n")
    stop("Cannot proceed without market data")
  })
  
  cat(sprintf("Successfully loaded price data: %d rows × %d columns\n", nrow(prices), ncol(prices)))
  
  # Step 2: Calculate returns
  cat("\nStep 2: Calculating returns...\n")
  returns <- ROC(prices, type = "discrete")
  returns <- na.omit(returns)
  cat(sprintf("Calculated returns: %d rows × %d columns\n", nrow(returns), ncol(returns)))
  
  # Step 3: Create market data directly with guaranteed date alignment
  cat("\nStep 3: Creating market data with perfect date alignment...\n")
  market_data <- create_market_data(prices)
  
  # Step 4: Adjust market data to match returns dates (after na.omit in returns)
  market_data <- market_data[index(returns)]
  prices <- prices[index(returns)]
  
  # Verify final alignment
  cat("\nVerifying final alignment...\n")
  cat("prices:", nrow(prices), "rows\n")
  cat("returns:", nrow(returns), "rows\n")
  cat("market_data:", nrow(market_data), "rows\n")
  
  # Run all three covariance methods with Z-score regime detection
  methods <- list(
    "LW+GARCH" = "ledoit-wolf",
    "EWMA+GARCH" = "ewma",
    "Sample+GARCH" = "sample"
  )
  
  for (method_name in names(methods)) {
    cov_method <- methods[[method_name]]
    
    cat(sprintf("\n\n==== METHOD: %s (WITH PURE Z-SCORE REGIMES) ====\n", method_name))
    
    results <- tryCatch({
      backtest_optimized_strategy(
        prices, returns, market_data, sleeve_mapping,
        target_vol = 0.075,
        max_drawdown = 0.075,
        rebalance_freq = "M",
        lookback_window = 252,
        use_garch = TRUE,
        cov_method = cov_method
      )
    }, error = function(e) {
      cat("Error in", method_name, "backtest:", e$message, "\n")
      if (exists("traceback")) {
        print(traceback())
      }
      NULL
    })
    
    if (!is.null(results)) {
      all_results[[method_name]] <- results
    }
  }
  
  # Generate performance analysis
  if (length(all_results) > 0) {
    cat("\n\n==== PERFORMANCE COMPARISON WITH PURE Z-SCORE REGIMES ====\n")
    perf_table <- create_performance_table(all_results)
    print(perf_table)
    
    # Yearly performance analysis
    cat("\n\n==== YEARLY PERFORMANCE COMPARISON ====\n")
    yearly_data <- analyze_yearly_performance(all_results)
    print_yearly_performance(yearly_data)
    
    # Identify challenging years
    high_dd_years <- identify_drawdown_years(yearly_data, threshold = 10)
    
    # Plot performance
    cat("\nGenerating performance charts...\n")
    tryCatch({
      cumulative_returns_plot <- plot_strategy_comparison(
        all_results, 
        title = paste0("Risk Parity Strategy with Pure Z-Score Regime Detection (", 
                       format(start_date, "%Y"), "-", format(end_date, "%Y"), ")")
      )
      
      ggsave("risk_parity_pure_zscore.png", 
             plot = cumulative_returns_plot, 
             width = 10, 
             height = 6)
      cat("Saved performance comparison chart to 'risk_parity_pure_zscore.png'\n")
      
      # Plot regime distribution if possible
      first_method <- names(all_results)[1]
      regime_history <- all_results[[first_method]]$regime_history
      
      regime_plot <- plot_regime_distribution(
        regime_history,
        title = paste0("Market Regimes Over Time (", 
                       format(start_date, "%Y"), "-", format(end_date, "%Y"), ")")
      )
      
      ggsave("market_regimes_zscore.png",
             plot = regime_plot,
             width = 10,
             height = 3)
      cat("Saved regime distribution chart to 'market_regimes_zscore.png'\n")
      
    }, error = function(e) {
      cat("Failed to create plots:", e$message, "\n")
    })
    
    # Analyze regime transitions - important for Z-score approach to see all 6 regimes
    first_method <- names(all_results)[1]
    regime_history <- all_results[[first_method]]$regime_history
    
    # Create improved regime distribution analysis
    regime_counts <- table(as.character(regime_history))
    regime_df <- data.frame(
      Regime = names(regime_counts),
      Count = as.numeric(regime_counts),
      Percentage = round(100 * regime_counts / sum(regime_counts), 2)
    )
    
    cat("\n=== REGIME DISTRIBUTION WITH PURE Z-SCORE DETECTION ===\n")
    print(regime_df[order(-regime_df$Count),])
  } else {
    cat("\nNo successful backtest results to display\n")
  }
  
}, error = function(e) {
  cat("ERROR: Failed to run strategies:", e$message, "\n")
  if (exists("traceback")) {
    print(traceback())
  }
})

cat("\n\nStrategy execution completed with Pure Z-Score based regime detection using real market data.\n")