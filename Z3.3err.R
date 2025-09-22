#=============================================================================
# ENHANCED RISK PARITY TRADING SYSTEM - PART 1
# - REAL DATA VERSION WITH NO SYNTHETIC GENERATION
# - Improved error handling and validation
# - Enhanced economic indicators from market data
# - Smooth regime transitions with probability weighting
#=============================================================================

# Load required packages with reliable error handling
required_packages <- c("tidyverse", "quantmod", "xts", "PerformanceAnalytics",
                       "rugarch", "robustbase", "nloptr", "TTR", "fGarch",
                       "tseries", "reshape2", "corpcor", "ggplot2", "RColorBrewer",
                       "zoo", "Quandl", "tidyquant", "dplyr")

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
cat(sprintf("Current Date and Time (UTC): %s\n", 
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
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
  
  # Define inverse ETF mapping first - moved here to fix the error
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
  
  # Rest of function continues as before...
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
# Robust VIX data loading - No synthetic data
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
      return(vix_close)
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

# Load sentiment indicators
load_sentiment_data <- function(price_dates, from = min(price_dates) - 30, to = max(price_dates) + 5) {
  cat("Loading market sentiment indicators...\n")
  
  # Ensure dates are proper Date objects
  price_dates <- as.Date(price_dates)
  from <- as.Date(from)
  to <- as.Date(to)
  
  # Create templates for sentiment indicators
  sentiment_data <- xts(matrix(NA, nrow=length(price_dates), ncol=2), 
                        order.by=price_dates)
  colnames(sentiment_data) <- c("PUT_CALL_RATIO", "AAII_BULL_BEAR")
  
  # Try to get Put/Call Ratio using CBOE Put/Call Ratio ETF proxy
  # Note: There's no direct ticker for Put/Call Ratio, so we use a proxy
  pc_ratio <- tryCatch({
    # We can use SPXW (S&P 500 Weekly Options) Put/Call ratio as a proxy through a combination of ETFs
    # Alternatively, use volatility ETFs as a sentiment proxy
    cat("Loading Put/Call ratio proxy (VXZ/XIV ratio)...\n")
    
    # Using VXZ (long volatility) and XIV or SVXY (short volatility) ratio as put/call proxy
    vxz_data <- getSymbols("VXZ", src = "yahoo", from = from - 30, to = to + 5, auto.assign = FALSE)
    svxy_data <- getSymbols("SVXY", src = "yahoo", from = from - 30, to = to + 5, auto.assign = FALSE)
    
    if (!is.null(vxz_data) && !is.null(svxy_data) && 
        nrow(vxz_data) > 10 && nrow(svxy_data) > 10) {
      vxz_close <- Cl(vxz_data)
      svxy_close <- Cl(svxy_data)
      
      # Get common dates
      common_dates <- intersect(index(vxz_close), index(svxy_close))
      if (length(common_dates) > 0) {
        # Calculate ratio (higher = more puts relative to calls)
        pc_ratio_data <- vxz_close[common_dates] / svxy_close[common_dates]
        colnames(pc_ratio_data) <- "PUT_CALL_RATIO"
        cat(sprintf("Successfully created Put/Call ratio proxy: %d observations\n", nrow(pc_ratio_data)))
        return(pc_ratio_data)
      }
    }
    
    # Alternative proxy: VIX/SPY ratio
    cat("Trying alternative Put/Call proxy (VIX/SPY ratio)...\n")
    vix_data <- getSymbols("^VIX", src = "yahoo", from = from - 30, to = to + 5, auto.assign = FALSE)
    spy_data <- getSymbols("SPY", src = "yahoo", from = from - 30, to = to + 5, auto.assign = FALSE)
    
    if (!is.null(vix_data) && !is.null(spy_data) && 
        nrow(vix_data) > 10 && nrow(spy_data) > 10) {
      vix_close <- Cl(vix_data)
      spy_close <- Cl(spy_data)
      
      # Get common dates
      common_dates <- intersect(index(vix_close), index(spy_close))
      if (length(common_dates) > 0) {
        # Normalize both series
        vix_norm <- vix_close[common_dates] / mean(vix_close[common_dates])
        spy_norm <- spy_close[common_dates] / mean(spy_close[common_dates])
        
        # Calculate ratio (higher VIX relative to SPY = more fear = more puts)
        pc_ratio_data <- vix_norm / spy_norm
        colnames(pc_ratio_data) <- "PUT_CALL_RATIO"
        cat(sprintf("Successfully created VIX/SPY ratio proxy: %d observations\n", nrow(pc_ratio_data)))
        return(pc_ratio_data)
      }
    }
    
    stop("Could not create Put/Call ratio proxy")
  }, error = function(e) {
    cat("Error creating Put/Call ratio:", e$message, "\n")
    return(NULL)
  })
  
  # Try to get AAII Bull/Bear Sentiment using an ETF proxy
  # Note: There's no direct ticker for AAII sentiment, so we use proxies
  aaii_sentiment <- tryCatch({
    cat("Creating AAII Bull/Bear proxy from ETF data...\n")
    
    # We'll use a consumer confidence ETF and market momentum ETF ratio as a proxy
    # Since no direct ETF exists for AAII sentiment, use momentum vs defensive ETFs
    momentum_etf <- getSymbols("MTUM", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)
    defensive_etf <- getSymbols("USMV", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)
    
    if (!is.null(momentum_etf) && !is.null(defensive_etf) && 
        nrow(momentum_etf) > 10 && nrow(defensive_etf) > 10) {
      momentum_close <- Cl(momentum_etf)
      defensive_close <- Cl(defensive_etf)
      
      # Get common dates
      common_dates <- intersect(index(momentum_close), index(defensive_close))
      if (length(common_dates) > 0) {
        # Calculate 30-day performance of each
        momentum_perf <- momentum_close[common_dates] / lag(momentum_close[common_dates], 30)
        defensive_perf <- defensive_close[common_dates] / lag(defensive_close[common_dates], 30)
        
        # Calculate ratio (higher = more bullish)
        aaii_data <- momentum_perf / defensive_perf
        colnames(aaii_data) <- "AAII_BULL_BEAR"
        cat(sprintf("Successfully created AAII Bull/Bear proxy: %d observations\n", nrow(aaii_data)))
        return(aaii_data)
      }
    }
    
    # Alternative: Use SPY/TLT ratio as risk appetite indicator
    cat("Trying alternative AAII proxy (SPY/TLT ratio)...\n")
    spy_data <- getSymbols("SPY", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)
    tlt_data <- getSymbols("TLT", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)
    
    if (!is.null(spy_data) && !is.null(tlt_data) && 
        nrow(spy_data) > 10 && nrow(tlt_data) > 10) {
      spy_close <- Cl(spy_data)
      tlt_close <- Cl(tlt_data)
      
      # Get common dates
      common_dates <- intersect(index(spy_close), index(tlt_close))
      if (length(common_dates) > 0) {
        # Calculate 30-day performance of each
        spy_perf <- spy_close[common_dates] / lag(spy_close[common_dates], 30)
        tlt_perf <- tlt_close[common_dates] / lag(tlt_close[common_dates], 30)
        
        # Calculate ratio (higher = more bullish)
        aaii_data <- spy_perf / tlt_perf
        colnames(aaii_data) <- "AAII_BULL_BEAR"
        cat(sprintf("Successfully created SPY/TLT risk appetite indicator: %d observations\n", nrow(aaii_data)))
        return(aaii_data)
      }
    }
    
    stop("Could not create AAII sentiment proxy")
  }, error = function(e) {
    cat("Error creating AAII sentiment proxy:", e$message, "\n")
    return(NULL)
  })
  
  # Align sentiment data with price dates
  if (!is.null(pc_ratio)) {
    for (i in 1:length(price_dates)) {
      price_date <- price_dates[i]
      
      if (price_date %in% index(pc_ratio)) {
        sentiment_data[i, "PUT_CALL_RATIO"] <- as.numeric(pc_ratio[price_date, "PUT_CALL_RATIO"])
      } else {
        earlier_dates <- index(pc_ratio)[index(pc_ratio) <= price_date]
        if (length(earlier_dates) > 0) {
          closest_earlier <- max(earlier_dates)
          sentiment_data[i, "PUT_CALL_RATIO"] <- as.numeric(pc_ratio[closest_earlier, "PUT_CALL_RATIO"])
        }
      }
    }
  }
  
  if (!is.null(aaii_sentiment)) {
    for (i in 1:length(price_dates)) {
      price_date <- price_dates[i]
      
      if (price_date %in% index(aaii_sentiment)) {
        sentiment_data[i, "AAII_BULL_BEAR"] <- as.numeric(aaii_sentiment[price_date, "AAII_BULL_BEAR"])
      } else {
        earlier_dates <- index(aaii_sentiment)[index(aaii_sentiment) <= price_date]
        if (length(earlier_dates) > 0) {
          closest_earlier <- max(earlier_dates)
          sentiment_data[i, "AAII_BULL_BEAR"] <- as.numeric(aaii_sentiment[closest_earlier, "AAII_BULL_BEAR"])
        }
      }
    }
  }
  
  # Fill any NA values
  sentiment_data <- na.locf(sentiment_data, na.rm = FALSE)
  sentiment_data <- na.locf(sentiment_data, fromLast = TRUE, na.rm = FALSE)
  
  cat(sprintf("Final sentiment data: %d rows, NA count: %d\n", 
              nrow(sentiment_data), sum(is.na(sentiment_data))))
  
  return(sentiment_data)
}

# Load macroeconomic data using ETF proxies
load_macro_indicators <- function(price_dates, from = min(price_dates) - 60, to = max(price_dates) + 5) {
  cat("Loading macroeconomic indicators...\n")
  
  # Ensure dates are proper Date objects
  price_dates <- as.Date(price_dates)
  from <- as.Date(from)
  to <- as.Date(to)
  
  # Create templates for macro indicators
  macro_data <- xts(matrix(NA, nrow=length(price_dates), ncol=3), 
                    order.by=price_dates)
  colnames(macro_data) <- c("GDP_PROXY", "INFLATION_SURPRISE", "UNEMPLOYMENT_PROXY")
  
  # GDP Growth Proxy: Use SPY/IEF ratio as economic growth indicator
  gdp_proxy <- tryCatch({
    cat("Creating GDP growth proxy from ETF data...\n")
    spy_data <- getSymbols("SPY", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)
    ief_data <- getSymbols("IEF", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)
    
    if (!is.null(spy_data) && !is.null(ief_data) && 
        nrow(spy_data) > 10 && nrow(ief_data) > 10) {
      spy_close <- Cl(spy_data)
      ief_close <- Cl(ief_data)
      
      # Get common dates
      common_dates <- intersect(index(spy_close), index(ief_close))
      if (length(common_dates) > 0) {
        # Calculate 90-day performance ratio (longer period for GDP proxy)
        spy_90d <- spy_close[common_dates] / lag(spy_close[common_dates], 90)
        ief_90d <- ief_close[common_dates] / lag(ief_close[common_dates], 90)
        
        # GDP proxy (higher = stronger growth)
        gdp_data <- spy_90d / ief_90d
        colnames(gdp_data) <- "GDP_PROXY"
        cat(sprintf("Successfully created GDP growth proxy: %d observations\n", nrow(gdp_data)))
        return(gdp_data)
      }
    }
    
    # Alternative: Use cyclical vs defensive sectors
    cat("Trying alternative GDP proxy (XLY/XLP ratio)...\n")
    xly_data <- getSymbols("XLY", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)  # Consumer Discretionary
    xlp_data <- getSymbols("XLP", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)  # Consumer Staples
    
    if (!is.null(xly_data) && !is.null(xlp_data) && 
        nrow(xly_data) > 10 && nrow(xlp_data) > 10) {
      xly_close <- Cl(xly_data)
      xlp_close <- Cl(xlp_data)
      
      # Get common dates
      common_dates <- intersect(index(xly_close), index(xlp_close))
      if (length(common_dates) > 0) {
        # Calculate ratio (higher = stronger growth)
        gdp_data <- xly_close[common_dates] / xlp_close[common_dates]
        colnames(gdp_data) <- "GDP_PROXY"
        cat(sprintf("Successfully created XLY/XLP GDP proxy: %d observations\n", nrow(gdp_data)))
        return(gdp_data)
      }
    }
    
    stop("Could not create GDP proxy")
  }, error = function(e) {
    cat("Error creating GDP proxy:", e$message, "\n")
    return(NULL)
  })
  
  # Inflation Surprise: Use TIP/IEF breakeven as inflation indicator
  inflation_proxy <- tryCatch({
    cat("Creating inflation surprise proxy from ETF data...\n")
    tip_data <- getSymbols("TIP", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)
    ief_data <- getSymbols("IEF", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)
    
    if (!is.null(tip_data) && !is.null(ief_data) && 
        nrow(tip_data) > 10 && nrow(ief_data) > 10) {
      tip_close <- Cl(tip_data)
      ief_close <- Cl(ief_data)
      
      # Get common dates
      common_dates <- intersect(index(tip_close), index(ief_close))
      if (length(common_dates) > 0) {
        # Calculate ratio of TIPS to regular Treasuries
        raw_ratio <- tip_close[common_dates] / ief_close[common_dates]
        
        # Inflation surprise = current ratio relative to 60-day moving average
        # Higher = inflation coming in higher than expected
        ma60 <- SMA(raw_ratio, n = 60)
        inflation_data <- raw_ratio / ma60
        colnames(inflation_data) <- "INFLATION_SURPRISE"
        cat(sprintf("Successfully created inflation surprise indicator: %d observations\n", nrow(inflation_data)))
        return(inflation_data)
      }
    }
    
    # Alternative: Use gold/treasury ratio
    cat("Trying alternative inflation proxy (GLD/IEF ratio)...\n")
    gld_data <- getSymbols("GLD", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)
    ief_data <- getSymbols("IEF", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)
    
    if (!is.null(gld_data) && !is.null(ief_data) && 
        nrow(gld_data) > 10 && nrow(ief_data) > 10) {
      gld_close <- Cl(gld_data)
      ief_close <- Cl(ief_data)
      
      # Get common dates
      common_dates <- intersect(index(gld_close), index(ief_close))
      if (length(common_dates) > 0) {
        # Calculate ratio
        raw_ratio <- gld_close[common_dates] / ief_close[common_dates]
        
        # Inflation surprise = current ratio relative to 60-day moving average
        ma60 <- SMA(raw_ratio, n = 60)
        inflation_data <- raw_ratio / ma60
        colnames(inflation_data) <- "INFLATION_SURPRISE"
        cat(sprintf("Successfully created GLD/IEF inflation proxy: %d observations\n", nrow(inflation_data)))
        return(inflation_data)
      }
    }
    
    stop("Could not create inflation surprise proxy")
  }, error = function(e) {
    cat("Error creating inflation surprise proxy:", e$message, "\n")
    return(NULL)
  })
  
  # Unemployment Proxy: Use Industrial/Utility ratio as jobs indicator
  unemployment_proxy <- tryCatch({
    cat("Creating unemployment proxy from ETF data...\n")
    xli_data <- getSymbols("XLI", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)  # Industrials
    xlu_data <- getSymbols("XLU", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)  # Utilities
    
    if (!is.null(xli_data) && !is.null(xlu_data) && 
        nrow(xli_data) > 10 && nrow(xlu_data) > 10) {
      xli_close <- Cl(xli_data)
      xlu_close <- Cl(xlu_data)
      
      # Get common dates
      common_dates <- intersect(index(xli_close), index(xlu_close))
      if (length(common_dates) > 0) {
        # Calculate ratio (higher industrials relative to utilities = stronger job market)
        job_data <- xli_close[common_dates] / xlu_close[common_dates]
        
        # Invert so higher = higher unemployment (to match variable name)
        unemployment_data <- 1 / job_data
        colnames(unemployment_data) <- "UNEMPLOYMENT_PROXY"
        cat(sprintf("Successfully created unemployment proxy: %d observations\n", nrow(unemployment_data)))
        return(unemployment_data)
      }
    }
    
    # Alternative: Use consumer discretionary vs financial sector
    cat("Trying alternative unemployment proxy (XLY/XLF ratio)...\n")
    xly_data <- getSymbols("XLY", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)  # Consumer Discretionary
    xlf_data <- getSymbols("XLF", src = "yahoo", from = from - 60, to = to + 5, auto.assign = FALSE)  # Financial
    
    if (!is.null(xly_data) && !is.null(xlf_data) && 
        nrow(xly_data) > 10 && nrow(xlf_data) > 10) {
      xly_close <- Cl(xly_data)
      xlf_close <- Cl(xlf_data)
      
      # Get common dates
      common_dates <- intersect(index(xly_close), index(xlf_close))
      if (length(common_dates) > 0) {
        # Calculate ratio (invert so higher = higher unemployment)
        unemployment_data <- xlf_close[common_dates] / xly_close[common_dates]
        colnames(unemployment_data) <- "UNEMPLOYMENT_PROXY"
        cat(sprintf("Successfully created XLF/XLY unemployment proxy: %d observations\n", nrow(unemployment_data)))
        return(unemployment_data)
      }
    }
    
    stop("Could not create unemployment proxy")
  }, error = function(e) {
    cat("Error creating unemployment proxy:", e$message, "\n")
    return(NULL)
  })
  
  # Align macro data with price dates
  if (!is.null(gdp_proxy)) {
    for (i in 1:length(price_dates)) {
      price_date <- price_dates[i]
      
      if (price_date %in% index(gdp_proxy)) {
        macro_data[i, "GDP_PROXY"] <- as.numeric(gdp_proxy[price_date, "GDP_PROXY"])
      } else {
        earlier_dates <- index(gdp_proxy)[index(gdp_proxy) <= price_date]
        if (length(earlier_dates) > 0) {
          closest_earlier <- max(earlier_dates)
          macro_data[i, "GDP_PROXY"] <- as.numeric(gdp_proxy[closest_earlier, "GDP_PROXY"])
        }
      }
    }
  }
  
  if (!is.null(inflation_proxy)) {
    for (i in 1:length(price_dates)) {
      price_date <- price_dates[i]
      
      if (price_date %in% index(inflation_proxy)) {
        macro_data[i, "INFLATION_SURPRISE"] <- as.numeric(inflation_proxy[price_date, "INFLATION_SURPRISE"])
      } else {
        earlier_dates <- index(inflation_proxy)[index(inflation_proxy) <= price_date]
        if (length(earlier_dates) > 0) {
          closest_earlier <- max(earlier_dates)
          macro_data[i, "INFLATION_SURPRISE"] <- as.numeric(inflation_proxy[closest_earlier, "INFLATION_SURPRISE"])
        }
      }
    }
  }
  
  if (!is.null(unemployment_proxy)) {
    for (i in 1:length(price_dates)) {
      price_date <- price_dates[i]
      
      if (price_date %in% index(unemployment_proxy)) {
        macro_data[i, "UNEMPLOYMENT_PROXY"] <- as.numeric(unemployment_proxy[price_date, "UNEMPLOYMENT_PROXY"])
      } else {
        earlier_dates <- index(unemployment_proxy)[index(unemployment_proxy) <= price_date]
        if (length(earlier_dates) > 0) {
          closest_earlier <- max(earlier_dates)
          macro_data[i, "UNEMPLOYMENT_PROXY"] <- as.numeric(unemployment_proxy[closest_earlier, "UNEMPLOYMENT_PROXY"])
        }
      }
    }
  }
  
  # Fill any NA values
  macro_data <- na.locf(macro_data, na.rm = FALSE)
  macro_data <- na.locf(macro_data, fromLast = TRUE, na.rm = FALSE)
  
  cat(sprintf("Final macroeconomic data: %d rows, NA count: %d\n", 
              nrow(macro_data), sum(is.na(macro_data))))
  
  return(macro_data)
}

# Get economic indicators and create market data - NO SYNTHETIC DATA
get_economic_indicators <- function(start_date, end_date = Sys.Date(), price_dates = NULL,
                                    enhanced = TRUE) {
  cat("\nCreating economic indicators from market data...\n")
  
  # Use price_dates if provided, otherwise create business day sequence
  if (!is.null(price_dates)) {
    cat("Using provided price dates for alignment\n")
    target_dates <- as.Date(price_dates)
  } else {
    # Generate business days date range
    target_dates <- seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day")
    target_dates <- target_dates[!weekdays(target_dates) %in% c("Saturday", "Sunday")]
  }
  
  # Get VIX data
  vix_data <- get_vix_aligned(target_dates, 
                              from = as.Date(start_date) - 30, 
                              to = as.Date(end_date) + 5)
  
  # If VIX loading failed, we need to stop
  if (is.null(vix_data)) {
    stop("Critical failure: Unable to load VIX data from any source")
  }
  
  # Load ETFs for economic indicators
  etf_tickers <- c("SPY", "IEF", "GLD", "LQD", "IWM", "TIP", "EEM", "EFA", "DBC", 
                   "SHY", "TLT", "HYG", "VCSH", "XLY", "XLP", "XLI", "XLU", "XLF")
  
  # Load ETF data
  etf_prices <- load_market_data(etf_tickers, 
                                 start_date = as.Date(start_date) - 60,
                                 end_date = as.Date(end_date) + 5,
                                 include_inverse = FALSE)  # Don't need inverse ETFs here
  
  if (is.null(etf_prices)) {
    stop("Critical failure: Unable to load ETF data for economic indicators")
  }
  
  # Load sentiment indicators
  sentiment_data <- load_sentiment_data(target_dates,
                                        from = as.Date(start_date) - 60,
                                        to = as.Date(end_date) + 5)
  
  # Load macroeconomic indicators
  macro_data <- load_macro_indicators(target_dates,
                                      from = as.Date(start_date) - 60,
                                      to = as.Date(end_date) + 5)
  
  # Initialize indicators with VIX
  indicators <- vix_data
  
  # Process ETF data to create indicators
  if (!is.null(etf_prices) && ncol(etf_prices) >= 2) {
    # Fill any NAs
    for (col in colnames(etf_prices)) {
      if (any(is.na(etf_prices[, col]))) {
        etf_prices[, col] <- na.locf(etf_prices[, col], na.rm = FALSE)
        etf_prices[, col] <- na.locf(etf_prices[, col], fromLast = TRUE, na.rm = FALSE)
      }
    }
    
    # Calculate returns
    etf_returns <- ROC(etf_prices, type = "discrete")
    etf_returns <- na.omit(etf_returns)
    
    # Create indicators ONLY if the required ETFs exist
    # 1. Inflation indicator
    if (all(c("TIP", "IEF") %in% colnames(etf_prices))) {
      cat("Creating inflation indicator (TIP/IEF ratio)...\n")
      try({
        inflation_ratio <- etf_prices[, "TIP"] / etf_prices[, "IEF"]
        colnames(inflation_ratio) <- "CPI_YOY"
        indicators <- merge(indicators, inflation_ratio)
      }, silent = TRUE)
    }
    
    # 2. Growth indicator
    if (all(c("IWM", "IEF") %in% colnames(etf_prices))) {
      cat("Creating growth indicator (IWM/IEF ratio)...\n")
      try({
        growth_ratio <- etf_prices[, "IWM"] / etf_prices[, "IEF"]
        colnames(growth_ratio) <- "PMI"
        indicators <- merge(indicators, growth_ratio)
      }, silent = TRUE)
    }
    
    # 3. Bond-equity correlation (critical for regime detection)
    if (all(c("IEF", "SPY") %in% colnames(etf_returns))) {
      cat("Creating bond-equity correlation indicator...\n")
      try({
        # Get returns for bond and equity
        bond_returns <- etf_returns[, "IEF"]
        equity_returns <- etf_returns[, "SPY"]
        
        # Combine and calculate rolling correlation
        combined_returns <- merge(bond_returns, equity_returns)
        colnames(combined_returns) <- c("bond", "equity")
        
        # Use a reasonable correlation window
        corr_window <- min(60, nrow(combined_returns) - 10)
        if (corr_window > 10) {
          roll_corr <- rollapply(combined_returns, width = corr_window, 
                                 function(x) {
                                   cor(x[,"bond"], x[,"equity"], 
                                       use = "pairwise.complete.obs")
                                 }, 
                                 by.column = FALSE, align = "right")
          
          corr_xts <- xts(roll_corr, order.by = index(roll_corr))
          colnames(corr_xts) <- "BOND_EQUITY_CORR"
          indicators <- merge(indicators, corr_xts)
        }
      }, silent = TRUE)
    }
    
    # 4. Global growth indicator (if international ETFs available)
    if (all(c("EFA", "EEM", "IEF") %in% colnames(etf_prices))) {
      cat("Creating global growth indicator...\n")
      try({
        global_equity_avg <- (etf_prices[, "EFA"] + etf_prices[, "EEM"]) / 2
        global_growth_ratio <- global_equity_avg / etf_prices[, "IEF"]
        colnames(global_growth_ratio) <- "GLOBAL_PMI"
        indicators <- merge(indicators, global_growth_ratio)
      }, silent = TRUE)
    }
    
    # 5. Commodity trend
    if ("DBC" %in% colnames(etf_prices)) {
      cat("Creating commodity trend indicator...\n")
      try({
        # 60-day momentum
        dbc_mom <- ROC(etf_prices[, "DBC"], n = 60, type = "discrete")
        colnames(dbc_mom) <- "COMMODITY_TREND"
        indicators <- merge(indicators, dbc_mom)
      }, silent = TRUE)
    }
    
    # Enhanced indicators when requested
    if (enhanced) {
      # 6. Yield Curve indicator
      if (all(c("SHY", "IEF", "TLT") %in% colnames(etf_prices))) {
        cat("Creating yield curve indicator...\n")
        try({
          # Simple yield curve proxy: ratio of long to short bonds
          # When this ratio rises, yield curve is steepening
          # When falling, yield curve is flattening or inverting
          yield_curve <- etf_prices[, "TLT"] / etf_prices[, "SHY"]
          colnames(yield_curve) <- "YIELD_CURVE"
          indicators <- merge(indicators, yield_curve)
        }, silent = TRUE)
      }
      
      # 7. Credit spread indicator
      if (all(c("LQD", "HYG", "IEF") %in% colnames(etf_prices))) {
        cat("Creating credit spread indicator...\n")
        try({
          # When HYG underperforms LQD relative to treasuries, credit spreads are widening
          ig_vs_treasury <- etf_prices[, "LQD"] / etf_prices[, "IEF"]
          hy_vs_treasury <- etf_prices[, "HYG"] / etf_prices[, "IEF"]
          credit_spread <- ig_vs_treasury / hy_vs_treasury
          colnames(credit_spread) <- "CREDIT_SPREAD"
          indicators <- merge(indicators, credit_spread)
        }, silent = TRUE)
      }
      
      # 8. Sector rotation indicators
      if (all(c("XLY", "XLP", "XLI", "XLU") %in% colnames(etf_prices))) {
        cat("Creating sector rotation indicators...\n")
        try({
          # Cyclical vs defensive ratio
          cyclical_def_ratio <- (etf_prices[, "XLY"] + etf_prices[, "XLI"]) / 
            (etf_prices[, "XLP"] + etf_prices[, "XLU"])
          colnames(cyclical_def_ratio) <- "CYCLICAL_DEF_RATIO"
          indicators <- merge(indicators, cyclical_def_ratio)
        }, silent = TRUE)
      }
    }
  }
  
  # Add sentiment indicators if available
  if (!is.null(sentiment_data)) {
    cat("Adding market sentiment indicators...\n")
    # Align dates with main indicators
    common_dates <- intersect(index(indicators), index(sentiment_data))
    if (length(common_dates) > 0) {
      indicators <- merge(indicators, sentiment_data[common_dates])
    }
  }
  
  # Add macroeconomic indicators if available
  if (!is.null(macro_data)) {
    cat("Adding macroeconomic indicators...\n")
    # Align dates with main indicators
    common_dates <- intersect(index(indicators), index(macro_data))
    if (length(common_dates) > 0) {
      indicators <- merge(indicators, macro_data[common_dates])
    }
  }
  
  # Ensure all our target dates are covered by aligning indicators
  aligned_indicators <- xts(matrix(NA, nrow = length(target_dates), 
                                   ncol = ncol(indicators)),
                            order.by = target_dates)
  colnames(aligned_indicators) <- colnames(indicators)
  
  # For each target date, find the closest indicator date
  for (i in 1:length(target_dates)) {
    date <- target_dates[i]
    
    # If exact date exists, use it
    if (date %in% index(indicators)) {
      for (col in colnames(indicators)) {
        aligned_indicators[i, col] <- indicators[date, col]
      }
    } else {
      # Find closest earlier date
      earlier_dates <- index(indicators)[index(indicators) <= date]
      
      if (length(earlier_dates) > 0) {
        closest_date <- max(earlier_dates)
        for (col in colnames(indicators)) {
          aligned_indicators[i, col] <- indicators[closest_date, col]
        }
      }
    }
  }
  
  # Fill all NAs with LOCF and then LOCB
  aligned_indicators <- na.locf(aligned_indicators, na.rm = FALSE)
  aligned_indicators <- na.locf(aligned_indicators, fromLast = TRUE, na.rm = FALSE)
  
  # Check for any still-missing values and fill with column medians
  for (col in colnames(aligned_indicators)) {
    if (any(is.na(aligned_indicators[, col]))) {
      median_val <- median(aligned_indicators[, col], na.rm = TRUE)
      aligned_indicators[is.na(aligned_indicators[, col]), col] <- median_val
    }
  }
  
  # Make sure we have all required columns for regime detection
  required_cols <- c("VIX", "PMI", "CPI_YOY")
  missing_cols <- required_cols[!required_cols %in% colnames(aligned_indicators)]
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required indicator columns:", paste(missing_cols, collapse=", ")))
  }
  
  cat(sprintf("Final indicators: %d rows × %d columns\n", 
              nrow(aligned_indicators), ncol(aligned_indicators)))
  cat("Indicator columns:", paste(colnames(aligned_indicators), collapse=", "), "\n")
  
  return(aligned_indicators)
}

# Create market data with guaranteed perfect alignment
create_market_data <- function(prices, enhanced_indicators = TRUE) {
  cat("\nCreating market data with guaranteed alignment...\n")
  
  # Safety checks
  if (is.null(prices) || nrow(prices) == 0) {
    stop("Cannot create market data - price data is empty")
  }
  
  # Ensure prices has proper dates
  prices <- ensure_date_index(prices)
  price_dates <- index(prices)
  
  # Get indicators for exact price dates with enhanced mode if requested
  indicators <- get_economic_indicators(
    start_date = min(price_dates),
    end_date = max(price_dates),
    price_dates = price_dates,
    enhanced = enhanced_indicators
  )
  
  # Verify dimensions match exactly
  if (nrow(indicators) != nrow(prices)) {
    stop(sprintf("Dimension mismatch! Prices: %d rows, Indicators: %d rows", 
                 nrow(prices), nrow(indicators)))
  }
  
  # CRITICAL FIX: Create market data with guaranteed perfect alignment
  # Copy all price data first
  market_data <- prices
  
  # Carefully add indicators one by one
  for (col in colnames(indicators)) {
    # Skip if column already exists
    if (col %in% colnames(market_data)) {
      cat(sprintf("Column %s already exists in market data, skipping\n", col))
      next
    }
    
    # Add column
    market_data[, col] <- indicators[, col]
  }
  
  # Add attribute to identify inverse ETFs if any exist
  if (!is.null(attr(prices, "inverse_etf_map"))) {
    attr(market_data, "inverse_etf_map") <- attr(prices, "inverse_etf_map")
  }
  
  cat("Market data created successfully with perfect alignment\n")
  return(market_data)
}

# Enhanced transaction cost estimation (continued)
get_transaction_costs <- function(tickers) {
  # Safety check for empty tickers
  if (is.null(tickers) || length(tickers) == 0) {
    warning("No tickers provided for transaction costs")
    return(numeric(0))
  }
  
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
    
    # Aggregate/Balanced ETFs
    "AGG" = 1.8,    # US Aggregate Bond
    "BND" = 1.8,    # US Aggregate Bond alternative
    "BNDX" = 2.5,   # International Bond
    "AOA" = 2.0,    # Aggressive allocation
    "AOR" = 2.0,    # Moderate allocation
    
    # Inverse ETFs - Always higher costs due to less liquidity
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
  
  # Determine asset class for unknown tickers based on name pattern
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

cat("\nPART 1 LOADED: Data handling and market data preparation\n")
#=============================================================================
# ENHANCED RISK PARITY PART 2: REGIME DETECTION AND RISK PARITY
# - REAL DATA VERSION WITH NO SYNTHETIC GENERATION
# - Improved GARCH convergence handling
# - Smooth regime transitions with probability weighting
# - Support for short overlays through inverse ETFs
# - Comprehensive transaction cost modeling
#=============================================================================

#=============================================================================
# VOLATILITY FORECASTING - IMPROVED FOR BETTER CONVERGENCE
#=============================================================================

# Helper function to safely process regime probabilities
ensure_regime_probabilities <- function(probs) {
  # Default empty result - single row data frame with standard regime columns
  default_cols <- c("growth", "reflation", "deflation", "stagflation", "risk_off", "inflation_shock")
  default_result <- as.data.frame(matrix(0, nrow=1, ncol=length(default_cols)))
  colnames(default_result) <- default_cols
  default_result$growth <- 1.0  # Default to growth regime
  
  if (is.null(probs)) {
    return(default_result)
  }
  
  # Handle different input types
  if (is.data.frame(probs)) {
    # Ensure no empty column names
    if (any(colnames(probs) == "")) {
      warning("Empty column names detected in regime probabilities, removing")
      probs <- probs[, colnames(probs) != ""]
    }
    return(probs)
  } else if (is.list(probs)) {
    # Convert list to data frame
    # First check for empty names
    if (any(names(probs) == "" | is.null(names(probs)))) {
      warning("Empty list names detected in regime probabilities, removing")
      probs <- probs[names(probs) != ""]
    }
    df <- as.data.frame(t(as.numeric(probs)))
    colnames(df) <- names(probs)
    return(df)
  } else {
    warning("Unknown regime probability format, returning default")
    return(default_result)
  }
}

# Robust GARCH volatility forecasting with multiple fallbacks
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
      if (length(return_series) < 60) { # Increased minimum data requirement for stability
        forecasted_vols[col, "forecasted_vol"] <- sd(return_series, na.rm = TRUE) * sqrt(252)
        forecasted_vols[col, "method_used"] <- "historical"
        next
      }
      
      # Try multiple GARCH specifications for better convergence
      garch_models <- list(
        # Standard GARCH(1,1) with normal distribution - fastest but least robust
        normal = list(
          spec = ugarchspec(
            variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
            mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
            distribution.model = "norm"
          ),
          solver = "hybrid"
        ),
        
        # GARCH(1,1) with Student-t distribution - better for fat tails
        student = list(
          spec = ugarchspec(
            variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
            mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
            distribution.model = "std"
          ),
          solver = "hybrid"
        ),
        
        # GJR-GARCH for leverage effects (asymmetric volatility)
        gjr = list(
          spec = ugarchspec(
            variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
            mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
            distribution.model = "std"
          ),
          solver = "hybrid"
        ),
        
        # eGARCH - another option for asymmetric volatility
        egarch = list(
          spec = ugarchspec(
            variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
            mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
            distribution.model = "std"
          ),
          solver = "solnp"
        )
      )
      
      # Try each GARCH model until one converges
      for (model_name in names(garch_models)) {
        # Extract model specification
        model_spec <- garch_models[[model_name]]$spec
        model_solver <- garch_models[[model_name]]$solver
        
        # Attempt to fit the model with controlled warnings
        withCallingHandlers({
          garch_fit <- try(ugarchfit(
            spec = model_spec,
            data = return_series,
            solver = model_solver,
            solver.control = list(tol = 1e-6, delta = 1e-9)
          ), silent = TRUE)
          
          # Check if model converged successfully
          if (!inherits(garch_fit, "try-error") && garch_fit@fit$convergence) {
            # Forecast volatility
            garch_forecast <- ugarchforecast(garch_fit, n.ahead = forecast_horizon)
            forecast_sigma <- as.numeric(sigma(garch_forecast)[forecast_horizon])
            
            # Annualize the volatility forecast
            forecasted_vols[col, "forecasted_vol"] <- forecast_sigma * sqrt(252)
            forecasted_vols[col, "method_used"] <- paste0("GARCH-", model_name)
            
            # Break the loop as we have a successful model
            break
          }
        }, warning = function(w) {
          # Suppress specific GARCH warnings to reduce output noise
          if (grepl("failed to invert hessian", w$message, ignore.case = TRUE)) {
            invokeRestart("muffleWarning")
          }
        })
      }
      
      # Check if we have a forecast after trying all GARCH models
      if (forecasted_vols[col, "method_used"] == "") {
        # Fall back to EWMA if all GARCH models failed
        cat(sprintf("GARCH failed for %s, trying EWMA\n", col))
        
        # Calculate EWMA variance
        lambda <- 0.94  # RiskMetrics standard
        weights <- lambda^(0:(length(return_series)-1))
        weights <- rev(weights / sum(weights))  # Normalize and reverse
        
        # Calculate weighted variance
        ewma_var <- sum(weights * return_series^2, na.rm = TRUE)
        forecast_vol <- sqrt(ewma_var) * sqrt(252)
        
        forecasted_vols[col, "forecasted_vol"] <- forecast_vol
        forecasted_vols[col, "method_used"] <- "EWMA"
      }
      
    }, error = function(e) {
      # If all else fails, use historical volatility
      cat(sprintf("All volatility methods failed for %s: %s, using historical\n", col, e$message))
      hist_vol <- sd(returns[, col], na.rm = TRUE) * sqrt(252)
      forecasted_vols[col, "forecasted_vol"] <- hist_vol
      forecasted_vols[col, "method_used"] <- "historical"
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
# RISK ESTIMATION - ROBUST COVARIANCE ESTIMATION
#=============================================================================

# EWMA covariance estimation with extensive numerical stability enhancements
estimate_ewma_covariance <- function(returns, lambda = 0.94) {
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
  
  # Need at least 60 observations for a stable estimate
  if (n_obs < 60) {
    warning("EWMA needs at least 60 observations - using robust sample covariance")
    
    # Use robust covariance (minimum covariance determinant) instead
    tryCatch({
      if (requireNamespace("robustbase", quietly = TRUE)) {
        rob_cov <- robustbase::covMcd(returns)$cov
        return(rob_cov)
      } else {
        return(cov(returns))
      }
    }, error = function(e) {
      return(cov(returns))
    })
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
  
  # SAFER IMPLEMENTATION: Calculate EWMA directly for each element
  try({
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
  }, silent = FALSE)
  
  # NUMERICAL STABILITY: Ensure the matrix is positive definite
  eigen_vals <- eigen(cov_matrix, only.values = TRUE)$values
  if (min(eigen_vals) <= 0) {
    cat("EWMA matrix not positive definite, applying regularization\n")
    
    # Apply multiple stabilization techniques
    
    # 1. First try shrinkage toward well-conditioned target
    target <- diag(diag(cov_matrix))
    shrink_factor <- 0.1  # 10% shrinkage
    cov_matrix <- (1 - shrink_factor) * cov_matrix + shrink_factor * target
    
    # 2. Verify it worked, if not add small constant to diagonal
    eigen_vals2 <- eigen(cov_matrix, only.values = TRUE)$values
    if (min(eigen_vals2) <= 0) {
      cat("Additional regularization needed\n")
      epsilon <- 1e-5 * mean(diag(cov_matrix))
      diag(cov_matrix) <- diag(cov_matrix) + epsilon
      
      # 3. Final check - if still not positive definite, use robust alternative
      eigen_vals3 <- eigen(cov_matrix, only.values = TRUE)$values
      if (min(eigen_vals3) <= 0) {
        cat("Final fallback: using shrinkage estimator\n")
        
        # Use shrinkage estimator
        if (requireNamespace("corpcor", quietly = TRUE)) {
          cov_matrix <- corpcor::cov.shrink(returns)
          colnames(cov_matrix) <- colnames(returns)
          rownames(cov_matrix) <- colnames(returns)
        } else {
          # Add larger constant to diagonal as last resort
          diag(cov_matrix) <- diag(cov_matrix) * 1.05
        }
      }
    }
  }
  
  # Final check for validity - if still problematic, fall back to sample cov with regularization
  if (any(is.na(cov_matrix)) || any(!is.finite(cov_matrix)) || min(eigen(cov_matrix)$values) <= 0) {
    cat("WARNING: EWMA estimation failed. Using sample covariance with regularization.\n")
    
    # Use sample covariance with diagonal boost
    cov_matrix <- cov(returns)
    diag(cov_matrix) <- diag(cov_matrix) * 1.05
  }
  
  # Print diagnostics about the resulting matrix
  cat(sprintf("EWMA covariance matrix: min eigenvalue = %.6g, max eigenvalue = %.6g\n", 
              min(eigen(cov_matrix)$values), max(eigen(cov_matrix)$values)))
  
  return(cov_matrix)
}

# Ultra-robust covariance estimation with multiple methods and fallbacks
estimate_robust_covariance <- function(returns, method = "ledoit-wolf") {
  # Safety check
  if (is.null(returns) || nrow(returns) < 10 || ncol(returns) < 2) {
    warning("Insufficient data for covariance estimation")
    # Return identity matrix as absolute fallback
    id_matrix <- diag(ncol(returns))
    colnames(id_matrix) <- colnames(returns)
    rownames(id_matrix) <- colnames(returns)
    return(id_matrix)
  }
  
  # Make sure we have sufficient non-NA data
  returns <- na.omit(returns)
  
  if (nrow(returns) < 10) {
    warning("Not enough data after NA removal, using diagonal covariance")
    
    # Use diagonal matrix with historical variances
    vars <- apply(returns, 2, var, na.rm = TRUE)
    vars[is.na(vars) | vars <= 0] <- 0.01^2  # Replace invalid with 1% daily variance
    
    diag_cov <- diag(vars)
    colnames(diag_cov) <- colnames(returns)
    rownames(diag_cov) <- colnames(returns)
    
    return(diag_cov)
  }
  
  # Try primary method
  cov_matrix <- NULL
  method_used <- method
  
  # Try the requested method first
  if (method == "ledoit-wolf") {
    tryCatch({
      if (requireNamespace("corpcor", quietly = TRUE)) {
        # Extract matrix from xts object
        returns_matrix <- as.matrix(returns)
        
        # Apply Ledoit-Wolf shrinkage
        cov_matrix <- corpcor::cov.shrink(returns_matrix, verbose = FALSE)
        
        # Return as matrix with column and row names preserved
        cov_matrix <- as.matrix(cov_matrix)
        colnames(cov_matrix) <- colnames(returns)
        rownames(cov_matrix) <- colnames(returns)
        
        # Print eigenvalues to check matrix quality
        eigen_values <- eigen(cov_matrix, only.values = TRUE)$values
        cat(sprintf("Ledoit-Wolf Covariance - Min eigenvalue: %.6f, Max eigenvalue: %.6f\n",
                    min(eigen_values), max(eigen_values)))
      } else {
        method_used <- "shrinkage-fallback"
      }
    }, error = function(e) {
      cat("Error with Ledoit-Wolf estimation:", e$message, "\n")
      method_used <- "shrinkage-fallback"
    })
  } else {
    method_used <- "shrinkage-fallback"
  }
  
  # Try alternate methods if primary failed
  if (is.null(cov_matrix) && method_used == "shrinkage-fallback") {
    tryCatch({
      cat("Using manual shrinkage estimation as fallback\n")
      
      # Calculate sample covariance
      sample_cov <- cov(returns)
      
      # Calculate target (diagonal matrix with sample variances)
      target <- diag(diag(sample_cov))
      
      # Apply shrinkage (30% toward target)
      alpha <- 0.3
      cov_matrix <- (1 - alpha) * sample_cov + alpha * target
      
      # Verify it's positive definite
      eigen_values <- eigen(cov_matrix, only.values = TRUE)$values
      if (min(eigen_values) <= 0) {
        # If not, increase shrinkage
        alpha <- 0.5
        cov_matrix <- (1 - alpha) * sample_cov + alpha * target
      }
      
      # Print diagnostics
      eigen_values <- eigen(cov_matrix, only.values = TRUE)$values
      cat(sprintf("Manual Shrinkage Covariance - Min eigenvalue: %.6f, Max eigenvalue: %.6f\n",
                  min(eigen_values), max(eigen_values)))
      
      method_used <- "manual-shrinkage"
    }, error = function(e) {
      cat("Error with manual shrinkage:", e$message, "\n")
      method_used <- "sample-cov"
    })
  }
  
  # Last resort: sample covariance with regularization
  if (is.null(cov_matrix)) {
    cat("Using regularized sample covariance as last resort\n")
    
    # Calculate sample covariance
    cov_matrix <- cov(returns)
    
    # Add small constant to diagonal
    epsilon <- 1e-5 * mean(diag(cov_matrix))
    diag(cov_matrix) <- diag(cov_matrix) + epsilon
    
    method_used <- "sample-with-regularization"
  }
  
  # Final check
  if (any(is.na(cov_matrix)) || any(!is.finite(cov_matrix))) {
    warning("Invalid values in covariance matrix after all attempts. Using diagonal matrix.")
    
    # Use diagonal matrix as last resort
    vars <- apply(returns, 2, var, na.rm = TRUE)
    vars[is.na(vars) | vars <= 0] <- 0.01^2  # Replace invalid with 1% daily variance
    
    cov_matrix <- diag(vars)
    method_used <- "diagonal-fallback"
  }
  
  # Set column and row names
  colnames(cov_matrix) <- colnames(returns)
  rownames(cov_matrix) <- colnames(returns)
  
  cat(sprintf("Final covariance method used: %s\n", method_used))
  return(cov_matrix)
}

#=============================================================================
# VISUALIZATION FUNCTIONS
#=============================================================================

# Function to plot correlation/covariance matrix as heatmap
plot_covariance_matrix <- function(cov_matrix, title = "Asset Correlation Matrix") {
  # Safety check
  if (is.null(cov_matrix) || !is.matrix(cov_matrix)) {
    stop("Invalid covariance matrix provided")
  }
  
  # Convert to correlation matrix if it's a covariance matrix
  if (mean(diag(cov_matrix)) > 0.1) {  # Heuristic to detect covariance vs correlation
    # Convert to correlation matrix safely
    diag_sqrt <- sqrt(pmax(diag(cov_matrix), 1e-8))  # Prevent division by zero
    corr_matrix <- cov_matrix / (diag_sqrt %*% t(diag_sqrt))
    
    # Ensure values are in valid range
    corr_matrix[corr_matrix > 1] <- 1
    corr_matrix[corr_matrix < -1] <- -1
  } else {
    corr_matrix <- cov_matrix  # Already a correlation matrix
  }
  
  # Ensure the matrix is symmetric and has proper names
  colnames(corr_matrix) <- rownames(corr_matrix)
  
  # Melt the matrix for ggplot safely
  corr_df <- tryCatch({
    reshape2::melt(corr_matrix)
  }, error = function(e) {
    # Fallback if reshape2 fails
    df <- data.frame(
      Asset1 = rep(rownames(corr_matrix), each = ncol(corr_matrix)),
      Asset2 = rep(colnames(corr_matrix), times = nrow(corr_matrix)),
      Correlation = as.vector(corr_matrix)
    )
    return(df)
  })
  
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
  # Safety check
  if (is.null(results_list) || length(results_list) == 0) {
    stop("Empty results list provided")
  }
  
  # Create data frame for plotting
  plot_data <- tryCatch({
    data.frame(Date = index(results_list[[1]]$cumulative_returns))
  }, error = function(e) {
    stop("Could not extract dates from cumulative returns")
  })
  
  # Add returns for each method
  for (method_name in names(results_list)) {
    tryCatch({
      method_returns <- results_list[[method_name]]$cumulative_returns
      plot_data[[method_name]] <- as.numeric(method_returns)
    }, error = function(e) {
      warning(sprintf("Could not extract returns for method %s: %s", 
                      method_name, e$message))
    })
  }
  
  # Check if we have any return data
  if (ncol(plot_data) <= 1) {
    stop("No valid return data found in results")
  }
  
  # Convert to long format safely
  plot_data_long <- tryCatch({
    reshape2::melt(plot_data, id.vars = "Date", 
                   variable.name = "Method", 
                   value.name = "Cumulative_Return")
  }, error = function(e) {
    # Manual reshape if reshape2 fails
    methods <- colnames(plot_data)[-1]  # All columns except Date
    n_methods <- length(methods)
    n_dates <- nrow(plot_data)
    
    long_df <- data.frame(
      Date = rep(plot_data$Date, times = n_methods),
      Method = rep(methods, each = n_dates),
      Cumulative_Return = numeric(n_dates * n_methods)
    )
    
    idx <- 1
    for (method in methods) {
      long_df$Cumulative_Return[((idx-1)*n_dates+1):(idx*n_dates)] <- plot_data[[method]]
      idx <- idx + 1
    }
    
    return(long_df)
  })
  
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

# Function to plot regime distribution over time with probabilities
plot_regime_distribution <- function(regime_history, regime_probs = NULL, 
                                     title = "Market Regimes Over Time") {
  # Safety check
  if (is.null(regime_history) || length(regime_history) == 0) {
    stop("No regime history data provided")
  }
  
  # Convert to data frame for ggplot
  regime_df <- tryCatch({
    data.frame(
      Date = index(regime_history),
      Regime = as.character(regime_history)
    )
  }, error = function(e) {
    stop(sprintf("Could not process regime history: %s", e$message))
  })
  
  # Define colors for different regimes
  regime_colors <- c(
    "growth" = "#4CAF50",         # Green
    "reflation" = "#FF9800",      # Orange
    "deflation" = "#2196F3",      # Blue
    "stagflation" = "#F44336",    # Red
    "risk_off" = "#9C27B0",       # Purple
    "inflation_shock" = "#E91E63" # Pink
  )
  
  # Add fallback color for any unexpected regimes
  all_regimes <- unique(regime_df$Regime)
  for (regime in all_regimes) {
    if (!regime %in% names(regime_colors)) {
      regime_colors[regime] <- "#607D8B"  # Gray-blue for unknown regimes
    }
  }
  
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
  
  # If we have probability data, create a second visualization
  if (!is.null(regime_probs) && nrow(regime_probs) > 0) {
    # Convert to data frame
    probs_df <- data.frame(
      Date = index(regime_probs),
      regime_probs
    )
    
    # Melt to long format
    probs_long <- reshape2::melt(probs_df, id.vars = "Date", 
                                 variable.name = "Regime", 
                                 value.name = "Probability")
    
    # Create probability plot
    p2 <- ggplot(probs_long, aes(x = Date, y = Probability, fill = Regime)) +
      geom_area(position = "stack") +
      scale_fill_manual(values = regime_colors) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      ) +
      labs(title = "Regime Probabilities Over Time", x = "Date", y = "Probability")
    
    # Return both plots
    return(list(regime_plot = p, probability_plot = p2))
  }
  
  return(p)
}

#=============================================================================
# ENHANCED Z-SCORE BASED REGIME DETECTION WITH SMOOTH TRANSITIONS
#=============================================================================

# Ultra-robust Z-score calculation with multiple safeguards
calculate_safe_zscore <- function(current_value, history, smoothing_window = 10) {
  # Safety checks
  if (is.null(current_value) || is.null(history)) {
    return(0)  # Default to neutral if data is missing
  }
  
  if (length(history) < 10) {
    return(0)  # Need at least 10 data points for meaningful Z-score
  }
  
  # Remove any NA values
  history <- history[!is.na(history)]
  
  # Check if we still have enough data
  if (length(history) < 10) {
    return(0)
  }
  
  # Make sure current_value is numeric and scalar
  if (length(current_value) > 1) {
    current_value <- current_value[1]
  }
  
  # Calculate mean and SD with safety checks
  hist_mean <- mean(history, na.rm = TRUE)
  hist_sd <- sd(history, na.rm = TRUE)
  
  # If SD is too small or zero, return a small value
  if (is.na(hist_sd) || hist_sd < 1e-8) {
    return(0)
  }
  
  # Calculate raw Z-score with sanity check
  raw_zscore <- (current_value - hist_mean) / hist_sd
  
  # Check for extreme Z-scores and cap them
  max_zscore <- 4.0  # Cap Z-scores at 4 standard deviations
  raw_zscore <- min(max(raw_zscore, -max_zscore), max_zscore)
  
  # Apply smoothing if possible - IMPROVED EXPONENTIAL SMOOTHING
  if (smoothing_window > 1 && length(history) >= smoothing_window) {
    # Calculate recent Z-scores
    recent_values <- tail(history, smoothing_window)
    recent_zscores <- (recent_values - hist_mean) / hist_sd
    
    # Cap recent Z-scores too
    recent_zscores <- pmin(pmax(recent_zscores, -max_zscore), max_zscore)
    
    # Use exponential weighting for smoother transitions
    weights <- exp(seq(0, 2, length.out = length(recent_zscores) + 1))
    weights <- weights / sum(weights)
    
    # Combine current and recent z-scores with exponential weighting
    all_zscores <- c(recent_zscores, raw_zscore)
    smoothed_zscore <- sum(weights * all_zscores)
    
    return(smoothed_zscore)
  } else {
    # Just return the raw Z-score if smoothing not possible
    return(raw_zscore)
  }
}

# Enhanced Z-score detection with robust error handling and PROBABILITY WEIGHTING
detect_market_regime <- function(data, lookback = 252, z_score_smoothing = 10,
                                 regime_smoothing = TRUE) {
  # Log that we're using Z-score detection
  cat("\n---- USING Z-SCORE REGIME DETECTION WITH PROBABILISTIC WEIGHTING ----\n")
  
  # Safety check for data
  if (is.null(data) || nrow(data) < 60) {
    cat("WARNING: Insufficient data for Z-score regime detection. Using default growth regime.\n")
    return(list(
      regime = "growth", 
      metrics = list(
        vol_zscore = 0, 
        pmi_zscore = 0,
        global_growth_zscore = 0,
        cpi_zscore = 0,
        commodity_zscore = 0,
        bond_equity_zscore = 0,
        lookback_window = lookback
      ),
      regime_probabilities = data.frame(
        growth = 1.0,
        reflation = 0.0,
        deflation = 0.0,
        stagflation = 0.0,
        risk_off = 0.0,
        inflation_shock = 0.0
      )
    ))
  }
  
  # Get latest values
  latest <- tail(data, 1)
  
  # Initialize Z-scores with safe defaults
  vol_zscore <- 0
  pmi_zscore <- 0
  cpi_zscore <- 0
  bond_equity_zscore <- 0
  commodity_zscore <- 0
  global_growth_zscore <- 0
  yield_curve_zscore <- 0
  credit_spread_zscore <- 0
  put_call_zscore <- 0
  bull_bear_zscore <- 0
  unemployment_zscore <- 0
  inflation_surprise_zscore <- 0
  
  # Check available indicators
  cat("Available indicators:", paste(colnames(data), collapse=", "), "\n")
  
  # Use available history for Z-score calculation
  history_length <- min(nrow(data) - 1, lookback)
  if (history_length > 0) {
    # Get history data safely
    history <- tail(data, history_length + 1)
    if (nrow(history) > 1) {
      history <- history[-nrow(history),]  # Remove the last row (current values)
    }
    
    # ------ Volatility (VIX) ------
    if ("VIX" %in% colnames(data)) {
      vix_history <- as.numeric(history[, "VIX"])
      vix_value <- as.numeric(latest[, "VIX"])
      
      vol_zscore <- calculate_safe_zscore(
        vix_value, vix_history, z_score_smoothing
      )
      
      cat(sprintf("VIX Z-score: %.2f (Current: %.2f)\n", vol_zscore, vix_value))
    } else {
      cat("No VIX data found, using zero vol Z-score\n")
    }
    
    # ------ Growth (PMI) ------
    if ("PMI" %in% colnames(data)) {
      pmi_history <- as.numeric(history[, "PMI"])
      pmi_value <- as.numeric(latest[, "PMI"])
      
      pmi_zscore <- calculate_safe_zscore(
        pmi_value, pmi_history, z_score_smoothing
      )
      
      cat(sprintf("PMI Z-score: %.2f (Current: %.2f)\n", pmi_zscore, pmi_value))
    } else {
      cat("No PMI data found, using zero growth Z-score\n")
    }
    
    # ------ Global Growth ------
    if ("GLOBAL_PMI" %in% colnames(data)) {
      global_history <- as.numeric(history[, "GLOBAL_PMI"])
      global_value <- as.numeric(latest[, "GLOBAL_PMI"])
      
      global_growth_zscore <- calculate_safe_zscore(
        global_value, global_history, z_score_smoothing
      )
      
      cat(sprintf("Global Growth Z-score: %.2f (Current: %.2f)\n", 
                  global_growth_zscore, global_value))
    }
    
    # ------ Inflation (CPI) ------
    if ("CPI_YOY" %in% colnames(data)) {
      cpi_history <- as.numeric(history[, "CPI_YOY"])
      cpi_value <- as.numeric(latest[, "CPI_YOY"])
      
      cpi_zscore <- calculate_safe_zscore(
        cpi_value, cpi_history, z_score_smoothing
      )
      
      cat(sprintf("CPI Z-score: %.2f (Current: %.2f)\n", cpi_zscore, cpi_value))
    } else {
      cat("No CPI data found, using zero inflation Z-score\n")
    }
    
    # ------ Commodity Trend ------
    if ("COMMODITY_TREND" %in% colnames(data)) {
      comm_history <- as.numeric(history[, "COMMODITY_TREND"])
      comm_value <- as.numeric(latest[, "COMMODITY_TREND"])
      
      commodity_zscore <- calculate_safe_zscore(
        comm_value, comm_history, z_score_smoothing
      )
      
      cat(sprintf("Commodity Trend Z-score: %.2f (Current: %.2f)\n", 
                  commodity_zscore, comm_value))
    }
    
    # ------ Bond-equity correlation ------
    if ("BOND_EQUITY_CORR" %in% colnames(data)) {
      corr_history <- as.numeric(history[, "BOND_EQUITY_CORR"])
      corr_value <- as.numeric(latest[, "BOND_EQUITY_CORR"])
      
      bond_equity_zscore <- calculate_safe_zscore(
        corr_value, corr_history, z_score_smoothing
      )
      
      cat(sprintf("Bond-Equity Correlation Z-score: %.2f (Current: %.2f)\n", 
                  bond_equity_zscore, corr_value))
    }
    
    # ------ Yield Curve ------
    if ("YIELD_CURVE" %in% colnames(data)) {
      yc_history <- as.numeric(history[, "YIELD_CURVE"])
      yc_value <- as.numeric(latest[, "YIELD_CURVE"])
      
      yield_curve_zscore <- calculate_safe_zscore(
        yc_value, yc_history, z_score_smoothing
      )
      
      cat(sprintf("Yield Curve Z-score: %.2f (Current: %.2f)\n", 
                  yield_curve_zscore, yc_value))
    }
    
    # ------ Credit Spread ------
    if ("CREDIT_SPREAD" %in% colnames(data)) {
      cs_history <- as.numeric(history[, "CREDIT_SPREAD"])
      cs_value <- as.numeric(latest[, "CREDIT_SPREAD"])
      
      credit_spread_zscore <- calculate_safe_zscore(
        cs_value, cs_history, z_score_smoothing
      )
      
      cat(sprintf("Credit Spread Z-score: %.2f (Current: %.2f)\n", 
                  credit_spread_zscore, cs_value))
    }
    
    # ------ Put/Call Ratio (Sentiment) ------
    if ("PUT_CALL_RATIO" %in% colnames(data)) {
      pc_history <- as.numeric(history[, "PUT_CALL_RATIO"])
      pc_value <- as.numeric(latest[, "PUT_CALL_RATIO"])
      
      put_call_zscore <- calculate_safe_zscore(
        pc_value, pc_history, z_score_smoothing
      )
      
      cat(sprintf("Put/Call Ratio Z-score: %.2f (Current: %.2f)\n", 
                  put_call_zscore, pc_value))
    }
    
    # ------ AAII Bull/Bear (Sentiment) ------
    if ("AAII_BULL_BEAR" %in% colnames(data)) {
      bb_history <- as.numeric(history[, "AAII_BULL_BEAR"])
      bb_value <- as.numeric(latest[, "AAII_BULL_BEAR"])
      
      bull_bear_zscore <- calculate_safe_zscore(
        bb_value, bb_history, z_score_smoothing
      )
      
      cat(sprintf("AAII Bull/Bear Z-score: %.2f (Current: %.2f)\n", 
                  bull_bear_zscore, bb_value))
    }
    
    # ------ Unemployment Proxy ------
    if ("UNEMPLOYMENT_PROXY" %in% colnames(data)) {
      unemp_history <- as.numeric(history[, "UNEMPLOYMENT_PROXY"])
      unemp_value <- as.numeric(latest[, "UNEMPLOYMENT_PROXY"])
      
      unemployment_zscore <- calculate_safe_zscore(
        unemp_value, unemp_history, z_score_smoothing
      )
      
      cat(sprintf("Unemployment Z-score: %.2f (Current: %.2f)\n", 
                  unemployment_zscore, unemp_value))
    }
    
    # ------ Inflation Surprise ------
    if ("INFLATION_SURPRISE" %in% colnames(data)) {
      infl_history <- as.numeric(history[, "INFLATION_SURPRISE"])
      infl_value <- as.numeric(latest[, "INFLATION_SURPRISE"])
      
      inflation_surprise_zscore <- calculate_safe_zscore(
        infl_value, infl_history, z_score_smoothing
      )
      
      cat(sprintf("Inflation Surprise Z-score: %.2f (Current: %.2f)\n", 
                  inflation_surprise_zscore, infl_value))
    }
  }
  
  # Print Z-score summary
  cat("\nZ-SCORE SUMMARY:\n")
  cat(sprintf("  Volatility: %.2f\n", vol_zscore))
  cat(sprintf("  Growth: %.2f\n", pmi_zscore))
  cat(sprintf("  Global Growth: %.2f\n", global_growth_zscore))
  cat(sprintf("  Inflation: %.2f\n", cpi_zscore))
  cat(sprintf("  Inflation Surprise: %.2f\n", inflation_surprise_zscore))
  cat(sprintf("  Commodity Trend: %.2f\n", commodity_zscore))
  cat(sprintf("  Bond-Equity Correlation: %.2f\n", bond_equity_zscore))
  cat(sprintf("  Yield Curve: %.2f\n", yield_curve_zscore))
  cat(sprintf("  Credit Spread: %.2f\n", credit_spread_zscore))
  cat(sprintf("  Put/Call Ratio: %.2f\n", put_call_zscore))
  cat(sprintf("  Bull/Bear Sentiment: %.2f\n", bull_bear_zscore))
  cat(sprintf("  Unemployment: %.2f\n", unemployment_zscore))
  
  # Consider both US and global growth signals
  if (!is.na(global_growth_zscore) && !is.na(pmi_zscore) && abs(global_growth_zscore) > abs(pmi_zscore)) {
    cat("Using global growth signal as primary growth indicator\n")
    # Use the stronger signal (global or US)
    growth_signal <- global_growth_zscore
  } else {
    growth_signal <- pmi_zscore
  }
  
  # Enhanced inflation signal
  inflation_signal <- cpi_zscore
  
  # Add inflation surprise component if available
  if (!is.na(inflation_surprise_zscore)) {
    inflation_signal <- inflation_signal + (0.3 * inflation_surprise_zscore)
    cat(sprintf("Enhanced inflation signal with surprise data: %.2f\n", inflation_signal))
  }
  
  # Add commodity trend component if available
  if (!is.na(commodity_zscore) && commodity_zscore > 0.8) {
    cat("Strong commodity momentum detected, adding to inflation signal\n")
    inflation_signal <- inflation_signal + (0.3 * commodity_zscore)
  }
  
  # Incorporate yield curve inversion into growth signal
  if (!is.na(yield_curve_zscore) && yield_curve_zscore < -0.5) {
    cat("Yield curve inversion detected, reducing growth signal\n")
    growth_signal <- growth_signal - 0.5  # Reduce growth signal on inverted curve
  }
  
  # Incorporate unemployment into growth signal
  if (!is.na(unemployment_zscore) && unemployment_zscore > 0.7) {
    cat("High unemployment detected, reducing growth signal\n")
    growth_signal <- growth_signal - 0.3  # Reduce growth signal on high unemployment
  }
  
  # Enhanced risk signal with sentiment
  risk_signal <- vol_zscore  # Start with volatility as base risk signal
  
  # Add credit spreads into risk assessment
  if (!is.na(credit_spread_zscore) && credit_spread_zscore > 1.0) {
    cat("Wide credit spreads detected, increasing risk signal\n")
    risk_signal <- risk_signal + (0.5 * credit_spread_zscore)
  }
  
  # Add put/call ratio to risk signal
  if (!is.na(put_call_zscore) && put_call_zscore > 1.0) {
    cat("High put/call ratio detected, increasing risk signal\n")
    risk_signal <- risk_signal + (0.3 * put_call_zscore)
  }
  
  # Add bull/bear sentiment (inverted - negative means bullish)
  if (!is.na(bull_bear_zscore) && bull_bear_zscore < -1.0) {
    cat("Extreme bullish sentiment detected, potentially contrarian risk signal\n")
    risk_signal <- risk_signal + 0.2  # Add small contrarian component
  }
  
  #=============================================================================
  # NEW: PROBABILISTIC REGIME CLASSIFICATION WITH SMOOTH TRANSITIONS
  #=============================================================================
  
  # Initialize regime probabilities
  regime_probs <- list(
    growth = 0,
    reflation = 0,
    deflation = 0,
    stagflation = 0,
    risk_off = 0,
    inflation_shock = 0
  )
  
  # Define regime characteristic functions
  # These functions take signals and return a probability (0-1) that we're in that regime
  
  # Growth regime: Strong growth, controlled inflation, low risk
  growth_prob <- function(g, i, r) {
    # Base probability from growth signal (higher = more likely)
    growth_component <- pnorm(g, mean = 0.5, sd = 0.7)
    
    # Inflation penalty (higher inflation reduces probability)
    inflation_penalty <- 1 - pnorm(i, mean = 0.3, sd = 0.7)
    
    # Risk penalty (higher risk reduces probability)
    risk_penalty <- 1 - pnorm(r, mean = 0, sd = 0.7)
    
    # Combine with weights
    prob <- 0.5 * growth_component + 0.3 * inflation_penalty + 0.2 * risk_penalty
    return(prob)
  }
  
  # Reflation regime: Strong growth WITH rising inflation
  reflation_prob <- function(g, i, r) {
    # Need both strong growth AND rising inflation
    growth_component <- pnorm(g, mean = 0.5, sd = 0.7)
    inflation_component <- pnorm(i, mean = 0.5, sd = 0.7)
    
    # Risk penalty (higher risk reduces probability)
    risk_penalty <- 1 - pnorm(r, mean = 0, sd = 0.7)
    
    # Need BOTH growth AND inflation, so use product rather than average
    base_prob <- growth_component * inflation_component
    
    # Apply risk penalty
    prob <- base_prob * risk_penalty
    return(prob)
  }
  
  # Deflation regime: Weak growth, low inflation
  deflation_prob <- function(g, i, r) {
    # Base probability from negative growth (lower growth = more likely)
    growth_component <- 1 - pnorm(g, mean = 0, sd = 0.7)
    
    # Low inflation component (lower inflation = more likely)
    inflation_component <- 1 - pnorm(i, mean = 0, sd = 0.7)
    
    # Moderate risk component (peaks around 0.5-1.0 standard deviations)
    risk_component <- dnorm(r, mean = 0.75, sd = 0.5) / dnorm(0.75, mean = 0.75, sd = 0.5)
    
    # Combine with weights
    prob <- 0.4 * growth_component + 0.4 * inflation_component + 0.2 * risk_component
    return(prob)
  }
  
  # Stagflation regime: Weak growth WITH high inflation
  stagflation_prob <- function(g, i, r) {
    # Need BOTH weak growth AND high inflation
    growth_component <- 1 - pnorm(g, mean = 0, sd = 0.7)
    inflation_component <- pnorm(i, mean = 0.5, sd = 0.7)
    
    # Moderate risk component (peaks around 0.5-1.0 standard deviations)
    risk_component <- dnorm(r, mean = 0.75, sd = 0.5) / dnorm(0.75, mean = 0.75, sd = 0.5)
    
    # Need BOTH weak growth AND high inflation, so use product
    base_prob <- growth_component * inflation_component
    
    # Apply risk adjustment
    prob <- base_prob * (0.7 + 0.3 * risk_component)
    return(prob)
  }
  
  # Risk-off regime: High volatility/uncertainty, flight to quality
  risk_off_prob <- function(g, i, r, be) {
    # Primarily driven by risk signal
    risk_component <- pnorm(r, mean = 0.8, sd = 0.7)
    
    # Bond-equity correlation penalty (higher = more likely stocks and bonds fall together)
    # Normal flight-to-quality has negative correlation (bonds up when stocks down)
    be_component <- 1
    if (!is.na(be) && is.finite(be)) {
      be_component <- 1 - pnorm(be, mean = 0, sd = 0.7)
    }
    
    # Growth doesn't matter as much, but very negative growth increases probability
    growth_penalty <- 1
    if (g < -1) {
      growth_penalty <- 1 + 0.2 * abs(g)  # Increase probability for very negative growth
    }
    
    # Combine with weights - primarily risk-driven
    prob <- 0.7 * risk_component * be_component * growth_penalty
    return(prob)
  }
  
  # Inflation shock: High inflation with high risk, positive bond-equity correlation (continued)
  inflation_shock_prob <- function(g, i, r, be) {
    # Need high inflation
    inflation_component <- pnorm(i, mean = 1.0, sd = 0.7)
    
    # Need high risk
    risk_component <- pnorm(r, mean = 1.0, sd = 0.7)
    
    # Need positive bond-equity correlation (both bonds and stocks falling)
    be_component <- 0.5
    if (!is.na(be) && is.finite(be)) {
      be_component <- pnorm(be, mean = 0.5, sd = 0.7)
    }
    
    # Growth is less important, but very negative growth slightly increases probability
    growth_component <- 1
    if (g < -1) {
      growth_component = 1 + 0.1 * abs(g)
    }
    
    # Combine multiplicatively - need ALL conditions to be true
    prob <- inflation_component * risk_component * be_component * growth_component
    return(min(prob, 1.0))
  }
  
  # Calculate regime probabilities
  regime_probs$growth <- growth_prob(growth_signal, inflation_signal, risk_signal)
  regime_probs$reflation <- reflation_prob(growth_signal, inflation_signal, risk_signal)
  regime_probs$deflation <- deflation_prob(growth_signal, inflation_signal, risk_signal)
  regime_probs$stagflation <- stagflation_prob(growth_signal, inflation_signal, risk_signal)
  regime_probs$risk_off <- risk_off_prob(growth_signal, inflation_signal, risk_signal, bond_equity_zscore)
  regime_probs$inflation_shock <- inflation_shock_prob(growth_signal, inflation_signal, risk_signal, bond_equity_zscore)
  
  # Ensure probabilities are within bounds
  for (r in names(regime_probs)) {
    regime_probs[[r]] <- min(max(regime_probs[[r]], 0), 1)
  }
  
  # Normalize probabilities to sum to 1
  total_prob <- sum(unlist(regime_probs))
  if (total_prob > 0) {
    for (r in names(regime_probs)) {
      regime_probs[[r]] <- regime_probs[[r]] / total_prob
    }
  } else {
    # If all probabilities are 0, default to growth
    regime_probs$growth <- 1.0
  }
  
  # Convert to data frame for easier handling
  regime_probs_df <- as.data.frame(regime_probs)
  
  # Before returning, ensure regime_probs_df has valid column names and structure
  if (any(colnames(regime_probs_df) == "")) {
    warning("Empty column name found in regime_probs_df, fixing")
    # Remove columns with empty names
    regime_probs_df <- regime_probs_df[, colnames(regime_probs_df) != ""]
  }
  
  # Find the dominant regime (highest probability)
  dominant_regime <- names(which.max(regime_probs))
  
  # Calculate confidence in regime classification (probability of dominant regime)
  regime_confidence <- regime_probs[[dominant_regime]]
  
  # Print regime probabilities
  cat("\nREGIME PROBABILITIES:\n")
  for (regime in names(regime_probs)) {
    cat(sprintf("  %s: %.2f%%\n", regime, 100 * regime_probs[[regime]]))
  }
  
  cat(sprintf("\nDominant Regime: %s (%.1f%% confidence)\n", 
              dominant_regime, 100 * regime_confidence))
  
  # Return both the regime and confidence metrics
  return(list(
    regime = dominant_regime,
    confidence = regime_confidence,
    metrics = list(
      vol_zscore = vol_zscore,
      risk_signal = risk_signal,  # Combined risk signal
      pmi_zscore = pmi_zscore,
      global_growth_zscore = global_growth_zscore,
      growth_signal = growth_signal,  # Combined growth signal
      cpi_zscore = cpi_zscore,
      commodity_zscore = commodity_zscore,
      inflation_signal = inflation_signal,  # Combined inflation signal
      bond_equity_zscore = bond_equity_zscore,
      yield_curve_zscore = yield_curve_zscore,
      credit_spread_zscore = credit_spread_zscore,
      put_call_zscore = put_call_zscore,
      bull_bear_zscore = bull_bear_zscore,
      unemployment_zscore = unemployment_zscore,
      inflation_surprise_zscore = inflation_surprise_zscore,
      lookback_window = lookback
    ),
    regime_probabilities = regime_probs_df
  ))
}

# Define regime-based adjustments for portfolio weights with short overlays
get_regime_adjustments <- function(regime_probs, include_shorts = TRUE) {
  # Safety check for NULL or empty input
  if (is.null(regime_probs) || length(regime_probs) == 0) {
    warning("Empty regime probabilities provided, using default growth regime")
    regime_probs <- list(growth = 1.0)
  }
  
  # Check for and remove any empty string keys
  empty_keys <- which(names(regime_probs) == "")
  if (length(empty_keys) > 0) {
    warning("Empty regime name detected, removing")
    regime_probs <- regime_probs[-empty_keys]
  }
  
  # Base regime adjustments for each pure regime
  base_regime_adjustments <- list(
    growth = list(
      US_EQUITY = 1.1,
      US_SMALL_CAP = 1.1,
      INTL_DEVELOPED = 1.05,
      EMERGING_MARKETS = 1.1,
      US_TREASURY = 0.9,
      TIPS = 0.85,
      CREDIT_IG = 1.0,
      GOLD = 0.8,
      COMMODITIES = 0.95,
      REIT = 1.1,
      # Short overlay positions
      SHORT_TREASURY = -0.05,  # Small short in treasuries during growth
      SHORT_GOLD = -0.05       # Small short in gold during growth
    ),
    reflation = list(
      US_EQUITY = 1.05,
      US_SMALL_CAP = 1.1,
      INTL_DEVELOPED = 1.0,
      EMERGING_MARKETS = 1.15,
      US_TREASURY = 0.7,
      TIPS = 1.1,
      CREDIT_IG = 0.95,
      GOLD = 1.1,
      COMMODITIES = 1.2,
      REIT = 1.0,
      # Short overlay positions
      SHORT_TREASURY = -0.1,   # More significant treasury short in reflation
      SHORT_GOLD = 0           # No gold short in reflation (inflation hedge)
    ),
    deflation = list(
      US_EQUITY = 0.9,
      US_SMALL_CAP = 0.8,
      INTL_DEVELOPED = 0.85,
      EMERGING_MARKETS = 0.7,
      US_TREASURY = 1.3,
      TIPS = 0.9,
      CREDIT_IG = 1.2,
      GOLD = 1.1,
      COMMODITIES = 0.75,
      REIT = 0.8,
      # Short overlay positions
      SHORT_EQUITY = -0.1,     # Small equity short in deflation
      SHORT_COMMODITY = -0.05  # Small commodity short in deflation
    ),
    stagflation = list(
      US_EQUITY = 0.8,
      US_SMALL_CAP = 0.7,
      INTL_DEVELOPED = 0.75,
      EMERGING_MARKETS = 0.85,
      US_TREASURY = 0.9,
      TIPS = 1.2,
      CREDIT_IG = 0.9,
      GOLD = 1.3,
      COMMODITIES = 1.2,
      REIT = 0.7,
      # Short overlay positions
      SHORT_EQUITY = -0.1,     # Equity short during stagflation
      SHORT_TREASURY = -0.05   # Small treasury short during stagflation
    ),
    risk_off = list(
      US_EQUITY = 0.6,
      US_SMALL_CAP = 0.5,
      INTL_DEVELOPED = 0.55,
      EMERGING_MARKETS = 0.4,
      US_TREASURY = 1.5,
      TIPS = 1.1,
      CREDIT_IG = 1.0,
      GOLD = 1.4,
      COMMODITIES = 0.7,
      REIT = 0.5,
      # Short overlay positions
      SHORT_EQUITY = -0.15,    # More significant equity short during risk-off
      SHORT_CREDIT = -0.1      # Credit short during risk-off
    ),
    inflation_shock = list(
      US_EQUITY = 0.6,
      US_SMALL_CAP = 0.5,
      INTL_DEVELOPED = 0.6,
      EMERGING_MARKETS = 0.6,
      US_TREASURY = 0.7,
      TIPS = 1.3,
      CREDIT_IG = 0.8,
      GOLD = 1.3,
      COMMODITIES = 1.2,
      REIT = 0.5,
      # Short overlay positions
      SHORT_EQUITY = -0.15,    # Equity short during inflation shock
      SHORT_TREASURY = -0.1,   # Treasury short during inflation shock
      SHORT_CREDIT = -0.1      # Credit short during inflation shock
    )
  )
  
  # Get all asset classes across all regimes
  all_assets <- unique(unlist(lapply(base_regime_adjustments, names)))
  
  # Initialize the blended adjustments with all assets
  blended_adjustments <- list()
  for (asset in all_assets) {
    blended_adjustments[[asset]] <- 0
  }
  
  # Calculate probability-weighted average for each asset class
  for (regime in names(regime_probs)) {
    # Skip if regime not in our base adjustments
    if (!regime %in% names(base_regime_adjustments)) {
      next
    }
    
    # Get the probability of this regime
    prob <- regime_probs[[regime]]
    
    # Skip if probability is too low
    if (prob < 0.01) {
      next
    }
    
    # Get the adjustments for this regime
    regime_adj <- base_regime_adjustments[[regime]]
    
    # Add weighted adjustments to the blended result
    for (asset in names(regime_adj)) {
      # If this is a short position and shorts are disabled, skip it
      if (!include_shorts && grepl("^SHORT_", asset)) {
        next
      }
      
      # Initialize if needed
      if (!asset %in% names(blended_adjustments)) {
        blended_adjustments[[asset]] <- 0
      }
      
      # Add weighted adjustment
      blended_adjustments[[asset]] <- blended_adjustments[[asset]] + 
        prob * regime_adj[[asset]]
    }
  }
  
  # Ensure neutral adjustment (1.0) for assets not explicitly adjusted
  for (asset in names(blended_adjustments)) {
    # If short and shorts disabled, set to 0
    if (!include_shorts && grepl("^SHORT_", asset)) {
      blended_adjustments[[asset]] <- 0
    }
    # Otherwise ensure all long assets have at least a minimal weight
    else if (!grepl("^SHORT_", asset) && blended_adjustments[[asset]] < 0.5) {
      blended_adjustments[[asset]] <- 0.5  # Minimum weight factor for long assets
    }
  }
  
  # For assets not in any regime adjustments, default to neutral
  for (asset in setdiff(all_assets, names(blended_adjustments))) {
    if (!grepl("^SHORT_", asset)) {  # Only for standard assets, not shorts
      blended_adjustments[[asset]] <- 1.0
    }
  }
  
  return(blended_adjustments)
}

# Helper function to map inverse ETFs to their asset classes
map_inverse_etfs <- function(inverse_etf_map) {
  # Create mapping from inverse ETFs to asset sleeves
  inverse_to_sleeve <- list()
  
  # Standard mappings for common inverse ETFs
  standard_mappings <- list(
    "SH" = "SHORT_EQUITY",      # Inverse S&P 500
    "PSQ" = "SHORT_EQUITY",     # Inverse Nasdaq
    "RWM" = "SHORT_EQUITY",     # Inverse Russell 2000
    "EUM" = "SHORT_EQUITY",     # Inverse Emerging Markets
    "EFZ" = "SHORT_EQUITY",     # Inverse EAFE
    "DOG" = "SHORT_EQUITY",     # Inverse Dow Jones
    "TBF" = "SHORT_TREASURY",   # Inverse 7-10 Year Treasury
    "TBX" = "SHORT_TREASURY",   # Inverse 20+ Year Treasury
    "SJB" = "SHORT_CREDIT",     # Inverse High Yield
    "IGSD" = "SHORT_CREDIT",    # Inverse Investment Grade
    "DGZ" = "SHORT_GOLD",       # Inverse Gold
    "KOLD" = "SHORT_COMMODITY", # Inverse Natural Gas
    "SCO" = "SHORT_COMMODITY",  # Inverse Oil
    "DRV" = "SHORT_REIT"        # Inverse Real Estate
  )
  
  # Start with standard mappings
  inverse_to_sleeve <- standard_mappings
  
  # Add mappings from provided inverse_etf_map
  if (!is.null(inverse_etf_map)) {
    for (std_etf in names(inverse_etf_map)) {
      inv_etf <- inverse_etf_map[[std_etf]]
      
      # Map based on the standard ETF's type
      if (grepl("^(SPY|VOO|IVV|DIA|QQQ)", std_etf)) {
        inverse_to_sleeve[[inv_etf]] <- "SHORT_EQUITY"
      } else if (grepl("^(IWM|MDY|IJH)", std_etf)) {
        inverse_to_sleeve[[inv_etf]] <- "SHORT_EQUITY"
      } else if (grepl("^(EFA|VEA|IEFA)", std_etf)) {
        inverse_to_sleeve[[inv_etf]] <- "SHORT_EQUITY"
      } else if (grepl("^(EEM|VWO|IEMG)", std_etf)) {
        inverse_to_sleeve[[inv_etf]] <- "SHORT_EQUITY"
      } else if (grepl("^(IEF|TLT|SHY)", std_etf)) {
        inverse_to_sleeve[[inv_etf]] <- "SHORT_TREASURY"
      } else if (grepl("^(LQD|HYG|JNK)", std_etf)) {
        inverse_to_sleeve[[inv_etf]] <- "SHORT_CREDIT"
      } else if (grepl("^(GLD|IAU)", std_etf)) {
        inverse_to_sleeve[[inv_etf]] <- "SHORT_GOLD"
      } else if (grepl("^(DBC|PDBC|USO)", std_etf)) {
        inverse_to_sleeve[[inv_etf]] <- "SHORT_COMMODITY"
      } else if (grepl("^(VNQ|IYR)", std_etf)) {
        inverse_to_sleeve[[inv_etf]] <- "SHORT_REIT"
      } else {
        # Default for unrecognized
        inverse_to_sleeve[[inv_etf]] <- "SHORT_EQUITY"
      }
    }
  }
  
  return(inverse_to_sleeve)
}

#=============================================================================
# RISK PARITY ALLOCATION - ULTRA-ROBUST OPTIMIZATION
#=============================================================================

# Risk contribution calculation with maximum error handling
risk_contribution <- function(weights, cov_matrix) {
  # Safety checks for inputs with detailed diagnostics
  if (is.null(weights) || length(weights) == 0) {
    stop("Weights vector is NULL or empty")
  }
  
  if (is.null(cov_matrix) || !is.matrix(cov_matrix) || nrow(cov_matrix) == 0) {
    stop("Covariance matrix is NULL, not a matrix, or empty")
  }
  
  # Check dimensions match with helpful error message
  if (length(weights) != nrow(cov_matrix)) {
    stop(sprintf("Dimension mismatch: weights length %d doesn't match covariance matrix rows %d", 
                 length(weights), nrow(cov_matrix)))
  }
  
  # Make sure weights is a vector, not a one-column matrix
  weights <- as.numeric(weights)
  
  # Check for NAs or infinite values
  if (any(is.na(weights)) || any(!is.finite(weights))) {
    stop("Weights contain NA or infinite values")
  }
  
  if (any(is.na(cov_matrix)) || any(!is.finite(cov_matrix))) {
    stop("Covariance matrix contains NA or infinite values")
  }
  
  # Calculate portfolio volatility with safety checks
  portfolio_vol <- tryCatch({
    vol <- sqrt(as.numeric(t(weights) %*% cov_matrix %*% weights))
    if (is.na(vol) || !is.finite(vol)) {
      stop("Portfolio volatility calculation resulted in NA or infinite value")
    }
    vol
  }, error = function(e) {
    # Detailed error for troubleshooting
    stop(sprintf("Error calculating portfolio volatility: %s\nWeights: %s", 
                 e$message, paste(round(weights, 4), collapse=", ")))
  })
  
  # Safety check for zero volatility
  if (portfolio_vol <= 1e-8) {
    warning("Near-zero portfolio volatility detected, using small positive value")
    portfolio_vol <- 1e-8
  }
  
  # Calculate marginal contribution to risk
  marginal_contrib <- tryCatch({
    cov_matrix %*% weights / portfolio_vol
  }, error = function(e) {
    stop(sprintf("Error calculating marginal risk contribution: %s", e$message))
  })
  
  # Calculate risk contribution
  risk_contrib <- weights * marginal_contrib
  
  return(risk_contrib)
}

# Risk parity objective function with comprehensive error handling
risk_parity_objective <- function(weights, cov_matrix) {
  # Make sure weights is a vector
  weights <- as.numeric(weights)
  
  # Safety checks with descriptive errors
  if (any(is.na(weights))) {
    cat("WARNING: NA values in weights, returning large objective value\n")
    return(1e10)
  }
  
  if (any(is.na(cov_matrix))) {
    cat("WARNING: NA values in covariance matrix, returning large objective value\n")
    return(1e10)
  }
  
  # Target risk contribution (equal for all assets)
  n <- length(weights)
  target_risk <- 1.0 / n
  
  # Calculate actual risk contributions with robust error handling
  risk_contrib <- tryCatch({
    risk_contribution(weights, cov_matrix)
  }, error = function(e) {
    cat("Warning in risk contribution calculation:", e$message, "\n")
    cat("Using equal contributions as fallback\n")
    return(rep(1/n, n))  # Return equal contributions if calculation fails
  })
  
  # Sum of squared deviations from target (normalize by n for better scaling)
  objective_value <- tryCatch({
    sum((risk_contrib - target_risk)^2) / n
  }, error = function(e) {
    cat("Error computing objective function:", e$message, "\n")
    return(1e10)  # Return large value on error
  })
  
  return(objective_value)
}

# Ultra-robust risk parity weights calculation with multiple fallbacks
calculate_risk_parity_weights <- function(returns, target_vol = 0.075, 
                                          cov_method = "ledoit-wolf", 
                                          use_garch = TRUE, 
                                          ewma_lambda = 0.94) {
  # Detailed logging
  cat("\nCALCULATING RISK PARITY WEIGHTS\n")
  cat("Method:", cov_method, ifelse(use_garch, "with GARCH", "without GARCH"), "\n")
  
  # Remove NA values and ensure we have enough data
  returns <- na.omit(returns)
  
  # Comprehensive error checking
  if (nrow(returns) < 30 || ncol(returns) < 2) {
    cat("WARNING: Insufficient data for risk parity. Using equal weights.\n")
    equal_weights <- rep(1/ncol(returns), ncol(returns))
    names(equal_weights) <- colnames(returns)
    
    # Create identity covariance matrix
    ident_cov <- diag(ncol(returns))
    colnames(ident_cov) <- colnames(returns)
    rownames(ident_cov) <- colnames(returns)
    
    return(list(
      weights = equal_weights,
      cov_matrix = ident_cov,
      method = "equal",
      port_vol = sd(rowSums(returns)) * sqrt(252)
    ))
  }
  
  # Number of assets
  n <- ncol(returns)
  cat(sprintf("Processing %d assets with %d observations\n", n, nrow(returns)))
  
  # STEP 1: Calculate GARCH volatility forecasts if requested
  if (use_garch) {
    cat("Using GARCH volatility forecasts\n")
    
    # Get volatility forecasts
    vol_forecasts <- tryCatch({
      forecast_garch_volatility(returns)
    }, error = function(e) {
      cat("GARCH forecasting failed:", e$message, "\n")
      cat("Using historical volatilities instead\n")
      
      # Calculate historical volatilities
      hist_vols <- apply(returns, 2, function(x) sd(x, na.rm = TRUE) * sqrt(252))
      data.frame(
        asset = names(hist_vols),
        forecasted_vol = hist_vols,
        method_used = rep("historical", length(hist_vols)),
        row.names = names(hist_vols)
      )
    })
    
    # Extract the forecasted volatilities
    forecast_vol_vector <- vol_forecasts$forecasted_vol
    names(forecast_vol_vector) <- vol_forecasts$asset
    
    # Print some statistics on the forecasts
    cat(sprintf("Volatility forecasts - Min: %.2f%%, Max: %.2f%%, Mean: %.2f%%\n",
                100 * min(forecast_vol_vector),
                100 * max(forecast_vol_vector),
                100 * mean(forecast_vol_vector)))
  }
  
  # STEP 2: Choose covariance estimation method with comprehensive error handling
  cov_matrix <- tryCatch({
    if (cov_method == "ewma") {
      cat("Using EWMA covariance estimation (λ =", ewma_lambda, ")\n")
      estimate_ewma_covariance(returns, lambda = ewma_lambda)
    } else if (cov_method == "sample") {
      cat("Using sample covariance estimation\n")
      cov(returns)
    } else if (cov_method == "ledoit-wolf") {
      cat("Using Ledoit-Wolf shrinkage estimation\n")
      estimate_robust_covariance(returns, method = "ledoit-wolf")
    } else {
      # Default to sample if method not recognized
      cat("Warning: Unrecognized covariance method. Using sample covariance.\n")
      cov(returns)
    }
  }, error = function(e) {
    cat("ERROR in covariance estimation:", e$message, "\n")
    cat("Falling back to sample covariance with regularization\n")
    
    # Get sample covariance with safety
    sample_cov <- tryCatch({
      cov(returns)
    }, error = function(e2) {
      # Create diagonal matrix as last resort
      diag_var <- apply(returns, 2, var, na.rm = TRUE)
      diag_var[is.na(diag_var)] <- 0.01^2  # Default to 1% daily variance
      diag_cov <- diag(diag_var)
      colnames(diag_cov) <- colnames(returns)
      rownames(diag_cov) <- colnames(returns)
      return(diag_cov)
    })
    
    # Add small regularization
    diag(sample_cov) <- diag(sample_cov) * 1.05
    return(sample_cov)
  })
  
  # STEP 3: Use optimization to find risk parity weights
  # Define optimization constraints
  lower <- rep(1e-6, n)  # Lower bounds slightly above 0
  upper <- rep(0.5, n)   # Upper bounds to prevent extreme concentration
  
  # Starting point: equal weights
  x0 <- rep(1/n, n)
  
  # Optimization with multiple fallbacks
  weights <- tryCatch({
    # First try nloptr
    cat("Optimizing with nloptr (SLSQP algorithm)...\n")
    
    # Sum-to-one constraint
    heq <- function(w) sum(w) - 1
    
    # Run optimization
    opt_result <- nloptr(
      x0 = x0,
      eval_f = function(w) risk_parity_objective(w, cov_matrix),
      lb = lower,
      ub = upper,
      eval_g_eq = heq,
      opts = list(algorithm = "NLOPT_LD_SLSQP",
                  xtol_rel = 1e-8,
                  maxeval = 1000,
                  print_level = 0)
    )
    
    if (opt_result$status < 0) {
      cat("nloptr failed, trying alternative method...\n")
      stop("nloptr failed")
    }
    
    # Get weights from optimization result
    w <- opt_result$solution
    names(w) <- colnames(returns)
    w
  }, error = function(e) {
    cat("First optimization attempt failed:", e$message, "\n")
    cat("Trying simpler optimization approach...\n")
    
    # Try a simpler optimization method
    tryCatch({
      # Use optim with just box constraints
      obj_fn <- function(w) {
        # Normalize to sum to 1
        w_norm <- w / sum(w)
        # Calculate objective
        risk_parity_objective(w_norm, cov_matrix)
      }
      
      opt_result <- optim(
        par = x0,
        fn = obj_fn,
        method = "L-BFGS-B",
        lower = lower,
        upper = upper,
        control = list(maxit = 1000)
      )
      
      # Normalize result to sum to 1
      w <- opt_result$par / sum(opt_result$par)
      names(w) <- colnames(returns)
      w
    }, error = function(e2) {
      cat("Second optimization attempt failed:", e2$message, "\n")
      cat("Using inverse variance weights as fallback\n")
      
      # Fallback to inverse variance
      vars <- diag(cov_matrix)
      inv_vars <- 1/vars
      w <- inv_vars / sum(inv_vars)
      names(w) <- colnames(returns)
      w
    })
  })
  
  # STEP 4: Adjust for target volatility
  
  # Calculate portfolio volatility with current weights
  port_vol <- sqrt(as.numeric(t(weights) %*% cov_matrix %*% weights)) * sqrt(252)
  
  # Calculate leverage to match target volatility
  leverage <- target_vol / port_vol
  
  # Print portfolio statistics
  cat(sprintf("Portfolio volatility: %.2f%%\n", port_vol * 100))
  cat(sprintf("Target volatility: %.2f%%\n", target_vol * 100))
  cat(sprintf("Leverage factor: %.2f\n", leverage))
  
  # Return all relevant information
  return(list(
    weights = weights,
    cov_matrix = cov_matrix,
    method = cov_method,
    port_vol = port_vol,
    leverage = leverage
  ))
}

# Calculate drawdown for more responsive risk control
calculate_current_drawdown <- function(returns, lookback = NULL) {
  # Safety checks
  if (is.null(returns) || length(returns) < 2) {
    return(0)
  }
  
  # Limit drawdown calculation to lookback window if provided
  if (!is.null(lookback) && lookback > 0 && lookback < length(returns)) {
    returns <- tail(returns, lookback)
  }
  
  # Remove NA values
  returns <- returns[!is.na(returns)]
  
  if (length(returns) < 2) {
    return(0)
  }
  
  # Calculate cumulative returns safely
  cumul_returns <- tryCatch({
    cumprod(1 + returns)
  }, error = function(e) {
    cat("Error calculating cumulative returns:", e$message, "\n")
    return(exp(cumsum(returns)))  # Alternative calculation
  })
  
  # Calculate drawdown
  current_drawdown <- tryCatch({
    max_equity <- max(cumul_returns)
    if (max_equity <= 0) return(0)  # Prevent division by zero
    1 - cumul_returns[length(cumul_returns)] / max_equity
  }, error = function(e) {
    cat("Error calculating drawdown:", e$message, "\n")
    return(0)  # Default to zero on error
  })
  
  # Safety check for unreasonable values
  if (is.na(current_drawdown) || !is.finite(current_drawdown)) {
    return(0)
  }
  
  # Cap at reasonable maximum
  return(min(current_drawdown, 0.9))  # Cap at 90% drawdown for safety
}

# Calculate drawdown path (full history) for monitoring
calculate_drawdown_path <- function(returns) {
  # Safety checks
  if (is.null(returns) || length(returns) < 2) {
    return(rep(0, length(returns)))
  }
  
  # Calculate cumulative returns
  cumul_returns <- tryCatch({
    cumprod(1 + returns)
  }, error = function(e) {
    cat("Error calculating cumulative returns:", e$message, "\n")
    # Alternative calculation
    exp(cumsum(returns))
  })
  
  # Calculate running maximum (peak equity)
  running_max <- cummax(cumul_returns)
  
  # Calculate drawdown at each point
  drawdowns <- 1 - cumul_returns / running_max
  
  # Replace any NAs or invalid values
  drawdowns[is.na(drawdowns) | !is.finite(drawdowns)] <- 0
  
  # Cap at reasonable maximum
  drawdowns <- pmin(drawdowns, 0.9)  # Cap at 90%
  
  return(drawdowns)
}

# Improved cash allocation based on drawdown and volatility
calculate_cash_allocation <- function(current_drawdown = 0, drawdown_velocity = 0, 
                                      max_drawdown = 0.075, vol_zscore = 0, 
                                      max_cash_pct = 0.20) {
  # Safety checks
  if (is.na(current_drawdown) || !is.finite(current_drawdown)) {
    current_drawdown <- 0
  }
  
  if (is.na(vol_zscore) || !is.finite(vol_zscore)) {
    vol_zscore <- 0
  }
  
  if (is.na(drawdown_velocity) || !is.finite(drawdown_velocity)) {
    drawdown_velocity <- 0
  }
  
  # Default - no cash
  cash_pct <- 0
  
  # More responsive to smaller drawdowns (40% of max instead of 60%)
  if (current_drawdown > 0.4 * max_drawdown) {
    # Scale cash linearly from 0 to max as drawdown approaches max
    dd_ratio <- current_drawdown / max_drawdown
    
    # More aggressive cash allocation curve (cubic instead of linear)
    # This allocates cash more aggressively as drawdowns worsen
    dd_cash_pct <- min(max_cash_pct, (dd_ratio - 0.4)^1.5 * (max_cash_pct * 2))
    cash_pct <- max(cash_pct, dd_cash_pct)
  }
  
  # Use drawdown velocity (rate of change) for early warning
  # If drawdown is getting worse quickly, increase cash even if current level is not severe
  if (drawdown_velocity > 0.01) {  # If losing >1% drawdown per period
    vel_cash_pct <- min(max_cash_pct * 0.5, drawdown_velocity * 5)  # Up to half of max cash
    cash_pct <- max(cash_pct, vel_cash_pct)
  }
  
  # If volatility is extreme, also increase cash
  if (vol_zscore > 1.0) {  # High volatility
    vol_cash_pct <- min(max_cash_pct, (vol_zscore - 1.0) * 0.1)  # 10% per z-score unit above 1.0
    cash_pct <- max(cash_pct, vol_cash_pct)
  }
  
  # Cap at a reasonable but still effective maximum
  return(min(cash_pct, max_cash_pct))
}

cat("\nPART 2 LOADED: Risk Estimation, Regime Detection, and Portfolio Construction\n")
#=============================================================================
# ENHANCED RISK PARITY PART 3: BACKTESTING AND PERFORMANCE EVALUATION
# - REAL DATA VERSION WITH NO SYNTHETIC GENERATION
# - Fixed transaction cost tracking and application
# - More responsive drawdown control mechanism 
# - Support for short overlays through inverse ETFs
# - Comprehensive performance evaluation with regime analysis
#=============================================================================

#=============================================================================
# PORTFOLIO CONSTRUCTION WITH REGIME PROBABILITY WEIGHTING
#=============================================================================

# Portfolio construction with probability-weighted regime allocations and drawdown control
construct_optimized_risk_parity <- function(returns, market_data, sleeve_mapping,
                                            portfolio_returns = NULL, 
                                            prev_weights = NULL,
                                            target_vol = 0.075, 
                                            max_drawdown = 0.075,
                                            use_garch = TRUE,
                                            cov_method = "ledoit-wolf",
                                            transaction_costs = NULL,
                                            enable_cash = TRUE,
                                            enable_shorts = TRUE,
                                            drawdown_window = 63, # ~3 months
                                            debug = TRUE) {
  # Print debug header
  if (debug) {
    cat("\n===== CONSTRUCTING PORTFOLIO WITH REGIME PROBABILITY WEIGHTING =====\n")
    cat("Method:", cov_method, ifelse(use_garch, "with GARCH", ""), "\n")
    cat("Target volatility:", sprintf("%.2f%%", 100 * target_vol), "\n")
    cat("Target max drawdown:", sprintf("%.2f%%", 100 * max_drawdown), "\n")
    cat("Short overlays:", ifelse(enable_shorts, "enabled", "disabled"), "\n")
  }
  
  # COMPREHENSIVE SAFETY CHECKS
  # Check we have returns data
  if (is.null(returns) || nrow(returns) == 0 || ncol(returns) == 0) {
    stop("Invalid returns data - empty or NULL")
  }
  
  if (is.null(market_data) || nrow(market_data) == 0) {
    stop("Invalid market data - empty or NULL")
  }
  
  # Check for missing mapping and create default if needed
  if (is.null(sleeve_mapping) || length(sleeve_mapping) == 0) {
    cat("WARNING: Missing sleeve mapping. Creating default mapping.\n")
    sleeve_mapping <- list()
    for (ticker in colnames(returns)) {
      # Create default mappings based on name patterns
      if (grepl("SPY|VOO|IVV", ticker)) {
        sleeve_mapping[[ticker]] <- "US_EQUITY"
      } else if (grepl("IWM|IJR", ticker)) {
        sleeve_mapping[[ticker]] <- "US_SMALL_CAP"
      } else if (grepl("EFA|VEA", ticker)) {
        sleeve_mapping[[ticker]] <- "INTL_DEVELOPED"
      } else if (grepl("EEM|VWO", ticker)) {
        sleeve_mapping[[ticker]] <- "EMERGING_MARKETS"
      } else if (grepl("TLT|IEF", ticker)) {
        sleeve_mapping[[ticker]] <- "US_TREASURY"
      } else if (grepl("TIP", ticker)) {
        sleeve_mapping[[ticker]] <- "TIPS"
      } else if (grepl("LQD|CORP", ticker)) {
        sleeve_mapping[[ticker]] <- "CREDIT_IG"
      } else if (grepl("GLD|IAU", ticker)) {
        sleeve_mapping[[ticker]] <- "GOLD"
      } else if (grepl("DBC|GSG|PDBC", ticker)) {
        sleeve_mapping[[ticker]] <- "COMMODITIES"
      } else if (grepl("VNQ|IYR", ticker)) {
        sleeve_mapping[[ticker]] <- "REIT"
      } else if (grepl("^(SH|PSQ|DOG|RWM|EUM|EFZ)", ticker)) {
        # Handle inverse ETFs
        sleeve_mapping[[ticker]] <- "SHORT_EQUITY"
      } else if (grepl("^(TBF|TBX)", ticker)) {
        sleeve_mapping[[ticker]] <- "SHORT_TREASURY"
      } else if (grepl("^(SJB|IGSD)", ticker)) {
        sleeve_mapping[[ticker]] <- "SHORT_CREDIT"
      } else {
        # Default to equity for unknown assets
        sleeve_mapping[[ticker]] <- "US_EQUITY"
      }
    }
    
    if (debug) {
      cat("Created default sleeve mapping:\n")
      for (ticker in names(sleeve_mapping)) {
        cat(sprintf("  %s: %s\n", ticker, sleeve_mapping[[ticker]]))
      }
    }
  }
  
  # Identify inverse ETFs for shorts if available
  inverse_map <- attr(market_data, "inverse_etf_map")
  inverse_sleeve_map <- NULL
  
  if (enable_shorts && !is.null(inverse_map)) {
    inverse_sleeve_map <- map_inverse_etfs(inverse_map)
    if (debug && length(inverse_sleeve_map) > 0) {
      cat("\nInverse ETF mappings available for short overlays:\n")
      for (inv_ticker in names(inverse_sleeve_map)) {
        cat(sprintf("  %s -> %s\n", inv_ticker, inverse_sleeve_map[[inv_ticker]]))
      }
    }
  }
  
  # Ensure we have transaction costs
  if (is.null(transaction_costs) && !is.null(prev_weights)) {
    cat("No transaction costs provided, estimating from tickers...\n")
    transaction_costs <- get_transaction_costs(colnames(returns))
  }
  
  # Count assets
  n_assets <- ncol(returns)
  if (debug) {
    cat("Assets:", n_assets, "\n")
    cat("Assets:", paste(colnames(returns), collapse=", "), "\n")
  }
  
  #--------------------------------------------------------------------------
  # STEP 1: Detect market regime using probability-weighted approach
  #--------------------------------------------------------------------------
  regime_result <- tryCatch({
    detect_market_regime(market_data)
  }, error = function(e) {
    cat("Error in market regime detection:", e$message, "\n")
    cat("Using default growth regime\n")
    list(
      regime = "growth",
      confidence = 0.5,  # Moderate confidence in default
      metrics = list(
        vol_zscore = 0,
        pmi_zscore = 0,
        global_growth_zscore = 0,
        cpi_zscore = 0,
        commodity_zscore = 0,
        bond_equity_zscore = 0,
        lookback_window = 252
      ),
      regime_probabilities = data.frame(
        growth = 0.7,
        reflation = 0.1,
        deflation = 0.1,
        stagflation = 0.0,
        risk_off = 0.1,
        inflation_shock = 0.0
      )
    )
  })
  
  # Ensure regime_probabilities is properly formatted
  regime_result$regime_probabilities <- ensure_regime_probabilities(regime_result$regime_probabilities)
  
  regime <- regime_result$regime
  regime_confidence <- regime_result$confidence
  regime_metrics <- regime_result$metrics
  regime_probs <- as.list(regime_result$regime_probabilities[1,])
  
  if (debug) {
    cat("\n=== REGIME DETECTION WITH PROBABILITY WEIGHTING ===\n")
    cat("Dominant market regime:", regime, "\n")
    cat("Regime confidence:", sprintf("%.1f%%", 100 * regime_confidence), "\n")
    cat("Regime probabilities:\n")
    for (r in names(regime_probs)) {
      cat(sprintf("  %s: %.1f%%\n", r, 100 * regime_probs[[r]]))
    }
    cat(sprintf("Volatility Z-score: %.2f\n", regime_metrics$vol_zscore))
    cat(sprintf("Growth Z-score: %.2f\n", regime_metrics$growth_signal))
    cat(sprintf("Inflation Z-score: %.2f\n", regime_metrics$inflation_signal))
    cat(sprintf("Risk Signal: %.2f\n", regime_metrics$risk_signal))
  }
  
  #--------------------------------------------------------------------------
  # STEP 2: Calculate base risk parity weights
  #--------------------------------------------------------------------------
  rp_result <- tryCatch({
    calculate_risk_parity_weights(
      returns, 
      target_vol = target_vol, 
      cov_method = cov_method,
      use_garch = use_garch
    )
  }, error = function(e) {
    cat("Error in risk parity calculation:", e$message, "\n")
    cat("Using equal weights as fallback\n")
    
    # Create equal weight fallback
    equal_weights <- rep(1/ncol(returns), ncol(returns))
    names(equal_weights) <- colnames(returns)
    
    # Create diagonal covariance matrix
    equal_cov <- diag(ncol(returns))
    colnames(equal_cov) <- colnames(returns)
    rownames(equal_cov) <- colnames(returns)
    
    # Calculate simple volatility
    equal_vol <- 0.15  # Assume 15% volatility
    
    list(
      weights = equal_weights,
      cov_matrix = equal_cov,
      port_vol = equal_vol,
      leverage = target_vol / equal_vol
    )
  })
  
  # Extract components from result with safety checks
  base_weights <- rp_result$weights        # Base weights (sum to 1)
  cov_matrix <- rp_result$cov_matrix       # Covariance matrix
  base_port_vol <- rp_result$port_vol      # Base portfolio volatility
  
  # Safety check - ensure base weights have proper names
  if (is.null(names(base_weights))) {
    names(base_weights) <- colnames(returns)
  }
  
  # Debug output
  if (debug) {
    cat("\nBase Risk Parity Weights (before adjustments):\n")
    print(round(sort(base_weights, decreasing = TRUE) * 100, 2))
    cat("Base Portfolio Volatility:", sprintf("%.2f%%\n", 100 * base_port_vol))
  }
  
  #--------------------------------------------------------------------------
  # STEP 3: Apply regime-based adjustments using probability weighting
  #--------------------------------------------------------------------------
  adjusted_weights <- base_weights
  
  tryCatch({
    # Get probability-weighted regime adjustments
    regime_adjustments <- get_regime_adjustments(regime_probs, include_shorts = enable_shorts)
    
    # Create mapping from tickers to asset classes with error checks
    ticker_to_sleeve <- list()
    for (ticker in colnames(returns)) {
      if (ticker %in% names(sleeve_mapping)) {
        ticker_to_sleeve[[ticker]] <- sleeve_mapping[[ticker]]
      } else {
        cat(sprintf("WARNING: No sleeve mapping for %s, using UNKNOWN\n", ticker))
        ticker_to_sleeve[[ticker]] <- "UNKNOWN"
      }
    }
    
    # Apply regime adjustments with safety checks
    for (ticker in names(adjusted_weights)) {
      if (!ticker %in% names(ticker_to_sleeve)) {
        cat(sprintf("WARNING: Ticker %s not in sleeve mapping\n", ticker))
        next
      }
      
      sleeve <- ticker_to_sleeve[[ticker]]
      
      # Apply adjustment if available for this sleeve
      if (sleeve %in% names(regime_adjustments)) {
        adjustment <- regime_adjustments[[sleeve]]
        
        # Safety check for adjustment value
        if (!is.numeric(adjustment) || is.na(adjustment)) {
          cat(sprintf("WARNING: Invalid adjustment for %s sleeve, using 1.0\n", sleeve))
          adjustment <- 1.0
        }
        
        # Apply adjustment with additional safety bounds
        adjustment <- min(max(adjustment, 0.3), 2.0)  # Limit extreme adjustments
        adjusted_weights[ticker] <- adjusted_weights[ticker] * adjustment
        
        if (debug) {
          cat(sprintf("Adjusting %s (%s) by %.2fx based on regime probabilities\n", 
                      ticker, sleeve, adjustment))
        }
      } else {
        if (debug) {
          cat(sprintf("No regime adjustment for sleeve: %s\n", sleeve))
        }
      }
    }
    
    # Check if adjusted weights contain any invalid values
    if (any(is.na(adjusted_weights))) {
      cat("WARNING: NA values in adjusted weights, using base weights\n")
      adjusted_weights <- base_weights
    }
    
    # Normalize adjusted weights to sum to 1
    if (sum(adjusted_weights) > 0) {
      adjusted_weights <- adjusted_weights / sum(adjusted_weights)
    } else {
      cat("WARNING: All adjusted weights are zero, reverting to base weights\n")
      adjusted_weights <- base_weights
    }
    
    # Add short overlays via inverse ETFs if enabled
    short_weights <- c()
    if (enable_shorts && !is.null(inverse_sleeve_map) && length(inverse_sleeve_map) > 0) {
      # Extract short allocations from regime adjustments
      short_allocations <- regime_adjustments[grep("^SHORT_", names(regime_adjustments))]
      
      if (length(short_allocations) > 0) {
        cat("\nAdding short overlay positions:\n")
        
        # Find available inverse ETFs in our returns data
        available_inverse <- intersect(names(inverse_sleeve_map), colnames(returns))
        
        if (length(available_inverse) > 0) {
          # For each inverse ETF, check if we have a matching short allocation
          for (inv_ticker in available_inverse) {
            short_type <- inverse_sleeve_map[[inv_ticker]]
            
            # If this short type has an allocation in our regime
            if (short_type %in% names(short_allocations)) {
              # Get the allocation (negative number)
              alloc <- short_allocations[[short_type]]
              
              # Only add if actually short (negative allocation)
              if (alloc < 0) {
                # Convert to positive weight for the inverse ETF
                short_weights[inv_ticker] <- abs(alloc)
                cat(sprintf("  %s (%s): %.2f%%\n", inv_ticker, short_type, 100 * abs(alloc)))
              }
            }
          }
        }
      }
    }
    
    # If we have short positions, add them to adjusted weights
    if (length(short_weights) > 0) {
      # Calculate how much to scale down long positions to make room for shorts
      total_short_weight <- sum(short_weights)
      if (total_short_weight > 0) {
        # Scale longs to make room for shorts
        adjusted_weights <- adjusted_weights * (1 - total_short_weight)
        
        # Add shorts to weights
        for (ticker in names(short_weights)) {
          adjusted_weights[ticker] <- short_weights[ticker]
        }
        
        cat(sprintf("Added short overlay positions totaling %.2f%% of portfolio\n", 
                    100 * total_short_weight))
      }
    }
    
  }, error = function(e) {
    cat("Error applying regime adjustments:", e$message, "\n")
    cat("Using base weights without adjustments\n")
  })
  
  # Debug output
  if (debug) {
    cat("\nRegime-Adjusted Weights (before cash allocation):\n")
    print(round(sort(adjusted_weights, decreasing = TRUE) * 100, 2))
  }
  
  #--------------------------------------------------------------------------
  # STEP 4: Calculate cash allocation based on risk conditions
  #--------------------------------------------------------------------------
  cash_allocation <- 0
  
  if (enable_cash) {
    tryCatch({
      # Get current drawdown and its velocity if portfolio return history provided
      current_drawdown <- 0
      drawdown_velocity <- 0
      
      if (!is.null(portfolio_returns) && length(portfolio_returns) > 20) {
        # Calculate drawdown using recent window (more responsive)
        recent_returns <- tail(portfolio_returns, min(length(portfolio_returns), drawdown_window))
        current_drawdown <- calculate_current_drawdown(recent_returns)
        
        # Calculate drawdown velocity (change in drawdown)
        if (length(portfolio_returns) > 22) {  # Need at least 1 month of data
          # Calculate current drawdown and drawdown from 20 days ago
          current_dd_path <- calculate_drawdown_path(portfolio_returns)
          current_dd <- tail(current_dd_path, 1)
          past_dd <- current_dd_path[length(current_dd_path) - 20]  # ~1 month ago
          
          # Velocity is change in drawdown per day
          drawdown_velocity <- (current_dd - past_dd) / 20
          
          if (debug) {
            cat(sprintf("\nDrawdown velocity: %.2f%% per day\n", 100 * drawdown_velocity))
          }
        }
        
        if (debug) {
          cat(sprintf("\nCurrent drawdown: %.2f%%\n", 100 * current_drawdown))
        }
      }
      
      # Calculate cash allocation based on drawdown, velocity and volatility
      cash_allocation <- calculate_cash_allocation(
        current_drawdown = current_drawdown,
        drawdown_velocity = drawdown_velocity,
        max_drawdown = max_drawdown,
        vol_zscore = regime_metrics$vol_zscore
      )
      
      # Safety check for cash allocation
      if (is.na(cash_allocation) || !is.finite(cash_allocation)) {
        cash_allocation <- 0
      }
      
      # Cap at reasonable maximum (20%)
      cash_allocation <- min(cash_allocation, 0.2)  
      
      if (debug && cash_allocation > 0) {
        cat(sprintf("Cash allocation: %.2f%%\n", 100 * cash_allocation))
      }
    }, error = function(e) {
      cat("Error calculating cash allocation:", e$message, "\n")
      cash_allocation <- 0  # Default to no cash on error
    })
  }
  
  #--------------------------------------------------------------------------
  # STEP 5: Apply transaction cost optimization if we have previous weights
  #--------------------------------------------------------------------------
  if (!is.null(prev_weights) && !is.null(transaction_costs)) {
    tryCatch({
      # Find common tickers between prev_weights and current weights
      common_tickers <- intersect(names(prev_weights), names(adjusted_weights))
      
      if (length(common_tickers) > 0) {
        # Track changes for transaction cost calculation
        changes <- numeric(length(common_tickers))
        names(changes) <- common_tickers
        total_turnover <- 0
        big_changes <- c()
        
        for (ticker in common_tickers) {
          prev_weight <- prev_weights[ticker]
          curr_weight <- adjusted_weights[ticker]
          
          # Skip if either weight is NA
          if (is.na(prev_weight) || is.na(curr_weight)) {
            next
          }
          
          # Calculate change size
          change_size <- abs(curr_weight - prev_weight)
          changes[ticker] <- change_size
          total_turnover <- total_turnover + change_size
          
          # Identify large changes
          if (change_size > 0.05) {  # 5% or larger weight change
            big_changes <- c(big_changes, ticker)
          }
        }
        
        # If turnover is high, moderate changes to reduce costs
        if (total_turnover > 0.3 && length(big_changes) > 0) {  # 30% turnover threshold
          cat(sprintf("High turnover detected (%.1f%%), moderating changes\n", total_turnover * 100))
          
          # Progressively blend weights based on change size
          for (ticker in common_tickers) {
            prev_weight <- prev_weights[ticker]
            curr_weight <- adjusted_weights[ticker]
            
            # Skip if either weight is NA
            if (is.na(prev_weight) || is.na(curr_weight)) {
              next
            }
            
            # Calculate change size
            change_size <- abs(curr_weight - prev_weight)
            
            # Determine blending factor based on change size
            # Larger changes get more moderation
            if (change_size > 0.1) {
              # For very large changes (>10%), use 60/40 blend
              blend_factor <- 0.6
            } else if (change_size > 0.05) {
              # For medium changes (5-10%), use 70/30 blend
              blend_factor <- 0.7
            } else {
              # For small changes (<5%), use 80/20 blend
              blend_factor <- 0.8
            }
            
            # Apply blending
            adjusted_weights[ticker] <- blend_factor * curr_weight + (1 - blend_factor) * prev_weight
          }
          
          # Re-normalize weights
          adjusted_weights <- adjusted_weights / sum(adjusted_weights)
          
          # Recalculate turnover after blending
          new_turnover <- 0
          for (ticker in common_tickers) {
            prev_weight <- prev_weights[ticker]
            curr_weight <- adjusted_weights[ticker]
            
            if (!is.na(prev_weight) && !is.na(curr_weight)) {
              change_size <- abs(curr_weight - prev_weight)
              new_turnover <- new_turnover + change_size
            }
          }
          
          if (debug) {
            cat(sprintf("Turnover reduced from %.1f%% to %.1f%%\n", 
                        total_turnover * 100, new_turnover * 100))
          }
        }
      }
    }, error = function(e) {
      cat("Error in transaction cost optimization:", e$message, "\n")
      cat("Using unmodified weights\n")
    })
  }
  
  #--------------------------------------------------------------------------
  # STEP 6: Finalize weights, including cash allocation
  #--------------------------------------------------------------------------
  
  # Initialize final weights with adjusted weights
  final_weights <- adjusted_weights
  
  # Apply cash allocation if enabled
  if (enable_cash && cash_allocation > 0) {
    # Scale asset weights by (1 - cash_allocation)
    final_weights <- final_weights * (1 - cash_allocation)
    
    # Add cash allocation
    final_weights["CASH"] <- cash_allocation
  }
  
  # Check for any invalid weights
  if (any(is.na(final_weights)) || any(!is.finite(final_weights))) {
    cat("WARNING: Invalid values in final weights, replacing with fallback\n")
    
    # Create fallback weights
    fallback_weights <- rep(1/ncol(returns), ncol(returns))
    names(fallback_weights) <- colnames(returns)
    
    # Add cash if needed
    if (enable_cash && cash_allocation > 0) {
      fallback_weights <- fallback_weights * (1 - cash_allocation)
      fallback_weights["CASH"] <- cash_allocation
    }
    
    final_weights <- fallback_weights
  }
  
  # Verify final weights sum to 1
  weights_sum <- sum(final_weights)
  if (abs(weights_sum - 1) > 0.001) {
    cat(sprintf("WARNING: Final weights sum to %.4f, normalizing\n", weights_sum))
    final_weights <- final_weights / weights_sum
  }
  
  # Calculate expected portfolio volatility (excluding cash)
  expected_vol <- tryCatch({
    # Create a version of weights without cash
    asset_weights <- final_weights[names(final_weights) != "CASH"]
    
    # Normalize these weights to sum to 1
    if (sum(asset_weights) > 0) {
      asset_weights <- asset_weights / sum(asset_weights)
    }
    
    # Match names to covariance matrix
    matching_assets <- intersect(names(asset_weights), colnames(cov_matrix))
    
    if (length(matching_assets) > 0) {
      asset_weights_vec <- as.numeric(asset_weights[matching_assets])
      cov_submatrix <- cov_matrix[matching_assets, matching_assets]
      
      # Calculate portfolio variance
      port_var <- t(asset_weights_vec) %*% cov_submatrix %*% asset_weights_vec
      
      # Annualized vol, reduced by cash allocation
      vol <- sqrt(port_var) * sqrt(252) * (1 - cash_allocation)
      
      # Safety check
      if (is.na(vol) || !is.finite(vol) || vol < 0) {
        vol <- base_port_vol * (1 - cash_allocation)  # Fallback to base vol
      }
      
      vol
    } else {
      base_port_vol * (1 - cash_allocation)  # Fallback if no matching assets
    }
  }, error = function(e) {
    cat("Error calculating expected portfolio volatility:", e$message, "\n")
    base_port_vol * (1 - cash_allocation)  # Fallback to base vol
  })
  
  # Debug output
  if (debug) {
    cat("\nFinal Portfolio Weights:\n")
    print(round(sort(final_weights, decreasing = TRUE) * 100, 2))
    cat(sprintf("Expected Portfolio Volatility: %.2f%%\n", 100 * expected_vol))
    
    if ("CASH" %in% names(final_weights) && final_weights["CASH"] > 0) {
      cat(sprintf("Cash Allocation: %.2f%%\n", 100 * final_weights["CASH"]))
    }
  }
  
  # Calculate current drawdown if we have portfolio return history
  current_drawdown <- 0
  if (!is.null(portfolio_returns) && length(portfolio_returns) > 0) {
    current_drawdown <- calculate_current_drawdown(portfolio_returns)
  }
  
  # Return the complete result
  return(list(
    weights = final_weights,              # Final portfolio weights
    regime = regime,                      # Detected dominant regime
    regime_metrics = regime_metrics,      # Z-score metrics
    regime_confidence = regime_confidence, # Confidence in regime detection
    regime_probabilities = regime_result$regime_probabilities,  # Full probability distribution
    expected_vol = expected_vol,          # Expected portfolio volatility
    cash_allocation = cash_allocation,    # Cash allocation percentage
    current_drawdown = current_drawdown,  # Current portfolio drawdown
    cov_matrix = cov_matrix,              # Covariance matrix used
    base_weights = base_weights           # Base risk parity weights before adjustments
  ))
}

#=============================================================================
# BACKTESTING FRAMEWORK WITH IMPROVED TRANSACTION COST TRACKING
#=============================================================================

# Backtesting function with real data and fixed transaction cost tracking
backtest_optimized_strategy <- function(prices, returns, market_data, sleeve_mapping, 
                                        target_vol = 0.075,      # Target volatility
                                        max_drawdown = 0.075,    # Maximum drawdown limit
                                        rebalance_freq = "M",    # Monthly rebalancing
                                        lookback_window = 252,   # 1-year lookback
                                        use_garch = TRUE,        # Use GARCH volatility forecasting
                                        cov_method = "ledoit-wolf", # Covariance method
                                        transaction_costs = NULL,# Transaction costs by ticker
                                        enable_cash = TRUE,      # Enable cash allocation
                                        enable_shorts = TRUE) {  # Enable short overlays
  
  # Begin with comprehensive error handling and logging
  cat("\n========== STARTING BACKTEST WITH REAL DATA ==========\n")
  cat("Target volatility:", sprintf("%.2f%%", target_vol * 100), "\n")
  cat("Max drawdown limit:", sprintf("%.2f%%", max_drawdown * 100), "\n")
  cat("Rebalance frequency:", rebalance_freq, "\n")
  cat("Lookback window:", lookback_window, "days\n")
  cat("Covariance method:", cov_method, ifelse(use_garch, "with GARCH", "without GARCH"), "\n")
  cat("Short overlays:", ifelse(enable_shorts, "enabled", "disabled"), "\n")
  
  #--------------------------------------------------------------------------
  # STEP 1: Safety checks and data validation
  #--------------------------------------------------------------------------
  
  # CRITICAL: Ensure we have proper Date objects for indices
  tryCatch({
    # Convert all indices to proper Date objects
    if (!inherits(index(prices), "Date")) {
      cat("Converting price indices to Date objects\n")
      index(prices) <- as.Date(index(prices))
    }
    if (!inherits(index(returns), "Date")) {
      cat("Converting returns indices to Date objects\n")
      index(returns) <- as.Date(index(returns))
    }
    if (!inherits(index(market_data), "Date")) {
      cat("Converting market_data indices to Date objects\n")
      index(market_data) <- as.Date(index(market_data))
    }
  }, error = function(e) {
    stop(paste("Failed to convert indices to Date objects:", e$message))
  })

# Verify all data inputs are valid
if (is.null(prices) || nrow(prices) == 0 || ncol(prices) == 0) {
  stop("Invalid price data - empty or NULL")
}

if (is.null(returns) || nrow(returns) == 0 || ncol(returns) == 0) {
  stop("Invalid returns data - empty or NULL")
}

if (is.null(market_data) || nrow(market_data) == 0) {
  stop("Invalid market data - empty or NULL")
}

if (is.null(sleeve_mapping) || length(sleeve_mapping) == 0) {
  cat("WARNING: Missing sleeve mapping. Creating default mapping.\n")
  
  # Create default mapping for all assets
  sleeve_mapping <- list()
  for (ticker in colnames(returns)) {
    sleeve_mapping[[ticker]] <- if(grepl("^(SPY|IVV|VOO|QQQ)", ticker)) "US_EQUITY" else
      if(grepl("^(IWM|IJR)", ticker)) "US_SMALL_CAP" else
        if(grepl("^(EFA|VEA)", ticker)) "INTL_DEVELOPED" else
          if(grepl("^(EEM|VWO)", ticker)) "EMERGING_MARKETS" else
            if(grepl("^(IEF|TLT|SHY)", ticker)) "US_TREASURY" else
              if(grepl("^(TIP)", ticker)) "TIPS" else
                if(grepl("^(LQD|VCIT)", ticker)) "CREDIT_IG" else
                  if(grepl("^(GLD|IAU)", ticker)) "GOLD" else
                    if(grepl("^(DBC|GSG)", ticker)) "COMMODITIES" else
                      if(grepl("^(VNQ|IYR)", ticker)) "REIT" else
                        if(grepl("^(SH|PSQ|DOG|RWM)", ticker)) "SHORT_EQUITY" else
                          if(grepl("^(TBF|TBX)", ticker)) "SHORT_TREASURY" else
                            if(grepl("^(SJB|IGSD)", ticker)) "SHORT_CREDIT" else
                              "OTHER"
  }
}

# Align all data to common dates for guaranteed consistency
cat("\nEnsuring data alignment before backtest...\n")

common_dates <- tryCatch({
  as.Date(intersect(intersect(
    as.character(index(prices)), 
    as.character(index(returns))), 
    as.character(index(market_data))
  ))
}, error = function(e) {
  stop(paste("Failed to find common dates:", e$message))
})

if (length(common_dates) == 0) {
  stop("No common dates found between prices, returns, and market data.")
}

cat(sprintf("Found %d common dates from %s to %s\n", 
            length(common_dates), 
            format(min(common_dates), "%Y-%m-%d"),
            format(max(common_dates), "%Y-%m-%d")))

# Subset all data to common dates
prices <- prices[common_dates]
returns <- returns[common_dates]
market_data <- market_data[common_dates]

# Verify dimensions match after alignment
if (nrow(prices) != nrow(returns) || nrow(prices) != nrow(market_data)) {
  stop(sprintf("Dimension mismatch after alignment: prices=%d rows, returns=%d rows, market_data=%d rows",
               nrow(prices), nrow(returns), nrow(market_data)))
}

# Check if we have enough data
if (nrow(prices) < lookback_window) {
  stop(sprintf("Not enough data for backtest. Need at least %d rows, but only have %d.",
               lookback_window, nrow(prices)))
}

# If transaction costs not provided, estimate them
if (is.null(transaction_costs)) {
  cat("\n==== ESTIMATING TRANSACTION COSTS ====\n")
  transaction_costs <- get_transaction_costs(colnames(prices))
}

# CRITICAL: Ensure transaction costs exist for ALL assets
missing_costs <- setdiff(colnames(returns), names(transaction_costs))
if (length(missing_costs) > 0) {
  cat("WARNING: Adding missing transaction costs for", length(missing_costs), "assets\n")
  for (ticker in missing_costs) {
    transaction_costs[ticker] <- 0.0004  # Default 4 bps
  }
}

#--------------------------------------------------------------------------
# STEP 2: Determine rebalance dates with multiple fallbacks
#--------------------------------------------------------------------------

rebalance_dates <- tryCatch({
  if (rebalance_freq == "D") {
    # Daily rebalancing - use all dates
    index(prices)
  } else if (rebalance_freq == "W") {
    # Weekly rebalancing - find week ends
    week_ends <- endpoints(prices, on = "weeks")
    week_ends <- week_ends[week_ends > 0]  # Remove the starting 0
    index(prices)[week_ends]
  } else if (rebalance_freq == "M") {
    # Monthly rebalancing - find month ends
    month_ends <- endpoints(prices, on = "months")
    month_ends <- month_ends[month_ends > 0]  # Remove the starting 0
    index(prices)[month_ends]
  } else if (rebalance_freq == "Q") {
    # Quarterly rebalancing - find quarter ends
    quarter_ends <- endpoints(prices, on = "quarters")
    quarter_ends <- quarter_ends[quarter_ends > 0]  # Remove the starting 0
    index(prices)[quarter_ends]
  } else {
    stop("Invalid rebalance frequency. Use 'D', 'W', 'M', or 'Q'.")
  }
}, error = function(e) {
  cat("Error determining rebalance dates:", e$message, "\n")
  cat("Using month-end dates as fallback\n")
  
  # Fallback to month-end dates
  try({
    month_ends <- endpoints(prices, on = "months")
    month_ends <- month_ends[month_ends > 0]  # Remove the starting 0
    index(prices)[month_ends]
  }, silent = TRUE)
})

# Double safety check - if still no valid rebalance dates, use manual method
if (is.null(rebalance_dates) || length(rebalance_dates) == 0) {
  cat("ERROR: Failed to determine rebalance dates. Using manual date selection.\n")
  
  # Get all dates
  all_dates <- index(prices)
  
  # Try to identify month ends manually
  years <- as.numeric(format(all_dates, "%Y"))
  months <- as.numeric(format(all_dates, "%m"))
  year_month <- paste(years, months, sep = "-")
  unique_year_months <- unique(year_month)
  
  # Find the last date of each month
  rebalance_dates <- c()
  for (ym in unique_year_months) {
    indices <- which(year_month == ym)
    if (length(indices) > 0) {
      last_idx <- max(indices)
      rebalance_dates <- c(rebalance_dates, all_dates[last_idx])
    }
  }
  
  # If still no rebalance dates, use first and last date plus some in between
  if (length(rebalance_dates) == 0) {
    cat("CRITICAL: Still no rebalance dates. Using first, last, and quarterly points.\n")
    n_days <- length(all_dates)
    rebalance_indices <- unique(c(
      1,  # First date
      round(n_days * c(0.25, 0.5, 0.75)),  # Quarter points
      n_days  # Last date
    ))
    rebalance_dates <- all_dates[rebalance_indices]
  }
}

cat(sprintf("Identified %d rebalance dates from %s to %s\n", 
            length(rebalance_dates), 
            format(min(rebalance_dates), "%Y-%m-%d"),
            format(max(rebalance_dates), "%Y-%m-%d")))

#--------------------------------------------------------------------------
# STEP 3: Initialize backtest variables with guaranteed safety
#--------------------------------------------------------------------------

# Initialize weights matrix - one row per day, one column per asset plus cash
weights <- tryCatch({
  xts(matrix(0, nrow = nrow(prices), ncol = ncol(returns) + 1),
      order.by = index(prices),
      dimnames = list(NULL, c(colnames(returns), "CASH")))
}, error = function(e) {
  cat("Error initializing weights matrix:", e$message, "\n")
  # Fallback - create using simple data.frame then convert to xts
  w_matrix <- matrix(0, nrow = nrow(prices), ncol = ncol(returns) + 1)
  colnames(w_matrix) <- c(colnames(returns), "CASH")
  xts(w_matrix, order.by = index(prices))
})

# Initialize returns and other tracking variables
portfolio_returns <- xts(rep(0, nrow(prices)), order.by = index(prices))
portfolio_details <- list()

# Improved transaction cost tracking by asset and total
transaction_cost_history <- xts(rep(0, nrow(prices)), order.by = index(prices))
transaction_cost_by_asset <- matrix(0, nrow = nrow(prices), ncol = ncol(returns))
colnames(transaction_cost_by_asset) <- colnames(returns)
transaction_cost_by_asset <- xts(transaction_cost_by_asset, order.by = index(prices))

# Start with equal weights
current_weights <- rep(1/ncol(returns), ncol(returns))
names(current_weights) <- colnames(returns)

# Initialize with "growth" default for proper regime tracking
regime_history <- xts(rep("growth", nrow(prices)), order.by = index(prices))

# Track regime probabilities - create one column per regime
regime_probability_columns <- c("growth", "reflation", "deflation", "stagflation", "risk_off", "inflation_shock")
regime_probabilities <- matrix(0, nrow = nrow(prices), ncol = length(regime_probability_columns))
colnames(regime_probabilities) <- regime_probability_columns
regime_probabilities <- xts(regime_probabilities, order.by = index(prices))

# Track Z-scores with safe initialization
vol_zscore_history <- xts(rep(0, nrow(prices)), order.by = index(prices))
pmi_zscore_history <- xts(rep(0, nrow(prices)), order.by = index(prices))
cpi_zscore_history <- xts(rep(0, nrow(prices)), order.by = index(prices))
corr_zscore_history <- xts(rep(0, nrow(prices)), order.by = index(prices))

# Track drawdowns and cash
drawdown_history <- xts(rep(0, nrow(prices)), order.by = index(prices))
cash_history <- xts(rep(0, nrow(prices)), order.by = index(prices))

# Start backtest
cat("\nStarting backtest with comprehensive safety measures...\n")

# Force first rebalance on day 1
first_rebalance_done <- FALSE

# Store last valid regime detection for filling between rebalances
last_valid_regime <- "growth"
last_valid_regime_probs <- setNames(
  c(1.0, 0, 0, 0, 0, 0),
  regime_probability_columns
)

#--------------------------------------------------------------------------
# STEP 4: Run backtest with comprehensive error handling
#--------------------------------------------------------------------------

for (i in 1:nrow(prices)) {
  # Get current date
  date <- index(prices)[i]
  
  # PHASE 1: Initial portfolio construction if needed
  if (!first_rebalance_done && i >= lookback_window) {
    # Force first rebalance
    cat(sprintf("\nConstructing initial portfolio on %s (row %d of %d)...\n", 
                format(date, "%Y-%m-%d"), i, nrow(prices)))
    
    # Get historical data safely
    hist_start_idx <- max(1, i - lookback_window)
    
    # Safety check for index bounds
    if (hist_start_idx <= i && i <= nrow(returns)) {
      hist_returns <- returns[hist_start_idx:i,]
      hist_market_data <- market_data[hist_start_idx:i,]
      
      # Construct initial portfolio with error handling
      tryCatch({
        portfolio <- construct_optimized_risk_parity(
          hist_returns,
          hist_market_data,
          sleeve_mapping,
          target_vol = target_vol,
          max_drawdown = max_drawdown,
          use_garch = use_garch,
          cov_method = cov_method,
          transaction_costs = transaction_costs,
          enable_cash = enable_cash,
          enable_shorts = enable_shorts
        )
        
        # Set initial weights
        current_weights <- portfolio$weights
        
        # Store portfolio details
        portfolio_details[[as.character(date)]] <- portfolio
        
        # Track cash allocation with bounds checking
        if ("CASH" %in% names(current_weights)) {
          cash_history[i] <- current_weights["CASH"]
        } else {
          cash_history[i] <- 0
        }
        
        # Track the current regime reliably
        if (!is.null(portfolio$regime) && portfolio$regime != "") {
          regime_history[i] <- portfolio$regime
          last_valid_regime <- portfolio$regime
        } else {
          regime_history[i] <- last_valid_regime
        }
        
        # FIX: Properly handle regime probabilities from data.frame format
        if (!is.null(portfolio$regime_probabilities) && is.data.frame(portfolio$regime_probabilities) && 
            nrow(portfolio$regime_probabilities) > 0) {
          for (regime_name in colnames(portfolio$regime_probabilities)) {
            if (regime_name %in% colnames(regime_probabilities)) {
              # Access the probability correctly from data.frame
              regime_probabilities[i, regime_name] <- portfolio$regime_probabilities[1, regime_name]
              last_valid_regime_probs[regime_name] <- portfolio$regime_probabilities[1, regime_name]
            }
          }
        }
        
        # Track Z-scores with safety checks
        vol_zscore_history[i] <- ifelse(
          !is.null(portfolio$regime_metrics$vol_zscore) && 
            !is.na(portfolio$regime_metrics$vol_zscore),
          portfolio$regime_metrics$vol_zscore, 0)
        
        pmi_zscore_history[i] <- ifelse(
          !is.null(portfolio$regime_metrics$growth_signal) && 
            !is.na(portfolio$regime_metrics$growth_signal),
          portfolio$regime_metrics$growth_signal, 0)
        
        cpi_zscore_history[i] <- ifelse(
          !is.null(portfolio$regime_metrics$inflation_signal) && 
            !is.na(portfolio$regime_metrics$inflation_signal),
          portfolio$regime_metrics$inflation_signal, 0)
        
        corr_zscore_history[i] <- ifelse(
          !is.null(portfolio$regime_metrics$bond_equity_zscore) && 
            !is.na(portfolio$regime_metrics$bond_equity_zscore),
          portfolio$regime_metrics$bond_equity_zscore, 0)
        
        # Log success
        cat(sprintf("Initial portfolio on %s - Regime: %s (Vol Z=%.2f) - Expected Vol: %.2f%% - Cash: %.1f%%\n", 
                    format(date, "%Y-%m-%d"), portfolio$regime, 
                    portfolio$regime_metrics$vol_zscore,
                    100 * portfolio$expected_vol,
                    100 * ifelse("CASH" %in% names(current_weights), current_weights["CASH"], 0)))
        
        first_rebalance_done <- TRUE
        
      }, error = function(e) {
        cat("ERROR: Initial portfolio construction failed:", e$message, "\n")
        cat("Using equal weights as fallback\n")
        
        # Use equal weights as fallback
        current_weights <- setNames(rep(1/ncol(returns), ncol(returns)), colnames(returns))
        first_rebalance_done <- TRUE
        
        # Set default values for tracking variables
        regime_history[i] <- "growth"
        last_valid_regime <- "growth"
        
        # Set default regime probabilities
        regime_probabilities[i, "growth"] <- 1.0
      })
    } else {
      cat("WARNING: Invalid index range for initial portfolio construction\n")
      # Use equal weights as fallback
      current_weights <- setNames(rep(1/ncol(returns), ncol(returns)), colnames(returns))
      first_rebalance_done <- TRUE
    }
  }
  
  # PHASE 2: Calculate daily portfolio return with transaction costs
  # ULTRA-SAFE IMPLEMENTATION to prevent subscript out of bounds
  
  # Get today's returns for all assets
  daily_returns <- returns[i,]
  
  # Track transaction costs with maximum safety - FIXED IMPLEMENTATION
  daily_cost <- 0
  daily_cost_by_asset <- numeric(ncol(returns))
  names(daily_cost_by_asset) <- colnames(returns)
  
  # Calculate transaction costs when weights change (on rebalance dates)
  if (i > 1 && date %in% rebalance_dates) {
    # Create named vectors for all assets (prev weights)
    prev_asset_weights <- rep(0, ncol(returns))
    names(prev_asset_weights) <- colnames(returns)
    
    # Populate previous weights safely
    for (ticker in colnames(returns)) {
      if (ticker %in% colnames(weights)) {
        prev_asset_weights[ticker] <- as.numeric(weights[i-1, ticker])
      }
    }
    
    # Create named vectors for all assets (current weights)
    current_asset_weights <- rep(0, ncol(returns))
    names(current_asset_weights) <- colnames(returns)
    
    # Populate current weights safely
    for (ticker in names(current_weights)) {
      if (ticker %in% colnames(returns)) {
        current_asset_weights[ticker] <- current_weights[ticker]
      }
    }
    
    # Calculate costs ticker by ticker to avoid vector issues
    for (ticker in colnames(returns)) {
      # Skip if ticker doesn't have transaction costs
      if (!ticker %in% names(transaction_costs)) {
        next
      }
      
      # Calculate absolute change in weight
      prev_weight <- prev_asset_weights[ticker]
      curr_weight <- current_asset_weights[ticker]
      
      # Safety check for NA or infinite values
      if (is.na(prev_weight) || !is.finite(prev_weight)) {
        prev_weight <- 0
      }
      if (is.na(curr_weight) || !is.finite(curr_weight)) {
        curr_weight <- 0
      }
      
      # Calculate trade size and cost
      trade_size <- abs(curr_weight - prev_weight)
      cost <- trade_size * transaction_costs[ticker]
      
      # Safety check for cost
      if (is.na(cost) || !is.finite(cost)) {
        cost <- 0
      }
      
      # FIXED: Store cost both by asset and total
      daily_cost_by_asset[ticker] <- cost
      daily_cost <- daily_cost + cost
    }
    
    # Cap total costs at reasonable maximum
    if (daily_cost > 0.02) {  # Cap at 2%
      cat(sprintf("WARNING: Extremely high transaction cost (%.2f%%) on %s, capping at 2%%\n", 
                  100 * daily_cost, format(date, "%Y-%m-%d")))
      
      # Scale down all costs proportionally to maintain relative costs
      if (daily_cost > 0) {
        scaling_factor <- 0.02 / daily_cost
        daily_cost_by_asset <- daily_cost_by_asset * scaling_factor
        daily_cost <- 0.02
      }
    }
    
    # Record costs in both places
    transaction_cost_history[i] <- daily_cost
    for (ticker in names(daily_cost_by_asset)) {
      transaction_cost_by_asset[i, ticker] <- daily_cost_by_asset[ticker]
    }
  }
  
  # ULTRA-SAFE RETURN CALCULATION: Completely isolated from vector indexing issues
  if (all(is.na(daily_returns))) {
    portfolio_returns[i] <- 0  # Skip days with no return data
  } else {
    # Initialize variables for return calculation
    weighted_return <- 0
    weight_sum <- 0
    
    # Process each asset individually to avoid vector issues
    for (j in 1:length(colnames(returns))) {
      ticker <- colnames(returns)[j]
      
      # Get return for this asset
      asset_return <- as.numeric(daily_returns[1, j])
      
      # Only process if we have a valid return
      if (!is.na(asset_return)) {
        # Get weight for this asset
        asset_weight <- 0
        if (ticker %in% names(current_weights)) {
          asset_weight <- current_weights[ticker]
        }
        
        # Safety check on weight
        if (is.na(asset_weight) || !is.finite(asset_weight)) {
          asset_weight <- 0
        }
        
        # Add to weighted return
        if (asset_weight > 0) {
          weighted_return <- weighted_return + (asset_return * asset_weight)
          weight_sum <- weight_sum + asset_weight
        }
      }
    }
    
    # Normalize by weight sum if needed
    if (weight_sum > 0) {
      weighted_return <- weighted_return / weight_sum
    }
    
    # Account for cash allocation
    cash_weight <- 0
    if ("CASH" %in% names(current_weights)) {
      cash_weight <- current_weights["CASH"]
      if (is.na(cash_weight) || !is.finite(cash_weight)) {
        cash_weight <- 0
      }
    }
    
    # Only the invested portion (non-cash) gets the weighted return
    if (cash_weight < 1) {
      asset_return <- weighted_return * (1 - cash_weight)
    } else {
      asset_return <- 0  # All in cash
    }
    
    # Final return calculation (subtract costs)
    portfolio_returns[i] <- asset_return - daily_cost
  }
  
  # PHASE 3: Store current weights with maximum safety
  for (col in colnames(weights)) {
    if (col %in% names(current_weights)) {
      # Get weight for this column
      col_weight <- current_weights[col]
      
      # Safety check
      if (is.na(col_weight) || !is.finite(col_weight)) {
        col_weight <- 0
      }
      
      # Store weight
      weights[i, col] <- col_weight
    } else {
      weights[i, col] <- 0
    }
  }
  
  # PHASE 4: Calculate drawdown for tracking - IMPROVED MORE RESPONSIVE CALCULATION
  drawdown_history[i] <- tryCatch({
    if (i > 1) {
      # Calculate with recent window focus for more responsive tracking
      lookback_period <- 63  # ~3 months
      if (i > lookback_period) {
        # Use more recent history for drawdown calculation
        recent_returns <- portfolio_returns[(i-lookback_period):i]
        drawdown <- calculate_current_drawdown(recent_returns)
      } else {
        # Use all available history if not enough data
        available_returns <- portfolio_returns[1:i]
        drawdown <- calculate_current_drawdown(available_returns)
      }
      drawdown
    } else {
      0
    }
  }, error = function(e) {
    cat("Error calculating drawdown:", e$message, "\n")
    0
  })
  
  # PHASE 5: Perform rebalancing on schedule
  if (date %in% rebalance_dates && i > lookback_window && first_rebalance_done) {
    # Only log every few rebalances to reduce output
    if (which(date == rebalance_dates) %% 5 == 0) {
      cat(sprintf("\nRebalancing on %s (row %d of %d)...\n", 
                  format(date, "%Y-%m-%d"), i, nrow(prices)))
    }
    
    # Get historical data for lookback window with bounds checking
    hist_start_idx <- max(1, i - lookback_window)
    
    # Safety check for index bounds
    if (hist_start_idx <= i && i <= nrow(returns)) {
      hist_returns <- returns[hist_start_idx:i,]
      hist_market_data <- market_data[hist_start_idx:i,]
      
      # Get portfolio return history for drawdown control
      port_return_history <- portfolio_returns[1:i]
      
      # Construct new portfolio with comprehensive error handling
      tryCatch({
        portfolio <- construct_optimized_risk_parity(
          hist_returns,
          hist_market_data,
          sleeve_mapping,
          portfolio_returns = port_return_history,
          prev_weights = current_weights,  # Pass current weights for transaction cost optimization
          target_vol = target_vol,
          max_drawdown = max_drawdown,
          use_garch = use_garch,
          cov_method = cov_method,
          transaction_costs = transaction_costs,
          enable_cash = enable_cash,
          enable_shorts = enable_shorts,
          debug = FALSE  # Reduce verbosity during rebalance
        )
        
        # Update weights
        current_weights <- portfolio$weights
        
        # Track cash allocation
        if ("CASH" %in% names(current_weights)) {
          cash_history[i] <- current_weights["CASH"]
        } else {
          cash_history[i] <- 0
        }
        
        # Store portfolio details
        portfolio_details[[as.character(date)]] <- portfolio
        
        # Track regime reliably and update history
        if (!is.null(portfolio$regime) && portfolio$regime != "") {
          regime_history[i] <- portfolio$regime
          last_valid_regime <- portfolio$regime
        } else {
          regime_history[i] <- last_valid_regime
        }
        
        # FIX: Properly handle regime probabilities from data.frame format
        if (!is.null(portfolio$regime_probabilities) && is.data.frame(portfolio$regime_probabilities) && 
            nrow(portfolio$regime_probabilities) > 0) {
          # Copy regime probabilities to history
          for (regime_name in colnames(regime_probabilities)) {
            if (regime_name %in% colnames(portfolio$regime_probabilities)) {
              prob_value <- portfolio$regime_probabilities[1, regime_name]
              regime_probabilities[i, regime_name] <- prob_value
              last_valid_regime_probs[regime_name] <- prob_value
            }
          }
          
          # Update previous days since last rebalance with gradually interpolated regime probabilities
          if (i > 1) {
            # Find the last rebalance date with safety checks
            prev_rebalance_dates <- rebalance_dates[rebalance_dates < date]
            
            if (length(prev_rebalance_dates) > 0) {
              last_rebal_date <- max(prev_rebalance_dates)
              
              # Find index of last rebalance date
              last_rebal_idx <- which(index(regime_probabilities) == last_rebal_date)
              
              # If we have a previous rebalance record, interpolate regime probabilities
              if (length(last_rebal_idx) > 0 && last_rebal_idx < i) {
                update_range <- (last_rebal_idx+1):(i-1)
                
                # Safety check for valid range
                if (length(update_range) > 0 && 
                    min(update_range) >= 1 && 
                    max(update_range) <= nrow(regime_probabilities)) {
                  
                  # Get previous regime probs
                  prev_probs <- as.numeric(regime_probabilities[last_rebal_idx,])
                  
                  # Get current regime probs
                  curr_probs <- as.numeric(regime_probabilities[i,])
                  
                  # Number of days to interpolate
                  days <- length(update_range)
                  
                  # For each day, linearly interpolate probabilities
                  for (j in 1:days) {
                    # Weight (0 to 1) based on position
                    weight <- j / (days + 1)
                    
                    # Interpolate all regime probabilities
                    interp_probs <- (1 - weight) * prev_probs + weight * curr_probs
                    
                    # Normalize to sum to 1
                    interp_probs <- interp_probs / sum(interp_probs)
                    
                    # Store interpolated probabilities
                    regime_probabilities[update_range[j],] <- interp_probs
                    
                    # Also update the most likely regime for that day
                    max_prob_idx <- which.max(interp_probs)
                    regime_history[update_range[j]] <- colnames(regime_probabilities)[max_prob_idx]
                  }
                }
              }
            }
          }
        }
        
        # Track Z-scores with safety checks
        vol_zscore_history[i] <- ifelse(
          !is.null(portfolio$regime_metrics$vol_zscore) && 
            !is.na(portfolio$regime_metrics$vol_zscore),
          portfolio$regime_metrics$vol_zscore, 0)
        
        pmi_zscore_history[i] <- ifelse(
          !is.null(portfolio$regime_metrics$growth_signal) && 
            !is.na(portfolio$regime_metrics$growth_signal),
          portfolio$regime_metrics$growth_signal, 0)
        
        cpi_zscore_history[i] <- ifelse(
          !is.null(portfolio$regime_metrics$inflation_signal) && 
            !is.na(portfolio$regime_metrics$inflation_signal),
          portfolio$regime_metrics$inflation_signal, 0)
        
        corr_zscore_history[i] <- ifelse(
          !is.null(portfolio$regime_metrics$bond_equity_zscore) && 
            !is.na(portfolio$regime_metrics$bond_equity_zscore),
          portfolio$regime_metrics$bond_equity_zscore, 0)
        
        # Report status on major rebalances
        if (which(date == rebalance_dates) %% 5 == 0) {
          # Get current drawdown safely
          current_drawdown <- ifelse(
            !is.null(portfolio$current_drawdown) && 
              !is.na(portfolio$current_drawdown) &&
              is.finite(portfolio$current_drawdown),
            portfolio$current_drawdown, 0)
          
          # Only report significant drawdowns
          dd_msg <- ""
          if (current_drawdown > max_drawdown * 0.5) {
            dd_msg <- sprintf(" - Drawdown: %.2f%% (%.0f%% of max)",
                              current_drawdown * 100, 
                              (current_drawdown/max_drawdown) * 100)
          }
          
          # Format regime probabilities for display
          prob_str <- "unknown"
          if (!is.null(portfolio$regime_probabilities) && is.data.frame(portfolio$regime_probabilities) && 
              nrow(portfolio$regime_probabilities) > 0) {
            # Get top 2 regimes by probability
            regime_names <- colnames(portfolio$regime_probabilities)
            prob_values <- as.numeric(portfolio$regime_probabilities[1,])
            
            # Sort by probability and get top 2
            sorted_indices <- order(prob_values, decreasing = TRUE)
            top_count <- min(2, length(sorted_indices))
            top_regimes <- regime_names[sorted_indices[1:top_count]]
            top_probs <- prob_values[sorted_indices[1:top_count]]
            
            # Format the string
            prob_str <- paste(sprintf("%s: %.0f%%", top_regimes, 100 * top_probs), collapse = ", ")
          }
          
          # Log status
          cat(sprintf("Rebalanced on %s - Regime: %s (%s) - Vol: %.2f%% - Cash: %.1f%%%s\n",
                      format(date, "%Y-%m-%d"), 
                      portfolio$regime, 
                      prob_str,
                      100 * portfolio$expected_vol,
                      100 * ifelse("CASH" %in% names(current_weights), current_weights["CASH"], 0),
                      dd_msg))
        }
      }, error = function(e) {
        cat(sprintf("ERROR on %s: Portfolio construction failed: %s\n", 
                    format(date, "%Y-%m-%d"), e$message))
        cat("Keeping previous weights\n")
        # Keep previous weights (no action needed since current_weights is unchanged)
      })
    } else {
      cat(sprintf("WARNING on %s: Invalid index range for rebalance\n", 
                  format(date, "%Y-%m-%d")))
    }
  }
  
  # For non-rebalance days, ensure we still have all tracking variables filled
  if (!(date %in% rebalance_dates)) {
    # Fill regime history with last valid regime
    regime_history[i] <- last_valid_regime
    
    # Fill regime probabilities with last valid values
    for (regime_name in colnames(regime_probabilities)) {
      regime_probabilities[i, regime_name] <- last_valid_regime_probs[regime_name]
    }
    
    # Fill cash history based on current weights
    if ("CASH" %in% names(current_weights)) {
      cash_history[i] <- current_weights["CASH"]
    } else {
      cash_history[i] <- 0
    }
  }
}

#--------------------------------------------------------------------------
# STEP 5: Calculate performance metrics
#--------------------------------------------------------------------------

# Calculate cumulative returns
cumulative_returns <- cumprod(1 + portfolio_returns)

# Calculate drawdowns
drawdowns <- calculate_drawdown_path(portfolio_returns)

# Calculate total transaction costs
total_costs <- sum(transaction_cost_history, na.rm = TRUE)
avg_annual_costs_bps <- 10000 * total_costs / (nrow(prices) / 252)

# Calculate days with cash allocation
days_with_cash <- sum(cash_history > 0, na.rm = TRUE)
avg_cash <- mean(cash_history, na.rm = TRUE)

# Calculate turnover (sum of all changes / 2)
turnover <- 0
for (i in 2:nrow(weights)) {
  # Calculate absolute weight changes for all assets
  weight_changes <- abs(weights[i, ] - weights[i-1, ])
  # Sum all changes (ignoring NA values)
  daily_turnover <- sum(weight_changes, na.rm = TRUE)
  # Add to total turnover (divide by 2 because both buys and sells are counted)
  turnover <- turnover + daily_turnover / 2
}

# Annualize turnover (assuming 252 trading days per year)
annual_turnover <- turnover * 252 / nrow(weights)

# Calculate all standard performance metrics
perf_metrics <- tryCatch({
  PerformanceAnalytics::table.AnnualizedReturns(portfolio_returns)
}, error = function(e) {
  cat("Error calculating performance metrics:", e$message, "\n")
  matrix(NA, nrow = 3, ncol = 1, dimnames = list(
    c("Annualized Return", "Annualized Std Dev", "Annualized Sharpe"),
    "portfolio"))
})

# Calculate more comprehensive metrics
annual_return <- as.numeric(perf_metrics["Annualized Return", ])
annual_vol <- as.numeric(perf_metrics["Annualized Std Dev", ])
sharpe_ratio <- as.numeric(perf_metrics["Annualized Sharpe", ])

# Calculate additional metrics manually for safety
if (is.na(annual_return) || !is.finite(annual_return)) {
  # Manual calculation of annualized return
  total_return <- as.numeric(tail(cumulative_returns, 1)) - 1
  years <- nrow(portfolio_returns) / 252
  annual_return <- (1 + total_return)^(1/years) - 1
}

if (is.na(annual_vol) || !is.finite(annual_vol)) {
  # Manual calculation of annualized volatility
  annual_vol <- sd(portfolio_returns, na.rm = TRUE) * sqrt(252)
}

if (is.na(sharpe_ratio) || !is.finite(sharpe_ratio)) {
  # Manual calculation of Sharpe ratio (assuming zero risk-free rate for simplicity)
  sharpe_ratio <- annual_return / annual_vol
}

# Calculate maximum drawdown
max_dd <- max(drawdowns, na.rm = TRUE)

# Calculate Calmar ratio (return / max drawdown)
calmar_ratio <- annual_return / max_dd

# Calculate Sortino ratio (downside deviation)
downside_returns <- portfolio_returns[portfolio_returns < 0]
downside_deviation <- sd(downside_returns, na.rm = TRUE) * sqrt(252)
sortino_ratio <- annual_return / downside_deviation

# Calculate percentage of positive months
monthly_returns <- apply.monthly(portfolio_returns, sum)
pct_positive_months <- sum(monthly_returns > 0, na.rm = TRUE) / length(monthly_returns)

# Calculate regime-specific performance
regime_performance <- list()
regime_names <- unique(as.character(regime_history))

for (r in regime_names) {
  # Get days in this regime
  regime_days <- which(regime_history == r)
  
  if (length(regime_days) > 5) { # Only calculate if we have enough data
    # Get returns for this regime
    regime_rets <- portfolio_returns[regime_days]
    
    # Calculate metrics
    regime_performance[[r]] <- list(
      days = length(regime_days),
      pct_of_time = length(regime_days) / nrow(portfolio_returns),
      annual_return = mean(regime_rets, na.rm = TRUE) * 252,
      annual_vol = sd(regime_rets, na.rm = TRUE) * sqrt(252),
      sharpe = mean(regime_rets, na.rm = TRUE) / sd(regime_rets, na.rm = TRUE) * sqrt(252),
      max_drawdown = max(calculate_drawdown_path(regime_rets), na.rm = TRUE),
      total_return = prod(1 + regime_rets) - 1
    )
  }
}

# Print summary statistics
cat("\n========== BACKTEST RESULTS ==========\n")
cat(sprintf("Period: %s to %s (%.1f years)\n", 
            format(index(prices)[1], "%Y-%m-%d"),
            format(index(prices)[nrow(prices)], "%Y-%m-%d"),
            nrow(prices) / 252))
cat(sprintf("Annual Return: %.2f%%\n", 100 * annual_return))
cat(sprintf("Annual Volatility: %.2f%%\n", 100 * annual_vol))
cat(sprintf("Sharpe Ratio: %.2f\n", sharpe_ratio))
cat(sprintf("Sortino Ratio: %.2f\n", sortino_ratio))
cat(sprintf("Calmar Ratio: %.2f\n", calmar_ratio))
cat(sprintf("Maximum Drawdown: %.2f%%\n", 100 * max_dd))
cat(sprintf("Win Rate (Monthly): %.1f%%\n", 100 * pct_positive_months))
cat(sprintf("Annual Turnover: %.1f%%\n", 100 * annual_turnover))
cat(sprintf("Total Transaction Costs: %.1f bps (%.1f bps/year)\n", 
            10000 * total_costs, avg_annual_costs_bps))
cat(sprintf("Days with Cash > 0: %d (%.1f%%) - Average Cash: %.1f%%\n", 
            days_with_cash, 100 * days_with_cash / nrow(prices), 100 * avg_cash))

# Print regime statistics
cat("\n===== REGIME-SPECIFIC PERFORMANCE =====\n")
for (r in names(regime_performance)) {
  perf <- regime_performance[[r]]
  cat(sprintf("%s Regime: %.1f%% of time\n", r, 100 * perf$pct_of_time))
  cat(sprintf("  Return: %.2f%% (annual), Volatility: %.2f%%, Sharpe: %.2f\n", 
              100 * perf$annual_return, 100 * perf$annual_vol, perf$sharpe))
  cat(sprintf("  Max Drawdown: %.2f%%, Total Return: %.2f%%\n", 
              100 * perf$max_drawdown, 100 * perf$total_return))
}

# Return all results in a structured list
result <- list(
  # Portfolio returns and analytics
  portfolio_returns = portfolio_returns,
  cumulative_returns = cumulative_returns,
  drawdowns = drawdowns,
  weights = weights,
  cash_allocation = cash_history,
  transaction_costs = transaction_cost_history,
  transaction_costs_by_asset = transaction_cost_by_asset,
  
  # Regime information
  regime_history = regime_history,
  regime_probabilities = regime_probabilities,
  
  # Z-score histories
  vol_zscore = vol_zscore_history,
  growth_zscore = pmi_zscore_history,
  inflation_zscore = cpi_zscore_history,
  bond_equity_zscore = corr_zscore_history,
  
  # Performance metrics
  metrics = list(
    annual_return = annual_return,
    annual_vol = annual_vol,
    sharpe_ratio = sharpe_ratio,
    sortino_ratio = sortino_ratio,
    calmar_ratio = calmar_ratio,
    max_drawdown = max_dd,
    pct_positive_months = pct_positive_months,
    annual_turnover = annual_turnover,
    total_costs = total_costs,
    avg_annual_costs_bps = avg_annual_costs_bps,
    days_with_cash = days_with_cash,
    avg_cash = avg_cash
  ),
  
  # Regime-specific performance
  regime_performance = regime_performance,
  
  # Portfolio details on rebalance dates
  portfolio_details = portfolio_details,
  
  # Configuration parameters
  config = list(
    target_vol = target_vol,
    max_drawdown = max_drawdown,
    rebalance_freq = rebalance_freq,
    lookback_window = lookback_window,
    use_garch = use_garch,
    cov_method = cov_method,
    enable_cash = enable_cash,
    enable_shorts = enable_shorts
  )
)

return(result)
}

#=============================================================================
# PERFORMANCE ANALYSIS AND VISUALIZATION FUNCTIONS
#=============================================================================

# Function to analyze transaction costs in detail
analyze_transaction_costs <- function(backtest_result) {
  if (is.null(backtest_result$transaction_costs)) {
    cat("No transaction cost data available in backtest results\n")
    return(NULL)
  }
  
  # Get total costs
  total_costs <- sum(backtest_result$transaction_costs, na.rm = TRUE)
  
  # Calculate annual costs
  years <- nrow(backtest_result$portfolio_returns) / 252
  annual_costs <- total_costs / years
  
  # Calculate costs by asset if available
  costs_by_asset <- NULL
  if (!is.null(backtest_result$transaction_costs_by_asset)) {
    costs_by_asset <- colSums(backtest_result$transaction_costs_by_asset, na.rm = TRUE)
    costs_by_asset <- sort(costs_by_asset, decreasing = TRUE)
  }
  
  # Calculate costs by rebalance period
  non_zero_costs <- backtest_result$transaction_costs[backtest_result$transaction_costs > 0]
  
  # Results
  cat("\n===== TRANSACTION COST ANALYSIS =====\n")
  cat(sprintf("Total Transaction Costs: %.2f%%\n", 100 * total_costs))
  cat(sprintf("Annual Transaction Costs: %.2f bps\n", 10000 * annual_costs))
  cat(sprintf("Average Cost per Rebalance: %.1f bps\n", 
              10000 * mean(non_zero_costs, na.rm = TRUE)))
  cat(sprintf("Median Cost per Rebalance: %.1f bps\n", 
              10000 * median(non_zero_costs, na.rm = TRUE)))
  cat(sprintf("Maximum Cost per Rebalance: %.1f bps\n", 
              10000 * max(non_zero_costs, na.rm = TRUE)))
  
  if (!is.null(costs_by_asset) && length(costs_by_asset) > 0) {
    cat("\nCosts by Asset (Top 10):\n")
    top_assets <- head(costs_by_asset, 10)
    for (asset in names(top_assets)) {
      cat(sprintf("  %s: %.1f bps\n", asset, 10000 * top_assets[asset]))
    }
  }
  
  # Create plot of costs over time
  p <- ggplot(data = data.frame(
    Date = index(non_zero_costs),
    Cost = as.numeric(non_zero_costs) * 10000  # Convert to basis points
  ), aes(x = Date, y = Cost)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    labs(title = "Transaction Costs by Rebalance Period",
         x = "Date",
         y = "Cost (basis points)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list(
    total_costs = total_costs,
    annual_costs_bps = 10000 * annual_costs,
    average_cost_per_rebalance_bps = 10000 * mean(non_zero_costs, na.rm = TRUE),
    median_cost_per_rebalance_bps = 10000 * median(non_zero_costs, na.rm = TRUE),
    max_cost_per_rebalance_bps = 10000 * max(non_zero_costs, na.rm = TRUE),
    costs_by_asset = costs_by_asset,
    cost_plot = p
  ))
}

# Function to create comprehensive performance report
create_performance_report <- function(backtest_result, benchmark_returns = NULL) {
  # Initialize report components
  plots <- list()
  tables <- list()
  
  # Create cumulative return plot
  cum_ret_data <- data.frame(
    Date = index(backtest_result$cumulative_returns),
    Strategy = as.numeric(backtest_result$cumulative_returns)
  )
  
  # Add benchmark if provided
  if (!is.null(benchmark_returns) && length(benchmark_returns) > 0) {
    # Calculate benchmark cumulative returns
    bench_cum_ret <- cumprod(1 + benchmark_returns)
    
    # Get common dates
    common_dates <- intersect(index(bench_cum_ret), index(backtest_result$cumulative_returns))
    
    if (length(common_dates) > 0) {
      cum_ret_data$Benchmark <- as.numeric(bench_cum_ret[common_dates])
    }
  }
  
  # Plot cumulative returns
  p1 <- ggplot(cum_ret_data, aes(x = Date)) +
    geom_line(aes(y = Strategy), color = "steelblue", size = 1) +
    {if ("Benchmark" %in% colnames(cum_ret_data)) 
      geom_line(aes(y = Benchmark), color = "darkred", linetype = "dashed", size = 1)} +
    theme_minimal() +
    labs(title = "Cumulative Returns",
         x = "Date",
         y = "Growth of $1") +
    {if ("Benchmark" %in% colnames(cum_ret_data))
      scale_color_manual(values = c("Strategy" = "steelblue", "Benchmark" = "darkred"))} +
    theme(legend.position = "bottom")
  
  plots$cumulative_returns <- p1
  
  # Plot drawdowns
  dd_data <- data.frame(
    Date = index(backtest_result$drawdowns),
    Drawdown = as.numeric(backtest_result$drawdowns)
  )
  
  p2 <- ggplot(dd_data, aes(x = Date, y = -100 * Drawdown)) +
    geom_area(fill = "darkred", alpha = 0.7) +
    theme_minimal() +
    labs(title = "Drawdowns",
         x = "Date",
         y = "Drawdown (%)") +
    scale_y_continuous(labels = function(x) paste0("-", x))
  
  plots$drawdowns <- p2
  
  # Plot regime distribution
  regime_data <- data.frame(
    Date = index(backtest_result$regime_history),
    Regime = as.character(backtest_result$regime_history)
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
  
  # Add fallback color for any unexpected regimes
  all_regimes <- unique(regime_data$Regime)
  for (regime in all_regimes) {
    if (!regime %in% names(regime_colors)) {
      regime_colors[regime] <- "#607D8B"  # Gray-blue for unknown regimes
    }
  }
  
  p3 <- ggplot(regime_data, aes(x = Date, y = 1, fill = Regime)) +
    geom_tile() +
    scale_fill_manual(values = regime_colors) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom"
    ) +
    labs(title = "Market Regimes",
         x = "Date",
         fill = "Regime")
  
  plots$regime_distribution <- p3
  
  # Plot regime probabilities if available
  if (!is.null(backtest_result$regime_probabilities)) {
    # Convert to data frame
    prob_data <- data.frame(
      Date = index(backtest_result$regime_probabilities),
      backtest_result$regime_probabilities
    )
    
    # Melt to long format for ggplot
    prob_data_long <- reshape2::melt(prob_data, id.vars = "Date", 
                                     variable.name = "Regime", 
                                     value.name = "Probability")
    
    p4 <- ggplot(prob_data_long, aes(x = Date, y = Probability, fill = Regime)) +
      geom_area(position = "stack") +
      scale_fill_manual(values = regime_colors) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(title = "Regime Probabilities",
           x = "Date",
           y = "Probability")
    
    plots$regime_probabilities <- p4
  }
  
  # Plot cash allocation
  cash_data <- data.frame(
    Date = index(backtest_result$cash_allocation),
    Cash = as.numeric(backtest_result$cash_allocation) * 100
  )
  
  p5 <- ggplot(cash_data, aes(x = Date, y = Cash)) +
    geom_line(color = "darkblue") +
    geom_area(fill = "lightblue", alpha = 0.5) +
    theme_minimal() +
    labs(title = "Cash Allocation",
         x = "Date",
         y = "Cash (%)") +
    scale_y_continuous(limits = c(0, max(cash_data$Cash) * 1.1))
  
  plots$cash_allocation <- p5
  
  # Plot asset weights over time for top assets
  top_assets <- colnames(backtest_result$weights)[
    order(colMeans(backtest_result$weights), decreasing = TRUE)[1:min(6, ncol(backtest_result$weights)-1)]
  ]
  
  if (length(top_assets) > 0) {
    # Extract weights for top assets
    weight_data <- as.data.frame(backtest_result$weights[, top_assets, drop = FALSE])
    weight_data$Date <- index(backtest_result$weights)
    
    # Convert to long format
    weight_long <- reshape2::melt(weight_data, id.vars = "Date", 
                                  variable.name = "Asset", 
                                  value.name = "Weight")
    
    p6 <- ggplot(weight_long, aes(x = Date, y = Weight * 100, color = Asset)) +
      geom_line() +
      theme_minimal() +
      scale_color_brewer(palette = "Set1") +
      labs(title = "Top Asset Weights Over Time",
           x = "Date",
           y = "Weight (%)") +
      theme(legend.position = "bottom")
    
    plots$asset_weights <- p6
  }
  
  # Create performance table
  perf_table <- data.frame(
    Metric = c(
      "Annual Return",
      "Annual Volatility",
      "Sharpe Ratio",
      "Sortino Ratio",
      "Calmar Ratio",
      "Maximum Drawdown",
      "% Positive Months",
      "Annual Turnover",
      "Transaction Costs (bps/yr)",
      "Average Cash Allocation"
    ),
    Value = c(
      paste0(round(100 * backtest_result$metrics$annual_return, 2), "%"),
      paste0(round(100 * backtest_result$metrics$annual_vol, 2), "%"),
      round(backtest_result$metrics$sharpe_ratio, 2),
      round(backtest_result$metrics$sortino_ratio, 2),
      round(backtest_result$metrics$calmar_ratio, 2),
      paste0(round(100 * backtest_result$metrics$max_drawdown, 2), "%"),
      paste0(round(100 * backtest_result$metrics$pct_positive_months, 1), "%"),
      paste0(round(100 * backtest_result$metrics$annual_turnover, 1), "%"),
      round(backtest_result$metrics$avg_annual_costs_bps, 1),
      paste0(round(100 * backtest_result$metrics$avg_cash, 1), "%")
    )
  )
  
  tables$performance <- perf_table
  
  # Create regime performance table
  regime_perf_rows <- list()
  
  for (regime in names(backtest_result$regime_performance)) {
    perf <- backtest_result$regime_performance[[regime]]
    
    regime_perf_rows[[regime]] <- c(
      paste0(round(100 * perf$pct_of_time, 1), "%"),
      paste0(round(100 * perf$annual_return, 2), "%"),
      paste0(round(100 * perf$annual_vol, 2), "%"),
      round(perf$sharpe, 2),
      paste0(round(100 * perf$max_drawdown, 2), "%"),
      paste0(round(100 * perf$total_return, 2), "%")
    )
  }
  
  # Convert to data frame
  if (length(regime_perf_rows) > 0) {
    regime_table <- as.data.frame(do.call(rbind, regime_perf_rows))
    colnames(regime_table) <- c("% of Time", "Annual Return", "Annual Volatility", 
                                "Sharpe", "Max Drawdown", "Total Return")
    
    tables$regime_performance <- regime_table
  }
  
  # Return all plots and tables
  return(list(
    plots = plots,
    tables = tables
  ))
}

# Function to calculate and visualize regime transition probabilities
analyze_regime_transitions <- function(regime_history) {
  # Convert to character vector
  regimes <- as.character(regime_history)
  
  # Initialize transition count matrix
  unique_regimes <- unique(regimes)
  n_regimes <- length(unique_regimes)
  
  transitions <- matrix(0, nrow = n_regimes, ncol = n_regimes)
  rownames(transitions) <- unique_regimes
  colnames(transitions) <- unique_regimes
  
  # Count transitions
  for (i in 1:(length(regimes) - 1)) {
    from_regime <- regimes[i]
    to_regime <- regimes[i + 1]
    
    # Only count when regime changes
    if (from_regime != to_regime) {
      transitions[from_regime, to_regime] <- transitions[from_regime, to_regime] + 1
    }
  }
  
  # Calculate transition probabilities
  prob_transitions <- transitions / rowSums(transitions)
  
  # Replace NaN with 0
  prob_transitions[is.nan(prob_transitions)] <- 0
  
  # Print transitions
  cat("===== REGIME TRANSITION PROBABILITIES =====\n")
  cat("When regime changes, probability of transitioning to:\n\n")
  
  for (from_regime in unique_regimes) {
    cat(sprintf("From %s regime:\n", from_regime))
    for (to_regime in unique_regimes) {
      if (from_regime != to_regime && prob_transitions[from_regime, to_regime] > 0) {
        cat(sprintf("  → %s: %.1f%%\n", to_regime, 
                    100 * prob_transitions[from_regime, to_regime]))
      }
    }
    cat("\n")
  }
  
  # Create heatmap data
  heatmap_data <- reshape2::melt(prob_transitions, varnames = c("From", "To"), 
                                 value.name = "Probability")
  
  # Create heatmap
  p <- ggplot(heatmap_data, aes(x = To, y = From, fill = Probability)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.0f%%", 100 * Probability)), 
              color = "white", size = 3) +
    scale_fill_gradient(low = "navy", high = "red", limits = c(0, 1)) +
    theme_minimal() +
    labs(title = "Regime Transition Probabilities",
         x = "To Regime", 
         y = "From Regime") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list(
    transition_counts = transitions,
    transition_probabilities = prob_transitions,
    plot = p
  ))
}

cat("\nPART 3 LOADED: Backtesting and Performance Analysis\n")
cat("\nCurrent Date and Time (UTC - YYYY-MM-DD HH:MM:SS formatted): 2025-09-02 22:36:02\n")
cat("Current User's Login: balint27\n")

# Load ETF data
tickers <- c("SPY", "IEF", "GLD", "LQD", "EEM", "EFA", "VNQ")
etf_prices <- load_market_data(tickers, start_date = "2010-01-01", include_inverse = TRUE)

# Calculate returns
etf_returns <- ROC(etf_prices, type = "discrete")
etf_returns <- na.omit(etf_returns)

# Create market data with economic indicators
market_data <- create_market_data(etf_prices, enhanced_indicators = TRUE)

# Define sleeve mapping
sleeve_mapping <- list(
  "SPY" = "US_EQUITY",
  "IEF" = "US_TREASURY",
  "GLD" = "GOLD",
  "LQD" = "CREDIT_IG",
  "EEM" = "EMERGING_MARKETS",
  "EFA" = "INTL_DEVELOPED",
  "VNQ" = "REIT"
)

# Run backtest
results <- backtest_optimized_strategy(
  prices = etf_prices,
  returns = etf_returns,
  market_data = market_data,
  sleeve_mapping = sleeve_mapping,
  target_vol = 0.08,
  max_drawdown = 0.10,
  rebalance_freq = "M",
  lookback_window = 252,
  use_garch = TRUE,
  cov_method = "ledoit-wolf",
  enable_cash = TRUE,
  enable_shorts = TRUE
)

# Analyze results
performance_report <- create_performance_report(results)