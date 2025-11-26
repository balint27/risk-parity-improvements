###############################################################################
# Unified Risk Parity Engine & Professional Assessment Pack Generator (v160.9.9)
#
# This version provides a definitive fix for the volatility target undershoot by
# re-ordering the weight calculation pipeline. All exposure-reducing rules
# (sleeve de-risking, high-VIX layoff) are now applied *before* the final
# volatility scaling, ensuring the scaler has the correct risk budget.
#
# KEY ENHANCEMENTS in v160.9.9 (Leverage Pipeline & Cap Semantics Fix):
# 1.  BUGFIX (Leverage Pipeline): Re-ordered the `calculate_target_weights`
#     function. High-VIX and sleeve-based de-risking rules now modify the raw
#     weights *prior* to the `apply_vol_target_and_leverage` call. This ensures
#     the final scaler correctly targets 20% volatility on the "true" portfolio.
# 2.  BUGFIX (Gross Cap Semantics): The `apply_vol_target_and_leverage` scaler
#     now correctly interprets the `max_leverage` parameter as a true gross
#     leverage cap (L) by deriving the equivalent scale-factor cap (s_cap). This
#     resolves a semantic mismatch and makes the gross cap behave as expected.
# 3.  DIAGNOSTICS (Leverage Probes): The `LEV` diagnostic function calls have been
#     re-positioned to audit the new, correct weight calculation flow, providing
#     clear insight into how leverage is built and capped.
# 4.  ROBUSTNESS: All date-handling and previous fixes from v160.9.8 are retained.
# 5.  METADATA UPDATE: Version 160.9.9, user 'balint27', timestamp updated.
###############################################################################

options(stringsAsFactors = FALSE)
options("PerformanceAnalytics.chart.engine" = "base")
options(xts.message.period.apply.mean = FALSE)

# ============================ PACKAGE BOOTSTRAP ==============================
required_pkgs <- c("Rblpapi","xts","zoo","PerformanceAnalytics","TTR","MSwM","optimx",
                   "corpcor","data.table","jsonlite","openxlsx","digest","lubridate",
                   "nloptr", "riskParityPortfolio", "tidyr", "plyr", "dplyr")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("[Setup] Installing missing package: ", pkg)
    install.packages(pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
}

# ============================== PATHS =======================================
REPORTS_DIR       <- "./reports/assessment_packs"
DIAGNOSTICS_DIR   <- "./diagnostics"
ARTIFACTS_DIR     <- "./artifacts"
.ensure_dir <- function(path) { if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE); invisible(path) }
.ensure_dir(REPORTS_DIR); .ensure_dir(DIAGNOSTICS_DIR); .ensure_dir(ARTIFACTS_DIR)

# ============================== HELPERS (v160.9.9 - Hardened) ===============================
`%||%` <- function(a, b) if (is.null(a)) b else a

to_Date_or_die <- function(x, hint = "") {
  plausible_year <- function(d) {
    if (is.na(d)) return(FALSE)
    y <- as.integer(format(d, "%Y"))
    y >= 1900 && y <= (as.integer(format(Sys.Date(), "%Y")) + 2)
  }
  
  # Already time?
  if (inherits(x, c("Date","POSIXct","POSIXt"))) return(as.Date(x))
  
  # Factors -> treat as the *labels* (not as.numeric(level))
  if (is.factor(x)) x <- as.character(x)
  
  # If it's a *string of digits*, treat it as numeric date, not text
  is_digit_string <- is.character(x) && all(grepl("^\\d+(?:\\.\\d+)?$", x[!is.na(x)]))
  
  # Branch: numeric-like first
  if (is.numeric(x) || is.integer(x) || is_digit_string) {
    x_num <- as.numeric(x)
    out <- rep(as.Date(NA), length(x_num))
    
    # Candidate 1: Unix epoch seconds (big numbers)
    try_unix <- rep(as.Date(NA), length(x_num))
    big <- which(is.finite(x_num) & x_num >= 1e9)
    if (length(big)) {
      try_unix[big] <- as.Date(as.POSIXct(x_num[big], origin = "1970-01-01", tz = "UTC"))
    }
    
    # Candidate 2: days since 1970-01-01
    try_days1970 <- as.Date(x_num, origin = "1970-01-01")
    
    # Candidate 3: Excel serial (Windows 1900 system)
    try_excel <- as.Date(x_num, origin = "1899-12-30")
    
    for (i in seq_along(x_num)) {
      cand <- NA
      if (!is.na(try_unix[i])     && plausible_year(try_unix[i]))     cand <- try_unix[i]
      else if (!is.na(try_days1970[i]) && plausible_year(try_days1970[i])) cand <- try_days1970[i]
      else if (!is.na(try_excel[i])    && plausible_year(try_excel[i]))    cand <- try_excel[i]
      out[i] <- cand
    }
    if (any(is.na(out))) {
      bad <- unique(x_num[is.na(out)])
      stop("to_Date_or_die: unparseable numeric date values: ",
           paste(head(bad, 20), collapse = ", "),
           if (nzchar(hint)) paste0(" | hint: ", hint) else "")
    }
    return(out)
  }
  
  # Character (non-digit) -> try fast ISO, then flexible
  x_chr <- as.character(x)
  if (all(grepl("^\\d{4}-\\d{2}-\\d{2}$", x_chr))) return(as.Date(x_chr))
  
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("to_Date_or_die: need lubridate for flexible parsing",
         if (nzchar(hint)) paste0(" | hint: ", hint) else "")
  }
  parsed <- lubridate::parse_date_time(
    x_chr, orders = c("Ymd","Y-m-d","Ymd HMS","Y-m-d H:M:S","dmy","mdy","d/m/Y","m/d/Y"),
    tz = "UTC"
  )
  out <- as.Date(parsed)
  bad <- which(is.na(out))
  if (length(bad)) {
    stop("to_Date_or_die: unparseable date strings: ",
         paste(utils::head(unique(x_chr[bad]), 20), collapse = ", "),
         if (nzchar(hint)) paste0(" | hint: ", hint) else "")
  }
  out
}

assert_time_index <- function(x, name = "object") {
  if (!xts::is.xts(x))
    stop(name, " must be xts; got: ", paste(class(x), collapse = "/"))
  idx <- index(x)
  if (!inherits(idx, c("Date","POSIXct","POSIXt")))
    stop(name, " index must be Date/POSIXct; got: ", paste(class(idx), collapse = "/"))
  if (any(is.na(idx)))
    stop(name, " index contains NA timestamps")
  invisible(TRUE)
}

make_row_xts_strict <- function(w_vec, cols, date_scalar, name = "weights_row") {
  if (length(date_scalar) != 1L)
    stop(name, ": date_scalar must be length 1; got length=", length(date_scalar))
  tstamp <- to_Date_or_die(date_scalar, hint = paste0(name, " -> order.by"))
  
  full_vec <- setNames(numeric(length(cols)), cols)
  if (!is.null(w_vec)) {
    w_vec_named <- setNames(as.numeric(w_vec), names(w_vec))
    valid_names <- intersect(names(full_vec), names(w_vec_named))
    if(length(valid_names) > 0) full_vec[valid_names] <- w_vec_named[valid_names]
  }
  
  mat <- matrix(full_vec, nrow = 1, dimnames = list(NULL, cols))
  xts::xts(mat, order.by = tstamp)
}

# Ensure a named numeric vector over the full asset set
.normalize_w <- function(w_any, all_cols) {
  if (is.xts(w_any) || is.matrix(w_any) || is.data.frame(w_any)) {
    v  <- as.numeric(w_any[1, , drop=TRUE]); nm <- colnames(w_any)
  } else {
    v  <- as.numeric(w_any); nm <- names(w_any)
  }
  if (is.null(nm) && length(v) == length(all_cols)) {
    nm <- all_cols
  }
  stopifnot(!is.null(nm))
  out <- setNames(numeric(length(all_cols)), all_cols)
  ov  <- intersect(nm, all_cols)
  if (length(ov)) out[ov] <- v[match(ov, nm)]
  out[is.na(out)] <- 0
  return(as.numeric(out))
}

# Core scaling function for volatility targeting and leverage capping
apply_vol_target_and_leverage <- function(w_raw, cov_mat, vol_target,
                                          vol_annualize = 252,
                                          max_leverage = 3.5,
                                          cash_name = "BIL",
                                          eps = 1e-12) {
  if (is.null(names(w_raw))) stop("w_raw must be a named vector")
  nms <- names(w_raw)
  stopifnot(all(nms %in% colnames(cov_mat)))
  cov_mat <- cov_mat[nms, nms, drop = FALSE]
  w_raw[is.na(w_raw)] <- 0
  
  has_cash <- !is.null(cash_name) && cash_name %in% nms
  risky_names <- if (has_cash) setdiff(nms, cash_name) else nms
  
  if (all(abs(w_raw[risky_names]) < eps)) {
    w_out <- setNames(numeric(length(nms)), nms)
    if (has_cash) w_out[cash_name] <- 1.0
    return(w_out)
  }
  
  w_risky <- w_raw[risky_names]
  cov_risky <- cov_mat[risky_names, risky_names, drop = FALSE]
  port_var_daily <- as.numeric(t(w_risky) %*% cov_risky %*% w_risky)
  
  if (!is.finite(port_var_daily) || port_var_daily <= 0) {
    w_out <- setNames(numeric(length(nms)), nms)
    if (has_cash) w_out[cash_name] <- 1.0
    return(w_out)
  }
  
  port_vol_annual <- sqrt(port_var_daily * vol_annualize)
  
  s_target <- if (!is.na(port_vol_annual) && port_vol_annual > eps) vol_target / port_vol_annual else 0
  
  # --- FIX: Interpret max_leverage as a true Gross Leverage cap (L) ---
  # For a net=1 portfolio, L = 2s - 1 => s_max = (L_max + 1) / 2
  s_cap <- (max_leverage + 1) / 2
  s_used <- min(s_target, s_cap)
  
  if (!is.finite(s_used) || s_used < 0) s_used <- 0
  
  w_scaled <- w_raw
  w_scaled[risky_names] <- w_risky * s_used
  
  if (has_cash) {
    w_scaled[cash_name] <- 1.0 - sum(w_scaled[risky_names])
  }
  
  w_scaled[abs(w_scaled) < eps] <- 0
  return(setNames(as.numeric(w_scaled), nms))
}


# Ensure xts with given column set (add missing as 0, reorder)
align_xts_cols <- function(x, cols, fill = 0) {
  assert_time_index(x)
  have <- colnames(x)
  add  <- setdiff(cols, have)
  keep <- intersect(have, cols)
  if (length(add)) {
    x <- merge(x[, keep, drop = FALSE],
               xts::xts(matrix(fill, nrow = NROW(x), ncol = length(add),
                               dimnames = list(NULL, add)),
                        order.by = index(x)),
               join = "left")
  } else {
    x <- x[, keep, drop = FALSE]
  }
  # Reorder to 'cols'
  x <- x[, cols, drop = FALSE]
  return(x)
}

# Map base index rows to enhanced index rows (post activation)
index_match_after <- function(base_idx, enh_idx, activation_date) {
  act <- to_Date_or_die(activation_date, "index_match_after activation_date")
  base_after <- base_idx[base_idx >= act]
  common     <- base_after[base_after %in% enh_idx]
  base_rows  <- match(common, base_idx)
  enh_rows   <- match(common, enh_idx)
  list(common_days = common, base_rows = base_rows, enh_rows = enh_rows)
}

# Strict splice for 1-column xts series (e.g., Net_Portfolio_Return)
splice_xts_series <- function(base_xts, enh_xts, activation_date, label = "series") {
  assert_time_index(base_xts, "splice_xts_series base_xts"); assert_time_index(enh_xts, "splice_xts_series enh_xts")
  if (NCOL(base_xts) != 1L || NCOL(enh_xts) != 1L) {
    stop(sprintf("[SPLICE/%s] Expected single-column xts; got base=%d, enh=%d",
                 label, NCOL(base_xts), NCOL(enh_xts)))
  }
  m <- index_match_after(index(base_xts), index(enh_xts), activation_date)
  cat(sprintf("[SPLICE/%s] days=%d\n", label, length(m$common_days)))
  if (!length(m$common_days)) return(base_xts)  # nothing to splice
  
  # Align enh to base order for common days
  enh_sub <- enh_xts[m$enh_rows, , drop = FALSE]
  stopifnot(NROW(enh_sub) == length(m$base_rows))
  
  # Assign by exact row positions (no recycling)
  coredata(base_xts)[m$base_rows, 1] <- as.numeric(coredata(enh_sub)[, 1])
  return(base_xts)
}

# Strict splice for positions_xts (handles differing column sets)
splice_positions <- function(base_pos, enh_pos, activation_date, fill_missing = 0) {
  assert_time_index(base_pos, "splice_positions base_pos"); assert_time_index(enh_pos, "splice_positions enh_pos")
  # Union schema, then align both to union (zeros where missing)
  union_cols <- union(colnames(base_pos), colnames(enh_pos))
  base_u <- align_xts_cols(base_pos, union_cols, fill_missing)
  enh_u  <- align_xts_cols(enh_pos,  union_cols, fill_missing)
  
  m <- index_match_after(index(base_u), index(enh_u), activation_date)
  cat(sprintf("[SPLICE/positions] days=%d, cols=%d (union)\n", length(m$common_days), length(union_cols)))
  if (!length(m$common_days)) return(base_u)
  
  # Row subsets in aligned order
  enh_sub  <- enh_u[m$enh_rows, , drop = FALSE]
  base_row <- m$base_rows
  stopifnot(NROW(enh_sub) == length(base_row),
            identical(colnames(enh_sub), colnames(base_u)))
  
  # Assign column-wise to prevent recycling; faster and safe
  bdat <- coredata(base_u)
  edat <- coredata(enh_sub)
  bdat[base_row, ] <- edat  # exact dim match
  xts::xts(bdat, order.by = index(base_u))
}

# Bind enhanced audit rows after activation date (data.frame)
splice_audit_df <- function(base_df, enh_df, activation_date) {
  act <- to_Date_or_die(activation_date, "splice_audit_df activation_date")
  
  if (is.null(base_df) || NROW(base_df) == 0) return(enh_df)
  if (is.null(enh_df) || NROW(enh_df) == 0) return(base_df)
  
  base_df$Date <- to_Date_or_die(base_df$Date, "splice_audit_df base_df$Date")
  enh_df$Date  <- to_Date_or_die(enh_df$Date, "splice_audit_df enh_df$Date")
  
  # Filter enhanced rows on/after activation
  enh_sub <- enh_df[enh_df$Date >= act, , drop = FALSE]
  base_pre <- base_df[base_df$Date < act, , drop = FALSE]
  
  # Align missing columns (wide-safe)
  base_names <- names(base_pre); enh_names <- names(enh_sub)
  all_names  <- union(base_names, enh_names)
  
  add_cols_to_base   <- setdiff(all_names, base_names)
  if (length(add_cols_to_base)) for (nm in add_cols_to_base) base_pre[[nm]] <- NA
  
  add_cols_to_enh   <- setdiff(all_names, enh_names)
  if (length(add_cols_to_enh)) for (nm in add_cols_to_enh) enh_sub[[nm]] <- NA
  
  # Reorder and rbind
  base_pre <- base_pre[, all_names, drop = FALSE]
  enh_sub <- enh_sub[, all_names, drop = FALSE]
  
  out <- rbind(base_pre, enh_sub)
  rownames(out) <- NULL
  out
}

snap_to_next_trading_day <- function(d, trading_idx) {
  d <- to_Date_or_die(d, "snap_to_next_trading_day d"); trading_idx <- to_Date_or_die(trading_idx, "snap_to_next_trading_day trading_idx")
  if (d %in% trading_idx) return(d)
  i <- which(trading_idx >= d)
  if (length(i)) trading_idx[i[1]] else tail(trading_idx,1)
}

bbg_default_options <- function() {
  c(periodicitySelection="DAILY", nonTradingDayFillOption="ACTIVE_DAYS_ONLY",
    nonTradingDayFillMethod="NIL_VALUE", adjustmentSplit="TRUE",
    adjustmentAbnormal="TRUE", adjustmentNormal="TRUE")
}
as_bbg <- function(sym) if (grepl("\\s", sym)) sym else sprintf("%s US Equity", sym)
ensure_date_index <- function(x) { 
  if (!xts::is.xts(x)) stop("ensure_date_index requires an xts object")
  index(x) <- to_Date_or_die(index(x), "ensure_date_index")
  assert_time_index(x)
  x 
}

smart_rbind <- function(list_of_xts) {
  if (length(list_of_xts) == 0) return(xts())
  list_of_xts <- list_of_xts[!sapply(list_of_xts, function(x) is.null(x) || !is.xts(x))]
  if (length(list_of_xts) == 0) return(xts())
  if (length(list_of_xts) == 1) return(list_of_xts[[1]])
  
  all_colnames <- unique(unlist(lapply(list_of_xts, colnames)))
  
  aligned_list <- lapply(list_of_xts, function(x) {
    if(!is.xts(x) || is.null(colnames(x))) return(NULL)
    missing_cols <- setdiff(all_colnames, colnames(x))
    if (length(missing_cols) > 0) {
      na_matrix <- matrix(NA, nrow = nrow(x), ncol = length(missing_cols))
      colnames(na_matrix) <- missing_cols
      x <- merge(x, xts(na_matrix, order.by = index(x)))
    }
    return(x[, all_colnames, drop = FALSE])
  })
  
  aligned_list <- aligned_list[!sapply(aligned_list, is.null)]
  if(length(aligned_list) == 0) return(xts())
  
  do.call(rbind, aligned_list)
}

# ============================== ROBUST BLOOMBERG I/O ================================
.connect_bbg_internal <- function() {
  tryCatch({
    Rblpapi::blpConnect()
  }, error = function(e) {
    stop("[FATAL] Bloomberg connection failed. Please ensure BBG terminal is running and logged in. Error: ", e$message)
  })
}

bbg_get_history_xts <- function(tickers, start_date, end_date, field = "PX_LAST") {
  .connect_bbg_internal()
  tickers_bbg <- vapply(tickers, as_bbg, "", USE.NAMES = FALSE)
  res_in <- Rblpapi::bdh(securities = tickers_bbg, fields = field,
                         start.date = to_Date_or_die(start_date, "bbg_get_history_xts start_date"), 
                         end.date = to_Date_or_die(end_date, "bbg_get_history_xts end_date"),
                         options    = bbg_default_options())
  
  if (is.data.frame(res_in)) res_list <- setNames(list(res_in), tickers_bbg[1])
  else if (is.list(res_in)) res_list <- res_in
  else stop(paste("[BBG] Unexpected return structure for tickers:", paste(tickers, collapse=", ")))
  
  out <- list()
  for (i in seq_along(res_list)) {
    df <- res_list[[i]]; nm <- names(res_list)[i]
    if (!is.data.frame(df) || !all(c("date", field) %in% names(df))) next
    df$date <- to_Date_or_die(df$date, paste("bbg_get_history_xts ticker", nm))
    if (any(duplicated(df$date))) {
      warning(sprintf("[BBG_FIX] Duplicate timestamps found and removed for ticker %s.", nm))
      df <- df[!duplicated(df$date), ]
    }
    df[[field]] <- suppressWarnings(as.numeric(df[[field]]))
    df <- df[stats::complete.cases(df), , drop = FALSE]
    if (NROW(df) == 0) next
    x <- xts(df[[field]], order.by = df$date)
    colnames(x) <- sub("\\s.*$", "", nm)
    out[[length(out)+1L]] <- x
  }
  if (!length(out)) stop("[BBG] No valid securities returned.")
  wide <- Reduce(function(a, b) merge(a, b, join = "outer"), out)
  ensure_date_index(wide)
}

get_vix_aligned <- function(dates, start_date, end_date) {
  .connect_bbg_internal()
  vix_raw <- try(Rblpapi::bdh("VIX Index", "PX_LAST", start.date=to_Date_or_die(start_date), end.date=to_Date_or_die(end_date)), silent=TRUE)
  if (inherits(vix_raw, "try-error") || NROW(vix_raw) == 0) {
    warning("[VIX] VIX data could not be loaded from Bloomberg. VIX-related analytics will be disabled.")
    return(NULL)
  }
  vix <- xts(vix_raw$PX_LAST, order.by=to_Date_or_die(vix_raw$date)); colnames(vix) <- "VIX"
  vix_aligned <- merge(xts(order.by=to_Date_or_die(dates)), vix, join="left")
  vix_aligned <- zoo::na.locf(vix_aligned, na.rm=FALSE)
  vix_aligned <- zoo::na.locf(vix_aligned, fromLast=TRUE, na.rm=FALSE)
  return(vix_aligned)
}

# ========================== CORE ENGINE (v160.9.9) =============================
# --- Diagnostic Helper for Leverage ---
LEV <- function(stage, w, cov_mat = NULL, vol_annualize = 252, cash = "BIL") {
  gross <- sum(abs(w))
  net   <- sum(w)
  csh   <- if (cash %in% names(w)) w[[cash]] else NA_real_
  vol_a <- NA_real_
  risky <- setdiff(names(w), cash)
  if (!is.null(cov_mat) && length(risky) > 0 && all(risky %in% colnames(cov_mat))) {
    w_risky <- w[risky]
    cov_risky <- cov_mat[risky, risky, drop=FALSE]
    v <- as.numeric(t(w_risky) %*% cov_risky %*% w_risky)
    if (is.finite(v) && v >= 0) vol_a <- sqrt(v * vol_annualize)
  }
  message(sprintf("[LEV] %-20s net=% .4f | gross=% .4f | cash=% .4f | volA=% .4f",
                  stage, net, gross, csh, vol_a))
}

ts_returns <- function(price_xts, method = c("discrete","log")) {
  r <- PerformanceAnalytics::Return.calculate(price_xts, method = match.arg(method))
  r <- r[-1,]; ensure_date_index(r)
}
.pd_fix <- function(S, eps = 1e-10) { S <- as.matrix(S); S <- 0.5*(S+t(S)); ev <- eigen(S, TRUE); v<-ev$values; U<-ev$vectors; v[v<eps]<-eps; Sp<-U%*%diag(v,length(v))%*%t(U); dimnames(Sp)<-dimnames(S); Sp }
ewma_cov <- function(R, lambda = 0.94) {
  X <- as.matrix(na.omit(R)); if (NROW(X) < 2) stop("[ewma_cov] too few rows")
  mu <- colMeans(X); X <- sweep(X, 2, mu, "-")
  p  <- ncol(X); S <- matrix(0, p, p); sf <- 1 - lambda
  for (t in 1:NROW(X)) S <- lambda * S + sf * crossprod(X[t,,drop=FALSE])
  S
}
rb_blend_cov <- function(X, lambda = 0.94) {
  X <- as.matrix(X); cols <- colnames(X); if (is.null(cols)) stop("[rb_blend_cov] returns must have column names")
  variances <- apply(X, 2, var, na.rm = TRUE)
  keep_cols <- variances > 1e-12
  if(!any(keep_cols)) {
    S_out <- matrix(0, ncol(X), ncol(X), dimnames=list(cols, cols))
    return(S_out)
  }
  X_filt <- X[, keep_cols, drop=FALSE]
  
  S_ewma_filt <- ewma_cov(X_filt, lambda = lambda)
  S_lw_filt <- try(corpcor::cov.shrink(X_filt, verbose = FALSE), silent = TRUE)
  if (inherits(S_lw_filt, "try-error")) S_lw_filt <- stats::cov(X_filt, use = "pairwise.complete.obs")
  
  D_filt <- diag(sqrt(pmax(diag(S_ewma_filt), 1e-12)), ncol(S_ewma_filt))
  R_lw_filt <- try(stats::cov2cor(S_lw_filt), silent = TRUE)
  if (inherits(R_lw_filt, "try-error")) { v <- sqrt(pmax(diag(S_lw_filt), 1e-12)); R_lw_filt <- S_lw_filt / (v %o% v) }
  R_lw_filt[!is.finite(R_lw_filt)] <- 0; diag(R_lw_filt) <- 1; R_lw_filt <- 0.5*(R_lw_filt + t(R_lw_filt))
  
  S_rb_filt <- D_filt %*% R_lw_filt %*% D_filt
  
  S_rb <- matrix(0, ncol(X), ncol(X), dimnames=list(cols, cols))
  S_rb[keep_cols, keep_cols] <- S_rb_filt
  
  .pd_fix(S_rb)
}
cov_engine <- function(R_win, method = "rb_blend", lambda = 0.94) {
  X <- if (is.xts(R_win)) coredata(na.omit(R_win)) else as.matrix(na.omit(R_win))
  if (!is.matrix(X) || NROW(X) < 3L) stop("[cov_engine] window too short")
  cols <- colnames(if (is.xts(R_win)) R_win else X); if (is.null(cols)) stop("[cov_engine] returns must have column names")
  S <- rb_blend_cov(X, lambda = lambda); dimnames(S) <- list(cols, cols); return(.pd_fix(S))
}

run_risk_parity_optimizer <- function(cov_matrix) {
  num_assets <- ncol(cov_matrix)
  if (num_assets == 0) return(numeric(0))
  if (num_assets == 1) return(setNames(1, colnames(cov_matrix)))
  
  tryCatch({
    w <- riskParityPortfolio::riskParityPortfolio(cov_matrix)$w
    if (length(w) != num_assets || any(!is.finite(w))) {
      stop("Optimizer returned invalid weights.")
    }
    return(w / sum(w))
  }, error = function(e) {
    message(sprintf("[OPTIMIZER_FALLBACK] Risk parity failed: %s. Defaulting to Equal Weight.", e$message))
    return(setNames(rep(1/num_assets, num_assets), colnames(cov_matrix)))
  })
}

calculate_risk_contributions <- function(weights, cov_matrix) {
  weights <- as.numeric(weights)
  asset_names <- colnames(cov_matrix)
  
  if (length(weights) == 1) {
    return(setNames(1.0, asset_names))
  }
  
  if (!is.matrix(cov_matrix)) {
    stop("cov_matrix must be a matrix.")
  }
  
  port_var <- as.numeric(t(weights) %*% cov_matrix %*% weights)
  
  if (is.na(port_var) || port_var < 1e-12) {
    return(setNames(rep(1 / length(weights), length(weights)), asset_names))
  }
  
  mcr <- as.numeric(cov_matrix %*% weights)
  risk_contributions <- weights * mcr
  perc_risk_contributions <- risk_contributions / port_var
  
  final_contributions <- perc_risk_contributions / sum(perc_risk_contributions)
  
  return(setNames(final_contributions, asset_names))
}

build_trend_mask_xts <- function(prices, trend_config) {
  
  if (!isTRUE(trend_config$enabled)) return(xts())
  
  assets <- colnames(prices)
  if (!length(assets)) return(xts())
  
  mode <- trend_config$strategy %||% "crossover"
  avg_type <- trend_config$ma_type %||% "EMA"
  
  n_fast <- trend_config$short_ma_lookback %||% 50
  n_slow <- trend_config$long_ma_lookback %||% 200
  n_single <- trend_config$lookback %||% 200
  
  lag_signals <- 1 # Lag to avoid look-ahead bias
  warmup_policy <- "risk_on" # Start as risk-on during warm-up
  floor_val <- trend_config$floor %||% 0
  
  f_ma <- switch(avg_type,
                 SMA = TTR::SMA,
                 EMA = TTR::EMA,
                 WMA = TTR::WMA,
                 DEMA = TTR::DEMA,
                 stop(paste("Unsupported ma_type:", avg_type))
  )
  
  px <- xts::as.xts(prices)
  stopifnot(ncol(px) > 0)
  idx <- zoo::index(px)
  
  make_sig <- function(x) {
    if (mode == "single_ma") {
      ma <- f_ma(x, n = n_single)
      sig <- ifelse(x > ma, 1, floor_val)
    } else { # crossover
      ma_f <- f_ma(x, n = n_fast)
      ma_s <- f_ma(x, n = n_slow)
      sig <- ifelse(ma_f > ma_s, 1, floor_val)
    }
    as.numeric(sig)
  }
  
  sig_mat <- sapply(assets, function(cn) make_sig(px[, cn, drop = TRUE]))
  colnames(sig_mat) <- assets
  sig_xts <- xts::xts(sig_mat, order.by = idx)
  
  if (lag_signals != 0) {
    sig_xts <- stats::lag(sig_xts, k = lag_signals, na.pad = TRUE)
  }
  
  if (warmup_policy == "hold_prev") {
    sig_xts <- zoo::na.locf(sig_xts, na.rm = FALSE)
    sig_xts[is.na(sig_xts)] <- 1
  } else if (warmup_policy == "risk_on") {
    sig_xts[is.na(sig_xts)] <- 1
  } else { # "risk_off"
    sig_xts[is.na(sig_xts)] <- 0
  }
  
  sig_xts[] <- pmin(pmax(sig_xts, 0), 1)
  sig_xts
}


.default_class_map <- function(additions = NULL) {
  base_map <- c(SPY="Equity", QQQ="Equity", IWM="Equity", EFA="Equity", EEM="Equity", 
                LQD="IG", HYG="HY", IEF="Rates", TLT="Rates", TIP="TIPS", VNQ="REITs", 
                DBC="Commodities", GLD="Commodities", BIL="Cash", CASH="Cash")
  if (!is.null(additions) && is.vector(additions) && !is.null(names(additions))) {
    return(c(base_map, additions))
  }
  return(base_map)
}

calculate_target_weights <- function(prices, R, rebal_idx, previous_weights, master_controls, vix_series, trend_signals) {
  
  mc <- master_controls
  calc_date <- index(R)[rebal_idx]
  target_start_date <- snap_to_next_trading_day(calc_date + 1, index(R))
  rebal_date_char <- as.character(calc_date)
  
  window <- (rebal_idx - mc$lookback + 1L):rebal_idx; window <- window[window > 0]
  r_win_full <- R[window, , drop = FALSE]
  
  cash_asset <- mc$cash_asset %||% (if ("BIL" %in% colnames(R)) "BIL" else "CASH")
  all_cols <- colnames(R)
  
  if (NROW(na.omit(r_win_full)) < 25) {
    w_final <- setNames(numeric(length(all_cols)), all_cols); w_final[cash_asset] <- 1
    target_weights <- make_row_xts_strict(w_final, all_cols, target_start_date, name="target_weights_insufficient_lookback")
    
    x_vol <- xts(0, order.by=target_start_date); colnames(x_vol) <- "ex_ante_vol"
    x_lev <- xts(0, order.by=target_start_date); colnames(x_lev) <- "leverage"
    x_gross <- xts(1, order.by=target_start_date); colnames(x_gross) <- "gross_exposure"
    x_corr <- xts(0, order.by=target_start_date); colnames(x_corr) <- "avg_corr"
    
    diag_data <- list(ex_ante_vol = x_vol, leverage = x_lev, gross_exposure = x_gross, avg_corr = x_corr)
    log_entry <- data.frame(Date=target_start_date, Type="Skip/InsufficientLookback", Details="Not enough observations for covariance calculation.")
    return(list(target_weights = target_weights, log_entry = log_entry, diag_data = diag_data, audit_df = NULL, vix_event_log = NULL))
  }
  
  active_assets <- setdiff(all_cols, c("BIL","CASH"))
  keep <- colSums(!is.na(r_win_full[, active_assets, drop=FALSE])) >= floor(mc$lookback * mc$min_obs_frac)
  active_assets <- active_assets[keep]
  
  class_map <- .default_class_map(mc$ytd_enhancements$new_asset_classes)
  
  eligible_assets <- active_assets
  if (isTRUE(mc$trend_filter$enabled) && !is.null(trend_signals) && rebal_date_char %in% as.character(index(trend_signals))) {
    classes_to_filter <- mc$trend_filter$filter_on_classes
    assets_to_filter <- intersect(names(class_map)[class_map %in% classes_to_filter], active_assets)
    assets_not_filtered <- setdiff(active_assets, assets_to_filter)
    assets_to_check_in_signal <- intersect(assets_to_filter, colnames(trend_signals))
    
    if (length(assets_to_check_in_signal) > 0) {
      sig_today <- trend_signals[rebal_date_char, assets_to_check_in_signal]
      ok_indices <- which(as.numeric(sig_today) > 0 | is.na(as.numeric(sig_today)))
      assets_in_uptrend_from_signal <- names(sig_today)[ok_indices]
      eligible_assets <- unique(c(assets_not_filtered, assets_in_uptrend_from_signal))
    }
  }
  
  if (length(eligible_assets) < 1) {
    w_final <- setNames(numeric(length(all_cols)), all_cols); w_final[cash_asset] <- 1
    target_weights <- make_row_xts_strict(w_final, all_cols, target_start_date, name="final_weights_derisk")
    
    x_vol <- xts(0, order.by=target_start_date); colnames(x_vol) <- "ex_ante_vol"
    x_lev <- xts(0, order.by=target_start_date); colnames(x_lev) <- "leverage"
    x_gross <- xts(1, order.by=target_start_date); colnames(x_gross) <- "gross_exposure"
    x_corr <- xts(0, order.by=target_start_date); colnames(x_corr) <- "avg_corr"
    
    diag_data <- list(ex_ante_vol = x_vol, leverage = x_lev, gross_exposure = x_gross, avg_corr = x_corr)
    return(list(target_weights = target_weights, log_entry = data.frame(Date=target_start_date, Type="DE-RISK", Details="Not enough eligible assets after filtering."), diag_data = diag_data, audit_df = NULL, vix_event_log = NULL))
  }
  
  all_assets_for_cov <- unique(c(eligible_assets, cash_asset))
  Sigma_full <- cov_engine(r_win_full[, all_assets_for_cov, drop=FALSE], method = mc$cov_method, lambda = mc$cov_lambda)
  
  log_entry <- data.frame(Date=target_start_date, Type="Rebalance", Details="Standard rebalance.")
  
  active_class_map <- class_map[eligible_assets]
  sleeves <- split(eligible_assets, active_class_map)
  sleeve_names <- names(sleeves)
  num_sleeves <- length(sleeve_names)
  
  w_raw <- setNames(numeric(length(all_assets_for_cov)), all_assets_for_cov)
  
  if (num_sleeves == 1) {
    sleeve_risk_budgets <- setNames(1, sleeve_names[1])
  } else {
    sleeve_returns_list <- lapply(sleeve_names, function(sleeve_name) {
      assets_in_sleeve <- sleeves[[sleeve_name]]
      r_sleeve_assets <- r_win_full[, assets_in_sleeve, drop = FALSE]
      
      if(length(assets_in_sleeve) == 1) {
        sleeve_ret <- r_sleeve_assets
      } else {
        cov_sleeve <- cov_engine(r_sleeve_assets, method = mc$cov_method, lambda = mc$cov_lambda)
        intra_sleeve_w <- run_risk_parity_optimizer(cov_sleeve)
        sleeve_ret <- xts(rowSums(sweep(r_sleeve_assets, 2, intra_sleeve_w, `*`)), order.by = index(r_sleeve_assets))
      }
      colnames(sleeve_ret) <- sleeve_name
      return(sleeve_ret)
    })
    sleeve_returns <- do.call(merge, sleeve_returns_list[!sapply(sleeve_returns_list, is.null)])
    sleeve_cov <- cov_engine(sleeve_returns, method = mc$cov_method, lambda = mc$cov_lambda)
    sleeve_risk_budgets <- run_risk_parity_optimizer(sleeve_cov)
  }
  
  for (sleeve_name in names(sleeves)) {
    if(sleeve_name %in% names(sleeve_risk_budgets)){
      risk_budget_for_sleeve <- sleeve_risk_budgets[sleeve_name]
      assets_in_sleeve <- sleeves[[sleeve_name]]
      cov_sleeve <- Sigma_full[assets_in_sleeve, assets_in_sleeve, drop = FALSE]
      intra_sleeve_w <- run_risk_parity_optimizer(cov_sleeve)
      w_raw[assets_in_sleeve] <- risk_budget_for_sleeve * intra_sleeve_w
    }
  }
  
  LEV("rp_raw", w_raw, Sigma_full, cash=cash_asset)
  
  # --- FIX: Apply exposure-reducing rules BEFORE volatility scaling ---
  vix_event_log <- NULL
  if (isTRUE(mc$high_vix_layoff$enabled) && !is.null(vix_series)) {
    current_vix <- as.numeric(vix_series[rebal_date_char])
    if (!is.na(current_vix) && current_vix > mc$high_vix_layoff$vix_threshold) {
      layoff_factor <- mc$high_vix_layoff$layoff_factor
      
      risky_assets_to_scale <- setdiff(names(w_raw), cash_asset)
      w_before_layoff <- w_raw
      
      w_raw[risky_assets_to_scale] <- w_raw[risky_assets_to_scale] * layoff_factor
      # No cash recalculation here; scaler will handle it.
      
      ret_trade_day_idx <- which(index(R) == target_start_date)
      if (length(ret_trade_day_idx) > 0) {
        # Performance impact must be calculated on FINAL scaled weights
      }
      
      log_entry$Details <- paste0("High VIX Layoff: VIX at ", round(current_vix, 1), " > ", mc$high_vix_layoff$vix_threshold, ". Pre-scale risk reduced by ", (1-layoff_factor)*100, "%.")
      vix_event_log <- list(Date = target_start_date, VIX_Level = current_vix, Performance_Impact_bps = NA) # Placeholder
      LEV("post_vix_layoff", w_raw, Sigma_full, cash=cash_asset)
    }
  }
  
  if(isTRUE(mc$sleeve_derisking_rules$enabled)) {
    num_sleeves_char <- as.character(num_sleeves)
    if(num_sleeves_char %in% names(mc$sleeve_derisking_rules$rules)) {
      total_risk_budget_cap_sleeve <- mc$sleeve_derisking_rules$rules[[num_sleeves_char]]
      if (log_entry$Details == "Standard rebalance.") log_entry$Details <- paste0("De-risking: ", num_sleeves, " sleeves active. Pre-scale risk budget capped at ", total_risk_budget_cap_sleeve*100, "%.")
      
      risky_assets_to_scale <- setdiff(names(w_raw), cash_asset)
      current_risky_sum <- sum(w_raw[risky_assets_to_scale])
      if (current_risky_sum > total_risk_budget_cap_sleeve) {
        scale_factor <- total_risk_budget_cap_sleeve / current_risky_sum
        w_raw[risky_assets_to_scale] <- w_raw[risky_assets_to_scale] * scale_factor
        # No cash recalculation here; scaler will handle it.
        LEV("post_sleeve_cap", w_raw, Sigma_full, cash=cash_asset)
      }
    }
  }
  
  w_scaled_vec <- apply_vol_target_and_leverage(w_raw = w_raw, cov_mat = Sigma_full, vol_target = mc$vol_target, max_leverage = mc$gross_cap, cash_name = cash_asset)
  LEV("post_vol_target", w_scaled_vec, Sigma_full, cash=cash_asset)
  
  target_weights <- make_row_xts_strict(w_scaled_vec, all_cols, target_start_date, name="target_weights_final")
  final_risk_weights_vec <- as.numeric(target_weights[1, eligible_assets]); names(final_risk_weights_vec) <- eligible_assets
  
  Sigma_eligible <- Sigma_full[eligible_assets, eligible_assets, drop=FALSE]
  port_var_diag_final <- if(length(final_risk_weights_vec) > 0) as.numeric(t(final_risk_weights_vec) %*% Sigma_eligible %*% final_risk_weights_vec) else 0
  port_vol_diag_final <- if(!is.na(port_var_diag_final) && port_var_diag_final > 0) sqrt(port_var_diag_final) * sqrt(252) else 0
  
  audit_df <- NULL
  if (length(eligible_assets) > 0 && length(final_risk_weights_vec) > 0) {
    rc_asset_diag_final <- calculate_risk_contributions(final_risk_weights_vec, Sigma_eligible)
    rc_by_sleeve <- tapply(rc_asset_diag_final, active_class_map[names(rc_asset_diag_final)], sum)
    if (length(eligible_assets) == 1) { rc_by_sleeve <- setNames(1.0, active_class_map[eligible_assets]) }
    target_rc_per_sleeve <- 1 / length(rc_by_sleeve)
    risk_error_pct <- (rc_by_sleeve - target_rc_per_sleeve) * 100
    
    asset_vols <- sqrt(diag(Sigma_eligible)) * sqrt(252)
    mcr <- (Sigma_eligible %*% final_risk_weights_vec)
    
    audit_list <- lapply(eligible_assets, function(asset) {
      data.frame(
        Date = target_start_date, Asset = asset, Sleeve = active_class_map[asset],
        Asset_Weight_Pct = final_risk_weights_vec[asset] * 100,
        ExAnte_Asset_Volatility = asset_vols[asset],
        Marginal_Contribution_Risk = mcr[asset,1],
        Risk_Contribution_Pct = rc_asset_diag_final[asset] * 100,
        Portfolio_Risk_Contribution_Pct = rc_asset_diag_final[asset] * sum(final_risk_weights_vec) * 100,
        Risk_Error_Pct = risk_error_pct[active_class_map[asset]],
        stringsAsFactors = FALSE
      )
    })
    audit_df <- if(length(audit_list) > 0) dplyr::bind_rows(audit_list) else data.frame()
  }
  
  corr_matrix <- suppressWarnings(cov2cor(Sigma_eligible))
  avg_corr_val <- if(length(eligible_assets) > 1 && is.matrix(corr_matrix)) mean(corr_matrix[lower.tri(corr_matrix)], na.rm=TRUE) else 0
  
  x_vol <- xts(port_vol_diag_final, order.by=target_start_date); colnames(x_vol) <- "ex_ante_vol"
  x_lev <- xts(sum(abs(final_risk_weights_vec)), order.by=target_start_date); colnames(x_lev) <- "leverage"
  x_gross <- xts(sum(abs(as.numeric(target_weights))), order.by=target_start_date); colnames(x_gross) <- "gross_exposure"
  x_corr <- xts(avg_corr_val, order.by=target_start_date); colnames(x_corr) <- "avg_corr"
  
  diag_data <- list(ex_ante_vol = x_vol, leverage = x_lev, gross_exposure = x_gross, avg_corr = x_corr)
  
  return(list(target_weights = target_weights, log_entry = log_entry, diag_data = diag_data, audit_df = audit_df, vix_event_log = vix_event_log))
}

risk_parity_backtest <- function(prices, master_controls) {
  
  prices <- ensure_date_index(prices)
  assert_time_index(prices, "risk_parity_backtest prices")
  
  mc <- master_controls
  R <- ts_returns(prices, method = mc$returns_method)
  if (any(duplicated(index(R)))) stop("FATAL: Duplicate timestamps found in returns index.")
  
  if (!("CASH" %in% colnames(R))) R$CASH <- 0
  if (!("BIL" %in% colnames(R))) R$BIL <- 0
  cash_asset <- if ("BIL" %in% colnames(R)) "BIL" else "CASH"
  mc$cash_asset <- cash_asset
  
  vix_series <- get_vix_aligned(index(prices), start(prices), end(prices))
  trend_assets_to_use <- intersect(colnames(prices), names(.default_class_map(mc$ytd_enhancements$new_asset_classes)))
  trend_signals <- if(isTRUE(mc$trend_filter$enabled)) build_trend_mask_xts(prices[, trend_assets_to_use, drop=FALSE], mc$trend_filter) else xts()
  
  all_dates <- index(R)
  ep_raw <- endpoints(R, on = mc$rebalance_on)
  ep_raw <- ep_raw[ep_raw > mc$lookback]
  rebal_dates <- index(R)[ep_raw]
  if (!is.null(mc$rebalance_day)) {
    rebal_dates <- rebal_dates[weekdays(rebal_dates) == mc$rebalance_day]
  }
  
  raw_trade_log <- list(); constraint_log <- list(); full_audit_log_list <- list(); high_vix_event_log <- list()
  ex_ante_vol_list <- list(); leverage_list <- list(); gross_exp_list <- list(); avg_corr_list <- list()
  stop_loss_log <- list()
  
  w_initial <- setNames(rep(0, ncol(R)), colnames(R)); w_initial[cash_asset] <- 1.0
  raw_trade_log[[1]] <- list(date = index(R)[1], reason = "initialization", deltas = matrix(w_initial, nrow=1, dimnames=list(NULL, names(w_initial))), w_after = w_initial)
  
  w_current <- w_initial
  peak_equity <- 1.0; current_equity <- 1.0
  n_days <- NROW(R); pb <- utils::txtProgressBar(min = 1, max = n_days, style = 3, width = 60, char = "=")
  
  for (i in 2:n_days) {
    today <- all_dates[i]
    w_prev_eod_vec <- w_current
    ret_today <- coredata(R[i, ]); ret_today[is.na(ret_today)] <- 0
    daily_pnl <- sum(w_prev_eod_vec * ret_today, na.rm=TRUE)
    current_equity <- current_equity * (1 + daily_pnl)
    w_sod <- w_prev_eod_vec * (1 + ret_today)
    w_current <- w_sod / sum(w_sod, na.rm=TRUE)
    
    if (today %in% rebal_dates) {
      use_prev <- identical(mc$rebalance_signal_day, "prior_day")
      signal_date  <- if (use_prev && i > 1L) all_dates[i - 1L] else today
      signal_date  <- to_Date_or_die(signal_date, hint = "rebal_date")
      signal_calc_idx <- which(index(R) == signal_date)
      
      if (length(signal_calc_idx) > 0) {
        prev_w_xts <- make_row_xts_strict(w_current, colnames(R), all_dates[i-1], name="prev_w_xts_rebal")
        calc_result <- calculate_target_weights(prices, R, signal_calc_idx, prev_w_xts, mc, vix_series, trend_signals)
        if (!is.null(calc_result)) {
          w_after_vec <- .normalize_w(calc_result$target_weights, colnames(R))
          names(w_after_vec) <- colnames(R)
          delta <- matrix(w_after_vec - w_current, nrow = 1); colnames(delta) <- names(w_current)
          raw_trade_log[[length(raw_trade_log) + 1]] <- list(date = today, reason = "rebalance", deltas = delta, w_after = w_after_vec)
          w_current <- w_after_vec
          
          if (!is.null(calc_result$log_entry) && NROW(calc_result$log_entry) > 0) constraint_log[[length(constraint_log)+1]] <- calc_result$log_entry
          if (!is.null(calc_result$audit_df)) full_audit_log_list[[length(full_audit_log_list)+1]] <- calc_result$audit_df
          if (!is.null(calc_result$vix_event_log)) high_vix_event_log[[length(high_vix_event_log)+1]] <- as.data.frame(calc_result$vix_event_log)
          if (length(calc_result$diag_data) > 0) {
            diag <- calc_result$diag_data
            ex_ante_vol_list[[length(ex_ante_vol_list)+1]] <- diag$ex_ante_vol
            leverage_list[[length(leverage_list)+1]] <- diag$leverage
            gross_exp_list[[length(gross_exp_list)+1]] <- diag$gross_exposure
            avg_corr_list[[length(avg_corr_list)+1]] <- diag$avg_corr
          }
        }
      }
    }
    
    if (isTRUE(mc$stop_loss$enabled)) {
      peak_equity <- max(peak_equity, current_equity)
      drawdown <- (peak_equity - current_equity) / peak_equity
      if (!is.na(drawdown) && drawdown > mc$stop_loss$thresholds_by_class$default) {
        sl_log_entry <- data.frame(Date = today, Asset = "PORTFOLIO", Drawdown = drawdown, Threshold = mc$stop_loss$thresholds_by_class$default, DeRisked = TRUE)
        stop_loss_log[[length(stop_loss_log) + 1]] <- sl_log_entry
        w_current_after_sl <- setNames(rep(0, ncol(R)), colnames(R)); w_current_after_sl[cash_asset] <- 1.0
        delta_sl <- matrix(w_current_after_sl - w_current, nrow = 1); colnames(delta_sl) <- names(w_current)
        raw_trade_log[[length(raw_trade_log) + 1]] <- list(date = today, reason = "stop-loss", deltas = delta_sl, w_after = w_current_after_sl)
        w_current <- w_current_after_sl
        peak_equity <- current_equity
      }
    }
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  
  net_trade_deltas_xts <- xts(matrix(0, 0, ncol(R)), order.by = as.Date(integer(0)))
  colnames(net_trade_deltas_xts) <- colnames(R)
  
  if (length(raw_trade_log) > 0) {
    trade_dates_char <- sapply(raw_trade_log, function(x) format(to_Date_or_die(x$date), "%Y-%m-%d"))
    by_date <- tapply(seq_along(raw_trade_log), trade_dates_char, c)
    dates <- to_Date_or_die(names(by_date))
    
    net_mat <- do.call(rbind, lapply(by_date, function(idxs) {
      Reduce(`+`, lapply(raw_trade_log[idxs], `[[`, "deltas"))
    }))
    
    net_trade_deltas_xts <- xts(net_mat, order.by = dates)
  }
  
  risk_cols <- setdiff(colnames(R), c("BIL","CASH"))
  turnover_sparse <- if (NROW(net_trade_deltas_xts) > 0) {
    xts(rowSums(abs(net_trade_deltas_xts[, risk_cols, drop = FALSE]), na.rm = TRUE) / 2,
        order.by = index(net_trade_deltas_xts))
  } else xts(order.by = index(R))
  colnames(turnover_sparse) <- "Turnover"
  
  turnover_xts <- xts(rep(0, NROW(R)), order.by = index(R))
  colnames(turnover_xts) <- "Turnover"
  if (NROW(turnover_sparse) > 0) turnover_xts[index(turnover_sparse)] <- turnover_sparse
  
  bps_map <- mc$friction$bps_by_symbol
  default_bps <- mc$friction$default_bps
  
  bps_vec_full <- setNames(rep(default_bps, length(risk_cols)), risk_cols)
  overlap <- intersect(names(bps_map), risk_cols)
  if (length(overlap) > 0) bps_vec_full[overlap] <- bps_map[overlap]
  
  vix_aligned <- if (!is.null(vix_series)) vix_series[index(net_trade_deltas_xts)] else NULL
  vix_mult <- if (!is.null(vix_aligned) && NROW(vix_aligned) > 0) {
    ifelse(as.numeric(vix_aligned$VIX) > mc$friction$vix_threshold, mc$friction$stress_multiplier, 1)
  } else 1
  
  costs_sparse <- if (NROW(net_trade_deltas_xts) > 0) {
    abs_deltas <- abs(net_trade_deltas_xts[, risk_cols, drop = FALSE])
    base_costs <- rowSums(sweep(abs_deltas, 2, bps_vec_full, `*`), na.rm = TRUE) * 1e-4
    xts(as.numeric(base_costs) * as.numeric(vix_mult), order.by = index(net_trade_deltas_xts))
  } else xts(order.by = index(R))
  colnames(costs_sparse) <- "Costs"
  
  costs_xts <- xts(rep(0, NROW(R)), order.by = index(R))
  colnames(costs_xts) <- "Costs"
  if (NROW(costs_sparse) > 0) costs_xts[index(costs_sparse)] <- costs_sparse
  
  positions_mat <- matrix(0, nrow = NROW(R), ncol = ncol(R))
  colnames(positions_mat) <- colnames(R)
  
  trade_lookup <- if (length(raw_trade_log) > 0) {
    trade_dates <- sapply(raw_trade_log, function(x) as.character(to_Date_or_die(x$date)))
    last_trade_indices <- tapply(seq_along(raw_trade_log), trade_dates, function(idxs) idxs[length(idxs)])
    setNames(lapply(raw_trade_log[last_trade_indices], `[[`, "w_after"), names(last_trade_indices))
  } else list()
  
  w_current <- setNames(rep(0, ncol(R)), colnames(R)); w_current[cash_asset] <- 1.0
  positions_mat[1, ] <- if ("initialization" %in% sapply(raw_trade_log, `[[`, "reason")) trade_lookup[[as.character(index(R)[1])]] else w_current
  
  for (i in 2:NROW(R)) {
    ret_today <- coredata(R[i, ]); ret_today[is.na(ret_today)] <- 0
    w_sod <- positions_mat[i-1, ] * (1 + ret_today)
    w_current <- w_sod / sum(w_sod, na.rm = TRUE)
    
    d_char <- as.character(index(R)[i])
    if (d_char %in% names(trade_lookup)) {
      w_current <- trade_lookup[[d_char]]
    }
    positions_mat[i, ] <- w_current
  }
  
  positions_xts <- xts(positions_mat, order.by = index(R))
  
  w_lag <- lag(positions_xts, 1)
  w_lag[1, ] <- w_initial
  w_lag <- na.locf(w_lag, na.rm = FALSE)
  w_lag <- na.locf(w_lag, fromLast = TRUE, na.rm = FALSE)
  
  common_cols <- intersect(colnames(R), colnames(w_lag))
  R_aligned <- R[, common_cols, drop=FALSE]
  w_lag_aligned <- w_lag[, common_cols, drop=FALSE]
  
  gross_vec <- rowSums(coredata(R_aligned) * coredata(w_lag_aligned), na.rm = TRUE)
  portfolio_returns_xts_gross <- xts(gross_vec, order.by = index(R))
  colnames(portfolio_returns_xts_gross) <- "Gross_Portfolio_Return"
  
  portfolio_returns_xts <- portfolio_returns_xts_gross - costs_xts
  colnames(portfolio_returns_xts) <- "Net_Portfolio_Return"
  
  trade_log_df <- if(length(raw_trade_log) > 0) dplyr::bind_rows(lapply(raw_trade_log, function(x) data.frame(Date=x$date, Reason=x$reason))) else data.frame()
  
  results <- list(
    status = "success", prices_xts = prices, asset_returns_xts = R,
    weights_xts = last(positions_xts), positions_xts = positions_xts,
    portfolio_returns_xts_gross = portfolio_returns_xts_gross, portfolio_returns_xts = portfolio_returns_xts,
    costs_xts = costs_xts, turnover_xts = turnover_xts, net_trade_deltas_xts = net_trade_deltas_xts,
    constraint_log = if (length(constraint_log) > 0) dplyr::bind_rows(constraint_log) else data.frame(),
    stop_loss_log = if (length(stop_loss_log) > 0) dplyr::bind_rows(stop_loss_log) else data.frame(),
    high_vix_event_log = if (length(high_vix_event_log) > 0) dplyr::bind_rows(high_vix_event_log) else data.frame(),
    full_audit_log = if(length(full_audit_log_list) > 0) dplyr::bind_rows(full_audit_log_list) else data.frame(),
    trade_log = trade_log_df,
    ex_ante_vol_xts = if(length(ex_ante_vol_list) > 0) smart_rbind(ex_ante_vol_list) else xts(),
    leverage_xts = if(length(leverage_list) > 0) smart_rbind(leverage_list) else xts(),
    gross_exposure_xts = if(length(gross_exp_list) > 0) smart_rbind(gross_exp_list) else xts(),
    avg_corr_xts = if(length(avg_corr_list) > 0) smart_rbind(avg_corr_list) else xts(),
    vix_series = vix_series, trend_signals = trend_signals, raw_trade_log = raw_trade_log
  )
  
  if (NROW(results$leverage_xts) == 0 && NROW(results$positions_xts) > 0) {
    results$leverage_xts <- xts(rowSums(abs(results$positions_xts[, risk_cols, drop=FALSE]), na.rm=TRUE), order.by=index(results$positions_xts)); colnames(results$leverage_xts) <- "Leverage"
  }
  if (NROW(results$gross_exposure_xts) == 0 && NROW(results$positions_xts) > 0) {
    results$gross_exposure_xts <- xts(rowSums(abs(results$positions_xts), na.rm=TRUE), order.by=index(results$positions_xts)); colnames(results$gross_exposure_xts) <- "Gross_Exposure"
  }
  
  return(results)
}

# ====================== SUPPORT & RF =========================
.get_bil_returns <- function(start_date, end_date) {
  bil_px <- bbg_get_history_xts("BIL", start_date, end_date, field="PX_LAST")
  PerformanceAnalytics::Return.calculate(bil_px, method="discrete")
}

get_risk_free_xts <- function(dates, start_date, end_date, method = c("usgg3m","bil"), money_market_day_count = 360L) {
  method <- match.arg(method)
  dates  <- to_Date_or_die(dates, "get_risk_free_xts dates")
  
  .connect_bbg_internal()
  rf_daily <- NULL
  if (method == "usgg3m") {
    out <- try({
      rf_raw <- Rblpapi::bdh("USGG3M Index","PX_LAST", start.date=to_Date_or_die(start_date), end.date=to_Date_or_die(end_date), options=bbg_default_options())
      d <- data.frame(date=to_Date_or_die(rf_raw$date), px=as.numeric(rf_raw$PX_LAST))
      xts((d$px/100)/money_market_day_count, order.by=d$date)
    }, silent=TRUE)
    if (inherits(out,"try-error") || is.null(out) || NROW(out)==0) {
      message("[RF] USGG3M unavailable; falling back to BIL.")
      rf_daily <- .get_bil_returns(start_date, end_date)
    } else {
      rf_daily <- out
    }
  } else {
    rf_daily <- .get_bil_returns(start_date, end_date)
  }
  
  rv <- xts(rep(NA_real_, length(dates)), order.by=dates); colnames(rv) <- "RF"
  common <- intersect(index(rv), index(rf_daily)); if (length(common)) rv[common] <- rf_daily[common]
  rv <- na.locf(rv, na.rm=FALSE); rv <- na.locf(rv, fromLast=TRUE, na.rm=FALSE)
  return(ensure_date_index(rv))
}

# ============================ RPQS ENGINE (v159.1 - ROBUST) =======================================
calculate_volatility_diagnostics <- function(ex_ante_vol_xts, portfolio_returns_xts) {
  
  .empty_diag_xts <- function() {
    empty_mat <- matrix(NA_real_, nrow = 0, ncol = 3)
    empty_xts <- xts(empty_mat, order.by = as.Date(character()))
    colnames(empty_xts) <- c("Forecasted", "Realized", "Error")
    return(empty_xts)
  }
  
  message("[VOL_DIAG] Starting volatility diagnostics...")
  if (!is_data_valid(ex_ante_vol_xts, "ex_ante_vol_xts") || !is_data_valid(portfolio_returns_xts, "portfolio_returns_xts")) {
    message("[VOL_DIAG] FAIL: Input data is invalid (null, not xts, or empty).")
    return(.empty_diag_xts())
  }
  
  if (NCOL(ex_ante_vol_xts) > 1) { ex_ante_vol_xts <- ex_ante_vol_xts[, 1, drop = FALSE] }
  if (NCOL(portfolio_returns_xts) > 1) { portfolio_returns_xts <- portfolio_returns_xts[, 1, drop = FALSE] }
  
  merged_data <- merge(portfolio_returns_xts, ex_ante_vol_xts, join = "left")
  colnames(merged_data) <- c("Returns", "Forecast")
  merged_data$Forecast <- na.locf(merged_data$Forecast, na.rm = FALSE)
  merged_data <- na.omit(merged_data)
  
  if (NROW(merged_data) < 2) return(.empty_diag_xts())
  
  change_indices <- which(diff(merged_data$Forecast) != 0)
  ep <- unique(c(0, change_indices, NROW(merged_data)))
  if(length(ep) < 2) return(.empty_diag_xts())
  
  realized_p <- period.apply(merged_data$Returns, INDEX = ep, FUN = function(x) if(NROW(x) < 2) NA else sd(x, na.rm = TRUE) * sqrt(252))
  forecast_p <- period.apply(merged_data$Forecast, INDEX = ep, FUN = function(x) last(na.omit(x)) %||% NA)
  
  diag_data <- merge(forecast_p, realized_p, join = "inner")
  if (NROW(diag_data) == 0 || NCOL(diag_data) != 2) return(.empty_diag_xts())
  
  colnames(diag_data) <- c("Forecasted", "Realized")
  diag_data <- diag_data[complete.cases(diag_data), ]
  if (NROW(diag_data) == 0) return(.empty_diag_xts())
  
  diag_data$Error <- diag_data$Realized - diag_data$Forecasted
  return(diag_data)
}

calculate_rpqs_granular <- function(data_artifact, analysis_periods) {
  
  message("[RPQS] Calculating Granular RPQS for all specified periods...")
  
  results <- data_artifact$results
  
  analysis_start_date <- min(sapply(analysis_periods, function(p) to_Date_or_die(p$start)))
  all_periods <- c(analysis_periods[sapply(analysis_periods, `[[`, "enabled")], 
                   list("Full Period" = list(start=analysis_start_date, end=end(results$portfolio_returns_xts))))
  
  pillar_weights <- list(performance = 0.40, drawdown = 0.20, diversification = 0.15, friction = 0.15, fidelity = 0.10)
  metric_config <- list(
    sharpe_ratio = list(anchor = 0.5, scale = 0.5, dir = 1),
    max_drawdown = list(anchor = 0.12, scale = 0.05, dir = -1),
    mar_ratio = list(anchor = 5.0, scale = 2.5, dir = 1),
    avg_pairwise_corr = list(anchor = 0.35, scale = 0.10, dir = -1),
    avg_parity_deviation = list(anchor = 5.0, scale = 2.5, dir = -1),
    max_parity_deviation = list(anchor = 15.0, scale = 10.0, dir = -1),
    annualized_turnover = list(anchor = 1.00, scale = 0.5, dir = -1),
    annualized_costs_bps = list(anchor = 40, scale = 20, dir = -1),
    stops_per_year = list(anchor = 10, scale = 5, dir = -1),
    vol_forecast_rmse = list(anchor = 0.03, scale = 0.015, dir = -1),
    high_vix_sharpe = list(anchor = 0.2, scale = 0.3, dir = 1)
  )
  normalize_metric <- function(value, anchor, scale, dir) { 
    if (is.null(value) || length(value) == 0 || !is.finite(value)) return(0)
    score <- tanh((value - anchor) / scale)
    return(score * dir)
  }
  
  calculate_metrics_for_period <- function(perf_range, results) {
    raw_metrics <- list()
    
    perf_returns <- results$portfolio_returns_xts[perf_range]; perf_rf <- results$rf_series_xts[perf_range]
    if (NROW(perf_returns) < 20) return(NULL)
    
    raw_metrics$sharpe_ratio <- as.numeric(SharpeRatio.annualized(perf_returns, Rf = perf_rf))
    raw_metrics$max_drawdown <- as.numeric(maxDrawdown(perf_returns))
    perf_cagr <- as.numeric(Return.annualized(perf_returns))
    raw_metrics$mar_ratio <- if (raw_metrics$max_drawdown != 0) perf_cagr / raw_metrics$max_drawdown else 0
    
    if (is_data_valid(results$avg_corr_xts, "avg_corr_xts")) {
      mean_corr <- mean(results$avg_corr_xts[perf_range], na.rm = TRUE)
      raw_metrics$avg_pairwise_corr <- if(is.finite(mean_corr)) mean_corr else NULL
    } else {
      raw_metrics$avg_pairwise_corr <- NULL
    }
    
    audit_log_period <- results$full_audit_log[to_Date_or_die(results$full_audit_log$Date) >= start(perf_returns) & to_Date_or_die(results$full_audit_log$Date) <= end(perf_returns), ]
    raw_metrics$avg_parity_deviation <- if(NROW(audit_log_period) > 0) mean(abs(audit_log_period$Risk_Error_Pct), na.rm = TRUE) else 0
    raw_metrics$max_parity_deviation <- if(NROW(audit_log_period) > 0) max(abs(audit_log_period$Risk_Error_Pct), na.rm = TRUE) else 0
    
    turnover_period <- results$turnover_xts[perf_range]; costs_period <- results$costs_xts[perf_range]
    period_years <- max(1/252, as.numeric(difftime(end(perf_returns), start(perf_returns), units = "days") / 365.25))
    raw_metrics$annualized_turnover <- if(period_years > 0 && is_data_valid(turnover_period, "turnover_period")) sum(turnover_period, na.rm = TRUE) / period_years else 0
    raw_metrics$annualized_costs_bps <- if(period_years > 0 && is_data_valid(costs_period, "costs_period")) (sum(costs_period, na.rm = TRUE) / period_years) * 10000 else 0
    
    stops_period <- results$stop_loss_log[to_Date_or_die(results$stop_loss_log$Date) >= start(perf_returns) & to_Date_or_die(results$stop_loss_log$Date) <= end(perf_returns), ]
    raw_metrics$stops_per_year <- if(period_years > 0) NROW(stops_period) / period_years else 0
    
    diag_vol_period <- results$volatility_diagnostics_xts[perf_range]
    raw_metrics$vol_forecast_rmse <- if(is_data_valid(diag_vol_period, "diag_vol_period") && NROW(diag_vol_period) > 0) sqrt(mean(diag_vol_period$Error^2, na.rm = TRUE)) else 0.10
    
    if (is_data_valid(results$vix_series, "vix_series")) {
      vix_period <- results$vix_series[perf_range]
      merged_vix_ret <- merge(perf_returns, vix_period, join = "inner")
      high_vix_data <- merged_vix_ret[merged_vix_ret$VIX > 25]
      high_vix_returns <- high_vix_data[,1]
      high_vix_rf <- perf_rf[index(high_vix_returns)]
      raw_metrics$high_vix_sharpe <- if(length(high_vix_returns) > 20) as.numeric(SharpeRatio.annualized(high_vix_returns, Rf = high_vix_rf)) else 0
    } else {
      raw_metrics$high_vix_sharpe <- 0
    }
    
    return(raw_metrics)
  }
  
  calculate_score_from_metrics <- function(raw_metrics) {
    if (is.null(raw_metrics)) return(list(rpqs_score = NA, pillar_summary = NULL, metric_details = NULL))
    
    report_df_list <- list(); normalized_scores <- list()
    metrics_to_run <- names(metric_config)
    
    for (metric_name in metrics_to_run) {
      raw_val <- raw_metrics[[metric_name]]
      cfg <- metric_config[[metric_name]]
      norm_score <- normalize_metric(raw_val, cfg$anchor, cfg$scale, cfg$dir)
      normalized_scores[[metric_name]] <- norm_score
      report_df_list[[metric_name]] <- data.frame(Metric = metric_name, Raw_Value = raw_val %||% NA, Normalized_Score = norm_score, stringsAsFactors = FALSE)
    }
    
    pillar_scores <- list(
      performance = normalized_scores$sharpe_ratio,
      drawdown = mean(c(normalized_scores$max_drawdown, normalized_scores$mar_ratio), na.rm=TRUE),
      diversification = mean(c(normalized_scores$avg_pairwise_corr, normalized_scores$avg_parity_deviation, normalized_scores$max_parity_deviation), na.rm=TRUE),
      friction = mean(c(normalized_scores$annualized_turnover, normalized_scores$annualized_costs_bps, normalized_scores$stops_per_year), na.rm=TRUE),
      fidelity = mean(c(normalized_scores$vol_forecast_rmse, normalized_scores$high_vix_sharpe), na.rm=TRUE)
    )
    pillar_scores <- lapply(pillar_scores, function(x) if(is.nan(x) || is.null(x)) 0 else x)
    
    final_rpqs <- sum(sapply(names(pillar_scores), function(p) pillar_scores[[p]] * pillar_weights[[p]]), na.rm=TRUE) * 100
    
    pillar_summary_df <- data.frame(Pillar = names(pillar_scores), Pillar_Weight = sapply(names(pillar_scores), function(p) pillar_weights[[p]]), Pillar_Score = sapply(names(pillar_scores), function(p) pillar_scores[[p]]), Contribution_To_RPQS = sapply(names(pillar_scores), function(p) pillar_scores[[p]] * pillar_weights[[p]] * 100), stringsAsFactors = FALSE)
    
    return(list(rpqs_score = final_rpqs, pillar_summary = pillar_summary_df, metric_details = dplyr::bind_rows(report_df_list)))
  }
  
  all_rpqs_results <- lapply(names(all_periods), function(period_name) {
    period <- all_periods[[period_name]]
    perf_range <- paste0(as.character(to_Date_or_die(period$start)), "/", as.character(to_Date_or_die(period$end)))
    
    message(sprintf("[RPQS] Evaluating period: %s (%s)", period_name, perf_range))
    
    raw_metrics <- calculate_metrics_for_period(perf_range, results)
    score_result <- calculate_score_from_metrics(raw_metrics)
    
    return(score_result)
  })
  names(all_rpqs_results) <- names(all_periods)
  
  message("[RPQS] Granular calculation complete.")
  return(all_rpqs_results)
}

# ============================ DIAGNOSTICS & REPORTING (v160.9) ====================================
print_console_summary <- function(data_artifact, analysis_periods) {
  results <- data_artifact$results
  enabled_periods <- analysis_periods[sapply(analysis_periods, `[[`, "enabled")]
  
  cat("\n")
  for (period_name in names(enabled_periods)) {
    period <- enabled_periods[[period_name]]
    range <- paste0(as.character(to_Date_or_die(period$start)), "/", as.character(to_Date_or_die(period$end)))
    Rp <- results$portfolio_returns_xts[range]
    if(NROW(Rp) < 20) { 
      cat(sprintf("\n--- NOT ENOUGH DATA FOR %s SUMMARY ---\n", toupper(period_name)))
      next 
    }
    Rf_period <- results$rf_series_xts[index(Rp)]
    cat(sprintf("================= %s PERFORMANCE SUMMARY (NET) =================\n", toupper(period_name)))
    cat(sprintf("Start: %s | End: %s | Avg. Ann. Return (CAGR): %.2f%% | Annual Vol: %.2f%% | Sharpe Ratio: %.2f | Max Drawdown: %.2f%%\n",
                as.character(start(Rp)),
                as.character(end(Rp)),
                Return.annualized(Rp)*100,
                StdDev.annualized(Rp)*100,
                SharpeRatio.annualized(Rp, Rf=Rf_period),
                maxDrawdown(Rp)*100))
  }
  cat("\n")
}

is_data_valid <- function(x, name) {
  obj_name <- name %||% deparse(substitute(x))
  if (is.null(x)) {
    return(FALSE)
  }
  if (!is.xts(x) && !is.data.frame(x)) {
    return(FALSE)
  }
  if (NROW(x) == 0) {
    return(FALSE) 
  }
  if (is.xts(x) && sum(is.finite(coredata(x)), na.rm = TRUE) == 0) {
    return(FALSE)
  }
  return(TRUE)
}

write_period_diagnostics <- function(wb, sheet_name = "00e_Period_Diagnostics",
                                     daily_net_xts, full_start, full_end, periods_df) {
  addWorksheet(wb, sheet_name)
  
  compound_ret <- function(r_xts) {
    if (is.null(r_xts) || NROW(r_xts) == 0) return(NA_real_)
    as.numeric(prod(1 + coredata(na.fill(r_xts, 0)), na.rm = TRUE) - 1)
  }
  
  idx_full_range <- index(daily_net_xts[paste0(full_start, "/", full_end)])
  
  periods_df$Start_Date <- to_Date_or_die(periods_df$Start_Date)
  periods_df$End_Date <- to_Date_or_die(periods_df$End_Date)
  
  periods_df$Trading_Days <- NA_integer_
  periods_df$Overlaps_Previous_Period <- FALSE
  periods_df$Gap_Days_From_Previous <- NA_integer_
  
  prev_end <- as.Date(NA)
  for (i in seq_len(nrow(periods_df))) {
    rng <- paste0(periods_df$Start_Date[i], "/", periods_df$End_Date[i])
    sub <- daily_net_xts[rng]
    periods_df$Trading_Days[i] <- NROW(sub)
    
    if (i > 1) {
      periods_df$Overlaps_Previous_Period[i] <- !is.na(prev_end) && periods_df$Start_Date[i] <= prev_end
      
      after_prev <- prev_end + 1L
      before_curr <- periods_df$Start_Date[i] - 1L
      if (after_prev <= before_curr) {
        gap_range <- paste0(after_prev, "/", before_curr)
        gap_idx <- index(daily_net_xts[gap_range])
        periods_df$Gap_Days_From_Previous[i] <- length(gap_idx)
      } else {
        periods_df$Gap_Days_From_Previous[i] <- 0
      }
    }
    prev_end <- periods_df$End_Date[i]
  }
  
  full_ret <- compound_ret(daily_net_xts[paste0(full_start, "/", full_end)])
  
  comp_by_period <- 1.0
  for (i in seq_len(nrow(periods_df))) {
    r <- daily_net_xts[paste0(periods_df$Start_Date[i], "/", periods_df$End_Date[i])]
    comp_by_period <- comp_by_period * (1 + compound_ret(r))
  }
  comp_by_period <- comp_by_period - 1
  
  identity_df <- data.frame(
    Metric = c("Full_Period_Start", "Full_Period_End", "Days_in_Full_Window",
               "Total_Return_from_Daily_Full_Window", "Total_Return_from_Compounding_Periods",
               "Difference_(Compounded_vs_Daily)"),
    Value = c(as.character(full_start), as.character(full_end), length(idx_full_range),
              full_ret, comp_by_period, comp_by_period - full_ret)
  )
  
  writeData(wb, sheet = sheet_name, x = "Return Identity Checks", startRow=1)
  writeData(wb, sheet = sheet_name, x = identity_df, startRow = 2)
  writeData(wb, sheet = sheet_name, x = "Period Coverage Diagnostics", startRow = NROW(identity_df) + 4)
  writeData(wb, sheet = sheet_name, x = periods_df, startRow = NROW(identity_df) + 5, colNames = TRUE)
}

generate_professional_assessment_pack <- function(data_artifact, analysis_periods) {
  run_id <- data_artifact$run_id
  message("\n===================================================================")
  message("Starting Comprehensive Assessment Pack Generation for Run ID: ", run_id)
  message("===================================================================\n")
  
  output_excel_dir <- REPORTS_DIR
  config  <- data_artifact$config; results <- data_artifact$results
  portfolio_returns_xts  <- results$portfolio_returns_xts; portfolio_returns_xts_gross <- results$portfolio_returns_xts_gross
  asset_returns_xts      <- results$asset_returns_xts
  weights_xts            <- results$weights_xts; positions_xts          <- results$positions_xts
  ex_ante_vol_xts        <- results$ex_ante_vol_xts
  costs_xts <- results$costs_xts; if (xts::is.xts(costs_xts)) costs_xts[is.na(costs_xts)] <- 0
  turnover_xts <- results$turnover_xts; if (xts::is.xts(turnover_xts)) turnover_xts[is.na(turnover_xts)] <- 0
  rf_series_xts          <- results$rf_series_xts
  leverage_xts           <- results$leverage_xts; gross_exposure_xts     <- results$gross_exposure_xts
  constraint_log         <- results$constraint_log
  stop_loss_log          <- results$stop_loss_log; vix_series             <- results$vix_series
  full_audit_log         <- results$full_audit_log
  high_vix_event_log     <- results$high_vix_event_log; rpqs_results           <- data_artifact$rpqs_results
  avg_corr_xts           <- results$avg_corr_xts
  trade_log              <- results$trade_log
  net_trade_deltas_xts   <- results$net_trade_deltas_xts
  
  wb <- createWorkbook()
  
  enabled_periods <- analysis_periods[sapply(analysis_periods, `[[`, "enabled")]
  analysis_start_date <- min(sapply(enabled_periods, function(p) to_Date_or_die(p$start)))
  
  message("[1/22] Generating methodology and forward signal tabs...")
  addWorksheet(wb, "00a_Methodology"); tryCatch({
    methodology_text <- data.frame(
      Sheet = c("00b_Forward_Signal", "00c_RPQ_Scores", "00d_Checks", "00e_Period_Diagnostics", "01_Run_Summary", "05_Costs_and_Turnover", "14_VIX_Regime_Analysis", "17_High_VIX_Analysis", "18_YTD_Deep_Dive"),
      Description = c("ACTIONABLE: The target portfolio weights for the upcoming period.", "NEW: Granular RPQS for every analysis period, with full metric-level breakdown.", "NEW: Automated checks to verify internal consistency of the report.", "NEW: Diagnostics to check for period overlaps/gaps and verify return compounding.", "High-level summary including Gross and Net performance, and Total Period Return.", "ENHANCED: Now includes 'Total Individual Trades' for a granular count of all buys/sells.", "Shows the annualized performance of the *actual* portfolio on days falling within each VIX regime.", "NEW: A log of all High-VIX Layoff events. A negative 'Performance_Impact_bps' is GOOD - it represents the loss that was avoided by de-risking.", "NOW POPULATED: A complete, daily breakdown of all data and signals for the YTD period, allowing for full manual validation.")
    )
    writeData(wb, "00a_Methodology", methodology_text, rowNames = FALSE)
    
  }, error = function(e) { message("Error generating Methodology tab: ", e$message) })
  
  addWorksheet(wb, "00b_Forward_Signal"); tryCatch({
    if (!is.null(data_artifact$forward_weights) && !is.null(data_artifact$forward_weights$target_weights)) {
      fw_result <- data_artifact$forward_weights; fw_df <- as.data.frame(t(coredata(fw_result$target_weights))) * 100
      colnames(fw_df) <- "Target_Weight_Pct"; fw_df <- fw_df[order(-fw_df$Target_Weight_Pct), , drop = FALSE]
      meta_df <- data.frame(Parameter=c("Signal_Date", "Target_Start_Date", "Strategy_Note"), Value=c(as.character(index(fw_result$target_weights)-1), as.character(index(fw_result$target_weights)), as.character(fw_result$log_entry$Details %||% "Standard Rebalance")))
      writeData(wb, "00b_Forward_Signal", "Forward-Looking Signal", startRow = 1)
      writeData(wb, "00b_Forward_Signal", meta_df, startRow = 2, rowNames=FALSE)
      writeData(wb, "00b_Forward_Signal", "Target Portfolio Weights (%)", startRow = NROW(meta_df)+4)
      writeData(wb, "00b_Forward_Signal", fw_df, startRow = NROW(meta_df)+5, rowNames=TRUE)
    } else { writeData(wb, "00b_Forward_Signal", "Forward signal was not calculated for this run.") }
  }, error = function(e) { message("Error generating Forward Signal tab: ", e$message) })
  
  message("[2/22] Generating granular and detailed RPQS tab...")
  addWorksheet(wb, "00c_RPQ_Scores"); tryCatch({
    if(!is.null(rpqs_results)){
      summary_scores <- sapply(rpqs_results, function(x) x$rpqs_score)
      summary_df <- data.frame(Period = names(summary_scores), RPQS_Score = summary_scores, row.names = NULL)
      
      writeData(wb, "00c_RPQ_Scores", "Granular RPQS Summary", startRow = 1)
      writeData(wb, "00c_RPQ_Scores", summary_df, startRow = 2, rowNames = FALSE)
      
      current_row <- NROW(summary_df) + 4
      
      for(period_name in names(rpqs_results)) {
        period_result <- rpqs_results[[period_name]]
        if(!is.null(period_result$pillar_summary)) {
          writeData(wb, "00c_RPQ_Scores", paste(period_name, "- Pillar Summary"), startRow = current_row)
          writeData(wb, "00c_RPQ_Scores", period_result$pillar_summary, startRow = current_row + 1, rowNames = FALSE)
          current_row <- current_row + NROW(period_result$pillar_summary) + 2
          
          if(!is.null(period_result$metric_details)) {
            writeData(wb, "00c_RPQ_Scores", paste(period_name, "- Detailed Metric Breakdown"), startRow = current_row)
            writeData(wb, "00c_RPQ_Scores", period_result$metric_details, startRow = current_row + 1, rowNames = FALSE)
            current_row <- current_row + NROW(period_result$metric_details) + 3
          }
        }
      }
    } else { writeData(wb, "00c_RPQ_Scores", "RPQS calculation failed or was not run.")}
  }, error = function(e) { message("Error generating RPQS tab: ", e$message) })
  
  message("[3/22] Generating new Checks tab...")
  addWorksheet(wb, "00d_Checks"); tryCatch({
    checks_spec <- data.frame(
      Check_Name = c("Net-Gross Wedge vs. Costs", "Leverage Policy", "Turnover Identity", "Trade Day Coverage", "Units Sanity"),
      Description = c("The total performance difference between Gross and Net curves must equal the total costs reported.", "Gross exposure must respect the leverage policy.", "Annualized turnover calculated from daily trades must match summary statistics.", "Number of days with trading activity must match reported number of trading days in the log.", "Columns labeled with _bps should contain values in BPS, not decimals."),
      Status = "",
      `Measured Value` = "",
      `Expected Value` = "",
      Difference = "",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    total_costs_val <- sum(costs_xts, na.rm=TRUE)
    total_wedge_val <- sum(portfolio_returns_xts_gross - portfolio_returns_xts, na.rm=TRUE)
    diff_wedge <- total_costs_val - total_wedge_val
    checks_spec[1, "Status"] <- ifelse(abs(diff_wedge) < 1e-6, "PASS", "FAIL")
    checks_spec[1, "Measured Value"] <- sprintf("%.4f bps", total_wedge_val * 10000)
    checks_spec[1, "Expected Value"] <- sprintf("%.4f bps", total_costs_val * 10000)
    checks_spec[1, "Difference"] <- sprintf("%.4f bps", diff_wedge * 10000)
    
    gross_cap <- config$master_controls$gross_cap %||% 1.0
    max_gross_exp <- if(is_data_valid(gross_exposure_xts, "gross_exposure_xts")) max(gross_exposure_xts, na.rm=TRUE) else 1.0
    diff_leverage <- max_gross_exp - gross_cap
    if (config$master_controls$allow_leverage) {
      checks_spec[2, "Status"] <- ifelse(diff_leverage <= 1e-6, "PASS", "FAIL")
      checks_spec[2, "Measured Value"] <- sprintf("%.4f", max_gross_exp)
      checks_spec[2, "Expected Value"] <- sprintf("<= %.4f", gross_cap)
      checks_spec[2, "Difference"] <- sprintf("%.4f", diff_leverage)
    } else {
      checks_spec[2, "Status"] <- ifelse(abs(max_gross_exp - 1.0) < 1e-6, "PASS", "FAIL")
      checks_spec[2, "Measured Value"] <- sprintf("%.4f", max_gross_exp)
      checks_spec[2, "Expected Value"] <- "1.0000"
      checks_spec[2, "Difference"] <- sprintf("%.4f", max_gross_exp - 1.0)
    }
    
    total_years <- if(is_data_valid(turnover_xts, "turnover_xts")) as.numeric(difftime(end(turnover_xts), start(turnover_xts), units="days")) / 365.25 else 1
    annualized_turnover_from_total <- (sum(turnover_xts, na.rm=T) / total_years)
    
    trade_events <- turnover_xts[turnover_xts > 1e-6]
    avg_turnover_per_trade_day <- if(length(trade_events) > 0) mean(trade_events, na.rm=TRUE) else 0
    total_trade_days <- length(trade_events)
    avg_trade_days_per_month <- total_trade_days / (total_years * 12)
    annualized_turnover_from_components <- avg_turnover_per_trade_day * avg_trade_days_per_month * 12
    
    diff_turnover <- annualized_turnover_from_total - annualized_turnover_from_components
    checks_spec[3, "Status"] <- ifelse(abs(diff_turnover) < 1e-4, "PASS", "FAIL")
    checks_spec[3, "Measured Value"] <- sprintf("%.4f", annualized_turnover_from_components)
    checks_spec[3, "Expected Value"] <- sprintf("%.4f", annualized_turnover_from_total)
    checks_spec[3, "Difference"] <- sprintf("%.6f", diff_turnover)
    
    eps <- 5e-5
    risk_cols_check <- setdiff(colnames(net_trade_deltas_xts), c("CASH","BIL"))
    days_from_deltas <- if(is_data_valid(net_trade_deltas_xts, "net_trade_deltas_xts")) sum(rowSums(abs(net_trade_deltas_xts[, risk_cols_check, drop=FALSE]) > eps, na.rm=TRUE) >= 1) else 0
    days_from_trade_log_raw <- if (is_data_valid(trade_log, "trade_log")) length(unique(to_Date_or_die(trade_log$Date[trade_log$Reason != "initialization"]))) else 0
    diff_trade_days <- days_from_deltas - days_from_trade_log_raw
    checks_spec[4, "Status"] <- ifelse(days_from_deltas == days_from_trade_log_raw, "PASS", "FAIL")
    checks_spec[4, "Measured Value"] <- sprintf("%d (from Deltas)", days_from_deltas)
    checks_spec[4, "Expected Value"] <- sprintf("%d (from Log)", days_from_trade_log_raw)
    checks_spec[4, "Difference"] <- diff_trade_days
    
    if (is_data_valid(high_vix_event_log, "high_vix_event_log") && NROW(high_vix_event_log) > 0 && "Performance_Impact_bps" %in% names(high_vix_event_log)) {
      median_impact <- median(abs(high_vix_event_log$Performance_Impact_bps), na.rm=TRUE)
      checks_spec[5, "Status"] <- ifelse(median_impact > 0.01, "PASS", "FAIL (Decimal?)")
      checks_spec[5, "Measured Value"] <- sprintf("Median abs impact: %.2f", median_impact)
      checks_spec[5, "Expected Value"] <- "O(bps)"
      checks_spec[5, "Difference"] <- ""
    } else {
      checks_spec[5, "Status"] <- "N/A"
    }
    
    writeData(wb, "00d_Checks", "Automated Sanity Checks", startRow = 1)
    writeData(wb, "00d_Checks", checks_spec, startRow = 2, rowNames=FALSE)
  }, error = function(e) { message("Error generating Checks tab: ", e$message)})
  
  message("[4/22] Generating Run Summary...")
  addWorksheet(wb, "01_Run_Summary"); tryCatch({
    params <- unlist(config$master_controls, recursive = TRUE); config_df <- data.frame(Parameter = names(params), Value = as.character(sapply(params, function(x) if(is.list(x)) jsonlite::toJSON(x, auto_unbox=T) else as.character(x))))
    weight_sums <- if(is_data_valid(positions_xts, "positions_xts")) rowSums(positions_xts, na.rm = TRUE) else 1.0
    avg_abs_error <- mean(abs(weight_sums - 1.0), na.rm = TRUE); health_score <- 100 * (1 - avg_abs_error)
    
    rpqs_main_oos <- rpqs_results$`Main OOS`$rpqs_score %||% NA
    rpqs_full <- rpqs_results$`Full Period`$rpqs_score %||% NA
    
    meta_df <- data.frame(Parameter = c("Run_ID", "Timestamp_UTC", "User", "Portfolio_Health_Score", "RPQS_Main_OOS", "RPQS_Full_Period"), 
                          Value = c(data_artifact$run_id, data_artifact$timestamp_utc, data_artifact$user, health_score, rpqs_main_oos, rpqs_full))
    full_config_df <- rbind(meta_df, config_df)
    
    summary_list <- list()
    full_period_row <- list("Full Period" = list(start=analysis_start_date, end=end(portfolio_returns_xts), enabled=TRUE))
    all_periods_with_full <- c(enabled_periods, full_period_row)
    
    for (period_name in names(all_periods_with_full)) {
      period <- all_periods_with_full[[period_name]]
      range <- paste0(as.character(to_Date_or_die(period$start)), "/", as.character(to_Date_or_die(period$end)))
      
      Rp_net <- portfolio_returns_xts[range]; Rp_gross <- portfolio_returns_xts_gross[range]
      if(NROW(Rp_net) < 2) {
        message("[RunSummary] Skipping period ", period_name, " (not enough data).")
        next
      }
      Rf <- rf_series_xts[range]
      
      summary_list[[period_name]] <- c(
        Period_Start = as.character(to_Date_or_die(period$start)),
        Period_End = as.character(to_Date_or_die(period$end)),
        Net_CAGR_Pct = Return.annualized(Rp_net)*100, 
        Net_Vol_Pct = StdDev.annualized(Rp_net)*100, 
        Net_Sharpe = SharpeRatio.annualized(Rp_net, Rf=Rf), 
        Net_MaxDD_Pct = maxDrawdown(Rp_net)*100,
        Net_Total_Return_Pct = (prod(1 + Rp_net, na.rm = TRUE) - 1) * 100,
        Gross_CAGR_Pct = Return.annualized(Rp_gross)*100, 
        Gross_Sharpe = SharpeRatio.annualized(Rp_gross, Rf=Rf),
        Gross_Total_Return_Pct = (prod(1 + Rp_gross, na.rm = TRUE) - 1) * 100
      )
    }
    summary_df <- dplyr::bind_rows(summary_list, .id = "Period")
    
    writeData(wb, "01_Run_Summary", "Full Configuration", startRow = 1); writeData(wb, "01_Run_Summary", full_config_df, startRow = 2, rowNames = FALSE)
    writeData(wb, "01_Run_Summary", "Performance Summary by Period (Gross vs. Net)", startRow = NROW(full_config_df) + 4)
    writeData(wb, "01_Run_Summary", summary_df, startRow = NROW(full_config_df) + 5, rowNames = FALSE)
  }, error = function(e) { message("Error generating Run Summary: ", e$message) })
  
  message("[5/22] Generating detailed performance tabs...")
  addWorksheet(wb, "02_Performance_Periods"); tryCatch({
    all_periods <- c(enabled_periods, list("Full Period" = list(start=analysis_start_date, end=end(portfolio_returns_xts))))
    perf_list <- list(); 
    for (period_name in names(all_periods)) {
      period <- all_periods[[period_name]]; range <- paste0(as.character(to_Date_or_die(period$start)), "/", as.character(to_Date_or_die(period$end)))
      Rp <- portfolio_returns_xts[range]; if(NROW(Rp) < 20) next; Rf <- rf_series_xts[range]
      
      den <- abs(sum(Rp[Rp<0],na.rm=TRUE))
      profit_factor <- if (den > 1e-9) sum(Rp[Rp>0],na.rm=T)/den else NA_real_
      
      metrics <- c(`Total Return (%)`= (prod(1+Rp) - 1) * 100, `CAGR (%)` = PerformanceAnalytics::Return.annualized(Rp, scale = 252) * 100, `Annual Volatility (%)` = PerformanceAnalytics::StdDev.annualized(Rp, scale = 252) * 100, `Annual Sharpe Ratio` = PerformanceAnalytics::SharpeRatio.annualized(Rp, Rf = Rf, scale = 252), `Annual Sortino Ratio` = as.numeric(PerformanceAnalytics::SortinoRatio(Rp, MAR = mean(Rf, na.rm=TRUE))), `Max Drawdown (%)` = PerformanceAnalytics::maxDrawdown(Rp) * 100, `Calmar Ratio` = as.numeric(PerformanceAnalytics::CalmarRatio(Rp)), `Sterling Ratio` = as.numeric(PerformanceAnalytics::SterlingRatio(Rp)), `Profit Factor` = profit_factor, `Daily Win Rate (%)` = (sum(Rp > 0, na.rm=T) / length(Rp[!is.na(Rp)])) * 100)
      perf_list[[period_name]] <- metrics 
    }
    perf_df <- as.data.frame(do.call(cbind, perf_list)); writeData(wb, "02_Performance_Periods", perf_df, rowNames = TRUE) 
  }, error = function(e) { message("Error generating Performance by Period: ", e$message) })
  
  addWorksheet(wb, "03_Performance_Annual"); tryCatch({
    if(is_data_valid(portfolio_returns_xts, "portfolio_returns_xts")) {
      annual_net <- xts::apply.yearly(portfolio_returns_xts, PerformanceAnalytics::Return.cumulative)
      annual_gross <- xts::apply.yearly(portfolio_returns_xts_gross, PerformanceAnalytics::Return.cumulative)
      annual_rf <- xts::apply.yearly(rf_series_xts, PerformanceAnalytics::Return.cumulative)
      annual_costs_bps <- xts::apply.yearly(costs_xts, sum) * 10000
      
      merged_annual <- merge(annual_net, annual_gross, annual_rf, annual_costs_bps)
      merged_annual[is.na(merged_annual)] <- 0
      
      annual_df <- data.frame(Year = format(index(merged_annual), "%Y"), coredata(merged_annual))
      colnames(annual_df) <- c("Year","Net_%","Gross_%","RiskFree_%","Costs_bps")
      
      writeData(wb, "03_Performance_Annual", annual_df, rowNames = FALSE)
    } else { writeData(wb, "03_Performance_Annual", "DATA ERROR: Invalid returns data.") }
  }, error = function(e) { message("Error generating Annual Performance: ", e$message) })
  
  message("[6/22] Generating Equity Curve and cost/turnover tabs...")
  addWorksheet(wb, "04_Equity_Curve_Daily"); tryCatch({
    if (!is_data_valid(portfolio_returns_xts, "portfolio_returns_xts")) {
      writeData(wb, "04_Equity_Curve_Daily", "DATA ERROR: Net portfolio returns were invalid or empty.")
    } else {
      merged <- portfolio_returns_xts
      if (is_data_valid(portfolio_returns_xts_gross, "portfolio_returns_xts_gross")) {
        merged <- merge(merged, portfolio_returns_xts_gross, join = "left")
      }
      if (is_data_valid(costs_xts, "costs_xts")) {
        merged <- merge(merged, costs_xts, join = "left")
      }
      
      if ("Costs" %in% colnames(merged)) merged$Costs[is.na(merged$Costs)] <- 0
      
      period_labels <- xts(rep(NA_character_, NROW(merged)), order.by = index(merged))
      for(pname in names(enabled_periods)) {
        period_range <- paste0(as.character(to_Date_or_die(enabled_periods[[pname]]$start)), "/", as.character(to_Date_or_die(enabled_periods[[pname]]$end)))
        period_labels[period_range] <- pname
      }
      
      if("Net_Portfolio_Return" %in% colnames(merged)) {
        merged$Cumulative_Return_Net <- cumprod(1 + merged$Net_Portfolio_Return)
        merged$Drawdown_Pct_Net <- as.numeric(PerformanceAnalytics::Drawdowns(merged$Net_Portfolio_Return)) * 100
      }
      if("Gross_Portfolio_Return" %in% colnames(merged)) {
        merged$Cumulative_Return_Gross <- cumprod(1 + merged$Gross_Portfolio_Return)
      }
      
      equity_df <- data.frame(
        Date = index(merged),
        coredata(merged),
        Period = as.character(coredata(period_labels[index(merged)])),
        check.names = FALSE
      )
      
      writeData(wb, "04_Equity_Curve_Daily", equity_df, rowNames = FALSE)
    }
  }, error = function(e) { message("Error generating Equity Curve: ", e$message) })
  
  addWorksheet(wb, "05_Costs_and_Turnover"); tryCatch({
    if (is_data_valid(turnover_xts, "turnover_xts") && is_data_valid(costs_xts, "costs_xts")) {
      all_periods <- c(enabled_periods, list("Full Period" = list(start=analysis_start_date, end=end(costs_xts))))
      cost_summary_list <- list()
      
      for (period_name in names(all_periods)) {
        period <- all_periods[[period_name]]; range_str <- paste0(as.character(to_Date_or_die(period$start)), "/", as.character(to_Date_or_die(period$end)))
        period_costs <- costs_xts[range_str]; period_turnover <- turnover_xts[range_str]
        
        period_deltas <- if (is_data_valid(net_trade_deltas_xts, "net_trade_deltas_xts")) net_trade_deltas_xts[range_str] else NULL
        total_individual_trades <- 0
        if(is_data_valid(period_deltas, "period_deltas")) {
          eps <- 5e-5
          risk_cols <- setdiff(colnames(period_deltas), c("CASH", "BIL"))
          total_individual_trades <- sum(abs(period_deltas[, risk_cols, drop=FALSE]) > eps, na.rm=TRUE)
        }
        
        if(!is_data_valid(period_costs, "period_costs")) { metrics <- rep(NA, 6) } else {
          time_span_years <- max(1/252, as.numeric(difftime(end(period_costs), start(period_costs), units="days")) / 365.25)
          
          trade_events_in_period <- period_costs[period_costs > 1e-8]
          total_days_with_trading <- length(trade_events_in_period)
          
          metrics <- c(
            (sum(period_turnover, na.rm=TRUE) / time_span_years),
            if(total_days_with_trading > 0) mean(period_turnover[index(trade_events_in_period)], na.rm=TRUE) else 0,
            total_days_with_trading / (time_span_years * 12),
            total_days_with_trading,
            total_individual_trades,
            (sum(period_costs, na.rm=TRUE) / time_span_years) * 10000
          )
        }
        cost_summary_list[[period_name]] <- metrics 
      }
      cost_summary_df <- as.data.frame(do.call(cbind, cost_summary_list))
      rownames(cost_summary_df) <- c("Annualized_One-Way_Turnover_(Ex-Cash)", "Avg_Turnover_Per_Trade_Day", "Avg_Trade_Days_Per_Month", "Total_Days_With_Trading_Activity", "Total_Individual_Trades", "Annualized_Costs_bps")
      
      annual_costs <- 10000 * xts::apply.yearly(costs_xts, sum, na.rm=TRUE)
      annual_turnover <- xts::apply.yearly(turnover_xts, sum, na.rm=TRUE)
      annual_costs_agg <- data.frame(Year = format(index(annual_costs), "%Y"), Cost = as.numeric(annual_costs))
      annual_turnover_agg <- data.frame(Year = format(index(annual_turnover), "%Y"), Turnover = as.numeric(annual_turnover))
      
      trade_counts_df <- if(is_data_valid(trade_log, "trade_log")) as.data.frame(table(trade_log$Reason)) else data.frame(Trade_Reason=character(), Count=integer())
      colnames(trade_counts_df) <- c("Trade_Reason", "Count_of_Trading_Days")
      
      writeData(wb, "05_Costs_and_Turnover", "Period Summary", startRow=1); writeData(wb, "05_Costs_and_Turnover", cost_summary_df, startRow=2, rowNames=TRUE)
      writeData(wb, "05_Costs_and_Turnover", "Annual Costs (bps)", startRow=NROW(cost_summary_df)+4); writeData(wb, "05_Costs_and_Turnover", annual_costs_agg, startRow=NROW(cost_summary_df)+5, rowNames=FALSE)
      writeData(wb, "05_Costs_and_Turnover", "Annual Turnover", startRow=NROW(cost_summary_df)+NROW(annual_costs_agg)+7); writeData(wb, "05_Costs_and_Turnover", annual_turnover_agg, startRow=NROW(cost_summary_df)+NROW(annual_costs_agg)+8, rowNames=FALSE)
      writeData(wb, "05_Costs_and_Turnover", "Total Trading Days by Reason", startRow=NROW(cost_summary_df)+NROW(annual_costs_agg)+NROW(annual_turnover_agg)+10); writeData(wb, "05_Costs_and_Turnover", trade_counts_df, startRow=NROW(cost_summary_df)+NROW(annual_costs_agg)+NROW(annual_turnover_agg)+11, rowNames=FALSE)
      
    } else { writeData(wb, "05_Costs_and_Turnover", "DATA ERROR: Turnover or Cost data was invalid.") } }, error = function(e) { message("Error generating Costs/Turnover: ", e$message) })
  
  message("[7/22] Generating allocation and contribution tabs...")
  aggregate_to_sleeves <- function(data_xts, class_map, name) {
    if (!is_data_valid(data_xts, name)) return(xts())
    all_assets <- colnames(data_xts)
    all_sleeves <- sort(unique(as.character(class_map[all_assets])))
    all_sleeves <- setdiff(all_sleeves, NA_character_)
    sleeve_matrix <- t(apply(data_xts, 1, function(row_data) {
      sleeve_sums <- tapply(row_data, class_map[names(row_data)], sum, na.rm=TRUE)
      out <- setNames(rep(0, length(all_sleeves)), all_sleeves); if(length(sleeve_sums) > 0) out[names(sleeve_sums)] <- sleeve_sums; out }))
    sleeve_matrix[is.na(sleeve_matrix)] <- 0; xts(sleeve_matrix, order.by = index(data_xts))
  }
  
  addWorksheet(wb, "06a_Capital_Allocation"); tryCatch({
    if(is_data_valid(positions_xts, "positions_xts")){
      class_map <- .default_class_map(data_artifact$config$master_controls$ytd_enhancements$new_asset_classes)
      sleeve_capital_xts <- aggregate_to_sleeves(positions_xts, class_map, "positions_xts") * 100
      sleeve_df <- data.frame(Date = index(sleeve_capital_xts), coredata(sleeve_capital_xts), check.names = FALSE)
      writeData(wb, "06a_Capital_Allocation", sleeve_df, rowNames = FALSE)
      stats_list <- list(); for (col_name in colnames(sleeve_df)[-1]) { col_data <- sleeve_df[[col_name]]; stats_list[[col_name]] <- c(mean(col_data, na.rm = TRUE), max(col_data, na.rm = TRUE), min(col_data, na.rm = TRUE)) }
      stats_df <- as.data.frame(do.call(cbind, stats_list)); rownames(stats_df) <- c("Average Capital (%)", "Max Capital (%)", "Min Capital (%)")
      start_row_stats <- NROW(sleeve_df) + 3; writeData(wb, "06a_Capital_Allocation", "Summary Statistics", startRow = start_row_stats); writeData(wb, "06a_Capital_Allocation", stats_df, startRow = start_row_stats + 1, rowNames = TRUE)
    } else { writeData(wb, "06a_Capital_Allocation", "DATA ERROR: Position data was invalid.") }
  }, error = function(e) { message("Error generating Capital Allocation: ", e$message) })
  
  generate_risk_summary_stats <- function(risk_df) {
    stats_list <- list()
    for (col_name in colnames(risk_df)[-1]) {
      col_data <- risk_df[[col_name]]; avg_alloc <- mean(col_data, na.rm = TRUE); max_alloc <- max(col_data, na.rm = TRUE)
      top_10_highs <- sort(unique(col_data), decreasing = TRUE)[1:min(10, length(unique(col_data)))]
      top_10_range <- range(top_10_highs, na.rm = TRUE)
      count_in_range <- sum(col_data >= top_10_range[1] & col_data <= top_10_range[2], na.rm = TRUE)
      range_label <- sprintf("%.1f%% to %.1f%%", top_10_range[1], top_10_range[2])
      stats_list[[col_name]] <- c(avg_alloc, max_alloc, range_label, count_in_range)
    }
    stats_df <- as.data.frame(do.call(cbind, stats_list)); rownames(stats_df) <- c("Average Portfolio Risk (%)", "Max Portfolio Risk (%)", "Top 10 Highs Range", "Count in Top 10 Range"); return(stats_df)
  }
  
  addWorksheet(wb, "06c_Sleeve_Risk_Daily"); tryCatch({
    if (is_data_valid(full_audit_log, "full_audit_log")) {
      full_audit_log$Date <- to_Date_or_die(full_audit_log$Date)
      sleeve_rc_sparse_df <- aggregate(Portfolio_Risk_Contribution_Pct ~ Date + Sleeve, data = full_audit_log, FUN = sum)
      sleeve_rc_wide_df <- tidyr::pivot_wider(sleeve_rc_sparse_df, names_from = "Sleeve", values_from = "Portfolio_Risk_Contribution_Pct", values_fill = 0)
      sleeve_rc_sparse_xts <- xts(sleeve_rc_wide_df[,-1], order.by = to_Date_or_die(sleeve_rc_wide_df$Date))
      daily_index <- index(portfolio_returns_xts); sleeve_rc_aligned <- merge(xts(order.by = daily_index), sleeve_rc_sparse_xts)
      sleeve_rc_daily_ff <- na.locf(sleeve_rc_aligned, na.rm = FALSE); sleeve_rc_daily_final <- na.locf(sleeve_rc_daily_ff, fromLast = TRUE, na.rm = FALSE)
      sleeve_df <- data.frame(Date = index(sleeve_rc_daily_final), coredata(sleeve_rc_daily_final), check.names = FALSE)
      writeData(wb, "06c_Sleeve_Risk_Daily", sleeve_df, rowNames = FALSE)
      stats_df <- generate_risk_summary_stats(sleeve_df); start_row_stats <- NROW(sleeve_df) + 3
      writeData(wb, "06c_Sleeve_Risk_Daily", "Summary Statistics (Contribution to Total Portfolio Risk)", startRow = start_row_stats)
      writeData(wb, "06c_Sleeve_Risk_Daily", stats_df, startRow = start_row_stats + 1, rowNames = TRUE)
    } else { writeData(wb, "06c_Sleeve_Risk_Daily", "DATA ERROR: Audit Log data was invalid.") }
  }, error = function(e) { message("Error generating Daily Sleeve Risk: ", e$message) })
  
  message("[8/22] Generating new Asset Risk Daily tab...")
  addWorksheet(wb, "06d_Asset_Risk_Daily"); tryCatch({
    if (is_data_valid(full_audit_log, "full_audit_log")) {
      full_audit_log$Date <- to_Date_or_die(full_audit_log$Date)
      asset_rc_wide_df <- tidyr::pivot_wider(full_audit_log[, c("Date", "Asset", "Portfolio_Risk_Contribution_Pct")], names_from = "Asset", values_from = "Portfolio_Risk_Contribution_Pct", values_fill = 0)
      asset_rc_sparse_xts <- xts(asset_rc_wide_df[,-1], order.by = to_Date_or_die(asset_rc_wide_df$Date))
      daily_index <- index(portfolio_returns_xts); asset_rc_aligned <- merge(xts(order.by = daily_index), asset_rc_sparse_xts)
      asset_rc_daily_ff <- na.locf(asset_rc_aligned, na.rm = FALSE); asset_rc_daily_final <- na.locf(asset_rc_daily_ff, fromLast = TRUE, na.rm = FALSE)
      asset_df <- data.frame(Date = index(asset_rc_daily_final), coredata(asset_rc_daily_final), check.names = FALSE)
      writeData(wb, "06d_Asset_Risk_Daily", asset_df, rowNames = FALSE)
      stats_df <- generate_risk_summary_stats(asset_df); start_row_stats <- NROW(asset_df) + 3
      writeData(wb, "06d_Asset_Risk_Daily", "Summary Statistics (Contribution to Total Portfolio Risk)", startRow = start_row_stats)
      writeData(wb, "06d_Asset_Risk_Daily", stats_df, startRow = start_row_stats + 1, rowNames = TRUE)
    } else { writeData(wb, "06d_Asset_Risk_Daily", "DATA ERROR: Audit Log data was invalid.") }
  }, error = function(e) { message("Error generating Daily Asset Risk: ", e$message) })
  
  message("[9/22] Generating leverage and exposure tabs...")
  addWorksheet(wb, "07_Leverage_and_Exposure"); tryCatch({
    if (is_data_valid(leverage_xts, "leverage_xts") && is_data_valid(gross_exposure_xts, "gross_exposure_xts")) {
      leverage_daily <- na.locf(leverage_xts[index(positions_xts)], na.rm = FALSE)
      gross_exposure_daily <- na.locf(gross_exposure_xts[index(positions_xts)], na.rm = FALSE)
      merged_leverage <- merge(leverage_daily, gross_exposure_daily, join='outer')
      leverage_df <- data.frame(Date=index(merged_leverage), Risk_Asset_Leverage = coredata(merged_leverage[,1]), Gross_Portfolio_Exposure = coredata(merged_leverage[,2]), check.names=FALSE)
      summary_df <- data.frame(Metric = c("Average Risk Asset Leverage", "Max Risk Asset Leverage", "Min Risk Asset Leverage", "Average Gross Exposure", "Max Gross Exposure", "Min Gross Exposure"), Value = c(mean(leverage_xts, na.rm=T), max(leverage_xts, na.rm=T), min(leverage_xts, na.rm=T), mean(gross_exposure_xts, na.rm=T), max(gross_exposure_xts, na.rm=T), min(gross_exposure_xts, na.rm=T)))
      writeData(wb, "07_Leverage_and_Exposure", "Summary Statistics", startRow = 1); writeData(wb, "07_Leverage_and_Exposure", summary_df, startRow = 2, rowNames = FALSE)
      writeData(wb, "07_Leverage_and_Exposure", "Daily Time Series", startRow = NROW(summary_df) + 4); writeData(wb, "07_Leverage_and_Exposure", leverage_df, startRow = NROW(summary_df) + 5, rowNames = FALSE) 
    } else { writeData(wb, "07_Leverage_and_Exposure", "DATA ERROR: Leverage or Exposure data was invalid.") } }, error = function(e) { message("Error generating Leverage Diagnostics: ", e$message) })
  
  message("[10/22] Generating asset-level tabs...")
  addWorksheet(wb, "08_Asset_Performance"); tryCatch({
    if(is_data_valid(asset_returns_xts, "asset_returns_xts") && is_data_valid(positions_xts, "positions_xts")) {
      asset_perf_list <- lapply(setdiff(colnames(asset_returns_xts), c("CASH", "BIL")), function(asset) {
        R_asset <- asset_returns_xts[,asset]; Rf_asset <- rf_series_xts[index(R_asset)]
        c(Asset=asset, CAGR_Pct = PerformanceAnalytics::Return.annualized(R_asset, scale=252) * 100, Volatility_Pct = PerformanceAnalytics::StdDev.annualized(R_asset, scale=252) * 100, Sharpe_Ratio = PerformanceAnalytics::SharpeRatio.annualized(R_asset, Rf=Rf_asset, scale=252), Max_Drawdown_Pct = PerformanceAnalytics::maxDrawdown(R_asset) * 100, Avg_Capital_Weight_Pct = mean(positions_xts[,asset], na.rm=TRUE) * 100)
      })
      asset_perf_df <- dplyr::bind_rows(asset_perf_list)
      writeData(wb, "08_Asset_Performance", asset_perf_df, rowNames = FALSE)
    } else { writeData(wb, "08_Asset_Performance", "DATA ERROR: Invalid asset returns or positions.")}
  }, error = function(e) { message("Error generating Asset Performance: ", e$message) })
  
  message("[11/22] Performing advanced forecast diagnostics...")
  addWorksheet(wb, "09_Volatility_Diagnostics"); tryCatch({
    if (is_data_valid(ex_ante_vol_xts, "ex_ante_vol_xts")) {
      diag_vol <- data_artifact$results$volatility_diagnostics_xts
      if (is_data_valid(diag_vol, "volatility_diagnostics_xts")) {
        mz_reg_vol <- lm(Realized ~ Forecasted, data = diag_vol); reg_summary <- summary(mz_reg_vol); mz_df <- as.data.frame(reg_summary$coefficients); bias_test <- NA; if(NROW(diag_vol) > 2) bias_test <- suppressWarnings(stats::t.test(diag_vol$Error))
        mz_summary_df <- data.frame(Metric = c("M-Z Intercept (alpha)", "M-Z Slope (beta)", "M-Z R-Squared", "RMSE", "QLIKE", "Bias t-statistic", "Bias Test p-value"), Value = c(mz_df[1,1], mz_df[2,1], reg_summary$r.squared, sqrt(mean(diag_vol$Error^2, na.rm=TRUE)), mean(diag_vol$Realized^2 / diag_vol$Forecasted^2 - log(diag_vol$Realized^2 / diag_vol$Forecasted^2) - 1, na.rm = TRUE), if(is.list(bias_test)) bias_test$statistic else NA, if(is.list(bias_test)) bias_test$p.value else NA), `P-Value` = c(mz_df[1,4], mz_df[2,4], NA, NA, NA, NA, if(is.list(bias_test)) bias_test$p.value else NA), check.names = FALSE)
        writeData(wb, "09_Volatility_Diagnostics", "Time Series", startRow=1); writeData(wb, "09_Volatility_Diagnostics", data.frame(Date=index(diag_vol), coredata(diag_vol)), startRow=2, rowNames=FALSE)
        writeData(wb, "09_Volatility_Diagnostics", "Summary Statistics", startRow=NROW(diag_vol)+4); writeData(wb, "09_Volatility_Diagnostics", mz_summary_df, startRow=NROW(diag_vol)+5, rowNames=FALSE)
      } else { writeData(wb, "09_Volatility_Diagnostics", "DATA ERROR: Volatility diagnostics failed or were skipped due to insufficient data.")}
    } else { writeData(wb, "09_Volatility_Diagnostics", "DATA ERROR: Volatility Forecast data invalid.") } }, error = function(e) { message("Error generating Volatility Diagnostics: ", e$message) })
  
  message("[12/22] Generating correlation tabs...")
  addWorksheet(wb, "10_Portfolio_Correlation"); tryCatch({
    if(is_data_valid(avg_corr_xts, "avg_corr_xts")) {
      annual_corr <- xts::apply.yearly(avg_corr_xts, colMeans, na.rm=TRUE); annual_corr_df <- data.frame(Year=year(index(annual_corr)), `Avg_Portfolio_Correlation`=coredata(annual_corr))
      corr_summary_df <- data.frame(Metric = c("Mean", "Median", "StdDev", "Min", "Max"), Value = c(mean(avg_corr_xts, na.rm=TRUE), median(avg_corr_xts, na.rm=TRUE), sd(avg_corr_xts, na.rm=TRUE), min(avg_corr_xts, na.rm=TRUE), max(avg_corr_xts, na.rm=TRUE)))
      writeData(wb, "10_Portfolio_Correlation", "Summary Statistics", startRow=1); writeData(wb, "10_Portfolio_Correlation", corr_summary_df, startRow=2, rowNames=FALSE)
      writeData(wb, "10_Portfolio_Correlation", "Annual Summary", startRow=NROW(corr_summary_df)+4); writeData(wb, "10_Portfolio_Correlation", annual_corr_df, startRow=NROW(corr_summary_df)+5, rowNames=FALSE)
    } else { writeData(wb, "10_Portfolio_Correlation", "DATA ERROR: Average Correlation data was invalid.") }
  }, error = function(e) { message("Error generating Portfolio Correlation: ", e$message) })
  
  message("[13/22] Generating sleeve correlation tab...")
  addWorksheet(wb, "11_Sleeve_Return_Correlations"); tryCatch({
    if (is_data_valid(positions_xts, "positions_xts") && is_data_valid(asset_returns_xts, "asset_returns_xts")) {
      class_map <- .default_class_map(data_artifact$config$master_controls$ytd_enhancements$new_asset_classes)
      sleeves <- split(setdiff(colnames(asset_returns_xts), c("BIL","CASH")),
                       class_map[setdiff(colnames(asset_returns_xts), c("BIL","CASH"))])
      
      sleeve_ret_list <- lapply(names(sleeves), function(slv) {
        assets <- sleeves[[slv]]
        if (length(assets) == 0) return(NULL)
        xr <- positions_xts[, assets, drop=FALSE] * asset_returns_xts[, assets, drop=FALSE]
        sr <- xts(rowSums(xr, na.rm=TRUE), order.by = index(xr))
        colnames(sr) <- slv
        sr
      })
      sleeve_returns <- do.call(merge, sleeve_ret_list[!sapply(sleeve_ret_list, is.null)])
      if (xts::is.xts(sleeve_returns)) {
        keep <- apply(sleeve_returns, 2, function(x) sd(x, na.rm=TRUE) > 1e-9)
        sleeve_returns <- sleeve_returns[, keep, drop = FALSE]
      }
      
      if (is_data_valid(sleeve_returns, "sleeve_returns") && NCOL(sleeve_returns) > 1) {
        sleeve_corr_matrix <- cor(sleeve_returns, use="pairwise.complete.obs")
        writeData(wb, "11_Sleeve_Return_Correlations", "Realized Sleeve Return Correlation Matrix (Full Period)", startRow = 1)
        writeData(wb, "11_Sleeve_Return_Correlations", sleeve_corr_matrix, startRow = 2, rowNames = TRUE)
      } else {
        writeData(wb, "11_Sleeve_Return_Correlations", "Could not calculate sleeve correlations (insufficient data or variance).")
      }
    } else { writeData(wb, "11_Sleeve_Return_Correlations", "Could not calculate sleeve correlations.")}
  }, error = function(e) { message("Error generating Sleeve Correlations: ", e$message) })
  
  message("[14/22] Generating asset correlation tab...")
  addWorksheet(wb, "12_Asset_Return_Correlations"); tryCatch({
    if(is_data_valid(asset_returns_xts, "asset_returns_xts")) {
      risk_asset_returns <- asset_returns_xts[, setdiff(colnames(asset_returns_xts), c("CASH","BIL"))]
      asset_corr_matrix <- cor(risk_asset_returns, use="pairwise.complete.obs")
      writeData(wb, "12_Asset_Return_Correlations", "Realized Asset Return Correlation Matrix (Full Period)", startRow = 1)
      writeData(wb, "12_Asset_Return_Correlations", asset_corr_matrix, startRow = 2, rowNames = TRUE)
    } else { writeData(wb, "12_Asset_Return_Correlations", "DATA ERROR: Invalid asset returns.")}
  }, error = function(e) { message("Error generating Asset Correlations: ", e$message) })
  
  message("[15/22] Generating Audit tab...")
  addWorksheet(wb, "13_Audit_and_Diagnostics"); tryCatch({
    if (is_data_valid(full_audit_log, "full_audit_log") && "Risk_Error_Pct" %in% names(full_audit_log)) {
      full_audit_log$Date <- to_Date_or_die(full_audit_log$Date)
      sorted_audit_log <- full_audit_log[order(-abs(full_audit_log$Risk_Error_Pct)), ]
      log_by_date <- split(sorted_audit_log, sorted_audit_log$Date)
      sleeve_sd_by_date <- sapply(log_by_date, function(daily_log) {
        sleeve_sums <- tapply(daily_log$Portfolio_Risk_Contribution_Pct, daily_log$Sleeve, sum, na.rm = TRUE)
        if (length(sleeve_sums) > 1) sd(sleeve_sums, na.rm = TRUE) else 0
      })
      risk_error <- sorted_audit_log$Risk_Error_Pct[!is.na(sorted_audit_log$Risk_Error_Pct)]
      deviation_summary <- data.frame(Metric = c("Max Absolute Deviation (%)", "Average Absolute Deviation (%)", "Avg. StDev of Sleeve Risk Contrib (%)"), Value = c(max(abs(risk_error), na.rm = TRUE), mean(abs(risk_error), na.rm = TRUE), mean(sleeve_sd_by_date, na.rm=TRUE)))
      writeData(wb, "13_Audit_and_Diagnostics", "Risk Parity Deviation Summary", startRow = 1)
      writeData(wb, "13_Audit_and_Diagnostics", deviation_summary, startRow = 2, rowNames = FALSE)
      writeData(wb, "13_Audit_and_Diagnostics", "Full Rebalance Audit Log (Sorted by Deviation)", startRow = NROW(deviation_summary) + 4)
      writeData(wb, "13_Audit_and_Diagnostics", sorted_audit_log, startRow = NROW(deviation_summary) + 5, rowNames = FALSE)
    } else { writeData(wb, "13_Audit_and_Diagnostics", "No audit data was generated for this run.") }
  }, error = function(e) { message("Error generating Audit Log: ", e$message) })
  
  message("[16/22] Generating VIX regime analysis...")
  addWorksheet(wb, "14_VIX_Regime_Analysis"); tryCatch({
    if(!is.null(vix_series)) {
      vix_aligned <- vix_series[index(portfolio_returns_xts)]; regimes <- cut(vix_aligned, breaks=c(0, 15, 25, Inf), labels=c("Low (VIX < 15)", "Normal (VIX 15-25)", "High (VIX > 25)"), right=FALSE); regimes_xts <- xts(regimes, order.by=index(vix_aligned))
      regime_perf_list <- lapply(levels(regimes), function(reg) {
        Rp_regime <- portfolio_returns_xts[regimes_xts == reg]; if(is.null(Rp_regime) || NROW(Rp_regime) < 20) return(NULL); Rf_regime <- rf_series_xts[index(Rp_regime)]
        c(CAGR_Pct = PerformanceAnalytics::Return.annualized(Rp_regime, scale=252) * 100, Volatility_Pct = PerformanceAnalytics::StdDev.annualized(Rp_regime, scale=252) * 100, Sharpe_Ratio = PerformanceAnalytics::SharpeRatio.annualized(Rp_regime, Rf=Rf_regime, scale=252), Max_Drawdown_Pct = PerformanceAnalytics::maxDrawdown(Rp_regime) * 100, Num_Days = NROW(Rp_regime))
      })
      regime_perf_df <- dplyr::bind_rows(regime_perf_list, .id = "VIX_Regime")
      writeData(wb, "14_VIX_Regime_Analysis", "Strategy Performance by VIX Regime", rowNames=FALSE)
      writeData(wb, "14_VIX_Regime_Analysis", regime_perf_df, startRow = 2, rowNames=FALSE)
      
      if (NROW(stop_loss_log) > 0) {
        stop_loss_log$Date <- to_Date_or_die(stop_loss_log$Date)
        sl_vix_base <- xts(order.by=stop_loss_log$Date)
        sl_vix_merged <- merge(sl_vix_base, vix_series, join='left')
        sl_vix <- na.locf(sl_vix_merged, na.rm=FALSE)
        
        sl_regimes <- cut(sl_vix$VIX, breaks=c(0, 15, 25, Inf), labels=c("Low (VIX < 15)", "Normal (VIX 15-25)", "High (VIX > 25)"), right=FALSE)
        sl_regime_counts <- as.data.frame(table(sl_regimes)); colnames(sl_regime_counts) <- c("VIX_Regime", "Stop_Loss_Count")
        writeData(wb, "14_VIX_Regime_Analysis", "Stop-Loss Events by VIX Regime", startRow = NROW(regime_perf_df) + 4)
        writeData(wb, "14_VIX_Regime_Analysis", sl_regime_counts, startRow = NROW(regime_perf_df) + 5, rowNames = FALSE)
      }
    } else { writeData(wb, "14_VIX_Regime_Analysis", "VIX data was not available for this run.") } }, error = function(e) { message("Error generating VIX Regime Analysis: ", e$message) })
  
  message("[17/22] Generating stop-loss diagnostics...")
  addWorksheet(wb, "15_StopLoss_Diagnostics"); tryCatch({
    if (is.null(stop_loss_log) || NROW(stop_loss_log) == 0) { writeData(wb, "15_StopLoss_Diagnostics", "No stop-loss events were triggered during this backtest run.")
    } else {
      stop_loss_log$Date <- to_Date_or_die(stop_loss_log$Date)
      total_stops <- NROW(stop_loss_log); total_years <- as.numeric(difftime(end(portfolio_returns_xts), start(portfolio_returns_xts), units="days")) / 365.25
      sl_summary_df <- data.frame(Metric = c("Total Stops Triggered", "Stops per Year"), Value = c(total_stops, total_stops / total_years))
      stops_by_asset <- as.data.frame(table(stop_loss_log$Asset)); colnames(stops_by_asset) <- c("Asset", "Stop_Count"); stops_by_asset <- stops_by_asset[order(-stops_by_asset$Stop_Count),]
      writeData(wb, "15_StopLoss_Diagnostics", "Stop-Loss Summary", startRow = 1); writeData(wb, "15_StopLoss_Diagnostics", sl_summary_df, startRow = 2, rowNames = FALSE)
      writeData(wb, "15_StopLoss_Diagnostics", "Stops by Asset", startRow = NROW(sl_summary_df) + 4); writeData(wb, "15_StopLoss_Diagnostics", stops_by_asset, startRow = NROW(sl_summary_df) + 5, rowNames = FALSE)
      writeData(wb, "15_StopLoss_Diagnostics", "Full Stop-Loss Event Log", startRow = NROW(sl_summary_df) + NROW(stops_by_asset) + 7); writeData(wb, "15_StopLoss_Diagnostics", stop_loss_log, startRow = NROW(sl_summary_df) + NROW(stops_by_asset) + 8, rowNames = FALSE)
    } }, error = function(e) { message("Error generating Stop-Loss Diagnostics: ", e$message) })
  
  message("[18/22] Generating Parity Override Log...")
  addWorksheet(wb, "16_Parity_Overrides"); tryCatch({
    if (is.null(constraint_log) || NROW(constraint_log) == 0) {
      writeData(wb, "16_Parity_Overrides", "No optimizer fallback or de-risking events were triggered during this backtest run.")
    } else {
      constraint_log$Date <- to_Date_or_die(constraint_log$Date)
      writeData(wb, "16_Parity_Overrides", "Rebalance Constraint Log", startRow = 1)
      writeData(wb, "16_Parity_Overrides", constraint_log, startRow = 2, rowNames = FALSE)
    }
  }, error = function(e) { message("Error generating Parity Overrides Log: ", e$message) })
  
  message("[19/22] Generating High VIX Analysis Log...")
  addWorksheet(wb, "17_High_VIX_Analysis"); tryCatch({
    if (is.null(high_vix_event_log) || NROW(high_vix_event_log) == 0) {
      writeData(wb, "17_High_VIX_Analysis", "No High-VIX Layoff events were triggered during this backtest run.")
    } else {
      high_vix_event_log$Date <- to_Date_or_die(high_vix_event_log$Date)
      total_impact <- if ("Performance_Impact_bps" %in% names(high_vix_event_log)) sum(high_vix_event_log$Performance_Impact_bps, na.rm=TRUE) else NA
      summary_df <- data.frame(Metric=c("Total VIX Layoff Events", "Total Performance Impact (bps)"), Value=c(NROW(high_vix_event_log), total_impact))
      writeData(wb, "17_High_VIX_Analysis", "High-VIX Layoff Event Summary", startRow = 1)
      writeData(wb, "17_High_VIX_Analysis", summary_df, startRow = 2, rowNames = FALSE)
      writeData(wb, "17_High_VIX_Analysis", "Full Event Log", startRow = NROW(summary_df) + 4)
      writeData(wb, "17_High_VIX_Analysis", high_vix_event_log, startRow = NROW(summary_df) + 5, rowNames = FALSE)
    }
  }, error = function(e) { message("Error generating High VIX Analysis: ", e$message) })
  
  message("[20/22] Generating YTD Deep Dive Tab...")
  addWorksheet(wb, "18_YTD_Deep_Dive"); tryCatch({
    ytd_period <- analysis_periods$`YTD 2025`
    
    guide_text <- data.frame(Step=1:6, Action=c(
      "Identify Trade Day", "Calculate Weight Before Trade (Drifted Weight)", "Calculate Trade Delta",
      "Calculate Daily Costs", "Calculate Daily Gross Return", "Calculate Daily Net Return"
    ), Description=c(
      "Find a row where 'Is_Rebalance' = 1 or 'Is_StopLoss' = 1.",
      "For a trade on Date D, the 'Weight_Before_Trade' for an asset is its '_EOD_W' from date D-1.",
      "For each asset: Trade_Delta = Target_Weight - Weight_Before_Trade.",
      "For each asset: Cost = ABS(Trade_Delta) * Cost_bps / 10000. Sum costs for all assets to get total daily cost.",
      "On any day D, Gross Return = SUMPRODUCT(Asset_EOD_W from D-1, Asset_RET from D).",
      "Net Return = Gross Return - Total Daily Cost."
    ))
    writeData(wb, "18_YTD_Deep_Dive", "YTD Manual Validation Guide", startRow=1)
    writeData(wb, "18_YTD_Deep_Dive", guide_text, startRow=2, rowNames=FALSE)
    start_row_data <- NROW(guide_text) + 3
    
    if(is.null(ytd_period) || !ytd_period$enabled) {
      writeData(wb, "18_YTD_Deep_Dive", "YTD 2025 period is not enabled. No data to display.", startRow=start_row_data)
    } else {
      range_str <- paste0(as.character(to_Date_or_die(ytd_period$start)), "/", as.character(to_Date_or_die(ytd_period$end)))
      
      if (NROW(results$prices_xts[range_str]) < 2) {
        writeData(wb, "18_YTD_Deep_Dive", "Insufficient data for the YTD 2025 period.", startRow=start_row_data)
      } else {
        prices_ytd <- results$prices_xts[range_str]
        returns_ytd <- results$asset_returns_xts[range_str]
        vix_ytd <- results$vix_series[range_str]
        trend_signals_ytd <- results$trend_signals[range_str]
        positions_ytd <- results$positions_xts[range_str]
        
        price_df <- data.frame(Date=index(prices_ytd), coredata(prices_ytd)); colnames(price_df) <- c("Date", paste0(colnames(prices_ytd), "_PX"))
        returns_df <- data.frame(Date=index(returns_ytd), coredata(returns_ytd)); colnames(returns_df) <- c("Date", paste0(colnames(returns_ytd), "_RET"))
        vix_df <- data.frame(Date=index(vix_ytd), VIX_Level = coredata(vix_ytd$VIX), VIX_Regime = ifelse(vix_ytd$VIX > config$master_controls$friction$vix_threshold, "High", "Normal"))
        
        trend_df <- NULL
        if (!is.null(trend_signals_ytd) && ncol(trend_signals_ytd) > 0) {
          trend_df <- data.frame(Date=index(trend_signals_ytd), coredata(trend_signals_ytd)); colnames(trend_df) <- c("Date", paste0(colnames(trend_signals_ytd), "_Trend"))
        }
        
        trade_df_list <- lapply(results$raw_trade_log, function(trade) {
          trade_date <- to_Date_or_die(trade$date)
          if (trade_date >= to_Date_or_die(ytd_period$start) && trade_date <= to_Date_or_die(ytd_period$end)) {
            w_after_vec <- trade$w_after; deltas_vec <- as.numeric(trade$deltas)
            target_w <- as.data.frame(as.list(w_after_vec)); colnames(target_w) <- paste0(names(w_after_vec), "_Target_W")
            trade_deltas <- as.data.frame(as.list(deltas_vec)); colnames(trade_deltas) <- paste0(names(w_after_vec), "_Trade_Delta")
            trade_info <- data.frame(Date = trade_date, Is_Rebalance = as.integer(trade$reason == "rebalance"), Is_StopLoss = as.integer(trade$reason == "stop-loss"))
            return(cbind(trade_info, target_w, trade_deltas))
          }
          return(NULL)
        })
        compact_trade_list <- trade_df_list[!sapply(trade_df_list, is.null)]
        trade_df <- if(length(compact_trade_list) > 0) dplyr::bind_rows(compact_trade_list) else NULL
        
        base_df <- data.frame(Date=index(prices_ytd))
        all_dfs <- list(base_df, price_df, returns_df, vix_df)
        if(!is.null(trend_df)) all_dfs <- append(all_dfs, list(trend_df))
        
        final_df <- Reduce(function(x, y) merge(x, y, by="Date", all.x=TRUE), all_dfs)
        
        if(!is.null(trade_df)) {
          final_df <- merge(final_df, trade_df, by="Date", all.x=TRUE)
          final_df$Is_Rebalance[is.na(final_df$Is_Rebalance)] <- 0
          final_df$Is_StopLoss[is.na(final_df$Is_StopLoss)] <- 0
        } else {
          final_df$Is_Rebalance <- 0
          final_df$Is_StopLoss <- 0
        }
        
        pos_df <- data.frame(Date=index(positions_ytd), coredata(positions_ytd))
        colnames(pos_df) <- c("Date", paste0(colnames(positions_ytd), "_EOD_W"))
        final_df <- merge(final_df, pos_df, by="Date", all.x=TRUE)
        
        writeData(wb, "18_YTD_Deep_Dive", "YTD Daily Data", startRow=start_row_data)
        writeData(wb, "18_YTD_Deep_Dive", final_df, startRow=start_row_data + 1, rowNames=FALSE)
      }
    }
  }, error = function(e) { 
    msg <- paste("Error generating YTD Deep Dive tab:", e$message)
    message(msg)
    try(writeData(wb, "18_YTD_Deep_Dive", msg, startRow=NROW(guide_text) + 3), silent=TRUE)
  })
  
  message("[21/22] Generating Period Diagnostics tab...")
  tryCatch({
    summary_table_for_diag <- dplyr::bind_rows(lapply(enabled_periods, as.data.frame.list), .id="Period")
    colnames(summary_table_for_diag) <- c("Period", "Start_Date", "End_Date", "Enabled")
    write_period_diagnostics(wb,
                             daily_net_xts = results$portfolio_returns_xts,
                             full_start = analysis_start_date,
                             full_end = end(results$portfolio_returns_xts),
                             periods_df = summary_table_for_diag)
  }, error = function(e) { message("Error generating Period Diagnostics tab: ", e$message) })
  
  
  message("[22/22] Saving Excel file...")
  excel_file_path <- file.path(output_excel_dir, sprintf("RiskParity_Assessment_%s.xlsx", run_id))
  
  try({
    for (s in names(wb)) {
      setColWidths(wb, sheet = s, cols = 1:200, widths = "auto")
    }
  }, silent = TRUE)
  
  saveWorkbook(wb, excel_file_path, overwrite = TRUE); message("\nExcel report saved: ", excel_file_path)
  
  message("\n===================================================================")
  message("Assessment Pack Generation COMPLETE.")
  message("===================================================================\n")
  invisible(TRUE)
}

# ============================ SCRIPT ORCHESTRATION (v160.9.9) ===================================

ANALYSIS_PERIODS <- list(
  `Pre-Crisis OOS` = list(start = "2008-01-01", end = "2008-12-31", enabled = TRUE),
  `Training`       = list(start = "2009-01-01", end = "2018-12-31", enabled = TRUE),
  `Validation`     = list(start = "2016-01-01", end = "2016-12-31", enabled = FALSE),
  `Main OOS`       = list(start = "2019-01-01", end = "2024-12-31", enabled = TRUE),
  `YTD 2025`       = list(start = "2025-01-01", end = "2025-11-19", enabled = TRUE)
)
FULL_BACKTEST_START <- "2007-01-01"
FULL_BACKTEST_END   <- "2025-11-19"

# --- FIX: Normalize all period definitions at the source ---
ANALYSIS_PERIODS <- lapply(ANALYSIS_PERIODS, function(p) {
  p$start <- to_Date_or_die(p$start, "ANALYSIS_PERIODS start")
  p$end   <- to_Date_or_die(p$end,   "ANALYSIS_PERIODS end")
  p
})
FULL_BACKTEST_START <- to_Date_or_die(FULL_BACKTEST_START)
FULL_BACKTEST_END   <- to_Date_or_die(FULL_BACKTEST_END)
# --- END FIX ---

MASTER_CONTROLS <- list(
  debug_mode = FALSE, 
  vol_target = 0.2,
  rebalance_on = "weeks",
  rebalance_day = "Friday",
  rebalance_signal_day = "same_day", # Options: "prior_day", "same_day"
  lookback = 252,
  min_obs_frac = 2/3,
  returns_method = "discrete",
  allow_leverage = TRUE, 
  gross_cap = 6.25,
  ytd_enhancements = list(
    enabled = TRUE,
    new_assets = c("IBIT", "ETHE"),
    new_asset_classes = c(IBIT="Crypto", ETHE="Crypto"),
    activation_date = "2025-01-01"
  ),
  sleeve_derisking_rules = list(
    enabled = TRUE,
    rules = c('1' = 0.13, '2' = 0.28, '3' = 0.30, '4' = 0.80) 
  ),
  high_vix_layoff = list(
    enabled = TRUE,
    vix_threshold = 40,
    layoff_factor = 0.5
  ),
  trend_filter = list(
    enabled = TRUE,
    strategy = "crossover",
    ma_type = "SMA",
    lookback = 150,
    short_ma_lookback = 100,
    long_ma_lookback = 200,
    floor = 0,
    filter_on_classes = c("Equity", "HY", "Commodities", "REITs","TIPS","IG","Rates", "Crypto")
  ),
  stop_loss = list(
    enabled = TRUE,
    thresholds_by_class = list(
      Equity = 0.06, HY = 0.02, Commodities = 0.06, default = 0.04
    )
  ),
  friction = list(
    bps_by_symbol = c(SPY=1, QQQ=5, IWM=6, EFA=6, EEM=1, LQD=8, HYG=8, IEF=5, TLT=5, TIP=5, VNQ=6, DBC=4, GLD=1, BIL=1, CASH=0, IBIT=1, ETHE=1),
    default_bps = 5,
    vix_threshold = 35,
    stress_multiplier = 2
  ),
  cov_method = "rb_blend",
  cov_lambda = 0.94
)

canonicalize_prices <- function(bp, name) {
  if (is.null(bp) || (!is.xts(bp) && !is.data.frame(bp) && !is.matrix(bp))) {
    stop(name, " must be xts, data.frame, or matrix; got ", class(bp))
  }
  
  looks_like_ticker <- function(v) if(is.null(v)) FALSE else all(grepl("^[A-Z.]{1,8}$", utils::head(v[!is.na(v)], 10)))
  looks_like_date_char <- function(v) if(is.null(v)) FALSE else any(grepl("^\\d{4}-\\d{2}-\\d{2}", utils::head(v[!is.na(v)], 10)))
  
  if (!xts::is.xts(bp)) {
    rn <- rownames(bp); cn <- colnames(bp)
    if (looks_like_ticker(rn) && looks_like_date_char(cn)) {
      message("[CANONICALIZE/", name, "] Data is transposed (tickers-as-rows). Re-orienting...")
      dates <- to_Date_or_die(cn, hint = paste(name, "colnames"))
      mat <- as.matrix(bp)
      bp <- xts::xts(t(mat), order.by = dates)
      colnames(bp) <- rn
    } else if (looks_like_date_char(rn)) {
      message("[CANONICALIZE/", name, "] Data is matrix with dates-as-rownames. Converting to xts...")
      dates <- to_Date_or_die(rn, hint = paste(name, "rownames"))
      bp <- xts::xts(as.matrix(bp), order.by = dates)
    } else {
      stop(name, " is not xts and has ambiguous orientation.")
    }
  }
  
  bp <- ensure_date_index(bp)
  bp <- bp[order(index(bp)), ]
  bp <- bp[!duplicated(index(bp)), ]
  bp <- bp[, colSums(is.na(bp)) < NROW(bp) * 0.5, drop=FALSE]
  
  assert_time_index(bp, paste0(name, " (canonicalized)"))
  return(bp)
}

base_tickers <- c("SPY", "EEM", "HYG", "GLD", "DBC", "VNQ", "TIP", "BIL", "IEF", "LQD")
run_id <- paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), "_", substr(digest::digest(list(MASTER_CONTROLS, ANALYSIS_PERIODS)), 1, 6))
message("\n==================================================================="); message("Starting Backtest Run. ID: ", run_id); message("===================================================================\n")

message("[RUN] Starting BASE backtest on long-history assets...")
base_prices_raw <- bbg_get_history_xts(base_tickers, start_date = FULL_BACKTEST_START, end_date = FULL_BACKTEST_END)
base_prices <- canonicalize_prices(base_prices_raw, "base_prices")

base <- risk_parity_backtest(prices = base_prices, master_controls = MASTER_CONTROLS)

results <- base
ytd_cfg <- MASTER_CONTROLS$ytd_enhancements
if (isTRUE(ytd_cfg$enabled)) {
  message("\n[RUN] YTD Enhancements enabled. Starting short-history backtest...")
  
  activation_date <- to_Date_or_die(ytd_cfg$activation_date)
  ytd_run_start_date <- activation_date - (MASTER_CONTROLS$lookback + 50)
  
  enhanced_tickers <- unique(c(base_tickers, ytd_cfg$new_assets))
  
  ytd_prices_raw <- bbg_get_history_xts(enhanced_tickers, start_date = ytd_run_start_date, end_date = FULL_BACKTEST_END)
  ytd_prices <- canonicalize_prices(ytd_prices_raw, "ytd_prices")
  
  enh <- risk_parity_backtest(prices = ytd_prices, master_controls = MASTER_CONTROLS)
  
  cat(sprintf("[SPLICE] Merging enhanced results into base results at activation date: %s\n", activation_date))
  
  assert_time_index(base$portfolio_returns_xts, "base_portfolio_returns_xts")
  assert_time_index(enh$portfolio_returns_xts, "enh_portfolio_returns_xts")
  
  base$portfolio_returns_xts <- splice_xts_series(base$portfolio_returns_xts, enh$portfolio_returns_xts, activation_date, "Net_Portfolio_Return")
  if (xts::is.xts(base$portfolio_returns_xts_gross) && xts::is.xts(enh$portfolio_returns_xts_gross)) {
    base$portfolio_returns_xts_gross <- splice_xts_series(base$portfolio_returns_xts_gross, enh$portfolio_returns_xts_gross, activation_date, "Gross_Portfolio_Return")
  }
  if (xts::is.xts(base$costs_xts) && xts::is.xts(enh$costs_xts)) {
    base$costs_xts <- splice_xts_series(base$costs_xts, enh$costs_xts, activation_date, "Costs")
  }
  if (xts::is.xts(base$turnover_xts) && xts::is.xts(enh$turnover_xts)) {
    base$turnover_xts <- splice_xts_series(base$turnover_xts, enh$turnover_xts, activation_date, "Turnover")
  }
  
  base$asset_returns_xts <- splice_positions(base$asset_returns_xts, enh$asset_returns_xts, activation_date, fill_missing = 0)
  base$positions_xts <- splice_positions(base$positions_xts, enh$positions_xts, activation_date, fill_missing = 0)
  
  for (nm in c("leverage_xts", "gross_exposure_xts", "avg_corr_xts", "ex_ante_vol_xts")) {
    if (xts::is.xts(base[[nm]]) && xts::is.xts(enh[[nm]])) {
      base[[nm]] <- splice_xts_series(base[[nm]], enh[[nm]], activation_date, label = nm)
    }
  }
  
  for (nm in c("full_audit_log", "constraint_log", "stop_loss_log", "high_vix_event_log", "trade_log")) {
    if (is.data.frame(base[[nm]]) && is.data.frame(enh[[nm]])) {
      base[[nm]] <- splice_audit_df(base[[nm]], enh[[nm]], activation_date)
    }
  }
  
  if (xts::is.xts(base$net_trade_deltas_xts) && xts::is.xts(enh$net_trade_deltas_xts)) {
    base$net_trade_deltas_xts <- splice_positions(base$net_trade_deltas_xts, enh$net_trade_deltas_xts, activation_date, fill_missing = 0)
  }
  results <- base
}

# Post-splicing sanity checks
assert_time_index(results$portfolio_returns_xts, "final_portfolio_returns_xts")
assert_time_index(results$positions_xts, "final_positions_xts")
stopifnot(identical(index(results$portfolio_returns_xts), index(results$positions_xts)))
w_sums <- rowSums(results$positions_xts, na.rm = TRUE)
if (any(abs(w_sums - 1) > 1e-6)) warning(sprintf("Weight sum deviates up to %.3e", max(abs(w_sums - 1))))
if (xts::is.xts(results$portfolio_returns_xts_gross) && xts::is.xts(results$costs_xts)) {
  wedge <- sum(results$portfolio_returns_xts_gross - results$portfolio_returns_xts, na.rm = TRUE)
  costs <- sum(results$costs_xts, na.rm = TRUE)
  if (abs(10000 * (wedge - costs)) > 0.5) warning(sprintf("[CHECK] Gross–Net wedge (bps) %.3f vs costs (bps) %.3f", 1e4*wedge, 1e4*costs))
}


message("\n[RUN] Finalizing results and generating reports...")
last_trade <- if (length(results$raw_trade_log) > 0) last(results$raw_trade_log) else NULL
last_weights <- if (!is.null(last_trade)) make_row_xts_strict(last_trade$w_after, colnames(results$positions_xts), last_trade$date, name="last_weights") else NULL

forward_tickers <- if(isTRUE(ytd_cfg$enabled) && Sys.Date() >= to_Date_or_die(ytd_cfg$activation_date)) {
  unique(c(base_tickers, ytd_cfg$new_assets))
} else {
  base_tickers
}
forward_prices_raw <- bbg_get_history_xts(forward_tickers, start_date = to_Date_or_die(FULL_BACKTEST_END) - (MASTER_CONTROLS$lookback + 50), end_date = FULL_BACKTEST_END)
forward_prices <- canonicalize_prices(forward_prices_raw, "forward_prices")

R_fwd <- ts_returns(forward_prices, method = MASTER_CONTROLS$returns_method)
if (!"CASH" %in% colnames(R_fwd)) R_fwd$CASH <- 0
if (!"BIL" %in% colnames(R_fwd)) R_fwd$BIL <- 0


use_prev_fwd <- identical(MASTER_CONTROLS$rebalance_signal_day, "prior_day")
latest_rebal_idx <- if(use_prev_fwd) NROW(R_fwd) - 1 else NROW(R_fwd)
if (latest_rebal_idx <= 0) latest_rebal_idx <- NROW(R_fwd)


vix_series_for_fwd <- get_vix_aligned(index(forward_prices), start(forward_prices), end(forward_prices))
trend_assets_for_fwd <- intersect(colnames(forward_prices), names(.default_class_map(ytd_cfg$new_asset_classes)))
trend_signals_for_fwd <- if(isTRUE(MASTER_CONTROLS$trend_filter$enabled)) build_trend_mask_xts(forward_prices[, trend_assets_for_fwd, drop=FALSE], MASTER_CONTROLS$trend_filter) else xts()

forward_weights_result <- calculate_target_weights(forward_prices, R_fwd, latest_rebal_idx, last_weights, MASTER_CONTROLS, vix_series_for_fwd, trend_signals_for_fwd)

rf_series <- get_risk_free_xts(index(results$prices_xts), start(results$prices_xts), end(results$prices_xts))
results$rf_series_xts <- rf_series[index(results$portfolio_returns_xts)]
results$volatility_diagnostics_xts <- calculate_volatility_diagnostics(results$ex_ante_vol_xts, results$portfolio_returns_xts)

data_artifact <- list(run_id = run_id, timestamp_utc = "2025-11-10 13:07:50", user = "balint27",
                      config = list(master_controls=MASTER_CONTROLS, analysis_periods=ANALYSIS_PERIODS), 
                      results = results,
                      forward_weights = forward_weights_result)

rpqs_results <- calculate_rpqs_granular(data_artifact, ANALYSIS_PERIODS)
data_artifact$rpqs_results <- rpqs_results

artifact_path <- file.path(ARTIFACTS_DIR, sprintf("DataArtifact_%s.rds", run_id))
saveRDS(data_artifact, file = artifact_path)
message("\n[SUCCESS] Engine run complete. Data artifact saved to: ", artifact_path)

print_console_summary(data_artifact, ANALYSIS_PERIODS)
generate_professional_assessment_pack(data_artifact, ANALYSIS_PERIODS)

message("\n===================================================================")
message("Actionable Portfolio Signal for Next Period")
message("===================================================================\n")
if (!is.null(forward_weights_result) && !is.null(forward_weights_result$target_weights)) {
  fw_df <- as.data.frame(t(coredata(forward_weights_result$target_weights))) * 100
  colnames(fw_df) <- "Target_Weight_Pct"
  fw_df <- fw_df[order(-fw_df$Target_Weight_Pct), , drop = FALSE]
  
  cat("Signal Date:", as.character(index(forward_weights_result$target_weights)-1), "\n")
  cat("Target Portfolio for period starting:", as.character(index(forward_weights_result$target_weights)), "\n\n")
  
  fw_to_print <- fw_df[fw_df$Target_Weight_Pct != 0, , drop=FALSE]
  if (nrow(fw_to_print) > 0) {
    cat(sprintf("%-10s %s\n", "Asset", "Target_Weight_Pct"))
    for (i in 1:nrow(fw_to_print)) {
      cat(sprintf("%-10s %.4f\n", rownames(fw_to_print)[i], fw_to_print[i, 1]))
    }
  } else {
    cat("All target weights are zero.\n")
  }
  
  if (!is.null(forward_weights_result$log_entry) && NROW(forward_weights_result$log_entry) > 0) {
    cat("\nStrategy Note:", as.character(forward_weights_result$log_entry$Details), "\n")
  } else {
    cat("\nStrategy Note: Standard Rebalance\n")
  }
} else {
  message("[ERROR] Could not calculate forward-looking weights.")
}
message("\n===================================================================\n")

# --- FIX: Robustly untrace all methods and the generic function ---
try({
  # Untrace all S3 methods for as.Date
  for (m in methods("as.Date")) {
    try(untrace(m), silent = TRUE)
  }
  # Untrace the generic in base namespace
  try(untrace("as.Date", where = baseenv()), silent = TRUE)
  
  # Untrace xts constructor
  try(untrace("xts", where = asNamespace("xts")), silent = TRUE)
  
  message("[CLEANUP] All tracers have been disabled.")
}, silent = TRUE)
# --- END FIX ---