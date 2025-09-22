# Robust formatter avoiding masked 'select()'
# Required packages: readxl, tidyr, purrr, lubridate, readr
# install.packages(c("readxl","tidyr","purrr","lubridate","readr"))

library(readxl)
library(tidyr)
library(purrr)
library(lubridate)
library(readr)

# ---------- SET: full path to your Excel file ----------
filepath <- "C:/Users/slizb/Documents/R/RP_Data_trimmed.xlsx"
if (!file.exists(filepath)) stop("File not found at: ", filepath)

sheetname <- 1
hdr_rows  <- 1:3   # headers in rows 1..3 (instrument, label, code)

# ---- Read raw sheet w/o names ----
raw <- read_excel(filepath, sheet = sheetname, col_names = FALSE)
ncols <- ncol(raw)
if (ncols < 2) stop("Sheet must have at least two columns (date + data).")

# ---- Extract header rows to character vectors ----
instrument_row <- raw[hdr_rows[1], ]
label_row      <- raw[hdr_rows[2], ]
code_row       <- raw[hdr_rows[3], ]

instrument_chars <- as.character(unlist(instrument_row))
label_chars      <- as.character(unlist(label_row))
code_chars       <- as.character(unlist(code_row))

# ---- Detect instrument blocks (columns 2..n) ----
search_cols <- 2:ncols
inst_start_rel <- which(!is.na(instrument_chars[search_cols]) & trimws(instrument_chars[search_cols]) != "")
if (length(inst_start_rel) == 0) stop("No instrument names found in header row (columns 2..n).")
inst_starts <- search_cols[inst_start_rel]
inst_ends   <- c((inst_starts[-1] - 1), ncols)
inst_blocks <- data.frame(start = inst_starts, end = inst_ends, instrument = instrument_chars[inst_starts], stringsAsFactors = FALSE)

# ---- Data rows (below the header rows) ----
data_block <- raw[-hdr_rows, , drop = FALSE]
rownames(data_block) <- NULL
n_obs <- nrow(data_block)

# ---- Parse first column as dates (robust heuristics) ----
raw_date_col <- data_block[[1]]
parse_dates_firstcol <- function(col) {
  if (is.numeric(col)) {
    colnum <- as.numeric(col)
    frac_serial <- mean(!is.na(colnum) & colnum >= 20000 & colnum <= 47000)
    if (!is.nan(frac_serial) && frac_serial > 0.9) return(as.Date(colnum, origin = "1899-12-30"))
    if (all(grepl("^\\d{8}$", as.character(na.omit(colnum))))) {
      return(as.Date(as.character(as.integer(colnum)), format = "%Y%m%d"))
    }
  }
  col_chr <- as.character(col)
  parsers <- list(ymd = ymd, dmy = dmy, mdy = mdy)
  for (p in parsers) {
    parsed <- suppressWarnings(p(col_chr, quiet = TRUE))
    frac_ok <- mean(!is.na(parsed), na.rm = TRUE)
    if (!is.nan(frac_ok) && frac_ok > 0.9) return(parsed)
  }
  parsed2 <- suppressWarnings(as.Date(col_chr))
  if (mean(!is.na(parsed2), na.rm = TRUE) > 0.9) return(parsed2)
  return(rep(NA, length(col_chr)))
}
date_vec <- parse_dates_firstcol(raw_date_col)
date_found <- !all(is.na(date_vec))
if (!date_found) {
  warning("Could not parse the first column as dates reliably. Using observation index as 'date'.")
  date_vec <- seq_len(n_obs)
} else if (length(date_vec) != n_obs) {
  warning("Parsed date length mismatch; falling back to obs index.")
  date_vec <- seq_len(n_obs)
  date_found <- FALSE
}

# ---- Build instrument-level wide tables and lookups using base indexing ----
build_instrument_df <- function(start, end) {
  cols_idx <- start:end
  field_codes  <- code_chars[cols_idx]
  field_labels <- label_chars[cols_idx]
  # prefer field code for column name; otherwise label; otherwise generic V#
  col_names <- ifelse(!is.na(field_codes) & trimws(field_codes) != "",
                      make.names(field_codes),
                      make.names(ifelse(!is.na(field_labels) & trimws(field_labels) != "", field_labels, paste0("V", cols_idx))))
  # base indexing to extract columns
  df_inst_fields <- data_block[, cols_idx, drop = FALSE]
  names(df_inst_fields) <- col_names
  # attach date & obs via cbind (avoids dplyr::mutate masking issues)
  df_inst_wide <- cbind(df_inst_fields, date = date_vec, obs = seq_len(n_obs))
  lookup <- data.frame(col_name = col_names,
                       field_code = as.character(field_codes),
                       field_label = as.character(field_labels),
                       orig_col = cols_idx,
                       stringsAsFactors = FALSE)
  list(instrument = as.character(instrument_chars[start]),
       wide = df_inst_wide,
       lookup = lookup)
}

rebuild <- purrr::map2(inst_blocks$start, inst_blocks$end, build_instrument_df)
inst_named_list <- setNames(lapply(rebuild, function(x) x$wide), vapply(rebuild, `[[`, character(1), "instrument"))

# ---- Build tidy long table (no dplyr::select) ----
long_tbls <- purrr::map(rebuild, function(item) {
  df_wide <- item$wide
  # pivot_longer using tidyr (qualified call)
  long <- tidyr::pivot_longer(df_wide, cols = setdiff(names(df_wide), c("date","obs")),
                              names_to = "col_name", values_to = "value")
  # attach lookup columns using match (no joins)
  long$field_code  <- item$lookup$field_code[match(long$col_name, item$lookup$col_name)]
  long$field_label <- item$lookup$field_label[match(long$col_name, item$lookup$col_name)]
  long$instrument  <- item$instrument
  # keep only columns we want, in order; if any missing, create NA
  want <- c("instrument","field_label","field_code","date","obs","value")
  for (w in want) if (!w %in% names(long)) long[[w]] <- NA
  long[, want]
})

# combine
tidy_long <- do.call(rbind, long_tbls)
# try to coerce values to numeric where sensible
tidy_long$value <- type.convert(as.character(tidy_long$value), as.is = TRUE)

# ---- Build overall wide table: one column per instrument_field ----
make_colname <- function(instr, code, label) {
  base <- ifelse(!is.na(code) & trimws(code) != "", code, label)
  base <- ifelse(is.na(base) | trimws(base) == "", "field", base)
  paste0(make.names(instr), "_", make.names(base))
}
wide_named_dfs <- lapply(rebuild, function(item) {
  instr <- item$instrument
  df <- item$wide
  info <- item$lookup
  new_names <- vapply(seq_len(nrow(info)), function(i) make_colname(instr, info$field_code[i], info$field_label[i]), character(1))
  # map old names to new names
  old_names <- info$col_name
  # replace names in df
  for (i in seq_along(old_names)) {
    if (old_names[i] %in% names(df)) names(df)[names(df) == old_names[i]] <- new_names[i]
  }
  df
})

# Merge all wide dfs by date & obs (full join)
overall_wide <- Reduce(function(a,b) merge(a, b, by = c("date","obs"), all = TRUE), wide_named_dfs, init = data.frame(date = date_vec, obs = seq_len(n_obs), stringsAsFactors = FALSE))

# ---- Save outputs next to the source file ----
base_outname <- tools::file_path_sans_ext(basename(filepath))
out_dir <- dirname(filepath)

readr::write_csv(tidy_long, file.path(out_dir, paste0(base_outname, "_tidy_long.csv")))
readr::write_csv(overall_wide, file.path(out_dir, paste0(base_outname, "_overall_wide.csv")))
saveRDS(inst_named_list, file.path(out_dir, paste0(base_outname, "_structured.rds")))

# Also save each instrument (wide)
for (nm in names(inst_named_list)) {
  fn <- file.path(out_dir, paste0(base_outname, "_", make.names(nm), ".csv"))
  readr::write_csv(as.data.frame(inst_named_list[[nm]]), fn)
}

cat("Done. Processed:", filepath, "\nSaved outputs in:", out_dir, "\n")
