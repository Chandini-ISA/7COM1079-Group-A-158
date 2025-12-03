# cleaning.R — 7COM1079 BMW project
# Purpose: load raw BMW dataset, perform cleaning, flag outliers, save cleaned data & summaries
# Author: Girish (R Code Lead)
# Usage: Rscript scripts/cleaning.R

# ---- Setup ----
library(tidyverse)

raw_path <- "data/bmw_raw.csv"
clean_path <- "data/bmw_cleaned.csv"
summary_clean_path <- "data/summary_stats_cleaned.csv"
clean_log_path <- "appendix/cleaning_log.md"

if(!file.exists(raw_path)) stop("Raw data not found at: ", raw_path)

# ---- Ingest ----
bmw_raw <- read_csv(raw_path, show_col_types = FALSE)

# Record initial counts
n_initial <- nrow(bmw_raw)

# ---- Basic type checks & coercion ----
# Ensure columns exist
required_cols <- c("price", "mileage", "year", "engineSize", "mpg", "transmission", "model")
missing_cols <- setdiff(required_cols, names(bmw_raw))
if(length(missing_cols) > 0) {
  warning("Missing expected columns: ", paste(missing_cols, collapse = ", "))
}

# Coerce numeric columns if necessary
bmw_raw <- bmw_raw %>%
  mutate(
    price = as.numeric(price),
    mileage = as.numeric(mileage),
    engineSize = as.numeric(engineSize),
    mpg = as.numeric(mpg),
    year = as.integer(year)
  )

# ---- Remove obvious invalid rows ----
bmw_step1 <- bmw_raw %>%
  filter(!is.na(price), !is.na(mileage)) %>%         # remove NA for key vars
  filter(price > 0, mileage > 0)                     # remove non-positive entries

n_after_positive <- nrow(bmw_step1)

# ---- Flag impossible / suspicious values ----
# engineSize == 0 is suspicious: keep but flag (we can remove if many)
n_engine0 <- sum(bmw_step1$engineSize == 0, na.rm = TRUE)

# mpg extreme values: flag values > 150 as implausible (likely data error)
n_mpg_extreme <- sum(bmw_step1$mpg > 150, na.rm = TRUE)

# year range check: reasonable 1980 - 2025
n_year_bad <- sum(bmw_step1$year < 1980 | bmw_step1$year > 2025 | is.na(bmw_step1$year))

# ---- Outlier handling for price and mileage ----
# We'll compute IQR-based fences and mark rows outside 1.5*IQR
outlier_flags <- bmw_step1 %>%
  select(price, mileage) %>%
  summarise(
    price_q1 = quantile(price, 0.25, na.rm = TRUE),
    price_q3 = quantile(price, 0.75, na.rm = TRUE),
    mileage_q1 = quantile(mileage, 0.25, na.rm = TRUE),
    mileage_q3 = quantile(mileage, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    price_iqr = price_q3 - price_q1,
    mileage_iqr = mileage_q3 - mileage_q1,
    price_lower = price_q1 - 1.5 * price_iqr,
    price_upper = price_q3 + 1.5 * price_iqr,
    mileage_lower = mileage_q1 - 1.5 * mileage_iqr,
    mileage_upper = mileage_q3 + 1.5 * mileage_iqr
  )

pf <- outlier_flags

bmw_flagged <- bmw_step1 %>%
  mutate(
    price_outlier = if_else(price < pf$price_lower | price > pf$price_upper, TRUE, FALSE),
    mileage_outlier = if_else(mileage < pf$mileage_lower | mileage > pf$mileage_upper, TRUE, FALSE)
  )

n_price_outliers <- sum(bmw_flagged$price_outlier, na.rm = TRUE)
n_mileage_outliers <- sum(bmw_flagged$mileage_outlier, na.rm = TRUE)

# Decision rule: create cleaned dataset by removing price outliers AND extreme mileage outliers.
# Rationale: outliers likely due to data entry; removing both yields robust correlation.
bmw_cleaned <- bmw_flagged %>%
  filter(!price_outlier, !mileage_outlier) %>%
  select(-price_outlier, -mileage_outlier)

n_cleaned <- nrow(bmw_cleaned)

# ---- Save cleaned data and summaries ----
write_csv(bmw_cleaned, clean_path)

# summary numeric stats for cleaned data
summary_clean <- bmw_cleaned %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), list(
    n = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  ), .names = "{col}_{fn}"))

write_csv(summary_clean, summary_clean_path)

# ---- Write cleaning log ----
log_lines <- c(
  "# Cleaning log",
  paste0("- Initial rows: ", n_initial),
  paste0("- Rows after removing NA/non-positive price or mileage: ", n_after_positive),
  paste0("- Rows with engineSize == 0 (flagged): ", n_engine0),
  paste0("- Rows with mpg > 150 (flagged): ", n_mpg_extreme),
  paste0("- Rows with Year outside 1980-2025 (flagged): ", n_year_bad),
  paste0("- Price outliers removed (IQR 1.5): ", n_price_outliers),
  paste0("- Mileage outliers removed (IQR 1.5): ", n_mileage_outliers),
  paste0("- Final cleaned rows saved: ", n_cleaned),
  "",
  "Cleaning decisions:",
  "- Removed rows with missing price/mileage or non-positive values.",
  "- Flagged suspicious engineSize==0 and extreme mpg values but retained for review.",
  "- Removed rows outside 1.5*IQR for price and mileage (documented above).",
  "- Saved cleaned dataset to data/bmw_cleaned.csv and numeric summary to data/summary_stats_cleaned.csv."
)

write_lines(log_lines, clean_log_path)

message("Cleaning complete. Cleaned rows: ", n_cleaned)
message("Cleaned data saved to: ", clean_path)
message("Summary stats saved to: ", summary_clean_path)
message("Cleaning log saved to: ", clean_log_path)
