library(tidyverse)

# --- Paths ---
raw_path <- "data/bmw_raw.csv"
summary_out <- "data/summary_stats_raw.csv"

# --- Ingest ----
if(!file.exists(raw_path)) {
  stop(paste("Raw data not found at", raw_path))
}
bmw_raw <- read_csv(raw_path, show_col_types = FALSE)

# Quick summary (numeric columns only)
num_cols <- bmw_raw %>% select(where(is.numeric))
summary_stats <- num_cols %>%
  summarise(across(everything(), list(
    n = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  ), .names = "{col}_{fn}"))

# write summary to CSV
write_csv(summary_stats, summary_out)

message("Basic summary written to: ", summary_out)
message("Next: implement cleaning, outlier handling, and plotting scripts.")
