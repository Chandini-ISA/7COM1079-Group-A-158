# Produces: cleaned data, summaries, correlation/regression outputs, two final plots
# Authors: Chandini / Gayatri
# Usage: Rscript scripts/Analysis.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
  library(scales)
})

# ---- Paths ----
raw_path <- "data/bmw_raw.csv"
summary_raw_out <- "data/summary_stats_raw.csv"

clean_path <- "data/bmw_cleaned.csv"
summary_clean_out <- "data/summary_stats_cleaned.csv"
clean_log_path <- "appendix/cleaning_log.md"

results_dir <- "data/results"
if(!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

pearson_out <- file.path(results_dir, "pearson_test_result.csv")
spearman_out <- file.path(results_dir, "spearman_test_result.csv")
lm_coeff_out <- file.path(results_dir, "lm_coefficients.csv")
lm_model_out <- file.path(results_dir, "lm_model_summary.csv")

plots_dir <- "plots"
if(!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

plot_scatter <- file.path(plots_dir, "price_vs_mileage_FINAL.png")
plot_hist_mileage <- file.path(plots_dir, "mileage_hist_FINAL.png")

# ---- Ingest raw ----
if(!file.exists(raw_path)) stop("Raw data not found at: ", raw_path)
message("Reading raw data from: ", raw_path)
bmw_raw <- read_csv(raw_path, show_col_types = FALSE)

# ---- Raw summary ----
num_cols <- bmw_raw %>% select(where(is.numeric))
summary_stats_raw <- num_cols %>%
  summarise(across(everything(), list(
    n = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  ), .names = "{col}_{fn}"))

write_csv(summary_stats_raw, summary_raw_out)
message("Raw numeric summary written to: ", summary_raw_out)

# ---- Cleaning ----
message("Starting cleaning pipeline...")

# expected columns (best-effort)
required_cols <- c("price", "mileage", "year", "engineSize", "mpg", "transmission", "model")
missing_cols <- setdiff(required_cols, names(bmw_raw))
if(length(missing_cols) > 0) {
  warning("Missing expected columns: ", paste(missing_cols, collapse = ", "))
}

bmw <- bmw_raw %>%
  mutate(
    price = as.numeric(price),
    mileage = as.numeric(mileage),
    engineSize = as.numeric(engineSize),
    mpg = as.numeric(mpg),
    year = as.integer(year)
  )

n_initial <- nrow(bmw)

# Remove missing/invalid primary variables
bmw_step1 <- bmw %>%
  filter(!is.na(price), !is.na(mileage)) %>%
  filter(price > 0, mileage >= 0)

n_after_positive <- nrow(bmw_step1)

# Flag suspicious values
n_engine0 <- sum(bmw_step1$engineSize == 0, na.rm = TRUE)
n_mpg_extreme <- sum(bmw_step1$mpg > 150, na.rm = TRUE)
n_year_bad <- sum(bmw_step1$year < 1980 | bmw_step1$year > 2025 | is.na(bmw_step1$year))

# IQR fences
price_q1 <- quantile(bmw_step1$price, 0.25, na.rm = TRUE)
price_q3 <- quantile(bmw_step1$price, 0.75, na.rm = TRUE)
price_iqr <- price_q3 - price_q1
price_lower <- price_q1 - 1.5 * price_iqr
price_upper <- price_q3 + 1.5 * price_iqr

mileage_q1 <- quantile(bmw_step1$mileage, 0.25, na.rm = TRUE)
mileage_q3 <- quantile(bmw_step1$mileage, 0.75, na.rm = TRUE)
mileage_iqr <- mileage_q3 - mileage_q1
mileage_lower <- mileage_q1 - 1.5 * mileage_iqr
mileage_upper <- mileage_q3 + 1.5 * mileage_iqr

bmw_flagged <- bmw_step1 %>%
  mutate(
    price_outlier = if_else(price < price_lower | price > price_upper, TRUE, FALSE),
    mileage_outlier = if_else(mileage < mileage_lower | mileage > mileage_upper, TRUE, FALSE)
  )

n_price_outliers <- sum(bmw_flagged$price_outlier, na.rm = TRUE)
n_mileage_outliers <- sum(bmw_flagged$mileage_outlier, na.rm = TRUE)

# Remove price or mileage outliers
bmw_cleaned <- bmw_flagged %>%
  filter(!price_outlier, !mileage_outlier) %>%
  select(-price_outlier, -mileage_outlier)

n_cleaned <- nrow(bmw_cleaned)

# Save cleaned dataset and summary
write_csv(bmw_cleaned, clean_path)
message("Cleaned data saved to: ", clean_path, " (rows: ", n_cleaned, ")")

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

write_csv(summary_clean, summary_clean_out)
message("Summary of cleaned numeric columns saved to: ", summary_clean_out)

# Cleaning log
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
  "- Removed rows with missing price/mileage and non-positive price.",
  "- Flagged suspicious engineSize==0 and extreme mpg values but retained them for manual inspection.",
  "- Removed rows outside 1.5*IQR for price and mileage.",
  "- Saved cleaned dataset to data/bmw_cleaned.csv and numeric summary to data/summary_stats_cleaned.csv."
)
if(!dir.exists(dirname(clean_log_path))) dir.create(dirname(clean_log_path), recursive = TRUE)
write_lines(log_lines, clean_log_path)
message("Cleaning log written to: ", clean_log_path)

# ---- Final required plots (exactly two) ----
message("Generating final two plots (required by prof)...")

# Plot 1: scatter (points + lm dashed + LOESS)
p1 <- ggplot(bmw_cleaned, aes(x = mileage, y = price)) +
  geom_point(alpha = 0.18, size = 0.7, colour = "black") +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", colour = "#1f78b4", size = 0.9) +
  geom_smooth(method = "loess", se = FALSE, colour = "red", size = 0.7, span = 0.3) +
  labs(
    title = "Price vs Mileage (Used BMW cars)",
    subtitle = "Linear fit (dashed) and LOESS smoother",
    x = "Mileage (miles)",
    y = "Price (GBP)"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 14)

ggsave(plot_scatter, p1, width = 10, height = 6, dpi = 300)
message("Saved main scatter to: ", plot_scatter)

p2 <- ggplot(bmw_cleaned, aes(x = mileage)) +
  geom_histogram(bins = 40, fill = "grey40", alpha = 0.85, color = NA) +
  labs(
    title = "Distribution of Mileage",
    x = "Mileage (miles)",
    y = "Count"
  ) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14)


ggsave(plot_hist_mileage, p2, width = 8, height = 4.5, dpi = 300)
message("Saved mileage histogram to: ", plot_hist_mileage)

# ---- Statistical tests ----
message("Running statistical tests...")

n_obs <- nrow(bmw_cleaned)

pearson_test <- cor.test(bmw_cleaned$mileage, bmw_cleaned$price, method = "pearson", alternative = "less")
pearson_df <- tibble(
  method = "pearson",
  n = n_obs,
  estimate_r = as.numeric(pearson_test$estimate),
  conf_low = pearson_test$conf.int[1],
  conf_high = pearson_test$conf.int[2],
  p_value_two_sided = pearson_test$p.value,
  alternative = pearson_test$alternative
)
write_csv(pearson_df, pearson_out)
message("Pearson test saved: ", pearson_out)

spearman_test <- cor.test(bmw_cleaned$mileage, bmw_cleaned$price, method = "spearman", exact = FALSE)
spearman_df <- tibble(
  method = "spearman",
  n = n_obs,
  rho = as.numeric(spearman_test$estimate),
  conf_low = NA_real_,
  conf_high = NA_real_,
  p_value_two_sided = spearman_test$p.value,
  alternative = spearman_test$alternative
)
write_csv(spearman_df, spearman_out)
message("Spearman test saved: ", spearman_out)

lm_fit <- lm(price ~ mileage, data = bmw_cleaned)
lm_tidy <- broom::tidy(lm_fit) %>%
  select(term, estimate, std.error, statistic, p.value)
lm_glance <- broom::glance(lm_fit) %>%
  select(r.squared, adj.r.squared, sigma, statistic, p.value) %>%
  mutate(n = n_obs)

write_csv(lm_tidy, lm_coeff_out)
write_csv(lm_glance, lm_model_out)
message("Linear model outputs saved to: ", lm_coeff_out, " and ", lm_model_out)

message("Analysis.R complete — cleaned data, results and two required plots saved.")


# Gopi: Reanalysis check completed — verified input paths and summarisation steps (03 Dec 2025)
