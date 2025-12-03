
library(tidyverse)
library(broom)

# Paths
clean_path <- "data/bmw_cleaned.csv"
results_dir <- "data/results"
if(!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

# Output files
pearson_out <- file.path(results_dir, "pearson_test_result.csv")
spearman_out <- file.path(results_dir, "spearman_test_result.csv")
lm_coeff_out <- file.path(results_dir, "lm_coefficients.csv")
lm_model_out <- file.path(results_dir, "lm_model_summary.csv")
diagn_resid <- "plots/residuals_vs_fitted.png"
diagn_qq <- "plots/qqplot_residuals.png"

# Load data
if(!file.exists(clean_path)) stop("Cleaned data not found at: ", clean_path)
bmw <- read_csv(clean_path, show_col_types = FALSE) %>%
  mutate(price = as.numeric(price), mileage = as.numeric(mileage)) %>%
  filter(!is.na(price), !is.na(mileage))

n_obs <- nrow(bmw)

# ----- Pearson correlation (two-sided CI, one-sided p if needed) -----
pearson_test <- cor.test(bmw$mileage, bmw$price, method = "pearson", alternative = "less")
# Extract outputs
pearson_df <- tibble(
  method = "pearson",
  n = n_obs,
  estimate_r = pearson_test$estimate,
  conf_low = pearson_test$conf.int[1],
  conf_high = pearson_test$conf.int[2],
  p_value_two_sided = pearson_test$p.value,
  alternative = pearson_test$alternative
)
# Save
write_csv(pearson_df, pearson_out)

# ----- Spearman correlation -----
spearman_test <- cor.test(bmw$mileage, bmw$price, method = "spearman", exact = FALSE)
spearman_df <- tibble(
  method = "spearman",
  n = n_obs,
  rho = spearman_test$estimate,
  conf_low = NA_real_,
  conf_high = NA_real_,
  p_value_two_sided = spearman_test$p.value,
  alternative = spearman_test$alternative
)
write_csv(spearman_df, spearman_out)

# ----- Simple linear regression -----
lm_fit <- lm(price ~ mileage, data = bmw)
lm_tidy <- broom::tidy(lm_fit) %>%
  select(term, estimate, std.error, statistic, p.value)

lm_glance <- broom::glance(lm_fit) %>%
  select(r.squared, adj.r.squared, sigma, statistic, p.value) %>%
  mutate(n = n_obs)

write_csv(lm_tidy, lm_coeff_out)
write_csv(lm_glance, lm_model_out)

# ----- Diagnostics plots -----
# Residuals vs fitted
png(diagn_resid, width = 800, height = 600, res = 120)
plot(lm_fit$fitted.values, lm_fit$residuals,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lwd = 1.2)
dev.off()

# Q-Q plot of residuals
png(diagn_qq, width = 800, height = 600, res = 120)
qqnorm(lm_fit$residuals, main = "Q-Q plot of residuals")
qqline(lm_fit$residuals, col = "red")
dev.off()

message("Analysis complete. Results saved to: ", results_dir)
message("Pearson: ", round(pearson_df$estimate_r, 4), "; p(two-sided)=", signif(pearson_df$p_value_two_sided,4))
