
library(tidyverse)
library(broom)

# Paths
clean_path <- "data/bmw_cleaned.csv"
out_scatter <- "plots/price_vs_mileage.png"
out_hist_mileage <- "plots/mileage_hist.png"
out_hist_price <- "plots/price_hist.png"

# Read cleaned data
if(!file.exists(clean_path)) stop("Cleaned data not found at: ", clean_path)
bmw <- read_csv(clean_path, show_col_types = FALSE)

# Ensure numeric types
bmw <- bmw %>%
  mutate(price = as.numeric(price), mileage = as.numeric(mileage))

# --- Scatterplot: Price vs Mileage with linear fit and LOESS ---
p_scatter <- ggplot(bmw, aes(x = mileage, y = price)) +
  geom_point(alpha = 0.35, size = 1) +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, linetype = "dashed", size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "black", size = 0.6) +
  labs(
    title = "Price vs Mileage (Used BMW cars)",
    subtitle = "Linear fit (dashed) and LOESS smoother shown",
    x = "Mileage (miles)",
    y = "Price (GBP)"
  ) +
  theme_minimal(base_size = 12)

ggsave(out_scatter, plot = p_scatter, width = 8, height = 5, dpi = 300)
message("Saved scatterplot: ", out_scatter)

# --- Histogram: Mileage with overlaid normal curve (for rubric) ---
mileage_vals <- bmw$mileage
m_mean <- mean(mileage_vals, na.rm = TRUE)
m_sd <- sd(mileage_vals, na.rm = TRUE)

p_hist_mileage <- ggplot(bmw, aes(x = mileage)) +
  geom_histogram(aes(y = ..density..), bins = 40, alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = m_mean, sd = m_sd), size = 0.8) +
  labs(
    title = "Distribution of Mileage",
    x = "Mileage (miles)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12)

ggsave(out_hist_mileage, plot = p_hist_mileage, width = 7, height = 4, dpi = 300)
message("Saved mileage histogram: ", out_hist_mileage)

# --- Histogram: Price with overlaid normal curve (supplementary) ---
price_vals <- bmw$price
p_mean <- mean(price_vals, na.rm = TRUE)
p_sd <- sd(price_vals, na.rm = TRUE)

p_hist_price <- ggplot(bmw, aes(x = price)) +
  geom_histogram(aes(y = ..density..), bins = 40, alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = p_mean, sd = p_sd), size = 0.8) +
  labs(
    title = "Distribution of Price",
    x = "Price (GBP)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12)

ggsave(out_hist_price, plot = p_hist_price, width = 7, height = 4, dpi = 300)
message("Saved price histogram: ", out_hist_price)

# Done
message("All plots generated.")
