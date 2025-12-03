# Cleaning log
- Initial rows: 10781
- Rows after removing NA/non-positive price or mileage: 10781
- Rows with engineSize == 0 (flagged): 47
- Rows with mpg > 150 (flagged): 95
- Rows with Year outside 1980-2025 (flagged): 0
- Price outliers removed (IQR 1.5): 484
- Mileage outliers removed (IQR 1.5): 296
- Final cleaned rows saved: 10001

Cleaning decisions:
- Removed rows with missing price/mileage or non-positive values.
- Flagged suspicious engineSize==0 and extreme mpg values but retained for review.
- Removed rows outside 1.5*IQR for price and mileage (documented above).
- Saved cleaned dataset to data/bmw_cleaned.csv and numeric summary to data/summary_stats_cleaned.csv.
