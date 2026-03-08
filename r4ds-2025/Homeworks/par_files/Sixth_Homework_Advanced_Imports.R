if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  DBI,
  duckdb,
  arrow,
  tidyverse,
  fs,
  benchmarkme
)

cat("CPU:\n", benchmarkme::get_cpu()$model_name, "\n\n")

data_path <- "par_files"
cat("Data path:\n", data_path, "\n\n")
dir_tree(data_path)

con <- dbConnect(duckdb(), dbdir = ":memory:")

taxi <- tbl(con, "read_parquet('par_files/yellow_tripdata_2024-*.parquet')")

taxi_prick <- tbl(con, "read_parquet('par_files')")

monthly_stats <- taxi |>
  mutate(
    month = strftime(tpep_pickup_datetime, '%Y-%m')
  ) |>
  group_by(month) |>
  summarise(
    mean_fare   = mean(fare_amount, na.rm = TRUE),
    mean_tip    = mean(tip_amount, na.rm = TRUE),
    total_trips = n()
  ) |>
  arrange(month) |>
  collect()

print(monthly_stats)

library(ggplot2)

ggplot(monthly_stats, aes(x = month)) +
  geom_line(aes(y = mean_fare, group = 1), linewidth = 1) +
  geom_line(aes(y = mean_tip, group = 1), linewidth = 1, linetype = "dashed") +
  labs(
    title = "NYC Yellow Taxi — Monthly Averages (2024)",
    subtitle = "Solid: Mean Fare | Dashed: Mean Tip",
    x = "Month",
    y = "USD",
    caption = "Data: NYC Taxi & Limousine Commission"
  ) +
  theme_minimal(base_size = 13)

ggplot(monthly_stats, aes(x = month, y = total_trips)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Total Taxi Trips per Month (2024)",
    x = "Month",
    y = "Number of Trips"
  ) +
  theme_minimal(base_size = 13)

