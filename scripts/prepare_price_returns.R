source("R/hourly_ohlc_quality.R")
source("R/price_returns.R")
source("R/exchange_comparison.R")

start_time <- as.POSIXct("2021-07-05 12:00:00", tz = "UTC")
end_time <- as.POSIXct("2026-07-01 00:00:00", tz = "UTC")

bybit_file <- file.path(
  "data",
  "cache",
  "bybit_btcusdt_spot_1h_2020-07-01_2026-06-30.rds"
)
binance_file <- file.path(
  "data",
  "cache",
  "btcusdt_spot_1h_2020-07-01_2026-06-30.rds"
)
prepared_file <- file.path(
  "data",
  "processed",
  "bybit_btcusdt_1h_price_returns_2021-07-05_2026-06-30.rds"
)
comparison_file <- file.path(
  "data",
  "processed",
  "bybit_binance_btcusdt_1h_comparison_2021-07-05_2026-06-30.rds"
)

required_files <- c(bybit_file, binance_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop(
    "Не знайдено кеші: ",
    paste(missing_files, collapse = ", "),
    ". Спочатку виконайте відповідні скрипти аудиту або оновлення."
  )
}

save_rds_atomic <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary_file <- tempfile(
    pattern = paste0(basename(path), "."),
    tmpdir = dirname(path)
  )
  on.exit(unlink(temporary_file), add = TRUE)
  saveRDS(object, temporary_file, version = 3)
  if (!file.rename(temporary_file, path)) {
    stop("Не вдалося атомарно зберегти файл: ", path)
  }
  invisible(path)
}

bybit_raw <- readRDS(bybit_file)
bybit_quality <- validate_hourly_ohlc(
  data = bybit_raw,
  start_time = start_time,
  end_time = end_time
)

if (nrow(bybit_quality$gaps) > 0L) {
  stop("Основний ряд Bybit містить часові розриви.")
}

expected_rows <- as.numeric(
  difftime(end_time, start_time, units = "hours")
)
if (nrow(bybit_quality$data) != expected_rows) {
  stop(
    "Основний ряд Bybit неповний: очікувалося ",
    expected_rows,
    ", отримано ",
    nrow(bybit_quality$data),
    "."
  )
}

prepared_data <- build_price_return_features(bybit_quality$data)
attr(prepared_data, "data_source") <- "Bybit Spot BTCUSDT"
attr(prepared_data, "interval") <- "1h"
attr(prepared_data, "period_start_utc") <- format(start_time, tz = "UTC")
attr(prepared_data, "period_end_exclusive_utc") <- format(end_time, tz = "UTC")
attr(prepared_data, "raw_sha256") <- sha256_file(bybit_file)
attr(prepared_data, "prepared_at_utc") <- format(Sys.time(), tz = "UTC")

binance_raw <- readRDS(binance_file)
binance_quality <- validate_hourly_ohlc(
  data = binance_raw,
  start_time = start_time,
  end_time = end_time
)
comparison <- compare_hourly_exchanges(
  primary = bybit_quality$data,
  reference = binance_quality$data,
  primary_name = "Bybit",
  reference_name = "Binance"
)
attr(comparison, "period_start_utc") <- format(start_time, tz = "UTC")
attr(comparison, "period_end_exclusive_utc") <- format(end_time, tz = "UTC")

save_rds_atomic(prepared_data, prepared_file)
save_rds_atomic(comparison, comparison_file)

cat("Основний підготовлений набір Bybit створено:\n", prepared_file, "\n")
cat("Рядків:", nrow(prepared_data), "\n")
cat(
  "Годин із логарифмічною дохідністю:",
  sum(!is.na(prepared_data$log_return_1h)),
  "\n"
)
cat("Внутрішніх часових розривів:", nrow(bybit_quality$gaps), "\n")
cat("Порівняння Bybit і Binance створено:\n", comparison_file, "\n")
cat("Спільних годин:", nrow(comparison$data), "\n")
cat("SHA-256 основного набору:", sha256_file(prepared_file), "\n")
cat("SHA-256 порівняння:", sha256_file(comparison_file), "\n")
