source("R/hourly_ohlc_quality.R")
source("R/bybit_klines.R")

start_time <- as.POSIXct("2020-07-01 00:00:00", tz = "UTC")
end_time <- as.POSIXct("2026-07-01 00:00:00", tz = "UTC")
workers <- as.integer(Sys.getenv("BYBIT_WORKERS", unset = "4"))

cat("Незалежний аудит Bybit Spot BTCUSDT 1h.\n")
cat("Це лише публічні ринкові дані, без входу й торгових операцій.\n")
cat("Паралельних процесів:", workers, "\n\n")

bybit_data <- download_bybit_klines(
  start_time = start_time,
  end_time = end_time,
  workers = workers
)

quality <- validate_hourly_ohlc(
  data = bybit_data,
  start_time = start_time,
  end_time = end_time
)

available_start <- min(quality$data$open_time)
available_quality <- validate_hourly_ohlc(
  data = quality$data,
  start_time = available_start,
  end_time = end_time
)

cache_file <- file.path(
  "data",
  "cache",
  "bybit_btcusdt_spot_1h_2020-07-01_2026-06-30.rds"
)
dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
temporary_file <- tempfile(
  pattern = paste0(basename(cache_file), "."),
  tmpdir = dirname(cache_file)
)
on.exit(unlink(temporary_file), add = TRUE)
saveRDS(quality$data, temporary_file, version = 3)
if (!file.rename(temporary_file, cache_file)) {
  stop("Не вдалося атомарно зберегти кеш Bybit.")
}

cat("Покриття всього запитаного періоду:\n")
print(quality$summary)
cat(
  "\nПерша наявна година:",
  format(available_start, "%Y-%m-%d %H:%M", tz = "UTC"),
  "\n"
)
cat(
  "Остання наявна година:",
  format(max(quality$data$open_time), "%Y-%m-%d %H:%M", tz = "UTC"),
  "\n"
)

cat("\nЯкість ряду від першої наявної години:\n")
print(available_quality$summary)

hours_before_listing <- as.numeric(
  difftime(available_start, start_time, units = "hours")
)
cat(
  "\nГодин до початку доступної історії Bybit:",
  hours_before_listing,
  "\n"
)

if (nrow(available_quality$gaps) > 0L) {
  cat("\nВнутрішні часові розриви Bybit:\n")
  print(available_quality$gaps, n = Inf)
} else {
  cat("Внутрішніх часових розривів після першої свічки немає.\n")
}

binance_file <- file.path(
  "data",
  "cache",
  "btcusdt_spot_1h_2020-07-01_2026-06-30.rds"
)
if (file.exists(binance_file)) {
  binance_data <- readRDS(binance_file)
  binance_quality <- validate_hourly_ohlc(
    data = binance_data,
    start_time = start_time,
    end_time = end_time
  )
  expected_grid <- seq.POSIXt(
    from = start_time,
    to = end_time - 3600,
    by = "hour"
  )
  missing_binance <- setdiff(expected_grid, binance_quality$data$open_time)
  covered <- missing_binance %in% quality$data$open_time
  cat(
    "\nBybit покриває",
    sum(covered),
    "із",
    length(missing_binance),
    "годин, відсутніх на Binance.\n"
  )
}

cat("Кеш аудиту Bybit:", cache_file, "\n")
cat("SHA-256:", sha256_file(cache_file), "\n")
