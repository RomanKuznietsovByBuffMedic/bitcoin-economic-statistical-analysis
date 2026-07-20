#!/usr/bin/env Rscript

source("R/binance_klines.R")
source("R/bitstamp_ohlc.R")

start_time <- as.POSIXct("2020-07-01 00:00:00", tz = "UTC")
end_time <- as.POSIXct("2026-07-01 00:00:00", tz = "UTC")
binance_cache <- "data/cache/btcusdt_spot_1h_2020-07-01_2026-06-30.rds"
bitstamp_cache <- "data/cache/bitstamp_btcusd_1h_2020-07-01_2026-06-30.rds"

if (!file.exists(binance_cache)) {
  stop("Не знайдено кеш Binance: ", binance_cache)
}

detected_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
if (!is.finite(detected_cores)) {
  detected_cores <- 4L
}
default_workers <- min(4L, max(2L, as.integer(detected_cores)))
workers <- suppressWarnings(as.integer(Sys.getenv(
  "BITSTAMP_WORKERS",
  as.character(default_workers)
)))

cat("Незалежний аудит Bitstamp BTC/USD 1h.\n")
cat("Паралельних процесів:", workers, "\n\n")

bitstamp_raw <- download_bitstamp_ohlc(
  market_symbol = "btcusd",
  start_time = start_time,
  end_time = end_time,
  workers = workers
)

bitstamp_check <- validate_hourly_klines(
  data = bitstamp_raw,
  start_time = start_time,
  end_time = end_time
)

dir.create(dirname(bitstamp_cache), recursive = TRUE, showWarnings = FALSE)
temporary_cache <- tempfile(
  pattern = paste0(basename(bitstamp_cache), "."),
  tmpdir = dirname(bitstamp_cache),
  fileext = ".rds"
)
on.exit(unlink(temporary_cache), add = TRUE)
saveRDS(bitstamp_raw, temporary_cache, compress = "gzip")

verification_copy <- readRDS(temporary_cache)
if (
  nrow(verification_copy) != nrow(bitstamp_raw) ||
  !identical(names(verification_copy), names(bitstamp_raw))
) {
  stop("Перевірка нового Bitstamp RDS завершилася помилкою.")
}
if (!file.rename(temporary_cache, bitstamp_cache)) {
  stop("Не вдалося замінити Bitstamp-кеш: ", bitstamp_cache)
}

binance_raw <- readRDS(binance_cache)
coverage <- compare_exchange_coverage(
  reference_data = binance_raw,
  alternative_data = bitstamp_raw,
  start_time = start_time,
  end_time = end_time
)

cat("Результат перевірки Bitstamp:\n")
print(bitstamp_check$summary, n = Inf, digits = 7)

if (nrow(bitstamp_check$gaps) > 0) {
  cat("\nВласні розриви Bitstamp:\n")
  print(bitstamp_check$gaps, n = Inf)
}

cat("\nПокриття годин, відсутніх на Binance:\n")
coverage_output <- coverage |>
  dplyr::transmute(
    `Година UTC` = format(open_time, "%Y-%m-%d %H:%M", tz = "UTC"),
    `Є на Bitstamp` = available_on_alternative,
    `Ціна закриття BTC/USD` = alternative_close
  )
print(coverage_output, n = Inf)

cat(
  "\nBitstamp покриває ",
  sum(coverage$available_on_alternative),
  " із ",
  nrow(coverage),
  " годин, відсутніх на Binance.\n",
  sep = ""
)
cat("Кеш Bitstamp:", bitstamp_cache, "\n")
cat("SHA-256:", sha256_file(bitstamp_cache), "\n")
