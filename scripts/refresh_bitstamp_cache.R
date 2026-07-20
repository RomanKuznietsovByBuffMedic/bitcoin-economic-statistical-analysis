#!/usr/bin/env Rscript

source("R/hourly_ohlc_quality.R")
source("R/bitstamp_ohlc.R")

start_time <- as.POSIXct("2020-07-01 00:00:00", tz = "UTC")
end_time <- as.POSIXct("2026-07-01 00:00:00", tz = "UTC")
cache_file <- "data/cache/bitstamp_btcusd_1h_2020-07-01_2026-06-30.rds"

detected_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
if (!is.finite(detected_cores)) {
  detected_cores <- 4L
}
default_workers <- min(4L, max(2L, as.integer(detected_cores)))
workers <- suppressWarnings(as.integer(Sys.getenv(
  "BITSTAMP_WORKERS",
  as.character(default_workers)
)))
if (!is.finite(workers) || workers < 1L) {
  stop("BITSTAMP_WORKERS має бути додатним цілим числом.")
}

cat("Оновлення основного ряду Bitstamp BTC/USD 1h.\n")
cat("Паралельних процесів:", workers, "\n\n")

btc_raw <- download_bitstamp_ohlc(
  market_symbol = "btcusd",
  start_time = start_time,
  end_time = end_time,
  workers = workers
)
data_check <- validate_hourly_ohlc(
  data = btc_raw,
  start_time = start_time,
  end_time = end_time
)

if (nrow(data_check$gaps) > 0) {
  stop("Новий ряд Bitstamp містить часові розриви. Основний кеш не замінено.")
}

dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
temporary_cache <- tempfile(
  pattern = paste0(basename(cache_file), "."),
  tmpdir = dirname(cache_file),
  fileext = ".rds"
)
on.exit(unlink(temporary_cache), add = TRUE)
saveRDS(btc_raw, temporary_cache, compress = "gzip")

verification_copy <- readRDS(temporary_cache)
if (
  nrow(verification_copy) != nrow(btc_raw) ||
  !identical(names(verification_copy), names(btc_raw))
) {
  stop("Перевірка нового Bitstamp RDS завершилася помилкою.")
}
if (!file.rename(temporary_cache, cache_file)) {
  stop("Не вдалося замінити Bitstamp-кеш: ", cache_file)
}

print(data_check$summary, n = Inf, digits = 7)
cat("\nКеш оновлено:", cache_file, "\n")
cat("SHA-256:", sha256_file(cache_file), "\n")
