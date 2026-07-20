#!/usr/bin/env Rscript

source("R/binance_klines.R")

symbol <- "BTCUSDT"
interval <- "1h"
start_time <- as.POSIXct("2020-07-01 00:00:00", tz = "UTC")
end_time <- as.POSIXct("2026-07-01 00:00:00", tz = "UTC")
cache_file <- "data/cache/btcusdt_spot_1h_2020-07-01_2026-06-30.rds"
archive_dir <- "data/cache/binance-archives"
detected_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
if (!is.finite(detected_cores)) {
  detected_cores <- 4L
}
default_workers <- min(8L, max(2L, as.integer(detected_cores)))
workers <- suppressWarnings(as.integer(Sys.getenv(
  "BINANCE_ARCHIVE_WORKERS",
  as.character(default_workers)
)))
refresh_checksums <- tolower(Sys.getenv(
  "BINANCE_REFRESH_CHECKSUMS",
  "false"
)) %in% c("1", "true", "yes")

if (!is.finite(workers) || workers < 1L) {
  stop("BINANCE_ARCHIVE_WORKERS має бути додатним цілим числом.")
}

cat("Гібридне оновлення даних Binance BTCUSDT 1h.\n")
cat("Повні місяці: офіційні ZIP-архіви з перевіркою SHA-256.\n")
cat("Неповний хвіст, резерв і повторна перевірка розривів: REST API.\n\n")
cat("Паралельних процесів:", workers, "\n")
cat("Оновлювати віддалені CHECKSUM:", refresh_checksums, "\n\n")

btc_raw <- download_binance_hybrid_klines(
  symbol = symbol,
  interval = interval,
  start_time = start_time,
  end_time = end_time,
  archive_dir = archive_dir,
  workers = workers,
  refresh_checksums = refresh_checksums
)

data_check <- validate_hourly_klines(
  data = btc_raw,
  start_time = start_time,
  end_time = end_time
)

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
  stop("Перевірка нового RDS-файлу завершилася помилкою.")
}

if (!file.rename(temporary_cache, cache_file)) {
  stop("Не вдалося атомарно замінити файл кешу: ", cache_file)
}

acquisition_info <- attr(btc_raw, "acquisition_info")

cat("\nРезультат отримання:\n")
cat("Метод:", acquisition_info$method, "\n")
cat("Місяців з архівів:", acquisition_info$archive_months_used, "\n")
cat("Місяців через резервний REST:", length(acquisition_info$rest_fallback_months), "\n")
if (length(acquisition_info$rest_fallback_errors) > 0) {
  cat("Причини резервного REST:\n")
  print(unique(acquisition_info$rest_fallback_errors))
}
cat("Свічок відновлено адресною перевіркою:", acquisition_info$gap_rows_recovered, "\n")
cat("Пропущених годин після перевірки:", acquisition_info$remaining_gap_hours, "\n\n")

print(data_check$summary, n = Inf, digits = 7)

if (nrow(data_check$gaps) > 0) {
  cat("\nРозриви, що залишилися в офіційних джерелах Binance:\n")
  print(data_check$gaps, n = Inf)
}

cat("\nКеш оновлено:", cache_file, "\n")
cat("SHA-256:", sha256_file(cache_file), "\n")
