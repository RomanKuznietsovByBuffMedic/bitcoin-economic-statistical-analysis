#!/usr/bin/env Rscript

source("R/project_config.R")
source("R/project_io.R")
source("R/data_provenance.R")
source("R/hourly_ohlc_quality.R")
source("R/download_progress.R")
source("R/binance_klines.R")

config <- read_project_config()
exchange <- config$exchanges$binance
start_time <- config$study$data_start
end_time <- config$study$data_end_exclusive
cache_file <- config$paths$cache$binance

workers <- suppressWarnings(as.integer(Sys.getenv(
  "BINANCE_ARCHIVE_WORKERS",
  as.character(config$runtime$workers)
)))
if (is.na(workers) || workers < 1L) {
  stop("BINANCE_ARCHIVE_WORKERS має бути додатним цілим числом.")
}

refresh_checksums <- tolower(Sys.getenv(
  "BINANCE_REFRESH_CHECKSUMS",
  "false"
)) %in% c("1", "true", "yes")

cat(
  "Оновлення ",
  exchange$market_label,
  " ",
  config$study$interval,
  ".\n",
  sep = ""
)
cat("Повні місяці: офіційні ZIP-архіви з перевіркою SHA-256.\n")
cat("Неповний хвіст і повторна перевірка розривів: REST API.\n")
cat("Паралельних процесів:", workers, "\n\n")

btc_raw <- download_binance_hybrid_klines(
  symbol = exchange$symbol,
  interval = config$study$interval,
  start_time = start_time,
  end_time = end_time,
  archive_dir = config$paths$binance_archives,
  workers = workers,
  refresh_checksums = refresh_checksums,
  archive_base_url = exchange$archive_base_url,
  rest_endpoint = exchange$rest_endpoint
)

data_check <- require_bounded_hourly_ohlc(
  data = btc_raw,
  start_time = start_time,
  end_time = end_time,
  source_label = exchange$market_label,
  allow_internal_gaps = TRUE
)

btc_raw <- attach_source_metadata(
  data = btc_raw,
  config = config,
  exchange_id = exchange$id,
  verification_level = "verified_on_download"
)

save_rds_atomic(btc_raw, cache_file)

acquisition_info <- attr(btc_raw, "acquisition_info")
cat("Метод:", acquisition_info$method, "\n")
cat("Місяців з архівів:", acquisition_info$archive_months_used, "\n")
cat(
  "Місяців через резервний REST:",
  length(acquisition_info$rest_fallback_months),
  "\n"
)
if (length(acquisition_info$rest_fallback_errors) > 0L) {
  cat("Причини використання резервного REST:\n")
  print(unique(acquisition_info$rest_fallback_errors))
}
cat(
  "Свічок відновлено адресною перевіркою:",
  acquisition_info$gap_rows_recovered,
  "\n\n"
)

print(data_check$summary, n = Inf, digits = 7)

if (nrow(data_check$gaps) > 0L) {
  cat("\nРозриви, що залишилися в офіційних джерелах:\n")
  print(data_check$gaps, n = Inf)
}

cat(
  "\n[ГОТОВО] ",
  exchange$market_label,
  " завантажено, перевірено й збережено.\n",
  sep = ""
)
cat("\nКеш оновлено:", cache_file, "\n")
cat("SHA-256:", sha256_file(cache_file), "\n")
