#!/usr/bin/env Rscript

source("R/project_config.R")
source("R/project_io.R")
source("R/data_provenance.R")
source("R/hourly_ohlc_quality.R")
source("R/download_progress.R")
source("R/binance_klines.R")
source("R/data_pipeline.R")

config <- read_project_config()
exchange <- config$exchanges$binance
workers <- runtime_worker_count(config, "BINANCE_ARCHIVE_WORKERS")
refresh_checksums <- environment_flag(
  "BINANCE_REFRESH_CHECKSUMS"
)

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

result <- refresh_binance_market_data(
  config = config,
  workers = workers,
  refresh_checksums = refresh_checksums
)
acquisition_info <- result$acquisition_info

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

print(result$quality$summary, n = Inf, digits = 7)
if (nrow(result$quality$gaps) > 0L) {
  cat("\nРозриви, що залишилися в офіційних джерелах:\n")
  print(result$quality$gaps, n = Inf)
}

cat(
  "\n[ГОТОВО] ",
  exchange$market_label,
  " завантажено, перевірено й збережено.\n",
  sep = ""
)
cat("\nКеш оновлено:", result$cache_file, "\n")
cat("SHA-256:", result$sha256, "\n")
