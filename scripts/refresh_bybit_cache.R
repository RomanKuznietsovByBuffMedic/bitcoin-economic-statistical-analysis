#!/usr/bin/env Rscript

source("R/project_config.R")
source("R/project_io.R")
source("R/data_provenance.R")
source("R/hourly_ohlc_quality.R")
source("R/download_progress.R")
source("R/bybit_klines.R")
source("R/data_pipeline.R")

config <- read_project_config()
exchange <- config$exchanges$bybit
workers <- runtime_worker_count(config, "BYBIT_WORKERS")

cat(
  "Оновлення ",
  exchange$market_label,
  " ",
  config$study$interval,
  ".\n",
  sep = ""
)
cat("Використовуються лише публічні ринкові дані.\n")
cat("Паралельних процесів:", workers, "\n\n")

result <- refresh_bybit_market_data(
  config = config,
  workers = workers
)
print(result$quality$summary, n = Inf, digits = 7)

cat(
  "\n[ГОТОВО] ",
  exchange$market_label,
  " завантажено, перевірено й збережено.\n",
  sep = ""
)
cat("\nКеш оновлено:", result$cache_file, "\n")
cat("SHA-256:", result$sha256, "\n")
