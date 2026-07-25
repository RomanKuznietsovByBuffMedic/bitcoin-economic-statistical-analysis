#!/usr/bin/env Rscript

source("R/project_config.R")
source("R/project_io.R")
source("R/data_provenance.R")
source("R/hourly_ohlc_quality.R")
source("R/download_progress.R")
source("R/bitstamp_ohlc.R")
source("R/data_pipeline.R")

config <- read_project_config()
exchange <- config$exchanges$bitstamp
workers <- runtime_worker_count(config, "BITSTAMP_WORKERS")

cat(
  "Оновлення ",
  exchange$market_label,
  " ",
  config$study$interval,
  ".\n",
  sep = ""
)
cat("Паралельних процесів:", workers, "\n\n")

result <- refresh_bitstamp_market_data(
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
