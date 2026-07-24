#!/usr/bin/env Rscript

source("R/project_config.R")
source("R/project_io.R")
source("R/data_provenance.R")
source("R/hourly_ohlc_quality.R")
source("R/download_progress.R")
source("R/bybit_klines.R")

config <- read_project_config()
exchange <- config$exchanges$bybit
start_time <- config$study$data_start
end_time <- config$study$data_end_exclusive
cache_file <- config$paths$cache$bybit

workers <- suppressWarnings(as.integer(Sys.getenv(
  "BYBIT_WORKERS",
  as.character(config$runtime$workers)
)))
if (is.na(workers) || workers < 1L) {
  stop("BYBIT_WORKERS має бути додатним цілим числом.")
}

bybit_interval <- switch(
  config$study$interval,
  "1h" = "60",
  stop("Bybit: непідтримуваний інтервал.")
)

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

bybit_raw <- download_bybit_klines(
  start_time = start_time,
  end_time = end_time,
  category = config$study$market_type,
  symbol = exchange$symbol,
  interval = bybit_interval,
  workers = workers,
  endpoint = exchange$endpoint
)

data_check <- require_complete_hourly_ohlc(
  data = bybit_raw,
  start_time = start_time,
  end_time = end_time,
  source_label = exchange$market_label
)

bybit_raw <- attach_source_metadata(
  data = bybit_raw,
  config = config,
  exchange_id = exchange$id,
  verification_level = "verified_on_download"
)

save_rds_atomic(bybit_raw, cache_file)

print(data_check$summary, n = Inf, digits = 7)

cat(
  "\n[ГОТОВО] ",
  exchange$market_label,
  " завантажено, перевірено й збережено.\n",
  sep = ""
)
cat("\nКеш оновлено:", cache_file, "\n")
cat("SHA-256:", sha256_file(cache_file), "\n")
