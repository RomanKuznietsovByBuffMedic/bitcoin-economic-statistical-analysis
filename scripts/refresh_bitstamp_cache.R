#!/usr/bin/env Rscript

source("R/project_config.R")
source("R/project_io.R")
source("R/data_provenance.R")
source("R/hourly_ohlc_quality.R")
source("R/download_progress.R")
source("R/bitstamp_ohlc.R")

config <- read_project_config()
exchange <- config$exchanges$bitstamp
start_time <- config$study$data_start
end_time <- config$study$data_end_exclusive
cache_file <- config$paths$cache$bitstamp

workers <- suppressWarnings(as.integer(Sys.getenv(
  "BITSTAMP_WORKERS",
  as.character(config$runtime$workers)
)))
if (is.na(workers) || workers < 1L) {
  stop("BITSTAMP_WORKERS має бути додатним цілим числом.")
}

step_seconds <- switch(
  config$study$interval,
  "1h" = 3600L,
  stop("Bitstamp: непідтримуваний інтервал.")
)

cat(
  "Оновлення ",
  exchange$market_label,
  " ",
  config$study$interval,
  ".\n",
  sep = ""
)
cat("Паралельних процесів:", workers, "\n\n")

bitstamp_raw <- download_bitstamp_ohlc(
  market_symbol = exchange$symbol,
  start_time = start_time,
  end_time = end_time,
  step_seconds = step_seconds,
  workers = workers,
  endpoint = exchange$endpoint
)
data_check <- require_complete_hourly_ohlc(
  data = bitstamp_raw,
  start_time = start_time,
  end_time = end_time,
  source_label = exchange$market_label
)

bitstamp_raw <- attach_source_metadata(
  data = bitstamp_raw,
  config = config,
  exchange_id = exchange$id,
  verification_level = "verified_on_download"
)

save_rds_atomic(bitstamp_raw, cache_file)

print(data_check$summary, n = Inf, digits = 7)
cat(
  "\n[ГОТОВО] ",
  exchange$market_label,
  " завантажено, перевірено й збережено.\n",
  sep = ""
)
cat("\nКеш оновлено:", cache_file, "\n")
cat("SHA-256:", sha256_file(cache_file), "\n")
