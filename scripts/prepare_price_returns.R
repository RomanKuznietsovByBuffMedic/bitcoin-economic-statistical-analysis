#!/usr/bin/env Rscript

source("R/project_config.R")
source("R/project_io.R")
source("R/data_provenance.R")
source("R/hourly_ohlc_quality.R")
source("R/price_returns.R")
source("R/time_split.R")

config <- read_project_config()
start_time <- config$study$data_start
end_time <- config$study$data_end_exclusive

primary_file <- config$paths$cache[[config$primary$id]]
prepared_file <- config$paths$prepared

primary_raw <- read_rds_required(
  primary_file,
  paste("кеш", config$primary$name)
)

source_metadata_summary(
  config = config,
  data_by_exchange = setNames(
    list(primary_raw),
    config$primary$id
  )
)

primary_quality <- require_complete_hourly_ohlc(
  data = primary_raw,
  start_time = start_time,
  end_time = end_time,
  source_label = config$primary$market_label
)

prepared_data <- build_price_return_features(
  data = primary_quality$data,
  price_column = config$study$price_field
)
attr(prepared_data, "data_source") <- config$primary$market_label
attr(prepared_data, "primary_exchange_id") <- config$primary$id
attr(prepared_data, "market_symbol") <- config$primary$symbol
attr(prepared_data, "market_type") <- config$study$market_type
attr(prepared_data, "interval") <- config$study$interval
attr(prepared_data, "price_field") <- config$study$price_field
attr(prepared_data, "period_start_utc") <- format_utc(
  start_time,
  include_seconds = TRUE
)
attr(prepared_data, "period_end_exclusive_utc") <- format_utc(
  end_time,
  include_seconds = TRUE
)
attr(prepared_data, "raw_sha256") <- sha256_file(primary_file)
attr(prepared_data, "test_start_utc") <- format_utc(
  config$evaluation$test_start,
  include_seconds = TRUE
)
attr(prepared_data, "test_end_exclusive_utc") <- format_utc(
  config$evaluation$test_end_exclusive,
  include_seconds = TRUE
)
attr(prepared_data, "prepared_at_utc") <- format(
  Sys.time(),
  "%Y-%m-%d %H:%M:%S UTC",
  tz = "UTC"
)

save_rds_atomic(prepared_data, prepared_file)
write_data_manifest(config)

data_split <- split_time_series(
  data = prepared_data,
  training_start = config$study$data_start,
  test_start = config$evaluation$test_start,
  test_end_exclusive = config$evaluation$test_end_exclusive
)

cat("Підготовлений набір створено:\n", prepared_file, "\n")
cat("Джерело:", config$primary$market_label, "\n")
cat("Рядків:", nrow(prepared_data), "\n")
cat(
  "Пропущених годин:",
  expected_hour_count(start_time, end_time) - nrow(prepared_data),
  "\n"
)
cat(
  "Годин із логарифмічною дохідністю:",
  sum(!is.na(prepared_data$log_return_1h)),
  "\n"
)
cat(
  "Навчальних рядків:",
  nrow(data_split$training),
  "\n"
)
cat(
  "Тестових рядків:",
  nrow(data_split$test),
  "\n"
)
cat("SHA-256 основного набору:", sha256_file(prepared_file), "\n")
cat("Маніфест оновлено: data-manifest.yml\n")
