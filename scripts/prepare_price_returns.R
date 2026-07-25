#!/usr/bin/env Rscript

source("R/project_config.R")
source("R/project_io.R")
source("R/data_provenance.R")
source("R/hourly_ohlc_quality.R")
source("R/price_returns.R")
source("R/time_split.R")
source("R/data_pipeline.R")

config <- read_project_config()
result <- prepare_price_return_files(config)
prepared_data <- result$data

cat("Підготовлений набір створено:\n", result$prepared_file, "\n")
cat("Джерело:", config$primary$market_label, "\n")
cat("Рядків:", nrow(prepared_data), "\n")
cat(
  "Пропущених годин:",
  expected_hour_count(
    config$study$data_start,
    config$study$data_end_exclusive
  ) - nrow(prepared_data),
  "\n"
)
cat(
  "Годин із логарифмічною дохідністю:",
  sum(!is.na(prepared_data$log_return_1h)),
  "\n"
)
cat(
  "Дослідницьких рядків:",
  nrow(result$data_split$exploration),
  "\n"
)
cat(
  "Рядків внутрішньої перевірки:",
  nrow(result$data_split$validation),
  "\n"
)
cat(
  "Рядків фінального тесту:",
  nrow(result$data_split$test),
  "\n"
)
cat("SHA-256 основного набору:", result$prepared_sha256, "\n")
cat("Маніфест оновлено:", result$manifest_path, "\n")
