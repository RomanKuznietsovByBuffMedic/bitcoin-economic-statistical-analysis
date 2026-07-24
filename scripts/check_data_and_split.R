#!/usr/bin/env Rscript

# Visible end-to-end check of data parameters, provenance, file integrity
# and the chronological training/test split.

source("R/project_config.R")
source("R/project_io.R")
source("R/data_provenance.R")
source("R/hourly_ohlc_quality.R")
source("R/time_split.R")

config <- read_project_config()

parameter_table <- tibble::tibble(
  `Параметр` = c(
    "Початковий кандидат",
    "Основна біржа",
    "Контрольна біржа",
    "Тип ринку",
    "Актив",
    "Валюта ціни",
    "Символ API",
    "Поле ціни",
    "Базовий інтервал",
    "Часова зона",
    "Початок даних",
    "Кінець даних без включення",
    "Тривалість даних, календарних років",
    "Початок фінального тесту",
    "Кінець фінального тесту без включення",
    "Тривалість тесту, календарних років",
    "Горизонт прогнозу, періодів",
    "Затримка виконання, періодів",
    "Ціна виконання",
    "Частота переоцінювання, місяців",
    "Навчальне вікно"
  ),
  `Значення` = c(
    config$candidate$name,
    config$primary$name,
    config$reference$name,
    config$study$market_type,
    config$primary$base_currency,
    config$primary$quote_currency,
    config$primary$symbol,
    config$study$price_field,
    config$study$interval,
    config$study$timezone,
    format_utc(config$study$data_start, include_seconds = TRUE),
    format_utc(
      config$study$data_end_exclusive,
      include_seconds = TRUE
    ),
    config$study$data_years,
    format_utc(config$evaluation$test_start, include_seconds = TRUE),
    format_utc(
      config$evaluation$test_end_exclusive,
      include_seconds = TRUE
    ),
    config$evaluation$test_years,
    config$evaluation$forecast_horizon_periods,
    config$evaluation$execution_lag_periods,
    "відкриття наступної свічки",
    config$evaluation$refit_every_months,
    "розширюване"
  )
)

raw_data <- lapply(
  config$paths$cache,
  read_rds_required,
  description = "кеш ринкових даних"
)

provenance_check <- source_metadata_summary(
  config = config,
  data_by_exchange = raw_data
)
manifest_check <- validate_data_manifest(config)

primary_file <- config$paths$cache[[config$primary$id]]
prepared <- read_rds_required(
  config$paths$prepared,
  "підготовлений набір ціни й дохідностей"
)

if (
  !identical(
    attr(prepared, "raw_sha256"),
    sha256_file(primary_file)
  )
) {
  stop(
    paste(
      "Підготовлений набір створено не з поточного основного кешу.",
      "Повторіть scripts/prepare_price_returns.R."
    )
  )
}

prepared_quality <- require_complete_hourly_ohlc(
  data = prepared,
  start_time = config$study$data_start,
  end_time = config$study$data_end_exclusive,
  source_label = "Підготовлений набір"
)

data_split <- split_time_series(
  data = prepared_quality$data,
  training_start = config$study$data_start,
  test_start = config$evaluation$test_start,
  test_end_exclusive = config$evaluation$test_end_exclusive
)

cat("\nПАРАМЕТРИ\n")
print(parameter_table, n = Inf, width = Inf)
cat("\nМЕТАДАНІ ДЖЕРЕЛ\n")
print(provenance_check, n = Inf, width = Inf)
cat("\nЦІЛІСНІСТЬ ФАЙЛІВ\n")
print(manifest_check, n = Inf, width = Inf)
cat("\nЧАСОВИЙ ПОДІЛ\n")
print(data_split$summary, n = Inf, width = Inf)
cat("\nУсі перевірки даних і часового поділу пройдено.\n")
