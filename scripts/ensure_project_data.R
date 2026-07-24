#!/usr/bin/env Rscript

# Idempotent data preparation --------------------------------------------
#
# Missing, unreadable, unverified or configuration-incompatible raw caches
# are downloaded again. A prepared dataset and manifest are rebuilt only
# when their inputs or experiment parameters have changed.

source("R/project_config.R")
source("R/project_io.R")
source("R/data_provenance.R")
source("R/hourly_ohlc_quality.R")
source("R/bitstamp_ohlc.R")

config <- read_project_config()
rscript <- file.path(R.home("bin"), "Rscript")

refresh_scripts <- c(
  binance = "scripts/refresh_binance_cache.R",
  bybit = "scripts/refresh_bybit_cache.R",
  bitstamp = "scripts/refresh_bitstamp_cache.R"
)
unknown_sources <- setdiff(
  names(config$paths$cache),
  names(refresh_scripts)
)
if (length(unknown_sources) > 0L) {
  stop(
    paste(
      "Не визначено сценарії оновлення",
      "для джерел:"
    ),
    " ",
    paste(unknown_sources, collapse = ", ")
  )
}

run_project_script <- function(path) {
  status <- system2(rscript, path)
  if (!isTRUE(status == 0L)) {
    stop("Сценарій завершився з помилкою: ", path)
  }
}

print_data_status <- function(status, text) {
  cat("[", status, "] ", text, "\n", sep = "")
}

cache_state <- function(exchange_id) {
  exchange <- config$exchanges[[exchange_id]]
  path <- config$paths$cache[[exchange_id]]

  if (!file.exists(path)) {
    return(list(
      current = FALSE,
      reason = paste("не знайдено", path)
    ))
  }

  data <- tryCatch(
    readRDS(path),
    error = function(error) error
  )
  if (inherits(data, "error")) {
    return(list(
      current = FALSE,
      reason = paste("RDS не читається:", conditionMessage(data))
    ))
  }

  metadata_check <- tryCatch(
    check_source_metadata(data, config, exchange_id),
    error = function(error) error
  )
  if (
    inherits(metadata_check, "error") ||
      !isTRUE(metadata_check$matches)
  ) {
    return(list(
      current = FALSE,
      reason = "метадані не відповідають config.yml"
    ))
  }
  if (
    !identical(
      metadata_check$verification_level,
      "verified_on_download"
    )
  ) {
    return(list(
      current = FALSE,
      reason = paste(
        "походження не перевірено",
        "новим завантажувачем"
      )
    ))
  }

  acquisition_info <- attr(data, "acquisition_info")
  identity_checked <- switch(
    exchange_id,
    binance =
      isTRUE(acquisition_info$archive_checksums_verified) &&
      identical(
        toupper(as.character(acquisition_info$symbol)),
        toupper(exchange$symbol)
      ) &&
      identical(
        as.character(acquisition_info$interval),
        config$study$interval
      ),
    bybit = isTRUE(acquisition_info$identity_checked) &&
      identical(
        toupper(as.character(acquisition_info$symbol)),
        toupper(exchange$symbol)
      ) &&
      identical(
        tolower(as.character(acquisition_info$category)),
        config$study$market_type
      ),
    bitstamp = isTRUE(acquisition_info$identity_checked) &&
      identical(
        normalize_market_identifier(
          acquisition_info$market_symbol
        ),
        normalize_market_identifier(exchange$symbol)
      ),
    FALSE
  )
  if (!identity_checked) {
    return(list(
      current = FALSE,
      reason = paste(
        "відповідь джерела не має",
        "потрібної перевірки ідентичності"
      )
    ))
  }

  allow_internal_gaps <- identical(
    exchange_id,
    config$candidate$id
  )
  quality <- tryCatch(
    require_bounded_hourly_ohlc(
      data = data,
      start_time = config$study$data_start,
      end_time = config$study$data_end_exclusive,
      source_label = exchange$market_label,
      allow_internal_gaps = allow_internal_gaps
    ),
    error = function(error) error
  )
  if (inherits(quality, "error")) {
    return(list(
      current = FALSE,
      reason = paste(
        "перевірка OHLC не пройдена:",
        conditionMessage(quality)
      )
    ))
  }

  list(
    current = TRUE,
    reason = "актуальний і перевірений"
  )
}

exchange_ids <- names(config$paths$cache)
cat("\nДЖЕРЕЛА ДАНИХ\n")
for (exchange_index in seq_along(exchange_ids)) {
  exchange_id <- exchange_ids[[exchange_index]]
  exchange_label <- config$exchanges[[exchange_id]]$market_label
  cat(
    "\n[",
    exchange_index,
    "/",
    length(exchange_ids),
    "] ",
    exchange_label,
    "\n",
    sep = ""
  )
  state <- cache_state(exchange_id)

  if (state$current) {
    print_data_status(
      "КЕШ ГОТОВИЙ",
      "повторне завантаження не потрібне."
    )
    next
  }

  print_data_status("ЗАВАНТАЖЕННЯ", state$reason)
  if (!config$runtime$auto_download_data) {
    stop(
      paste(
        paste(
          "Автоматичне завантаження",
          "вимкнено в config.yml."
        ),
        paste(
          "Увімкніть runtime.auto_download_data",
          "або запустіть"
        ),
        refresh_scripts[[exchange_id]],
        "вручну."
      )
    )
  }

  run_project_script(refresh_scripts[[exchange_id]])
  refreshed_state <- cache_state(exchange_id)
  if (!refreshed_state$current) {
    stop(
      "Новий кеш ",
      exchange_id,
      " не пройшов перевірку: ",
      refreshed_state$reason
    )
  }
  print_data_status(
    "ГОТОВО",
    paste(exchange_label, "завантажено й перевірено.")
  )
}

prepared_state <- function() {
  path <- config$paths$prepared
  if (!file.exists(path)) {
    return(list(
      current = FALSE,
      reason = paste("не знайдено", path)
    ))
  }

  data <- tryCatch(
    readRDS(path),
    error = function(error) error
  )
  if (inherits(data, "error")) {
    return(list(
      current = FALSE,
      reason = paste(
        "підготовлений RDS не читається:",
        conditionMessage(data)
      )
    ))
  }

  raw_path <- config$paths$cache[[config$primary$id]]
  expected_attributes <- list(
    primary_exchange_id = config$primary$id,
    market_symbol = config$primary$symbol,
    market_type = config$study$market_type,
    interval = config$study$interval,
    price_field = config$study$price_field,
    period_start_utc = format_utc(
      config$study$data_start,
      include_seconds = TRUE
    ),
    period_end_exclusive_utc = format_utc(
      config$study$data_end_exclusive,
      include_seconds = TRUE
    ),
    raw_sha256 = sha256_file(raw_path),
    test_start_utc = format_utc(
      config$evaluation$test_start,
      include_seconds = TRUE
    ),
    test_end_exclusive_utc = format_utc(
      config$evaluation$test_end_exclusive,
      include_seconds = TRUE
    )
  )
  attributes_match <- all(vapply(
    names(expected_attributes),
    function(name) {
      identical(
        as.character(attr(data, name)),
        as.character(expected_attributes[[name]])
      )
    },
    logical(1)
  ))
  if (!attributes_match) {
    return(list(
      current = FALSE,
      reason = paste(
        "метадані або SHA-256",
        "вхідного кешу змінилися"
      )
    ))
  }

  quality <- tryCatch(
    require_complete_hourly_ohlc(
      data = data,
      start_time = config$study$data_start,
      end_time = config$study$data_end_exclusive,
      source_label = "Підготовлений набір"
    ),
    error = function(error) error
  )
  if (inherits(quality, "error")) {
    return(list(
      current = FALSE,
      reason = paste(
        paste(
          "перевірка підготовленого",
          "набору не пройдена:"
        ),
        conditionMessage(quality)
      )
    ))
  }

  list(
    current = TRUE,
    reason = "актуальний і перевірений"
  )
}

cat("\nПІДГОТОВКА ДАНИХ\n")
prepared <- prepared_state()
if (!prepared$current) {
  print_data_status(
    "ОБРОБКА",
    paste("аналітичний набір:", prepared$reason)
  )
  run_project_script("scripts/prepare_price_returns.R")
  prepared <- prepared_state()
  if (!prepared$current) {
    stop(
      paste(
        "Новий підготовлений набір",
        "не пройшов перевірку:"
      ),
      " ",
      prepared$reason
    )
  }
  print_data_status(
    "ГОТОВО",
    "аналітичний набір створено й перевірено."
  )
} else {
  print_data_status(
    "КЕШ ГОТОВИЙ",
    "аналітичний набір не потрібно перебудовувати."
  )
}

manifest_check <- tryCatch(
  validate_data_manifest(config),
  error = function(error) error
)
if (inherits(manifest_check, "error")) {
  print_data_status(
    "ОБРОБКА",
    "маніфест відсутній або застарів."
  )
  write_data_manifest(config)
  validate_data_manifest(config)
  print_data_status("ГОТОВО", "маніфест оновлено.")
} else {
  print_data_status("ГОТОВО", "маніфест актуальний.")
}

print_data_status(
  "ПЕРЕВІРКА",
  "цілісність даних і часовий поділ."
)
run_project_script("scripts/check_data_and_split.R")
print_data_status(
  "ГОТОВО",
  "цілісність даних і часовий поділ перевірено."
)
cat("\nЛокальні дані готові до рендеру.\n")
