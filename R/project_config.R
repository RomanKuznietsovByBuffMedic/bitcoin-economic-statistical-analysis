# Project configuration ---------------------------------------------------
#
# All variable study dates, markets, evaluation boundaries and runtime values
# are read from config.yml. Changing an experiment should not require searching
# through scripts or book chapters.

require_config_value <- function(value, name) {
  if (
    is.null(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(as.character(value))
  ) {
    stop("У config.yml бракує значення: ", name)
  }

  as.character(value)
}

require_finite_number <- function(
  value,
  name,
  minimum = -Inf,
  maximum = Inf
) {
  result <- suppressWarnings(as.numeric(value))
  if (
    length(result) != 1L ||
      is.na(result) ||
      !is.finite(result) ||
      result < minimum ||
      result > maximum
  ) {
    stop("Некоректне числове значення в config.yml: ", name)
  }

  result
}

require_integer <- function(
  value,
  name,
  minimum = -Inf,
  maximum = Inf
) {
  result <- require_finite_number(
    value,
    name,
    minimum = minimum,
    maximum = maximum
  )
  if (result != floor(result)) {
    stop("Значення в config.yml має бути цілим числом: ", name)
  }

  as.integer(result)
}

require_flag <- function(value, name) {
  if (
    is.logical(value) &&
      length(value) == 1L &&
      !is.na(value)
  ) {
    return(value)
  }

  normalized <- tolower(require_config_value(value, name))
  if (!normalized %in% c("true", "false")) {
    stop(
      "Значення в config.yml має бути true або false: ",
      name
    )
  }

  identical(normalized, "true")
}

parse_utc_time <- function(value, name, timezone) {
  value <- require_config_value(value, name)
  result <- as.POSIXct(
    value,
    format = "%Y-%m-%d %H:%M:%S",
    tz = timezone
  )

  if (is.na(result)) {
    stop(
      "Некоректна дата в config.yml для ",
      name,
      ": ",
      value
    )
  }

  result
}

safe_filename_part <- function(value) {
  result <- gsub(
    "[^a-z0-9]+",
    "_",
    tolower(as.character(value))
  )
  gsub("^_+|_+$", "", result)
}

normalize_exchange_config <- function(
  id,
  exchange,
  timezone,
  default_start
) {
  required_fields <- c(
    "name",
    "symbol",
    "base_currency",
    "quote_currency",
    "documentation_url"
  )
  missing_fields <- required_fields[
    !required_fields %in% names(exchange)
  ]

  if (length(missing_fields) > 0L) {
    stop(
      "Для біржі ",
      id,
      " у config.yml бракує полів: ",
      paste(missing_fields, collapse = ", ")
    )
  }

  endpoint_fields <- switch(
    id,
    binance = c("archive_base_url", "rest_endpoint"),
    bybit = "endpoint",
    bitstamp = "endpoint",
    stop("Непідтримуване джерело в config.yml: ", id)
  )
  official_endpoints <- switch(
    id,
    binance = list(
      archive_base_url =
        "https://data.binance.vision/data/spot/monthly/klines",
      rest_endpoint =
        "https://data-api.binance.vision/api/v3/klines"
    ),
    bybit = list(
      endpoint = "https://api.bybit.com/v5/market/kline"
    ),
    bitstamp = list(
      endpoint = "https://www.bitstamp.net/api/v2/ohlc"
    ),
    list()
  )
  missing_endpoints <- endpoint_fields[
    !endpoint_fields %in% names(exchange)
  ]
  if (length(missing_endpoints) > 0L) {
    stop(
      "Для біржі ",
      id,
      " у config.yml бракує адрес: ",
      paste(missing_endpoints, collapse = ", ")
    )
  }
  for (field in endpoint_fields) {
    exchange[[field]] <- require_config_value(
      exchange[[field]],
      paste0("exchanges.", id, ".", field)
    )
    if (!grepl("^https://", exchange[[field]])) {
      stop(
        "Адреса джерела повинна використовувати HTTPS: ",
        paste0("exchanges.", id, ".", field)
      )
    }
    if (
      !is.null(official_endpoints[[field]]) &&
        !identical(exchange[[field]], official_endpoints[[field]])
    ) {
      stop(
        "Неофіційна адреса для ",
        id,
        ": ",
        exchange[[field]]
      )
    }
  }

  exchange$id <- id
  exchange$name <- require_config_value(
    exchange$name,
    paste0("exchanges.", id, ".name")
  )
  exchange$symbol <- require_config_value(
    exchange$symbol,
    paste0("exchanges.", id, ".symbol")
  )
  exchange$base_currency <- toupper(require_config_value(
    exchange$base_currency,
    paste0("exchanges.", id, ".base_currency")
  ))
  exchange$quote_currency <- toupper(require_config_value(
    exchange$quote_currency,
    paste0("exchanges.", id, ".quote_currency")
  ))
  exchange$documentation_url <- require_config_value(
    exchange$documentation_url,
    paste0("exchanges.", id, ".documentation_url")
  )
  if (!grepl("^https://", exchange$documentation_url)) {
    stop(
      "Адреса документації повинна використовувати HTTPS: ",
      paste0("exchanges.", id, ".documentation_url")
    )
  }
  exchange$available_start <- if (is.null(exchange$available_start)) {
    default_start
  } else {
    parse_utc_time(
      exchange$available_start,
      paste0("exchanges.", id, ".available_start"),
      timezone
    )
  }
  exchange$period_start <- max(
    default_start,
    exchange$available_start
  )
  exchange$market_label <- paste(
    exchange$name,
    "Spot",
    paste0(
      exchange$base_currency,
      "/",
      exchange$quote_currency
    )
  )

  exchange
}

calendar_years_before <- function(value, years, timezone) {
  years <- require_integer(
    years,
    "evaluation.test_years",
    minimum = 1
  )
  value_text <- format(
    value,
    "%Y-%m-%d %H:%M:%S",
    tz = timezone
  )
  target_year <- as.integer(substr(value_text, 1L, 4L)) - years
  result <- as.POSIXct(
    paste0(
      sprintf("%04d", target_year),
      substr(value_text, 5L, nchar(value_text))
    ),
    format = "%Y-%m-%d %H:%M:%S",
    tz = timezone
  )

  if (is.na(result)) {
    stop(
      "Не вдалося відняти календарні роки від кінця даних. ",
      "Задайте іншу праву межу."
    )
  }

  result
}

calendar_years_after <- function(value, years, timezone) {
  years <- require_integer(
    years,
    "study.data_years",
    minimum = 1
  )
  value_text <- format(
    value,
    "%Y-%m-%d %H:%M:%S",
    tz = timezone
  )
  target_year <- as.integer(substr(value_text, 1L, 4L)) + years
  result <- as.POSIXct(
    paste0(
      sprintf("%04d", target_year),
      substr(value_text, 5L, nchar(value_text))
    ),
    format = "%Y-%m-%d %H:%M:%S",
    tz = timezone
  )

  if (is.na(result)) {
    stop(
      "Не вдалося додати календарні роки до початку даних. ",
      "Задайте іншу початкову дату."
    )
  }

  result
}

project_data_paths <- function(config) {
  cache_paths <- lapply(
    config$exchanges,
    function(exchange) {
      filename <- paste(
        safe_filename_part(exchange$id),
        safe_filename_part(exchange$symbol),
        safe_filename_part(config$study$market_type),
        safe_filename_part(config$study$interval),
        sep = "_"
      )
      file.path("data", "cache", paste0(filename, ".rds"))
    }
  )

  primary <- config$primary

  prepared_name <- paste(
    safe_filename_part(primary$id),
    safe_filename_part(primary$symbol),
    safe_filename_part(config$study$interval),
    "price_returns",
    sep = "_"
  )

  list(
    cache = cache_paths,
    binance_archives = file.path(
      "data",
      "cache",
      "binance-archives"
    ),
    prepared = file.path(
      "data",
      "processed",
      paste0(prepared_name, ".rds")
    )
  )
}

read_project_config <- function(path = "config.yml") {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop(
      "Для читання config.yml потрібен пакет yaml. ",
      "Виконайте renv::restore()."
    )
  }
  if (!file.exists(path)) {
    stop("Не знайдено конфігурацію проєкту: ", path)
  }

  raw_config <- yaml::read_yaml(path)
  if (
    !all(
      c(
        "study",
        "evaluation",
        "visualization",
        "exchanges",
        "runtime"
      ) %in%
        names(raw_config)
    )
  ) {
    stop(
      paste(
        "config.yml повинен містити розділи",
        paste(
          "study, evaluation, visualization,",
          "exchanges і runtime."
        )
      )
    )
  }

  timezone <- require_config_value(
    raw_config$study$timezone,
    "study.timezone"
  )
  if (!timezone %in% OlsonNames()) {
    stop("Невідома часова зона в config.yml: ", timezone)
  }
  if (!identical(timezone, "UTC")) {
    stop("Поточний конвеєр використовує часову зону UTC.")
  }

  interval <- require_config_value(
    raw_config$study$interval,
    "study.interval"
  )
  if (!identical(interval, "1h")) {
    stop(
      "Поточний конвеєр підтримує лише базовий інтервал 1h."
    )
  }

  market_type <- tolower(require_config_value(
    raw_config$study$market_type,
    "study.market_type"
  ))
  if (!identical(market_type, "spot")) {
    stop("Поточний проєкт підтримує лише спотовий ринок.")
  }

  price_field <- tolower(require_config_value(
    raw_config$study$price_field,
    "study.price_field"
  ))
  if (!identical(price_field, "close")) {
    stop("Поточний проєкт визначає ціну лише як close.")
  }

  data_start <- parse_utc_time(
    raw_config$study$data_start,
    "study.data_start",
    timezone
  )
  data_years <- require_integer(
    raw_config$study$data_years,
    "study.data_years",
    minimum = 2
  )
  data_end_exclusive <- calendar_years_after(
    data_start,
    data_years,
    timezone
  )
  exchange_ids <- names(raw_config$exchanges)
  if (length(exchange_ids) == 0L) {
    stop("У config.yml не визначено жодної біржі.")
  }
  exchanges <- lapply(
    exchange_ids,
    function(id) {
      normalize_exchange_config(
        id = id,
        exchange = raw_config$exchanges[[id]],
        timezone = timezone,
        default_start = data_start
      )
    }
  )
  names(exchanges) <- exchange_ids

  candidate_id <- tolower(require_config_value(
    raw_config$study$candidate_exchange,
    "study.candidate_exchange"
  ))
  primary_id <- tolower(require_config_value(
    raw_config$study$primary_exchange,
    "study.primary_exchange"
  ))
  reference_id <- tolower(require_config_value(
    raw_config$study$reference_exchange,
    "study.reference_exchange"
  ))
  unknown_ids <- setdiff(
    c(candidate_id, primary_id, reference_id),
    exchange_ids
  )
  if (length(unknown_ids) > 0L) {
    stop(
      "У config.yml не описано біржі: ",
      paste(unknown_ids, collapse = ", ")
    )
  }
  selected_ids <- c(candidate_id, primary_id, reference_id)
  if (anyDuplicated(selected_ids)) {
    stop(
      paste(
        "Початковий кандидат, основна й контрольна біржі",
        "мають бути різними."
      )
    )
  }

  workers <- require_integer(
    raw_config$runtime$workers,
    "runtime.workers",
    minimum = 1
  )
  auto_download_data <- require_flag(
    raw_config$runtime$auto_download_data,
    "runtime.auto_download_data"
  )

  price_amount_btc <- require_finite_number(
    raw_config$visualization$price_amount_btc,
    "visualization.price_amount_btc",
    minimum = 1e-8,
    maximum = 1
  )
  price_amount_satoshis <- price_amount_btc * 1e8
  if (
    abs(price_amount_satoshis - round(price_amount_satoshis)) >
      1e-6
  ) {
    stop(
      paste(
        "visualization.price_amount_btc має містити",
        "цілу кількість сатоші."
      )
    )
  }

  test_years <- require_integer(
    raw_config$evaluation$test_years,
    "evaluation.test_years",
    minimum = 1
  )
  forecast_horizon_periods <- require_integer(
    raw_config$evaluation$forecast_horizon_periods,
    "evaluation.forecast_horizon_periods",
    minimum = 1
  )
  execution_lag_periods <- require_integer(
    raw_config$evaluation$execution_lag_periods,
    "evaluation.execution_lag_periods",
    minimum = 1
  )
  execution_price <- tolower(require_config_value(
    raw_config$evaluation$execution_price,
    "evaluation.execution_price"
  ))
  if (!identical(execution_price, "next_open")) {
    stop(
      paste(
        "Поточний план оцінювання підтримує лише",
        "evaluation.execution_price = next_open."
      )
    )
  }
  refit_every_months <- require_integer(
    raw_config$evaluation$refit_every_months,
    "evaluation.refit_every_months",
    minimum = 1
  )
  training_window <- tolower(require_config_value(
    raw_config$evaluation$training_window,
    "evaluation.training_window"
  ))
  if (!identical(training_window, "expanding")) {
    stop(
      paste(
        "Поточний план оцінювання підтримує лише",
        "evaluation.training_window = expanding."
      )
    )
  }

  test_start <- calendar_years_before(
    value = data_end_exclusive,
    years = test_years,
    timezone = timezone
  )
  if (test_start <= data_start) {
    stop(
      paste(
        "Навчальний період порожній або занадто короткий:",
        "зменште evaluation.test_years або розширте дані."
      )
    )
  }

  config <- list(
    study = list(
      timezone = timezone,
      market_type = market_type,
      interval = interval,
      price_field = price_field,
      data_start = data_start,
      data_years = data_years,
      data_end_exclusive = data_end_exclusive,
      candidate_exchange = candidate_id,
      primary_exchange = primary_id,
      reference_exchange = reference_id
    ),
    evaluation = list(
      test_years = test_years,
      test_start = test_start,
      test_end_exclusive = data_end_exclusive,
      forecast_horizon_periods = forecast_horizon_periods,
      execution_lag_periods = execution_lag_periods,
      execution_price = execution_price,
      refit_every_months = refit_every_months,
      training_window = training_window
    ),
    visualization = list(
      price_amount_btc = price_amount_btc,
      price_amount_satoshis = as.integer(
        round(price_amount_satoshis)
      )
    ),
    exchanges = exchanges,
    candidate = exchanges[[candidate_id]],
    primary = exchanges[[primary_id]],
    reference = exchanges[[reference_id]],
    runtime = list(
      workers = workers,
      auto_download_data = auto_download_data
    )
  )

  invalid_exchange_starts <- names(config$exchanges)[vapply(
    config$exchanges,
    function(exchange) {
      exchange$available_start >= data_end_exclusive
    },
    logical(1)
  )]
  if (length(invalid_exchange_starts) > 0L) {
    stop(
      "Початок доступності не передує кінцю періоду: ",
      paste(invalid_exchange_starts, collapse = ", ")
    )
  }

  boundary_seconds <- c(
    as.numeric(data_start),
    as.numeric(test_start),
    as.numeric(data_end_exclusive),
    vapply(
      config$exchanges,
      function(exchange) as.numeric(exchange$period_start),
      numeric(1)
    )
  )
  boundary_times <- as.POSIXct(
    boundary_seconds,
    origin = "1970-01-01",
    tz = timezone
  )
  if (
    any(format(boundary_times, "%M:%S", tz = "UTC") != "00:00")
  ) {
    stop(
      "Усі часові межі мають лежати на межі години UTC."
    )
  }

  selected_exchange_ids <- c(
    candidate_id,
    primary_id,
    reference_id
  )
  unavailable_selected <- selected_exchange_ids[vapply(
    selected_exchange_ids,
    function(id) {
      data_start < config$exchanges[[id]]$available_start
    },
    logical(1)
  )]
  if (length(unavailable_selected) > 0L) {
    stop(
      "study.data_start передує доступній історії бірж: ",
      paste(unavailable_selected, collapse = ", ")
    )
  }

  config$primary_start <- data_start
  config$paths <- project_data_paths(config)
  config
}

format_utc <- function(value, include_seconds = FALSE) {
  format(
    value,
    if (include_seconds) "%Y-%m-%d %H:%M:%S" else "%Y-%m-%d %H:%M",
    tz = "UTC"
  )
}

format_utc_period <- function(start_time, end_time) {
  paste(
    format_utc(start_time),
    "-",
    format_utc(end_time)
  )
}

expected_hour_count <- function(start_time, end_time) {
  as.numeric(difftime(end_time, start_time, units = "hours"))
}

same_instant <- function(left, right) {
  length(left) == 1L &&
    length(right) == 1L &&
    !is.na(left) &&
    !is.na(right) &&
    isTRUE(all.equal(as.numeric(left), as.numeric(right)))
}
