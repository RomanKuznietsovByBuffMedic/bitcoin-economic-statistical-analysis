# Reproducible market-data pipeline --------------------------------------
#
# This module coordinates downloading, validation, provenance and persistence.
# Command-line scripts call these functions and are responsible only for
# presenting progress and summaries to the user.

require_worker_count <- function(value, name = "workers") {
  workers <- suppressWarnings(as.numeric(value))
  if (
    length(workers) != 1L ||
      is.na(workers) ||
      !is.finite(workers) ||
      workers < 1L ||
      workers != floor(workers)
  ) {
    stop(name, " має бути додатним цілим числом.")
  }

  as.integer(workers)
}

runtime_worker_count <- function(config, environment_variable) {
  require_worker_count(
    Sys.getenv(
      environment_variable,
      as.character(config$runtime$workers)
    ),
    environment_variable
  )
}

environment_flag <- function(name, default = FALSE) {
  value <- tolower(trimws(Sys.getenv(
    name,
    if (isTRUE(default)) "true" else "false"
  )))
  if (value %in% c("1", "true", "yes")) {
    return(TRUE)
  }
  if (value %in% c("0", "false", "no")) {
    return(FALSE)
  }

  stop(
    name,
    " має бути одним зі значень: true, false, 1, 0, yes, no."
  )
}

refresh_exchange_cache <- function(
  config,
  exchange_id,
  downloader,
  allow_internal_gaps = identical(
    exchange_id,
    config$candidate$id
  )
) {
  if (!exchange_id %in% names(config$exchanges)) {
    stop("Невідоме джерело даних: ", exchange_id)
  }
  if (!is.function(downloader)) {
    stop("refresh_exchange_cache() очікує функцію завантаження.")
  }

  exchange <- config$exchanges[[exchange_id]]
  cache_file <- config$paths$cache[[exchange_id]]

  with_file_lock(
    target_path = cache_file,
    action = function() {
      downloaded_data <- downloader()
      acquisition_info <- attr(
        downloaded_data,
        "acquisition_info",
        exact = TRUE
      )

      quality <- require_bounded_hourly_ohlc(
        data = downloaded_data,
        start_time = config$study$data_start,
        end_time = config$study$data_end_exclusive,
        source_label = exchange$market_label,
        allow_internal_gaps = allow_internal_gaps
      )

      cache_data <- quality$data
      attr(cache_data, "acquisition_info") <- acquisition_info
      cache_data <- attach_source_metadata(
        data = cache_data,
        config = config,
        exchange_id = exchange_id,
        verification_level = "verified_on_download"
      )

      metadata_check <- check_source_metadata(
        cache_data,
        config,
        exchange_id
      )
      if (!isTRUE(metadata_check$matches)) {
        stop(
          "Внутрішня помилка метаданих перед збереженням ",
          exchange$market_label,
          "."
        )
      }

      save_rds_atomic(cache_data, cache_file)

      list(
        data = cache_data,
        quality = quality,
        cache_file = cache_file,
        sha256 = sha256_file(cache_file),
        acquisition_info = acquisition_info
      )
    }
  )
}

refresh_binance_market_data <- function(
  config,
  workers,
  refresh_checksums = FALSE
) {
  exchange <- config$exchanges$binance
  workers <- require_worker_count(workers, "BINANCE_ARCHIVE_WORKERS")

  refresh_exchange_cache(
    config = config,
    exchange_id = exchange$id,
    downloader = function() {
      download_binance_hybrid_klines(
        symbol = exchange$symbol,
        interval = config$study$interval,
        start_time = config$study$data_start,
        end_time = config$study$data_end_exclusive,
        archive_dir = config$paths$binance_archives,
        workers = workers,
        refresh_checksums = refresh_checksums,
        archive_base_url = exchange$archive_base_url,
        rest_endpoint = exchange$rest_endpoint
      )
    },
    allow_internal_gaps = TRUE
  )
}

refresh_bybit_market_data <- function(config, workers) {
  exchange <- config$exchanges$bybit
  workers <- require_worker_count(workers, "BYBIT_WORKERS")
  bybit_interval <- switch(
    config$study$interval,
    "1h" = "60",
    stop("Bybit: непідтримуваний інтервал.")
  )

  refresh_exchange_cache(
    config = config,
    exchange_id = exchange$id,
    downloader = function() {
      download_bybit_klines(
        start_time = config$study$data_start,
        end_time = config$study$data_end_exclusive,
        category = config$study$market_type,
        symbol = exchange$symbol,
        interval = bybit_interval,
        workers = workers,
        endpoint = exchange$endpoint
      )
    }
  )
}

refresh_bitstamp_market_data <- function(config, workers) {
  exchange <- config$exchanges$bitstamp
  workers <- require_worker_count(workers, "BITSTAMP_WORKERS")
  step_seconds <- switch(
    config$study$interval,
    "1h" = 3600L,
    stop("Bitstamp: непідтримуваний інтервал.")
  )

  refresh_exchange_cache(
    config = config,
    exchange_id = exchange$id,
    downloader = function() {
      download_bitstamp_ohlc(
        market_symbol = exchange$symbol,
        start_time = config$study$data_start,
        end_time = config$study$data_end_exclusive,
        step_seconds = step_seconds,
        workers = workers,
        endpoint = exchange$endpoint
      )
    }
  )
}

data_pipeline_lock_targets <- function(
  config,
  manifest_path = "data-manifest.yml"
) {
  c(
    unname(unlist(config$paths$cache, use.names = FALSE)),
    config$paths$prepared,
    manifest_path
  )
}

write_current_data_manifest <- function(
  config,
  path = "data-manifest.yml"
) {
  with_file_locks(
    target_paths = data_pipeline_lock_targets(config, path),
    action = function() {
      write_data_manifest(config, path)
      validate_data_manifest(config, path)
    }
  )
}

prepare_price_return_files <- function(
  config,
  manifest_path = "data-manifest.yml"
) {
  with_file_locks(
    target_paths = data_pipeline_lock_targets(
      config,
      manifest_path
    ),
    action = function() {
      primary_file <- config$paths$cache[[config$primary$id]]
      primary_raw <- read_rds_required(
        primary_file,
        paste("кеш", config$primary$name)
      )

      source_metadata_summary(
        config = config,
        data_by_exchange = stats::setNames(
          list(primary_raw),
          config$primary$id
        )
      )

      primary_quality <- require_complete_hourly_ohlc(
        data = primary_raw,
        start_time = config$study$data_start,
        end_time = config$study$data_end_exclusive,
        source_label = config$primary$market_label
      )

      raw_sha256 <- sha256_file(primary_file)
      prepared_data <- build_price_return_features(
        data = primary_quality$data,
        price_column = config$study$price_field
      )
      prepared_data <- attach_prepared_metadata(
        data = prepared_data,
        config = config,
        raw_sha256 = raw_sha256
      )

      save_rds_atomic(prepared_data, config$paths$prepared)
      write_data_manifest(config, manifest_path)

      data_split <- split_time_series(
        data = prepared_data,
        training_start = config$study$data_start,
        test_start = config$evaluation$test_start,
        test_end_exclusive = config$evaluation$test_end_exclusive
      )

      list(
        data = prepared_data,
        quality = primary_quality,
        data_split = data_split,
        prepared_file = config$paths$prepared,
        prepared_sha256 = sha256_file(config$paths$prepared),
        manifest_path = manifest_path
      )
    }
  )
}

project_parameter_table <- function(config) {
  tibble::tibble(
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
      "Початок внутрішнього validation",
      "Кінець validation без включення",
      "Тривалість validation, календарних років",
      "Початок фінального тесту",
      "Кінець фінального тесту без включення",
      "Тривалість тесту, календарних років",
      "Найбільший лаг ACF/PACF дохідності",
      "Найбільший лаг ACF масштабу рухів",
      "Кількість точок Q-Q",
      "Рухоме описове вікно, годин",
      "Крок описового вікна, годин"
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
      format_utc(
        config$evaluation$validation_start,
        include_seconds = TRUE
      ),
      format_utc(
        config$evaluation$validation_end_exclusive,
        include_seconds = TRUE
      ),
      config$evaluation$validation_years,
      format_utc(
        config$evaluation$test_start,
        include_seconds = TRUE
      ),
      format_utc(
        config$evaluation$test_end_exclusive,
        include_seconds = TRUE
      ),
      config$evaluation$test_years,
      config$analysis$mean_max_lag,
      config$analysis$dependence_max_lag,
      config$analysis$normal_qq_points,
      config$analysis$rolling_window_hours,
      config$analysis$rolling_step_hours
    )
  )
}

collect_project_data_checks <- function(config) {
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
  require_prepared_metadata(
    data = prepared,
    config = config,
    raw_sha256 = sha256_file(primary_file)
  )

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

  list(
    parameters = project_parameter_table(config),
    provenance = provenance_check,
    manifest = manifest_check,
    time_split = data_split$summary
  )
}
