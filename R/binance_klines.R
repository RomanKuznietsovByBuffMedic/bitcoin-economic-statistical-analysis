# Binance kline data -------------------------------------------------------
#
# This module is responsible only for:
# - downloading kline data from Binance;
# - reusing the local cache;
# - checking the integrity of hourly observations.

binance_kline_columns <- function() {
  c(
    "open_time_ms",
    "open",
    "high",
    "low",
    "close",
    "volume",
    "close_time_ms",
    "quote_asset_volume",
    "number_of_trades",
    "taker_buy_base_volume",
    "taker_buy_quote_volume",
    "ignore"
  )
}

interval_to_milliseconds <- function(interval) {
  switch(
    interval,
    "1h" = 60 * 60 * 1000,
    stop("Цей розділ підтримує лише інтервал 1h.")
  )
}

download_binance_klines <- function(
  symbol,
  interval,
  start_time,
  end_time,
  endpoint = "https://data-api.binance.vision/api/v3/klines",
  request_pause = 0.10
) {
  interval_ms <- interval_to_milliseconds(interval)
  column_names <- binance_kline_columns()

  cursor_ms <- floor(as.numeric(start_time) * 1000)
  end_ms <- floor(as.numeric(end_time) * 1000)

  batches <- list()
  batch_index <- 1L

  while (cursor_ms < end_ms) {
    request_url <- sprintf(
      paste0(
        "%s?symbol=%s&interval=%s&startTime=%.0f",
        "&endTime=%.0f&limit=1000"
      ),
      endpoint,
      symbol,
      interval,
      cursor_ms,
      end_ms - 1
    )

    response <- jsonlite::fromJSON(
      request_url,
      simplifyVector = TRUE,
      simplifyDataFrame = TRUE,
      simplifyMatrix = TRUE
    )

    if (
      is.list(response) &&
      !is.null(names(response)) &&
      "code" %in% names(response)
    ) {
      stop(
        "Binance повернув помилку ",
        response$code,
        ": ",
        response$msg
      )
    }

    if (length(response) == 0) {
      break
    }

    batch <- as.data.frame(
      response,
      stringsAsFactors = FALSE
    )

    if (ncol(batch) != length(column_names)) {
      stop(
        "Отримано неочікувану структуру даних Binance."
      )
    }

    names(batch) <- column_names

    numeric_columns <- setdiff(
      column_names,
      "ignore"
    )

    batch[numeric_columns] <- lapply(
      batch[numeric_columns],
      as.numeric
    )

    batches[[batch_index]] <- batch
    batch_index <- batch_index + 1L

    last_open_ms <- max(batch$open_time_ms)
    next_cursor_ms <- last_open_ms + interval_ms

    if (next_cursor_ms <= cursor_ms) {
      stop(
        paste(
          "Часовий курсор не змінився.",
          "Завантаження зупинено."
        )
      )
    }

    cursor_ms <- next_cursor_ms

    if (nrow(batch) < 1000) {
      break
    }

    Sys.sleep(request_pause)
  }

  if (length(batches) == 0) {
    stop("Binance не повернув жодної годинної свічки.")
  }

  dplyr::bind_rows(batches) |>
    dplyr::mutate(
      open_time = as.POSIXct(
        open_time_ms / 1000,
        origin = "1970-01-01",
        tz = "UTC"
      ),
      close_time = as.POSIXct(
        close_time_ms / 1000,
        origin = "1970-01-01",
        tz = "UTC"
      )
    ) |>
    dplyr::filter(
      open_time >= start_time,
      open_time < end_time
    ) |>
    dplyr::arrange(open_time) |>
    dplyr::distinct(open_time, .keep_all = TRUE)
}

load_or_download_binance_klines <- function(
  symbol,
  interval,
  start_time,
  end_time,
  cache_file
) {
  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  data <- download_binance_klines(
    symbol = symbol,
    interval = interval,
    start_time = start_time,
    end_time = end_time
  )

  dir.create(
    dirname(cache_file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  saveRDS(
    data,
    cache_file,
    compress = "gzip"
  )

  data
}

validate_hourly_klines <- function(data, start_time, end_time) {
  duplicate_rows <- sum(duplicated(data$open_time))

  clean_data <- data |>
    dplyr::arrange(open_time) |>
    dplyr::distinct(open_time, .keep_all = TRUE)

  gaps <- clean_data |>
    dplyr::transmute(
      open_time,
      hours_from_previous = as.numeric(
        difftime(
          open_time,
          dplyr::lag(open_time),
          units = "hours"
        )
      )
    ) |>
    dplyr::filter(
      !is.na(hours_from_previous),
      hours_from_previous != 1
    )

  nonpositive_prices <- sum(
    !is.finite(clean_data$close) |
      clean_data$close <= 0
  )

  if (nonpositive_prices > 0) {
    stop("Виявлено недодатні або нечислові ціни.")
  }

  expected_rows <- as.numeric(
    difftime(
      end_time,
      start_time,
      units = "hours"
    )
  )

  summary <- tibble::tibble(
    `Перевірка` = c(
      "Очікувана кількість годин",
      "Фактична кількість свічок",
      "Повторені години",
      "Часові розриви",
      "Недодатні або нечислові ціни"
    ),
    `Значення` = c(
      expected_rows,
      nrow(clean_data),
      duplicate_rows,
      nrow(gaps),
      nonpositive_prices
    )
  )

  list(
    data = clean_data,
    gaps = gaps,
    summary = summary
  )
}
