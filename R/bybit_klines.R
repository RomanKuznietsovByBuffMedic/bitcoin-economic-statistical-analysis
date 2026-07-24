# Bybit public spot kline data --------------------------------------------
#
# This module performs a read-only market-data audit through Bybit V5.
# It does not authenticate, place orders, or use a trading account.

empty_bybit_klines <- function() {
  tibble::tibble(
    open_time_ms = numeric(),
    open = numeric(),
    high = numeric(),
    low = numeric(),
    close = numeric(),
    volume = numeric(),
    turnover = numeric(),
    open_time = as.POSIXct(character(), tz = "UTC")
  )
}

bybit_request_url <- function(
  endpoint,
  category,
  symbol,
  interval,
  start_ms,
  end_ms,
  limit
) {
  sprintf(
    paste0(
      "%s?category=%s&symbol=%s&interval=%s",
      "&start=%.0f&end=%.0f&limit=%d"
    ),
    endpoint,
    utils::URLencode(category, reserved = TRUE),
    utils::URLencode(symbol, reserved = TRUE),
    utils::URLencode(interval, reserved = TRUE),
    start_ms,
    end_ms,
    limit
  )
}

download_bybit_window <- function(
  start_ms,
  end_ms,
  category = "spot",
  symbol = "BTCUSDT",
  interval = "60",
  limit = 1000L,
  endpoint = "https://api.bybit.com/v5/market/kline",
  attempts = 4L
) {
  hour_ms <- 60 * 60 * 1000
  api_end_ms <- end_ms - hour_ms

  if (api_end_ms < start_ms) {
    return(empty_bybit_klines())
  }

  # Bybit treats both start and end as candle timestamps. Passing the
  # exclusive window end would describe 1001 hours and, with limit = 1000,
  # drop the earliest candle because results are returned newest first.
  url <- bybit_request_url(
    endpoint = endpoint,
    category = category,
    symbol = symbol,
    interval = interval,
    start_ms = start_ms,
    end_ms = api_end_ms,
    limit = limit
  )

  response <- NULL
  last_error <- NULL
  for (attempt in seq_len(attempts)) {
    response <- tryCatch(
      jsonlite::fromJSON(url, simplifyVector = FALSE),
      error = function(error) {
        last_error <<- conditionMessage(error)
        NULL
      }
    )
    if (!is.null(response)) {
      break
    }
    Sys.sleep(min(2^(attempt - 1L), 4))
  }

  if (is.null(response)) {
    stop("Bybit API недоступний після повторів: ", last_error)
  }
  if (!identical(as.integer(response$retCode), 0L)) {
    stop(
      "Bybit повернув помилку ",
      response$retCode,
      ": ",
      response$retMsg
    )
  }

  returned_category <- as.character(response$result$category)
  returned_symbol <- as.character(response$result$symbol)
  if (
    length(returned_category) != 1L ||
      !identical(tolower(returned_category), tolower(category)) ||
      length(returned_symbol) != 1L ||
      !identical(toupper(returned_symbol), toupper(symbol))
  ) {
    stop(
      paste(
        "Bybit повернув дані іншого ринку або символу:",
        returned_category,
        returned_symbol
      )
    )
  }

  rows <- response$result$list
  if (length(rows) == 0L) {
    return(empty_bybit_klines())
  }

  matrix_data <- do.call(rbind, rows)
  if (ncol(matrix_data) < 7L) {
    stop("Отримано неочікувану структуру Bybit Kline.")
  }

  result <- tibble::tibble(
    open_time_ms = as.numeric(matrix_data[, 1]),
    open = as.numeric(matrix_data[, 2]),
    high = as.numeric(matrix_data[, 3]),
    low = as.numeric(matrix_data[, 4]),
    close = as.numeric(matrix_data[, 5]),
    volume = as.numeric(matrix_data[, 6]),
    turnover = as.numeric(matrix_data[, 7]),
    open_time = as.POSIXct(
      as.numeric(matrix_data[, 1]) / 1000,
      origin = "1970-01-01",
      tz = "UTC"
    )
  )

  result |>
    dplyr::filter(
      open_time_ms >= start_ms,
      open_time_ms < end_ms
    ) |>
    dplyr::arrange(open_time) |>
    dplyr::distinct(open_time, .keep_all = TRUE)
}

download_bybit_klines <- function(
  start_time,
  end_time,
  category = "spot",
  symbol = "BTCUSDT",
  interval = "60",
  workers = 2L,
  endpoint = "https://api.bybit.com/v5/market/kline"
) {
  if (
    length(start_time) != 1L ||
      length(end_time) != 1L ||
      is.na(start_time) ||
      is.na(end_time) ||
      start_time >= end_time
  ) {
    stop("Некоректні часові межі для завантаження Bybit.")
  }

  hour_ms <- 60 * 60 * 1000
  start_ms <- as.numeric(start_time) * 1000
  end_ms <- as.numeric(end_time) * 1000
  window_ms <- 1000 * hour_ms
  window_starts <- seq(start_ms, end_ms - 1, by = window_ms)
  windows <- lapply(
    window_starts,
    function(window_start) {
      c(
        start = window_start,
        end = min(window_start + window_ms, end_ms)
      )
    }
  )

  fetch_window <- function(window) {
    download_bybit_window(
      start_ms = window[["start"]],
      end_ms = window[["end"]],
      category = category,
      symbol = symbol,
      interval = interval,
      endpoint = endpoint
    )
  }

  workers <- max(1L, min(as.integer(workers), length(windows)))
  batches <- download_progress_lapply(
    values = windows,
    function_to_apply = fetch_window,
    workers = workers,
    label = "Завантаження Bybit",
    unit = "частин"
  )

  failed <- vapply(batches, inherits, logical(1), what = "try-error")
  if (any(failed)) {
    stop("Не вдалося отримати ", sum(failed), " пакетів Bybit.")
  }

  data <- dplyr::bind_rows(batches) |>
    dplyr::filter(open_time >= start_time, open_time < end_time) |>
    dplyr::arrange(open_time) |>
    dplyr::distinct(open_time, .keep_all = TRUE)

  if (nrow(data) == 0L) {
    stop(
      "Bybit не повернув свічок ",
      symbol,
      " у заданому періоді."
    )
  }

  attr(data, "acquisition_info") <- list(
    method = "Bybit V5 market kline API",
    downloaded_at_utc = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S UTC",
      tz = "UTC"
    ),
    endpoint = endpoint,
    category = category,
    symbol = symbol,
    interval = interval,
    identity_checked = TRUE,
    request_windows = length(windows)
  )
  data
}
