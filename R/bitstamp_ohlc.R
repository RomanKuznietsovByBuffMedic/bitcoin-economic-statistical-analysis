# Bitstamp OHLC data -------------------------------------------------------
#
# This module downloads an independent BTC/USD hourly series from the
# public Bitstamp API. It does not fill or modify another exchange's data.

empty_bitstamp_ohlc <- function() {
  tibble::tibble(
    timestamp = numeric(),
    open = numeric(),
    high = numeric(),
    low = numeric(),
    close = numeric(),
    volume = numeric(),
    open_time = as.POSIXct(character(), tz = "UTC"),
    exchange = character(),
    market = character()
  )
}

normalize_bitstamp_ohlc <- function(data, market_symbol) {
  if (is.null(data) || nrow(data) == 0) {
    return(empty_bitstamp_ohlc())
  }

  required_columns <- c(
    "timestamp",
    "open",
    "high",
    "low",
    "close",
    "volume"
  )
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "Неочікувана структура Bitstamp. Відсутні поля: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  data[required_columns] <- lapply(
    data[required_columns],
    function(value) suppressWarnings(as.numeric(value))
  )

  data |>
    dplyr::transmute(
      timestamp,
      open,
      high,
      low,
      close,
      volume,
      open_time = as.POSIXct(
        timestamp,
        origin = "1970-01-01",
        tz = "UTC"
      ),
      exchange = "Bitstamp",
      market = toupper(market_symbol)
    )
}

normalize_market_identifier <- function(value) {
  gsub("[^A-Z0-9]", "", toupper(as.character(value)))
}

bitstamp_response_identifiers <- function(response) {
  if (is.null(response$data)) {
    return(character())
  }

  identifiers <- unlist(
    response$data[c("pair", "market")],
    use.names = FALSE
  )
  identifiers <- as.character(identifiers)
  identifiers[
    !is.na(identifiers) & nzchar(identifiers)
  ]
}

normalize_bitstamp_response <- function(
  response,
  market_symbol,
  request_url
) {
  if (!is.list(response) || is.null(response$data)) {
    stop(
      "Bitstamp повернув відповідь без поля data: ",
      request_url
    )
  }

  error_values <- c(
    response$code,
    response$status,
    response$response_code,
    response$response_explanation
  )
  error_values <- as.character(unlist(
    error_values,
    use.names = FALSE
  ))
  error_values <- error_values[
    !is.na(error_values) & nzchar(error_values)
  ]
  if (length(error_values) > 0L) {
    stop(
      "Bitstamp повернув помилку: ",
      paste(error_values, collapse = "; "),
      ". Запит: ",
      request_url
    )
  }

  returned_identifiers <- bitstamp_response_identifiers(response)
  expected_identifier <- normalize_market_identifier(market_symbol)
  normalized_identifiers <- normalize_market_identifier(
    returned_identifiers
  )

  if (
    length(normalized_identifiers) == 0L ||
      any(normalized_identifiers != expected_identifier)
  ) {
    returned_text <- if (length(returned_identifiers) == 0L) {
      "ідентифікатор відсутній"
    } else {
      paste(returned_identifiers, collapse = ", ")
    }
    stop(
      "Bitstamp не підтвердив ринок ",
      market_symbol,
      ". Отримано: ",
      returned_text,
      ". Запит: ",
      request_url
    )
  }

  if (is.null(response$data$ohlc)) {
    stop(
      "Bitstamp повернув відповідь без поля data.ohlc: ",
      request_url
    )
  }

  normalize_bitstamp_ohlc(
    data = response$data$ohlc,
    market_symbol = market_symbol
  )
}

download_bitstamp_batch <- function(
  market_symbol,
  step_seconds,
  limit,
  end_timestamp,
  endpoint = "https://www.bitstamp.net/api/v2/ohlc",
  attempts = 3L
) {
  request_url <- sprintf(
    paste0(
      "%s/%s/?step=%d&limit=%d&end=%.0f",
      "&exclude_current_candle=true"
    ),
    endpoint,
    market_symbol,
    step_seconds,
    limit,
    end_timestamp
  )

  last_error <- NULL
  for (attempt in seq_len(attempts)) {
    result <- tryCatch(
      {
        response <- jsonlite::fromJSON(
          request_url,
          simplifyVector = TRUE,
          simplifyDataFrame = TRUE
        )
        normalize_bitstamp_response(
          response = response,
          market_symbol = market_symbol,
          request_url = request_url
        )
      },
      error = function(error) error
    )

    if (!inherits(result, "error")) {
      return(result)
    }

    last_error <- result
    if (attempt < attempts) {
      Sys.sleep(0.5 * attempt)
    }
  }

  stop(
    "Не вдалося отримати пакет Bitstamp після ",
    attempts,
    " спроб. Запит: ",
    request_url,
    ". Причина: ",
    conditionMessage(last_error)
  )
}

safe_bitstamp_batch <- function(end_timestamp, download_batch) {
  tryCatch(
    list(
      ok = TRUE,
      end_timestamp = end_timestamp,
      data = download_batch(end_timestamp),
      error = NULL
    ),
    error = function(error) {
      list(
        ok = FALSE,
        end_timestamp = end_timestamp,
        data = NULL,
        error = conditionMessage(error)
      )
    }
  )
}

bitstamp_batch_succeeded <- function(result) {
  is.list(result) &&
    isTRUE(result$ok) &&
    inherits(result$data, "data.frame")
}

bitstamp_batch_error <- function(result) {
  if (inherits(result, "try-error")) {
    return(as.character(result))
  }
  if (is.list(result) && !is.null(result$error)) {
    return(as.character(result$error))
  }
  "невідома помилка процесу"
}

download_bitstamp_ohlc <- function(
  market_symbol,
  start_time,
  end_time,
  step_seconds = 3600L,
  limit = 1000L,
  workers = 2L,
  endpoint = "https://www.bitstamp.net/api/v2/ohlc"
) {
  if (
    length(start_time) != 1L ||
      length(end_time) != 1L ||
      is.na(start_time) ||
      is.na(end_time) ||
      start_time >= end_time
  ) {
    stop("Некоректні часові межі для завантаження Bitstamp.")
  }

  expected_periods <- ceiling(
    as.numeric(difftime(end_time, start_time, units = "secs")) /
      step_seconds
  )
  batch_count <- ceiling(expected_periods / limit)
  final_open_timestamp <- as.numeric(end_time) - step_seconds
  batch_end_timestamps <- final_open_timestamp -
    (seq_len(batch_count) - 1L) * limit * step_seconds

  workers <- max(1L, min(as.integer(workers), batch_count))

  download_batch <- function(end_timestamp) {
    download_bitstamp_batch(
      market_symbol = market_symbol,
      step_seconds = step_seconds,
      limit = limit,
      end_timestamp = end_timestamp,
      endpoint = endpoint
    )
  }
  download_safely <- function(end_timestamp) {
    safe_bitstamp_batch(end_timestamp, download_batch)
  }

  results <- download_progress_lapply(
    values = batch_end_timestamps,
    function_to_apply = download_safely,
    workers = workers,
    label = "Завантаження Bitstamp",
    unit = "частин"
  )

  failed <- which(!vapply(
    results,
    bitstamp_batch_succeeded,
    logical(1)
  ))
  if (length(failed) > 0L) {
    message(
      "Повторна послідовна спроба для ",
      length(failed),
      " невдалих пакетів Bitstamp."
    )
    results[failed] <- download_progress_lapply(
      values = batch_end_timestamps[failed],
      function_to_apply = download_safely,
      workers = 1L,
      label = "Повтор Bitstamp",
      unit = "частин"
    )
  }

  failed <- which(!vapply(
    results,
    bitstamp_batch_succeeded,
    logical(1)
  ))
  if (length(failed) > 0L) {
    examples <- failed[seq_len(min(3L, length(failed)))]
    details <- vapply(
      examples,
      function(index) {
        paste0(
          "- end=",
          format(
            as.POSIXct(
              batch_end_timestamps[[index]],
              origin = "1970-01-01",
              tz = "UTC"
            ),
            "%Y-%m-%d %H:%M:%S UTC",
            tz = "UTC"
          ),
          ": ",
          bitstamp_batch_error(results[[index]])
        )
      },
      character(1)
    )
    stop(
      "Не вдалося отримати ",
      length(failed),
      " пакетів Bitstamp.\n",
      paste(details, collapse = "\n")
    )
  }

  batches <- lapply(results, function(result) result$data)

  data <- dplyr::bind_rows(batches) |>
    dplyr::filter(open_time >= start_time, open_time < end_time) |>
    dplyr::arrange(open_time) |>
    dplyr::distinct(open_time, .keep_all = TRUE)

  if (nrow(data) == 0) {
    stop("Bitstamp не повернув жодної свічки у заданих межах.")
  }

  attr(data, "acquisition_info") <- list(
    method = "Bitstamp OHLC API",
    downloaded_at_utc = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S UTC",
      tz = "UTC"
    ),
    endpoint = endpoint,
    market_symbol = market_symbol,
    step_seconds = step_seconds,
    identity_checked = TRUE,
    request_batches = batch_count
  )
  data
}
