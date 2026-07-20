# Bitstamp OHLC data -------------------------------------------------------
#
# This module downloads an independent BTC/USD hourly series from the
# public Bitstamp API. It does not fill or modify Binance observations.

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

read_bitstamp_ohlc_cache <- function(cache_file) {
  if (!file.exists(cache_file)) {
    stop(
      "Не знайдено підготовлений кеш Bitstamp: ",
      cache_file,
      ". Спочатку виконайте: Rscript scripts/refresh_bitstamp_cache.R"
    )
  }

  data <- readRDS(cache_file)
  required_columns <- c(
    "timestamp",
    "open",
    "high",
    "low",
    "close",
    "volume",
    "open_time",
    "exchange",
    "market"
  )
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "Кеш Bitstamp має неочікувану структуру. Відсутні поля: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  data
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
    response <- tryCatch(
      jsonlite::fromJSON(
        request_url,
        simplifyVector = TRUE,
        simplifyDataFrame = TRUE
      ),
      error = function(error) error
    )

    if (!inherits(response, "error")) {
      if (!is.null(response$code) || !is.null(response$status)) {
        stop("Bitstamp повернув помилку для запиту: ", request_url)
      }

      return(normalize_bitstamp_ohlc(
        data = response$data$ohlc,
        market_symbol = market_symbol
      ))
    }

    last_error <- response
    if (attempt < attempts) {
      Sys.sleep(0.5 * attempt)
    }
  }

  stop(
    "Не вдалося отримати пакет Bitstamp після ",
    attempts,
    " спроб: ",
    conditionMessage(last_error)
  )
}

download_bitstamp_ohlc <- function(
  market_symbol,
  start_time,
  end_time,
  step_seconds = 3600L,
  limit = 1000L,
  workers = 4L
) {
  expected_periods <- ceiling(
    as.numeric(difftime(end_time, start_time, units = "secs")) /
      step_seconds
  )
  batch_count <- ceiling(expected_periods / limit)
  final_open_timestamp <- as.numeric(end_time) - step_seconds
  batch_end_timestamps <- final_open_timestamp -
    (seq_len(batch_count) - 1L) * limit * step_seconds

  workers <- max(1L, min(as.integer(workers), batch_count))
  message(
    "Завантаження ",
    batch_count,
    " пакетів Bitstamp BTC/USD у ",
    workers,
    " процесах."
  )

  download_batch <- function(end_timestamp) {
    download_bitstamp_batch(
      market_symbol = market_symbol,
      step_seconds = step_seconds,
      limit = limit,
      end_timestamp = end_timestamp
    )
  }

  if (.Platform$OS.type == "unix" && workers > 1L) {
    batches <- parallel::mclapply(
      batch_end_timestamps,
      download_batch,
      mc.cores = workers,
      mc.preschedule = TRUE
    )
  } else {
    batches <- lapply(batch_end_timestamps, download_batch)
  }

  data <- dplyr::bind_rows(batches) |>
    dplyr::filter(open_time >= start_time, open_time < end_time) |>
    dplyr::arrange(open_time) |>
    dplyr::distinct(open_time, .keep_all = TRUE)

  if (nrow(data) == 0) {
    stop("Bitstamp не повернув жодної свічки у заданих межах.")
  }

  data
}

compare_exchange_coverage <- function(
  reference_data,
  alternative_data,
  start_time,
  end_time
) {
  expected_timestamps <- seq.POSIXt(
    from = start_time,
    to = end_time - 60 * 60,
    by = "hour"
  )
  reference_timestamps <- unique(reference_data$open_time)
  missing_reference <- expected_timestamps[
    !expected_timestamps %in% reference_timestamps
  ]

  alternative_match <- match(
    as.numeric(missing_reference),
    as.numeric(alternative_data$open_time)
  )

  tibble::tibble(
    open_time = missing_reference,
    available_on_alternative = !is.na(alternative_match),
    alternative_close = alternative_data$close[alternative_match]
  )
}
