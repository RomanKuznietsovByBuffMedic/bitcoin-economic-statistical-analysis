# Binance kline data -------------------------------------------------------
#
# This module is responsible only for:
# - downloading kline data from official Binance sources;
# - verifying and reusing local archive and data caches;
# - checking the integrity of hourly observations.
#
# Shared SHA-256 and time-gap helpers are defined in R/project_io.R and
# R/hourly_ohlc_quality.R.

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

empty_binance_klines <- function() {
  columns <- binance_kline_columns()
  data <- as.data.frame(
    setNames(
      replicate(length(columns), numeric(), simplify = FALSE),
      columns
    ),
    stringsAsFactors = FALSE
  )
  data$ignore <- character()
  data$open_time <- as.POSIXct(character(), tz = "UTC")
  data$close_time <- as.POSIXct(character(), tz = "UTC")
  data
}

normalize_epoch_milliseconds <- function(x) {
  x <- as.numeric(x)
  is_microseconds <- is.finite(x) & abs(x) >= 1e14
  x[is_microseconds] <- x[is_microseconds] / 1000
  x
}

normalize_binance_klines <- function(data, start_time, end_time) {
  columns <- binance_kline_columns()

  if (nrow(data) == 0) {
    return(empty_binance_klines())
  }

  if (ncol(data) != length(columns)) {
    stop("Отримано неочікувану структуру даних Binance.")
  }

  names(data) <- columns

  numeric_columns <- setdiff(columns, "ignore")
  data[numeric_columns] <- lapply(
    data[numeric_columns],
    function(x) suppressWarnings(as.numeric(x))
  )

  data <- data[is.finite(data$open_time_ms), , drop = FALSE]
  data$open_time_ms <- normalize_epoch_milliseconds(data$open_time_ms)
  data$close_time_ms <- normalize_epoch_milliseconds(data$close_time_ms)

  data |>
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

download_binance_klines <- function(
  symbol,
  interval,
  start_time,
  end_time,
  endpoint = "https://data-api.binance.vision/api/v3/klines",
  request_pause = 0.05,
  allow_empty = FALSE
) {
  interval_ms <- interval_to_milliseconds(interval)
  columns <- binance_kline_columns()

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

    batch <- as.data.frame(response, stringsAsFactors = FALSE)

    if (ncol(batch) != length(columns)) {
      stop("Отримано неочікувану структуру даних Binance REST API.")
    }

    names(batch) <- columns
    batches[[batch_index]] <- batch
    batch_index <- batch_index + 1L

    last_open_ms <- max(as.numeric(batch$open_time_ms))
    next_cursor_ms <- last_open_ms + interval_ms

    if (next_cursor_ms <= cursor_ms) {
      stop("Часовий курсор не змінився. Завантаження зупинено.")
    }

    cursor_ms <- next_cursor_ms

    if (nrow(batch) < 1000) {
      break
    }

    Sys.sleep(request_pause)
  }

  if (length(batches) == 0) {
    if (allow_empty) {
      return(empty_binance_klines())
    }
    stop("Binance REST API не повернув жодної годинної свічки.")
  }

  data <- normalize_binance_klines(
    data = dplyr::bind_rows(batches),
    start_time = start_time,
    end_time = end_time
  )

  if (nrow(data) == 0 && !allow_empty) {
    stop("Binance REST API не повернув свічок у заданих межах.")
  }

  data
}

read_binance_checksum <- function(path) {
  line <- readLines(path, n = 1L, warn = FALSE)
  if (length(line) == 0) {
    stop("Файл CHECKSUM порожній: ", path)
  }
  tolower(strsplit(trimws(line), "[[:space:]]+")[[1]][[1]])
}

verify_binance_archive <- function(zip_file, checksum_file) {
  identical(
    tolower(sha256_file(zip_file)),
    read_binance_checksum(checksum_file)
  )
}

download_file_atomic <- function(url, destination) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temporary_file <- tempfile(
    pattern = paste0(basename(destination), "."),
    tmpdir = dirname(destination)
  )
  on.exit(unlink(temporary_file), add = TRUE)

  status <- utils::download.file(
    url = url,
    destfile = temporary_file,
    mode = "wb",
    quiet = TRUE
  )
  if (!isTRUE(status == 0) || !file.exists(temporary_file)) {
    stop("Не вдалося завантажити: ", url)
  }

  if (!file.copy(temporary_file, destination, overwrite = TRUE)) {
    stop("Не вдалося зберегти: ", destination)
  }

  invisible(destination)
}

read_binance_archive <- function(zip_file, start_time, end_time) {
  archive_contents <- utils::unzip(zip_file, list = TRUE)
  csv_names <- archive_contents$Name[
    grepl("[.]csv$", archive_contents$Name, ignore.case = TRUE)
  ]
  if (length(csv_names) != 1L) {
    stop("У ZIP-архіві очікувався один CSV-файл: ", zip_file)
  }

  connection <- unz(zip_file, csv_names[[1]], open = "r")
  on.exit(close(connection), add = TRUE)
  data <- utils::read.csv(
    connection,
    header = FALSE,
    stringsAsFactors = FALSE,
    colClasses = "character"
  )

  normalize_binance_klines(
    data = data,
    start_time = start_time,
    end_time = end_time
  )
}

download_binance_monthly_archive <- function(
  symbol,
  interval,
  month,
  start_time,
  end_time,
  archive_dir,
  refresh_checksum = FALSE,
  base_url = "https://data.binance.vision/data/spot/monthly/klines"
) {
  month <- as.Date(month, origin = "1970-01-01")
  month_label <- format(month, "%Y-%m")
  archive_name <- sprintf("%s-%s-%s.zip", symbol, interval, month_label)
  archive_url <- paste(base_url, symbol, interval, archive_name, sep = "/")

  local_dir <- file.path(archive_dir, symbol, interval)
  zip_file <- file.path(local_dir, archive_name)
  checksum_file <- paste0(zip_file, ".CHECKSUM")

  cache_is_valid <- file.exists(zip_file) &&
    file.exists(checksum_file) &&
    isTRUE(tryCatch(
      verify_binance_archive(zip_file, checksum_file),
      error = function(error) FALSE
    ))

  checksum_updated <- FALSE
  if (!cache_is_valid || isTRUE(refresh_checksum)) {
    checksum_updated <- tryCatch(
      {
        download_file_atomic(paste0(archive_url, ".CHECKSUM"), checksum_file)
        TRUE
      },
      error = function(error) {
        if (!cache_is_valid) {
          stop(error)
        }
        FALSE
      }
    )
  }

  archive_matches_checksum <- file.exists(zip_file) &&
    file.exists(checksum_file) &&
    isTRUE(tryCatch(
      verify_binance_archive(zip_file, checksum_file),
      error = function(error) FALSE
    ))

  if (!archive_matches_checksum) {
    download_file_atomic(archive_url, zip_file)

    if (!verify_binance_archive(zip_file, checksum_file)) {
      stop("SHA-256 архіву не відповідає офіційному CHECKSUM: ", archive_name)
    }
  }

  if (isTRUE(refresh_checksum) && !checksum_updated && cache_is_valid) {
    message("Використано перевірений локальний архів: ", archive_name)
  }

  read_binance_archive(
    zip_file = zip_file,
    start_time = start_time,
    end_time = end_time
  )
}

next_month <- function(month) {
  seq.Date(
    as.Date(month, origin = "1970-01-01"),
    by = "month",
    length.out = 2L
  )[[2]]
}

complete_months_in_period <- function(start_time, end_time) {
  first_month <- as.Date(format(start_time, "%Y-%m-01", tz = "UTC"))
  final_month <- as.Date(format(end_time, "%Y-%m-01", tz = "UTC"))
  candidates <- seq.Date(first_month, final_month, by = "month")

  candidates[vapply(
    candidates,
    function(month) {
      as.POSIXct(next_month(month), tz = "UTC") <= end_time
    },
    logical(1)
  )]
}

recheck_binance_gaps <- function(
  data,
  symbol,
  interval,
  endpoint
) {
  gaps <- find_hourly_gaps(data)
  if (nrow(gaps) == 0) {
    return(list(data = data, recovered_rows = 0L))
  }

  recovered <- download_progress_lapply(
    values = seq_len(nrow(gaps)),
    function_to_apply = function(index) {
      download_binance_klines(
        symbol = symbol,
        interval = interval,
        start_time = gaps$previous_observation[[index]] + 60 * 60,
        end_time = gaps$next_observation[[index]],
        endpoint = endpoint,
        request_pause = 0,
        allow_empty = TRUE
      )
    },
    workers = 1L,
    label = "Перевірка розривів",
    unit = "розривів"
  )

  recovered_data <- dplyr::bind_rows(recovered)
  combined <- dplyr::bind_rows(data, recovered_data) |>
    dplyr::arrange(open_time) |>
    dplyr::distinct(open_time, .keep_all = TRUE)

  list(
    data = combined,
    recovered_rows = nrow(combined) - nrow(data)
  )
}

download_binance_hybrid_klines <- function(
  symbol,
  interval,
  start_time,
  end_time,
  archive_dir = "data/cache/binance-archives",
  workers = 2L,
  refresh_checksums = FALSE,
  archive_base_url =
    "https://data.binance.vision/data/spot/monthly/klines",
  rest_endpoint =
    "https://data-api.binance.vision/api/v3/klines"
) {
  months <- complete_months_in_period(start_time, end_time)
  workers <- max(1L, as.integer(workers))

  download_month <- function(month) {
    month <- as.Date(month, origin = "1970-01-01")
    month_start <- as.POSIXct(month, tz = "UTC")
    month_end <- as.POSIXct(next_month(month), tz = "UTC")
    period_start <- max(start_time, month_start)
    period_end <- min(end_time, month_end)

    tryCatch(
      list(
        data = download_binance_monthly_archive(
          symbol = symbol,
          interval = interval,
          month = month,
          start_time = period_start,
          end_time = period_end,
          archive_dir = archive_dir,
          refresh_checksum = refresh_checksums,
          base_url = archive_base_url
        ),
        source = "archive",
        month = format(month, "%Y-%m"),
        archive_error = NA_character_
      ),
      error = function(archive_error) {
        list(
          data = download_binance_klines(
            symbol = symbol,
            interval = interval,
            start_time = period_start,
            end_time = period_end,
            endpoint = rest_endpoint
          ),
          source = "rest_fallback",
          month = format(month, "%Y-%m"),
          archive_error = conditionMessage(archive_error)
        )
      }
    )
  }

  if (length(months) == 0) {
    monthly_results <- list()
  } else {
    monthly_results <- download_progress_lapply(
      values = months,
      function_to_apply = download_month,
      workers = min(workers, length(months)),
      label = "Завантаження Binance",
      unit = "місяців"
    )
  }

  data_parts <- lapply(monthly_results, `[[`, "data")

  if (length(months) == 0) {
    tail_start <- start_time
  } else {
    tail_start <- max(
      start_time,
      as.POSIXct(next_month(tail(months, 1L)), tz = "UTC")
    )
  }

  rest_tail_rows <- 0L
  if (tail_start < end_time) {
    cat("Binance: додатковий неповний місяць.\n")
    rest_tail <- download_binance_klines(
      symbol = symbol,
      interval = interval,
      start_time = tail_start,
      end_time = end_time,
      endpoint = rest_endpoint
    )
    data_parts[[length(data_parts) + 1L]] <- rest_tail
    rest_tail_rows <- nrow(rest_tail)
    cat("[ГОТОВО] Binance: неповний місяць.\n")
  }

  data <- dplyr::bind_rows(data_parts) |>
    dplyr::filter(open_time >= start_time, open_time < end_time) |>
    dplyr::arrange(open_time) |>
    dplyr::distinct(open_time, .keep_all = TRUE)

  if (nrow(data) == 0) {
    stop("Офіційні джерела Binance не повернули жодної свічки.")
  }

  gaps_before <- find_hourly_gaps(data)
  rechecked <- recheck_binance_gaps(
    data = data,
    symbol = symbol,
    interval = interval,
    endpoint = rest_endpoint
  )
  data <- rechecked$data
  gaps_after <- find_hourly_gaps(data)

  sources <- vapply(monthly_results, `[[`, character(1), "source")
  fallback_months <- vapply(
    monthly_results[sources == "rest_fallback"],
    `[[`,
    character(1),
    "month"
  )
  fallback_errors <- vapply(
    monthly_results[sources == "rest_fallback"],
    `[[`,
    character(1),
    "archive_error"
  )

  acquisition_info <- list(
    method = "Місячні архіви Binance Vision + REST API",
    downloaded_at_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
    archive_base_url = archive_base_url,
    rest_endpoint = rest_endpoint,
    symbol = symbol,
    interval = interval,
    archive_checksums_verified = TRUE,
    archive_months_requested = length(months),
    archive_months_used = sum(sources == "archive"),
    rest_fallback_months = fallback_months,
    rest_fallback_errors = fallback_errors,
    rest_tail_rows = rest_tail_rows,
    refreshed_remote_checksums = isTRUE(refresh_checksums),
    gap_hours_before_recheck = sum(gaps_before$missing_hours),
    gap_rows_recovered = rechecked$recovered_rows,
    remaining_gap_hours = sum(gaps_after$missing_hours)
  )
  attr(data, "acquisition_info") <- acquisition_info
  data
}
