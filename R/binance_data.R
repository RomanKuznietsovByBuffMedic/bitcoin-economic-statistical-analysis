# Binance BTCUSDT data pipeline
#
# The pipeline uses only base R and jsonlite.
# It never installs packages automatically.
#
# Main behavior:
# 1. Existing verified archives are reused.
# 2. Only missing monthly or daily archives are downloaded.
# 3. Every archive is checked against its official SHA-256 checksum.
# 4. Parsed archive chunks are cached as RDS files.
# 5. If the archive manifest has not changed, processed data are loaded directly.
# 6. If new archives appear, only new or changed chunks are parsed.
# 7. The final 1-minute, 1-hour, 4-hour and 1-day data are saved as RDS files.

btc_default_config <- function(project_root = ".") {
  list(
    project_root = normalizePath(project_root, winslash = "/", mustWork = TRUE),
    exchange = "binance",
    market_type = "spot",
    symbol = "BTCUSDT",
    interval = "1m",
    start_time = as.POSIXct("2017-08-17 04:00:00", tz = "UTC"),
    end_date = "latest",
    raw_dir = file.path(
      project_root, "data", "raw", "binance", "spot", "BTCUSDT", "1m"
    ),
    cache_dir = file.path(
      project_root, "data", "cache", "binance", "spot", "BTCUSDT", "1m"
    ),
    processed_dir = file.path(
      project_root, "data", "processed", "binance", "spot", "BTCUSDT"
    ),
    validation_dir = file.path(
      project_root, "data", "validation", "binance", "spot", "BTCUSDT"
    ),
    base_url = "https://data.binance.vision/data/spot",
    api_url = "https://data-api.binance.vision/api/v3/klines",
    api_sample_size = 5L,
    strict_validation = TRUE,
    quiet_downloads = FALSE
  )
}

btc_require_packages <- function() {
  required <- c("digest", "jsonlite")
  missing <- required[!vapply(
    required,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )]

  if (length(missing) > 0L) {
    stop(
      "Потрібний пакет відсутній: ",
      paste(missing, collapse = ", "),
      ". Скрипт не встановлює пакети автоматично.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

btc_make_dirs <- function(config) {
  dirs <- c(
    config$raw_dir,
    config$cache_dir,
    config$processed_dir,
    config$validation_dir
  )
  invisible(vapply(
    dirs,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE,
    FUN.VALUE = logical(1)
  ))
}

btc_utc_today <- function() {
  as.Date(format(Sys.time(), tz = "UTC", format = "%Y-%m-%d"))
}

btc_download_atomic <- function(url, destination, quiet = FALSE) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)

  temporary <- tempfile(
    pattern = paste0(basename(destination), "."),
    tmpdir = dirname(destination)
  )

  on.exit(unlink(temporary), add = TRUE)

  status <- tryCatch(
    utils::download.file(
      url = url,
      destfile = temporary,
      mode = "wb",
      quiet = quiet
    ),
    error = function(error) {
      if (!quiet) {
        message("Не вдалося завантажити: ", url)
        message("Причина: ", conditionMessage(error))
      }
      return(1L)
    }
  )

  if (!identical(status, 0L) || !file.exists(temporary)) {
    return(FALSE)
  }

  if (!file.rename(temporary, destination)) {
    copied <- file.copy(temporary, destination, overwrite = TRUE)
    if (!copied) {
      return(FALSE)
    }
  }

  TRUE
}

btc_read_expected_checksum <- function(checksum_file) {
  if (!file.exists(checksum_file)) {
    return(NA_character_)
  }

  line <- readLines(checksum_file, n = 1L, warn = FALSE)
  if (length(line) == 0L) {
    return(NA_character_)
  }

  fields <- strsplit(trimws(line), "[[:space:]]+")[[1]]
  if (length(fields) == 0L) {
    return(NA_character_)
  }

  value <- tolower(fields[[1]])
  if (!grepl("^[0-9a-f]{64}$", value)) {
    return(NA_character_)
  }

  value
}

btc_verify_checksum <- function(zip_file, checksum_file) {
  expected <- btc_read_expected_checksum(checksum_file)
  if (is.na(expected) || !file.exists(zip_file)) {
    return(FALSE)
  }

  actual <- tolower(digest::digest(zip_file, algo = "sha256", file = TRUE))
  identical(actual, expected)
}

btc_archive_spec <- function(config, period, date) {
  stopifnot(period %in% c("monthly", "daily"))

  suffix <- if (period == "monthly") {
    format(as.Date(date), "%Y-%m")
  } else {
    format(as.Date(date), "%Y-%m-%d")
  }

  filename <- sprintf(
    "%s-%s-%s.zip",
    config$symbol,
    config$interval,
    suffix
  )

  relative <- file.path(
    period,
    "klines",
    config$symbol,
    config$interval,
    filename
  )

  list(
    period = period,
    date = as.Date(date),
    filename = filename,
    url = paste(config$base_url, relative, sep = "/"),
    zip_file = file.path(config$raw_dir, period, filename),
    checksum_file = file.path(
      config$raw_dir,
      period,
      paste0(filename, ".CHECKSUM")
    )
  )
}

btc_fetch_archive <- function(spec, quiet = FALSE, optional = FALSE) {
  checksum_url <- paste0(spec$url, ".CHECKSUM")

  if (file.exists(spec$zip_file) &&
      file.exists(spec$checksum_file) &&
      btc_verify_checksum(spec$zip_file, spec$checksum_file)) {
    expected <- btc_read_expected_checksum(spec$checksum_file)
    return(c(spec, list(checksum = expected, reused = TRUE)))
  }

  unlink(c(spec$zip_file, spec$checksum_file))

  ok <- btc_download_atomic(
    checksum_url,
    spec$checksum_file,
    quiet = quiet
  )

  if (!ok) {
    if (!optional) {
      stop("Не вдалося отримати checksum: ", checksum_url, call. = FALSE)
    }
    return(NULL)
  }

  expected <- btc_read_expected_checksum(spec$checksum_file)
  if (is.na(expected)) {
    unlink(spec$checksum_file)
    if (!optional) {
      stop(
        "Некоректний checksum-файл: ",
        spec$checksum_file,
        call. = FALSE
      )
    }
    return(NULL)
  }

  ok <- btc_download_atomic(
    spec$url,
    spec$zip_file,
    quiet = quiet
  )

  if (!ok) {
    unlink(spec$checksum_file)
    if (!optional) {
      stop("Не вдалося отримати архів: ", spec$url, call. = FALSE)
    }
    return(NULL)
  }

  if (!btc_verify_checksum(spec$zip_file, spec$checksum_file)) {
    unlink(c(spec$zip_file, spec$checksum_file))
    stop(
      "SHA-256 не збігається для архіву: ",
      spec$filename,
      call. = FALSE
    )
  }

  c(spec, list(checksum = expected, reused = FALSE))
}

btc_find_latest_available_date <- function(config, lookback_days = 14L) {
  candidates <- rev(seq(
    from = btc_utc_today() - lookback_days,
    to = btc_utc_today() - 1L,
    by = "day"
  ))

  for (index in seq_along(candidates)) {
    candidate <- candidates[index]
    spec <- btc_archive_spec(config, "daily", candidate)
    archive <- btc_fetch_archive(
      spec,
      quiet = TRUE,
      optional = TRUE
    )
    if (!is.null(archive)) {
      return(as.Date(candidate))
    }
  }

  stop(
    "Не знайдено жодного доступного денного архіву за останні ",
    lookback_days,
    " днів.",
    call. = FALSE
  )
}

btc_resolve_end_date <- function(config) {
  value <- config$end_date

  if (inherits(value, "Date")) {
    return(value)
  }

  if (is.character(value) &&
      length(value) == 1L &&
      identical(tolower(value), "latest")) {
    return(btc_find_latest_available_date(config))
  }

  parsed <- as.Date(value)
  if (is.na(parsed)) {
    stop(
      "end_date має бути датою у форматі YYYY-MM-DD або значенням latest.",
      call. = FALSE
    )
  }

  parsed
}

btc_month_start <- function(date) {
  as.Date(format(as.Date(date), "%Y-%m-01"))
}

btc_month_end <- function(date) {
  next_month <- seq(
    from = btc_month_start(date),
    by = "month",
    length.out = 2L
  )[[2]]
  next_month - 1L
}

btc_collect_archives <- function(config, end_date) {
  start_date <- as.Date(config$start_time, tz = "UTC")
  months <- seq(
    from = btc_month_start(start_date),
    to = btc_month_start(end_date),
    by = "month"
  )

  selected <- list()

  for (index in seq_along(months)) {
    month <- months[index]
    month_first <- max(as.Date(month), start_date)
    month_last <- min(btc_month_end(month), end_date)
    can_use_monthly <- btc_month_end(month) <= end_date

    monthly_archive <- NULL

    if (can_use_monthly) {
      monthly_archive <- btc_fetch_archive(
        btc_archive_spec(config, "monthly", month),
        quiet = config$quiet_downloads,
        optional = TRUE
      )
    }

    if (!is.null(monthly_archive)) {
      selected[[length(selected) + 1L]] <- monthly_archive
      next
    }

    days <- seq(month_first, month_last, by = "day")

    for (day_index in seq_along(days)) {
      day <- days[day_index]
      daily_archive <- btc_fetch_archive(
        btc_archive_spec(config, "daily", day),
        quiet = config$quiet_downloads,
        optional = FALSE
      )
      selected[[length(selected) + 1L]] <- daily_archive
    }
  }

  selected
}

btc_archives_manifest <- function(archives, end_date) {
  files <- vapply(
    archives,
    function(item) normalizePath(
      item$zip_file,
      winslash = "/",
      mustWork = TRUE
    ),
    FUN.VALUE = character(1)
  )

  info <- file.info(files)

  data.frame(
    filename = basename(files),
    period = vapply(
      archives,
      function(item) item$period,
      FUN.VALUE = character(1)
    ),
    date = as.Date(vapply(
      archives,
      function(item) as.character(item$date),
      FUN.VALUE = character(1)
    )),
    bytes = unname(info$size),
    checksum = vapply(
      archives,
      function(item) item$checksum,
      FUN.VALUE = character(1)
    ),
    end_date = as.Date(end_date),
    stringsAsFactors = FALSE
  )
}

btc_manifests_equal <- function(old_manifest, new_manifest) {
  if (is.null(old_manifest) || is.null(new_manifest)) {
    return(FALSE)
  }

  columns <- c(
    "filename",
    "period",
    "date",
    "bytes",
    "checksum",
    "end_date"
  )

  if (!all(columns %in% names(old_manifest)) ||
      !all(columns %in% names(new_manifest))) {
    return(FALSE)
  }

  identical(
    old_manifest[, columns, drop = FALSE],
    new_manifest[, columns, drop = FALSE]
  )
}

btc_timestamp_to_posix <- function(values) {
  numeric_values <- suppressWarnings(as.numeric(values))

  if (anyNA(numeric_values)) {
    stop("Часова мітка містить нечислове значення.", call. = FALSE)
  }

  divisor <- if (stats::median(numeric_values) >= 1e14) {
    1e6
  } else {
    1e3
  }

  as.POSIXct(numeric_values / divisor, origin = "1970-01-01", tz = "UTC")
}

btc_read_archive_csv <- function(zip_file) {
  members <- utils::unzip(zip_file, list = TRUE)
  csv_members <- members$Name[grepl("\\.csv$", members$Name, ignore.case = TRUE)]

  if (length(csv_members) != 1L) {
    stop(
      "Очікувався один CSV-файл в архіві: ",
      basename(zip_file),
      call. = FALSE
    )
  }

  connection <- unz(zip_file, csv_members[[1]], open = "r")
  on.exit(close(connection), add = TRUE)

  raw <- utils::read.csv(
    connection,
    header = FALSE,
    colClasses = "character",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (ncol(raw) != 12L) {
    stop(
      "Некоректна кількість колонок в архіві: ",
      basename(zip_file),
      ". Отримано ",
      ncol(raw),
      ", очікувалося 12.",
      call. = FALSE
    )
  }

  if (!grepl("^[0-9]+$", raw[[1]][[1]])) {
    raw <- raw[-1L, , drop = FALSE]
  }

  names(raw) <- c(
    "open_time",
    "open",
    "high",
    "low",
    "close",
    "base_volume",
    "close_time",
    "quote_volume",
    "number_of_trades",
    "taker_buy_base_volume",
    "taker_buy_quote_volume",
    "ignore"
  )

  numeric_columns <- c(
    "open",
    "high",
    "low",
    "close",
    "base_volume",
    "quote_volume",
    "number_of_trades",
    "taker_buy_base_volume",
    "taker_buy_quote_volume"
  )

  raw$open_time <- btc_timestamp_to_posix(raw$open_time)
  raw$close_time <- btc_timestamp_to_posix(raw$close_time)

  for (column in numeric_columns) {
    raw[[column]] <- suppressWarnings(as.numeric(raw[[column]]))
    if (anyNA(raw[[column]])) {
      stop(
        "Колонка ",
        column,
        " містить нечислові значення в архіві ",
        basename(zip_file),
        ".",
        call. = FALSE
      )
    }
  }

  raw$ignore <- NULL
  raw$exchange <- factor("Binance")
  raw$market_type <- factor("spot")
  raw$symbol <- factor("BTCUSDT")
  raw$source_file <- factor(basename(zip_file))
  raw$downloaded_at <- as.POSIXct(
    file.info(zip_file)$mtime,
    tz = "UTC"
  )

  raw[, c(
    "exchange",
    "market_type",
    "symbol",
    "open_time",
    "close_time",
    "open",
    "high",
    "low",
    "close",
    "base_volume",
    "quote_volume",
    "number_of_trades",
    "taker_buy_base_volume",
    "taker_buy_quote_volume",
    "source_file",
    "downloaded_at"
  )]
}

btc_chunk_cache_path <- function(config, archive) {
  file.path(
    config$cache_dir,
    archive$period,
    sub("\\.zip$", ".rds", archive$filename)
  )
}

btc_load_or_parse_chunk <- function(config, archive) {
  cache_file <- btc_chunk_cache_path(config, archive)
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(cache_file)) {
    cached <- readRDS(cache_file)

    if (is.list(cached) &&
        identical(cached$checksum, archive$checksum) &&
        is.data.frame(cached$data)) {
      return(cached$data)
    }
  }

  data <- btc_read_archive_csv(archive$zip_file)

  saveRDS(
    list(
      checksum = archive$checksum,
      source_file = archive$filename,
      data = data
    ),
    cache_file,
    compress = "gzip"
  )

  data
}

btc_bind_chunks <- function(chunks, start_time, end_date) {
  data <- do.call(rbind, chunks)
  data <- data[order(data$open_time), , drop = FALSE]

  duplicate_rows <- duplicated(data$open_time)
  if (any(duplicate_rows)) {
    data <- data[!duplicate_rows, , drop = FALSE]
  }

  end_exclusive <- as.POSIXct(
    paste(as.Date(end_date) + 1L, "00:00:00"),
    tz = "UTC"
  )

  keep <- data$open_time >= start_time &
    data$open_time < end_exclusive

  data <- data[keep, , drop = FALSE]
  rownames(data) <- NULL
  data
}

btc_aggregate_ohlcv <- function(data, seconds, label) {
  stopifnot(seconds %% 60 == 0)

  time_numeric <- as.numeric(data$open_time)
  group_id <- floor(time_numeric / seconds)
  runs <- rle(group_id)

  end_index <- cumsum(runs$lengths)
  start_index <- c(1L, head(end_index, -1L) + 1L)
  group_factor <- factor(group_id, levels = runs$values)

  sum_columns <- c(
    "base_volume",
    "quote_volume",
    "number_of_trades",
    "taker_buy_base_volume",
    "taker_buy_quote_volume"
  )

  sums <- lapply(
    data[sum_columns],
    function(values) {
      as.numeric(rowsum(values, group_factor, reorder = FALSE))
    }
  )

  high_values <- as.numeric(tapply(
    data$high,
    group_factor,
    max,
    na.rm = TRUE
  ))

  low_values <- as.numeric(tapply(
    data$low,
    group_factor,
    min,
    na.rm = TRUE
  ))

  expected_minutes <- as.integer(seconds / 60)

  result <- data.frame(
    exchange = factor("Binance"),
    market_type = factor("spot"),
    symbol = factor("BTCUSDT"),
    interval = factor(label),
    open_time = as.POSIXct(
      runs$values * seconds,
      origin = "1970-01-01",
      tz = "UTC"
    ),
    close_time = as.POSIXct(
      runs$values * seconds + seconds - 0.000001,
      origin = "1970-01-01",
      tz = "UTC"
    ),
    open = data$open[start_index],
    high = high_values,
    low = low_values,
    close = data$close[end_index],
    base_volume = sums$base_volume,
    quote_volume = sums$quote_volume,
    number_of_trades = sums$number_of_trades,
    taker_buy_base_volume = sums$taker_buy_base_volume,
    taker_buy_quote_volume = sums$taker_buy_quote_volume,
    observations = runs$lengths,
    is_complete = runs$lengths == expected_minutes,
    stringsAsFactors = FALSE
  )

  result$log_close <- log(result$close)
  result$log_return <- c(
    NA_real_,
    100 * diff(result$log_close)
  )
  result$simple_return <- c(
    NA_real_,
    100 * (result$close[-1L] / result$close[-nrow(result)] - 1)
  )
  result$high_low_range <- 100 * log(result$high / result$low)
  result$taker_buy_share <- ifelse(
    result$quote_volume > 0,
    result$taker_buy_quote_volume / result$quote_volume,
    NA_real_
  )

  result
}

btc_validate_1m <- function(data, config, end_date) {
  times <- as.numeric(data$open_time)
  unique_times <- unique(times)
  differences <- diff(unique_times)

  gap_positions <- which(differences > 60)
  missing_minutes <- if (length(gap_positions) > 0L) {
    sum(round(differences[gap_positions] / 60) - 1L)
  } else {
    0L
  }

  gaps <- if (length(gap_positions) > 0L) {
    data.frame(
      previous_open_time = as.POSIXct(
        unique_times[gap_positions],
        origin = "1970-01-01",
        tz = "UTC"
      ),
      next_open_time = as.POSIXct(
        unique_times[gap_positions + 1L],
        origin = "1970-01-01",
        tz = "UTC"
      ),
      missing_minutes = round(
        differences[gap_positions] / 60
      ) - 1L
    )
  } else {
    data.frame(
      previous_open_time = as.POSIXct(character(), tz = "UTC"),
      next_open_time = as.POSIXct(character(), tz = "UTC"),
      missing_minutes = integer()
    )
  }

  expected_end <- as.POSIXct(
    paste(as.Date(end_date), "23:59:00"),
    tz = "UTC"
  )

  report <- data.frame(
    metric = c(
      "rows",
      "first_open_time",
      "last_open_time",
      "expected_first_open_time",
      "expected_last_open_time",
      "duplicate_open_times",
      "gap_count",
      "missing_minutes",
      "non_positive_prices",
      "negative_volumes",
      "invalid_high",
      "invalid_low",
      "invalid_range"
    ),
    value = c(
      nrow(data),
      format(min(data$open_time), tz = "UTC", usetz = TRUE),
      format(max(data$open_time), tz = "UTC", usetz = TRUE),
      format(config$start_time, tz = "UTC", usetz = TRUE),
      format(expected_end, tz = "UTC", usetz = TRUE),
      sum(duplicated(times)),
      length(gap_positions),
      missing_minutes,
      sum(
        data$open <= 0 |
          data$high <= 0 |
          data$low <= 0 |
          data$close <= 0
      ),
      sum(
        data$base_volume < 0 |
          data$quote_volume < 0 |
          data$taker_buy_base_volume < 0 |
          data$taker_buy_quote_volume < 0
      ),
      sum(data$high < pmax(data$open, data$close)),
      sum(data$low > pmin(data$open, data$close)),
      sum(data$high < data$low)
    ),
    stringsAsFactors = FALSE
  )

  critical_ok <- identical(
    min(data$open_time),
    config$start_time
  ) &&
    identical(
      max(data$open_time),
      expected_end
    ) &&
    sum(duplicated(times)) == 0L &&
    sum(
      data$open <= 0 |
        data$high <= 0 |
        data$low <= 0 |
        data$close <= 0
    ) == 0L &&
    sum(
      data$base_volume < 0 |
        data$quote_volume < 0 |
        data$taker_buy_base_volume < 0 |
        data$taker_buy_quote_volume < 0
    ) == 0L &&
    sum(data$high < pmax(data$open, data$close)) == 0L &&
    sum(data$low > pmin(data$open, data$close)) == 0L &&
    sum(data$high < data$low) == 0L

  list(
    report = report,
    gaps = gaps,
    critical_ok = critical_ok
  )
}

btc_verify_api_samples <- function(data, config) {
  sample_size <- min(config$api_sample_size, nrow(data))
  indices <- unique(as.integer(round(seq(
    from = 1,
    to = nrow(data),
    length.out = sample_size
  ))))

  results <- vector("list", length(indices))

  for (i in seq_along(indices)) {
    row_index <- indices[[i]]
    local_row <- data[row_index, , drop = FALSE]
    start_time_ms <- format(
      as.numeric(local_row$open_time) * 1000,
      scientific = FALSE,
      trim = TRUE
    )

    url <- paste0(
      config$api_url,
      "?symbol=",
      config$symbol,
      "&interval=1m&startTime=",
      start_time_ms,
      "&limit=1"
    )

    temporary <- tempfile(fileext = ".json")
    status <- tryCatch(
      utils::download.file(
        url,
        temporary,
        mode = "wb",
        quiet = TRUE
      ),
      error = function(error) 1L
    )

    if (!identical(status, 0L) || !file.exists(temporary)) {
      results[[i]] <- data.frame(
        open_time = local_row$open_time,
        checked = FALSE,
        matched = NA,
        stringsAsFactors = FALSE
      )
      unlink(temporary)
      next
    }

    response <- tryCatch(
      jsonlite::fromJSON(temporary, simplifyVector = FALSE),
      error = function(error) NULL
    )
    unlink(temporary)

    if (is.null(response) || length(response) == 0L) {
      results[[i]] <- data.frame(
        open_time = local_row$open_time,
        checked = FALSE,
        matched = NA,
        stringsAsFactors = FALSE
      )
      next
    }

    remote <- response[[1]]
    remote_values <- c(
      open = as.numeric(remote[[2]]),
      high = as.numeric(remote[[3]]),
      low = as.numeric(remote[[4]]),
      close = as.numeric(remote[[5]]),
      base_volume = as.numeric(remote[[6]]),
      quote_volume = as.numeric(remote[[8]]),
      number_of_trades = as.numeric(remote[[9]])
    )

    local_values <- c(
      open = local_row$open,
      high = local_row$high,
      low = local_row$low,
      close = local_row$close,
      base_volume = local_row$base_volume,
      quote_volume = local_row$quote_volume,
      number_of_trades = local_row$number_of_trades
    )

    matched <- isTRUE(all.equal(
      unname(local_values),
      unname(remote_values),
      tolerance = 1e-10,
      check.attributes = FALSE
    ))

    results[[i]] <- data.frame(
      open_time = local_row$open_time,
      checked = TRUE,
      matched = matched,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, results)
}

btc_processed_paths <- function(config) {
  list(
    data_1m = file.path(
      config$processed_dir,
      "BTCUSDT_1m.rds"
    ),
    data_1h = file.path(
      config$processed_dir,
      "BTCUSDT_1h.rds"
    ),
    data_4h = file.path(
      config$processed_dir,
      "BTCUSDT_4h.rds"
    ),
    data_1d = file.path(
      config$processed_dir,
      "BTCUSDT_1d.rds"
    ),
    manifest = file.path(
      config$processed_dir,
      "BTCUSDT_manifest.rds"
    )
  )
}

btc_all_processed_exist <- function(paths) {
  all(file.exists(unlist(paths)))
}

btc_load_processed <- function(config) {
  paths <- btc_processed_paths(config)

  if (!btc_all_processed_exist(paths)) {
    stop(
      "Оброблені дані ще не створено. Запустіть функцію з update = TRUE.",
      call. = FALSE
    )
  }

  list(
    data_1m = readRDS(paths$data_1m),
    data_1h = readRDS(paths$data_1h),
    data_4h = readRDS(paths$data_4h),
    data_1d = readRDS(paths$data_1d),
    manifest = readRDS(paths$manifest),
    source = "local_processed_files"
  )
}

btc_save_processed <- function(config, datasets, manifest) {
  paths <- btc_processed_paths(config)
  dir.create(config$processed_dir, recursive = TRUE, showWarnings = FALSE)

  saveRDS(datasets$data_1m, paths$data_1m, compress = "gzip")
  saveRDS(datasets$data_1h, paths$data_1h, compress = "gzip")
  saveRDS(datasets$data_4h, paths$data_4h, compress = "gzip")
  saveRDS(datasets$data_1d, paths$data_1d, compress = "gzip")
  saveRDS(manifest, paths$manifest, compress = "gzip")

  invisible(paths)
}

btc_write_validation <- function(config, validation, api_check) {
  dir.create(config$validation_dir, recursive = TRUE, showWarnings = FALSE)

  utils::write.csv(
    validation$report,
    file.path(config$validation_dir, "quality_report.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  utils::write.csv(
    validation$gaps,
    file.path(config$validation_dir, "missing_intervals.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  utils::write.csv(
    api_check,
    file.path(config$validation_dir, "api_sample_check.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  invisible(TRUE)
}

btc_load_or_update <- function(
  config = btc_default_config(),
  update = TRUE
) {
  btc_require_packages()
  btc_make_dirs(config)
  paths <- btc_processed_paths(config)

  if (!isTRUE(update)) {
    return(btc_load_processed(config))
  }

  end_date <- btc_resolve_end_date(config)
  message("Кінцева дата набору: ", end_date)

  archives <- btc_collect_archives(config, end_date)
  new_manifest <- btc_archives_manifest(archives, end_date)

  old_manifest <- if (file.exists(paths$manifest)) {
    readRDS(paths$manifest)
  } else {
    NULL
  }

  processed_data_exist <- all(file.exists(c(
    paths$data_1m,
    paths$data_1h,
    paths$data_4h,
    paths$data_1d
  )))

  if (processed_data_exist &&
      btc_manifests_equal(old_manifest, new_manifest)) {
    message("Нових архівів немає. Підключаю локальні оброблені дані.")
    return(btc_load_processed(config))
  }

  message("Є нові або змінені архіви. Оновлюю набір даних.")

  chunks <- lapply(
    archives,
    function(archive) btc_load_or_parse_chunk(config, archive)
  )

  data_1m <- btc_bind_chunks(
    chunks,
    start_time = config$start_time,
    end_date = end_date
  )
  rm(chunks)
  invisible(gc())

  validation <- btc_validate_1m(data_1m, config, end_date)
  api_check <- btc_verify_api_samples(data_1m, config)
  btc_write_validation(config, validation, api_check)

  api_checked <- sum(api_check$checked, na.rm = TRUE)
  api_failures <- sum(
    api_check$checked & !api_check$matched,
    na.rm = TRUE
  )

  if (api_checked == 0L) {
    warning(
      "Вибіркова перевірка через API не виконана. ",
      "Checksum і структурні перевірки виконано.",
      call. = FALSE
    )
  }

  if (nrow(validation$gaps) > 0L) {
    warning(
      "У джерелі виявлено пропущені хвилинні інтервали. ",
      "Вони записані у missing_intervals.csv і не заповнюються автоматично.",
      call. = FALSE
    )
  }

  if (api_failures > 0L) {
    stop(
      "Деякі вибіркові записи не збігаються з Binance API. ",
      "Перевірте api_sample_check.csv.",
      call. = FALSE
    )
  }

  if (isTRUE(config$strict_validation) &&
      !isTRUE(validation$critical_ok)) {
    stop(
      "Критична перевірка даних не пройдена. ",
      "Перевірте quality_report.csv і missing_intervals.csv.",
      call. = FALSE
    )
  }

  datasets <- list(
    data_1m = data_1m,
    data_1h = btc_aggregate_ohlcv(data_1m, 3600, "1h"),
    data_4h = btc_aggregate_ohlcv(data_1m, 14400, "4h"),
    data_1d = btc_aggregate_ohlcv(data_1m, 86400, "1d")
  )

  btc_save_processed(config, datasets, new_manifest)

  datasets$manifest <- new_manifest
  datasets$source <- "updated_from_verified_archives"
  datasets
}
