# Контрольні spot-дані Bybit BTCUSDT
#
# Завантажуються тільки інтервали 4h і 1d.
# Це достатньо для міжбіржового порівняння без мільйонів API-запитів.
# Пакети автоматично не встановлюються.

bybit_default_config = function(
  project_root = "."
) {
  root = normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )

  list(
    project_root = root,
    exchange = "Bybit",
    market_type = "spot",
    symbol = "BTCUSDT",
    category = "spot",
    start_time = as.POSIXct(
      "2020-01-01 00:00:00",
      tz = "UTC"
    ),
    api_url = "https://api.bybit.com/v5/market/kline",
    limit = 1000L,
    request_pause = 0.08,
    processed_dir = file.path(
      root,
      "data",
      "processed",
      "bybit",
      "spot",
      "BTCUSDT"
    ),
    parquet_dir = file.path(
      root,
      "data",
      "parquet",
      "bybit",
      "spot",
      "BTCUSDT"
    ),
    validation_dir = file.path(
      root,
      "data",
      "validation",
      "bybit",
      "spot",
      "BTCUSDT"
    ),
    intervals = data.frame(
      api_interval = c(
        "240",
        "D"
      ),
      label = c(
        "4h",
        "1d"
      ),
      seconds = c(
        14400,
        86400
      ),
      stringsAsFactors = FALSE
    )
  )
}

bybit_require_packages = function() {
  required_packages = c(
    "data.table",
    "DBI",
    "duckdb",
    "jsonlite"
  )

  missing_packages = required_packages[
    !vapply(
      required_packages,
      requireNamespace,
      quietly = TRUE,
      FUN.VALUE = logical(1)
    )
  ]

  if (length(missing_packages) > 0L) {
    stop(
      paste0(
        "Відсутні пакети: ",
        paste(
          missing_packages,
          collapse = ", "
        ),
        ". Встановіть їх через renv::install()."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

bybit_make_dirs = function(config) {
  directories = c(
    config$processed_dir,
    config$parquet_dir,
    config$validation_dir
  )

  invisible(
    vapply(
      directories,
      dir.create,
      recursive = TRUE,
      showWarnings = FALSE,
      FUN.VALUE = logical(1)
    )
  )
}

bybit_last_complete_open = function(
  seconds
) {
  now_numeric = as.numeric(
    Sys.time()
  )

  as.POSIXct(
    floor(
      now_numeric / seconds
    ) * seconds - seconds,
    origin = "1970-01-01",
    tz = "UTC"
  )
}

bybit_build_url = function(
  config,
  api_interval,
  end_ms
) {
  paste0(
    config$api_url,
    "?category=",
    utils::URLencode(
      config$category,
      reserved = TRUE
    ),
    "&symbol=",
    utils::URLencode(
      config$symbol,
      reserved = TRUE
    ),
    "&interval=",
    utils::URLencode(
      api_interval,
      reserved = TRUE
    ),
    "&end=",
    format(
      end_ms,
      scientific = FALSE,
      trim = TRUE
    ),
    "&limit=",
    as.integer(
      config$limit
    )
  )
}

bybit_request_page = function(
  config,
  api_interval,
  end_ms
) {
  url = bybit_build_url(
    config = config,
    api_interval = api_interval,
    end_ms = end_ms
  )

  temporary_file = tempfile(
    fileext = ".json"
  )

  on.exit(
    unlink(
      temporary_file,
      force = TRUE
    ),
    add = TRUE
  )

  status = tryCatch(
    utils::download.file(
      url = url,
      destfile = temporary_file,
      mode = "wb",
      quiet = TRUE
    ),
    error = function(error) {
      structure(
        1L,
        message = conditionMessage(
          error
        )
      )
    }
  )

  if (
    !identical(
      status,
      0L
    ) ||
    !file.exists(
      temporary_file
    )
  ) {
    stop(
      paste0(
        "Не вдалося отримати сторінку Bybit. URL: ",
        url
      ),
      call. = FALSE
    )
  }

  response = jsonlite::fromJSON(
    temporary_file,
    simplifyVector = FALSE
  )

  if (
    is.null(
      response$retCode
    ) ||
    as.integer(
      response$retCode
    ) != 0L
  ) {
    message_text = if (
      !is.null(
        response$retMsg
      )
    ) {
      response$retMsg
    } else {
      "невідома помилка"
    }

    stop(
      paste0(
        "Bybit API повернув помилку: ",
        message_text
      ),
      call. = FALSE
    )
  }

  rows = response$result$list

  if (
    is.null(rows) ||
    length(rows) == 0L
  ) {
    return(
      data.table::data.table()
    )
  }

  parsed_rows = lapply(
    rows,
    function(row) {
      data.table::data.table(
        open_time_ms = as.numeric(
          row[[1L]]
        ),
        open = as.numeric(
          row[[2L]]
        ),
        high = as.numeric(
          row[[3L]]
        ),
        low = as.numeric(
          row[[4L]]
        ),
        close = as.numeric(
          row[[5L]]
        ),
        base_volume = as.numeric(
          row[[6L]]
        ),
        quote_volume = as.numeric(
          row[[7L]]
        )
      )
    }
  )

  data.table::rbindlist(
    parsed_rows,
    use.names = TRUE
  )
}

bybit_fetch_interval = function(
  config,
  api_interval,
  label,
  seconds
) {
  start_ms = as.numeric(
    config$start_time
  ) * 1000

  last_complete = bybit_last_complete_open(
    seconds
  )

  end_ms = (
    as.numeric(
      last_complete
    ) +
      seconds -
      0.001
  ) * 1000

  pages = list()
  page_number = 0L

  repeat {
    page_number = page_number + 1L

    message(
      "Bybit ",
      label,
      ", сторінка ",
      page_number,
      "."
    )

    page = bybit_request_page(
      config = config,
      api_interval = api_interval,
      end_ms = end_ms
    )

    if (nrow(page) == 0L) {
      break
    }

    pages[[length(pages) + 1L]] = page

    oldest_ms = min(
      page$open_time_ms,
      na.rm = TRUE
    )

    if (
      oldest_ms <= start_ms ||
      nrow(page) < config$limit
    ) {
      break
    }

    next_end = oldest_ms - 1

    if (next_end >= end_ms) {
      stop(
        "Bybit API не змістив межу пагінації.",
        call. = FALSE
      )
    }

    end_ms = next_end

    Sys.sleep(
      config$request_pause
    )
  }

  if (length(pages) == 0L) {
    stop(
      paste0(
        "Bybit не повернув дані для інтервалу ",
        label,
        "."
      ),
      call. = FALSE
    )
  }

  data = data.table::rbindlist(
    pages,
    use.names = TRUE,
    fill = TRUE
  )

  data = unique(
    data,
    by = "open_time_ms"
  )

  data[
    ,
    open_time := as.POSIXct(
      open_time_ms / 1000,
      origin = "1970-01-01",
      tz = "UTC"
    )
  ]

  data = data[
    open_time >= config$start_time &
      open_time <= last_complete
  ]

  data.table::setorder(
    data,
    open_time
  )

  data[
    ,
    `:=`(
      exchange = config$exchange,
      market_type = config$market_type,
      symbol = config$symbol,
      interval = label,
      close_time = open_time +
        seconds -
        0.000001,
      number_of_trades = NA_real_,
      taker_buy_base_volume = NA_real_,
      taker_buy_quote_volume = NA_real_,
      observations = 1L,
      is_complete = TRUE
    )
  ]

  data[
    ,
    log_close := log(
      close
    )
  ]

  data[
    ,
    log_return := 100 * (
      log_close -
        data.table::shift(
          log_close
        )
    )
  ]

  data[
    ,
    simple_return := 100 * (
      close /
        data.table::shift(
          close
        ) -
        1
    )
  ]

  data[
    ,
    high_low_range := 100 * log(
      high / low
    )
  ]

  data[
    ,
    taker_buy_share := NA_real_
  ]

  data[
    ,
    open_time_ms := NULL
  ]

  data.table::setcolorder(
    data,
    c(
      "exchange",
      "market_type",
      "symbol",
      "interval",
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
      "observations",
      "is_complete",
      "log_close",
      "log_return",
      "simple_return",
      "high_low_range",
      "taker_buy_share"
    )
  )

  data
}

bybit_validate = function(
  data,
  seconds,
  label
) {
  differences = diff(
    as.numeric(
      data$open_time
    )
  )

  open_seconds = as.numeric(
    data$open_time
  )

  off_interval_grid = sum(
    abs(
      open_seconds -
        round(open_seconds / seconds) * seconds
    ) > 1e-6
  )

  gap_positions = which(
    differences > seconds
  )

  gaps = if (
    length(
      gap_positions
    ) > 0L
  ) {
    data.frame(
      previous_open_time = data$open_time[
        gap_positions
      ],
      next_open_time = data$open_time[
        gap_positions + 1L
      ],
      missing_intervals = round(
        differences[
          gap_positions
        ] / seconds
      ) - 1L,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      previous_open_time = as.POSIXct(
        character(),
        tz = "UTC"
      ),
      next_open_time = as.POSIXct(
        character(),
        tz = "UTC"
      ),
      missing_intervals = integer(),
      stringsAsFactors = FALSE
    )
  }

  report = data.frame(
    interval = label,
    metric = c(
      "rows",
      "first_open_time",
      "last_open_time",
      "duplicate_open_times",
      "off_interval_grid",
      "gap_count",
      "missing_intervals",
      "non_positive_prices",
      "negative_volumes",
      "invalid_high",
      "invalid_low",
      "invalid_range"
    ),
    value = c(
      nrow(data),
      format(
        min(
          data$open_time
        ),
        tz = "UTC",
        usetz = TRUE
      ),
      format(
        max(
          data$open_time
        ),
        tz = "UTC",
        usetz = TRUE
      ),
      sum(
        duplicated(
          data$open_time
        )
      ),
      off_interval_grid,
      nrow(gaps),
      if (nrow(gaps) > 0L) {
        sum(
          gaps$missing_intervals
        )
      } else {
        0
      },
      sum(
        data$open <= 0 |
          data$high <= 0 |
          data$low <= 0 |
          data$close <= 0
      ),
      sum(
        data$base_volume < 0 |
          data$quote_volume < 0
      ),
      sum(
        data$high <
          pmax(
            data$open,
            data$close
          )
      ),
      sum(
        data$low >
          pmin(
            data$open,
            data$close
          )
      ),
      sum(
        data$high <
          data$low
      )
    ),
    stringsAsFactors = FALSE
  )

  list(
    report = report,
    gaps = gaps
  )
}

bybit_write_parquet = function(
  data,
  path
) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temporary_path = paste0(
    path,
    ".tmp"
  )

  unlink(
    temporary_path,
    force = TRUE
  )

  connection = DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = ":memory:"
  )

  on.exit(
    try(
      DBI::dbDisconnect(
        connection,
        shutdown = TRUE
      ),
      silent = TRUE
    ),
    add = TRUE
  )

  duckdb::duckdb_register(
    connection,
    "bybit_data",
    data
  )

  on.exit(
    try(
      duckdb::duckdb_unregister(
        connection,
        "bybit_data"
      ),
      silent = TRUE
    ),
    add = TRUE
  )

  DBI::dbExecute(
    connection,
    paste0(
      "COPY bybit_data TO '",
      gsub(
        "'",
        "''",
        temporary_path,
        fixed = TRUE
      ),
      "' (FORMAT PARQUET, COMPRESSION ZSTD)"
    )
  )

  if (!file.rename(
    temporary_path,
    path
  )) {
    copied = file.copy(
      temporary_path,
      path,
      overwrite = TRUE
    )

    unlink(
      temporary_path,
      force = TRUE
    )

    if (!isTRUE(copied)) {
      stop(
        paste0(
          "Не вдалося створити ",
          path,
          "."
        ),
        call. = FALSE
      )
    }
  }

  invisible(path)
}

bybit_update = function(
  config = bybit_default_config()
) {
  bybit_require_packages()
  bybit_make_dirs(config)

  datasets = list()
  reports = list()
  gaps = list()

  for (index in seq_len(
    nrow(
      config$intervals
    )
  )) {
    api_interval = config$intervals$api_interval[[index]]

    label = config$intervals$label[[index]]

    seconds = config$intervals$seconds[[index]]

    rds_path = file.path(
      config$processed_dir,
      paste0(
        config$symbol,
        "_",
        label,
        ".rds"
      )
    )

    parquet_path = file.path(
      config$parquet_dir,
      paste0(
        config$symbol,
        "_",
        label,
        ".parquet"
      )
    )

    existing_data = if (
      file.exists(
        rds_path
      )
    ) {
      data.table::as.data.table(
        readRDS(
          rds_path
        )
      )
    } else {
      NULL
    }

    last_complete = bybit_last_complete_open(
      seconds
    )

    if (
      !is.null(
        existing_data
      ) &&
      nrow(
        existing_data
      ) > 0L &&
      max(
        existing_data$open_time,
        na.rm = TRUE
      ) >= last_complete
    ) {
      message(
        "Bybit ",
        label,
        " уже актуальний. Підключаю локальний RDS."
      )

      data = existing_data
    } else {
      fetch_config = config

      if (
        !is.null(
          existing_data
        ) &&
        nrow(
          existing_data
        ) > 0L
      ) {
        fetch_config$start_time = max(
          existing_data$open_time,
          na.rm = TRUE
        ) + seconds

        message(
          "Оновлюю Bybit ",
          label,
          " від ",
          format(
            fetch_config$start_time,
            tz = "UTC",
            usetz = TRUE
          ),
          "."
        )
      }

      new_data = bybit_fetch_interval(
        config = fetch_config,
        api_interval = api_interval,
        label = label,
        seconds = seconds
      )

      if (
        !is.null(
          existing_data
        ) &&
        nrow(
          existing_data
        ) > 0L
      ) {
        data = data.table::rbindlist(
          list(
            existing_data,
            new_data
          ),
          use.names = TRUE,
          fill = TRUE
        )

        data = unique(
          data,
          by = "open_time"
        )

        data.table::setorder(
          data,
          open_time
        )
      } else {
        data = new_data
      }
    }

    # Після інкрементального оновлення перша дохідність у новій порції
    # була б NA, якщо не перерахувати похідні поля на об'єднаному ряді.
    data = data.table::as.data.table(
      data
    )

    data.table::setorder(
      data,
      open_time
    )

    data[
      ,
      `:=`(
        close_time = open_time +
          seconds -
          0.000001,
        log_close = log(
          close
        ),
        high_low_range = 100 * log(
          high / low
        )
      )
    ]

    data[
      ,
      log_return := 100 * (
        log_close -
          data.table::shift(
            log_close
          )
      )
    ]

    data[
      ,
      simple_return := 100 * (
        close /
          data.table::shift(
            close
          ) -
          1
      )
    ]

    validation = bybit_validate(
      data = data,
      seconds = seconds,
      label = label
    )

    saveRDS(
      as.data.frame(
        data
      ),
      rds_path,
      compress = "gzip"
    )

    bybit_write_parquet(
      data = data,
      path = parquet_path
    )

    datasets[[label]] = as.data.frame(
      data
    )

    reports[[label]] = validation$report

    if (nrow(
      validation$gaps
    ) > 0L) {
      gap_table = validation$gaps
      gap_table$interval = label
      gaps[[label]] = gap_table
    }
  }

  report_table = data.table::rbindlist(
    reports,
    use.names = TRUE,
    fill = TRUE
  )

  gap_table = if (
    length(gaps) > 0L
  ) {
    data.table::rbindlist(
      gaps,
      use.names = TRUE,
      fill = TRUE
    )
  } else {
    data.table::data.table(
      previous_open_time = as.POSIXct(
        character(),
        tz = "UTC"
      ),
      next_open_time = as.POSIXct(
        character(),
        tz = "UTC"
      ),
      missing_intervals = integer(),
      interval = character()
    )
  }

  utils::write.csv(
    as.data.frame(
      report_table
    ),
    file.path(
      config$validation_dir,
      "quality_report.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  utils::write.csv(
    as.data.frame(
      gap_table
    ),
    file.path(
      config$validation_dir,
      "missing_intervals.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  list(
    data_4h = datasets[["4h"]],
    data_1d = datasets[["1d"]],
    report = as.data.frame(
      report_table
    ),
    gaps = as.data.frame(
      gap_table
    ),
    source = "bybit_public_api"
  )
}
