# Оптимізований конвеєр Binance BTCUSDT
#
# Основні принципи:
# 1. Первинні ZIP-архіви залишаються незмінними.
# 2. Кожен архів один раз перетворюється на окремий Parquet-файл.
# 3. Нові архіви можна обробляти двома паралельними процесами.
# 4. DuckDB агрегує Parquet без завантаження всього 1m-ряду в пам'ять R.
# 5. Компактні 1h, 4h і 1d ряди зберігаються також як RDS для книги.
# 6. Хвилинні дані читаються вибірково через btc_fast_query_1m().
#
# Цей файл використовує допоміжні функції з R/binance_data.R.
# Пакети автоматично не встановлюються.

btc_fast_default_config = function(project_root = ".") {
  base_config = btc_default_config(project_root)

  base_config$parquet_dir = file.path(
    base_config$project_root,
    "data",
    "parquet",
    "binance",
    "spot",
    base_config$symbol
  )

  base_config$duckdb_dir = file.path(
    base_config$project_root,
    "data",
    "duckdb"
  )

  base_config$fast_manifest = file.path(
    base_config$processed_dir,
    paste0(
      base_config$symbol,
      "_fast_manifest.rds"
    )
  )

  base_config$workers = 2L
  base_config$data_table_threads = 2L
  base_config$duckdb_threads = 2L
  base_config$duckdb_memory_limit = "4GB"
  base_config$force_verify_all = FALSE

  base_config$derived_intervals = data.frame(
    label = c(
      "5m",
      "15m",
      "30m",
      "1h",
      "2h",
      "4h",
      "6h",
      "12h",
      "1d"
    ),
    seconds = c(
      300,
      900,
      1800,
      3600,
      7200,
      14400,
      21600,
      43200,
      86400
    ),
    stringsAsFactors = FALSE
  )

  base_config
}

btc_fast_require_packages = function() {
  required_packages = c(
    "data.table",
    "DBI",
    "digest",
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
        ". Встановіть їх через renv::install(), ",
        "після чого запустіть скрипт повторно."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

btc_fast_make_dirs = function(config) {
  directories = c(
    config$raw_dir,
    config$cache_dir,
    config$processed_dir,
    config$validation_dir,
    config$parquet_dir,
    config$duckdb_dir
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

btc_fast_set_threads = function(config) {
  data.table::setDTthreads(
    threads = as.integer(
      config$data_table_threads
    )
  )

  invisible(TRUE)
}

btc_fast_sql_string = function(value) {
  paste0(
    "'",
    gsub(
      "'",
      "''",
      value,
      fixed = TRUE
    ),
    "'"
  )
}

btc_fast_sql_file_list = function(paths) {
  normalized_paths = normalizePath(
    paths,
    winslash = "/",
    mustWork = TRUE
  )

  paste0(
    "[",
    paste(
      vapply(
        normalized_paths,
        btc_fast_sql_string,
        FUN.VALUE = character(1)
      ),
      collapse = ", "
    ),
    "]"
  )
}

btc_fast_connect = function(config) {
  connection = DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = ":memory:"
  )

  DBI::dbExecute(
    connection,
    paste0(
      "SET threads = ",
      as.integer(config$duckdb_threads)
    )
  )

  DBI::dbExecute(
    connection,
    paste0(
      "SET memory_limit = ",
      btc_fast_sql_string(
        config$duckdb_memory_limit
      )
    )
  )

  connection
}

btc_fast_disconnect = function(connection) {
  if (!is.null(connection)) {
    try(
      DBI::dbDisconnect(
        connection,
        shutdown = TRUE
      ),
      silent = TRUE
    )
  }

  invisible(TRUE)
}

btc_fast_archive_parquet_path = function(
  config,
  archive
) {
  archive_date = as.Date(
    archive$date
  )

  year_value = format(
    archive_date,
    "%Y"
  )

  month_value = format(
    archive_date,
    "%m"
  )

  folder = file.path(
    config$parquet_dir,
    "1m",
    paste0(
      "archive_type=",
      archive$period
    ),
    paste0(
      "year=",
      year_value
    ),
    paste0(
      "month=",
      month_value
    )
  )

  if (identical(archive$period, "daily")) {
    folder = file.path(
      folder,
      paste0(
        "day=",
        format(
          archive_date,
          "%d"
        )
      )
    )
  }

  file.path(
    folder,
    sub(
      "[.]zip$",
      ".parquet",
      archive$filename
    )
  )
}

btc_fast_checksum_sidecar = function(
  parquet_path
) {
  paste0(
    parquet_path,
    ".source-sha256"
  )
}

btc_fast_parquet_current = function(
  parquet_path,
  checksum
) {
  sidecar = btc_fast_checksum_sidecar(
    parquet_path
  )

  if (
    !file.exists(parquet_path) ||
    !file.exists(sidecar)
  ) {
    return(FALSE)
  }

  stored_checksum = trimws(
    readLines(
      sidecar,
      n = 1L,
      warn = FALSE
    )
  )

  identical(
    stored_checksum,
    checksum
  )
}

btc_fast_read_archive = function(
  zip_file
) {
  members = utils::unzip(
    zip_file,
    list = TRUE
  )

  csv_members = members$Name[
    grepl(
      "[.]csv$",
      members$Name,
      ignore.case = TRUE
    )
  ]

  if (length(csv_members) != 1L) {
    stop(
      paste0(
        "Очікувався один CSV-файл в архіві ",
        basename(zip_file),
        "."
      ),
      call. = FALSE
    )
  }

  temporary_directory = tempfile(
    pattern = "binance-csv-"
  )

  dir.create(
    temporary_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  on.exit(
    unlink(
      temporary_directory,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  utils::unzip(
    zipfile = zip_file,
    files = csv_members[[1]],
    exdir = temporary_directory,
    junkpaths = TRUE
  )

  csv_file = file.path(
    temporary_directory,
    basename(
      csv_members[[1]]
    )
  )

  data = data.table::fread(
    csv_file,
    header = FALSE,
    showProgress = FALSE,
    nThread = 1L
  )

  if (ncol(data) != 12L) {
    stop(
      paste0(
        "Некоректна кількість колонок в архіві ",
        basename(zip_file),
        ". Отримано ",
        ncol(data),
        ", очікувалося 12."
      ),
      call. = FALSE
    )
  }

  if (
    nrow(data) > 0L &&
    !grepl(
      "^[0-9]+$",
      as.character(
        data[[1L]][[1L]]
      )
    )
  ) {
    data = data[-1L]
  }

  data.table::setnames(
    data,
    c(
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
  )

  data[
    ,
    open_time := btc_timestamp_to_posix(
      open_time
    )
  ]

  data[
    ,
    close_time := btc_timestamp_to_posix(
      close_time
    )
  ]

  numeric_columns = c(
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

  for (column_name in numeric_columns) {
    data[
      ,
      (column_name) := as.numeric(
        get(column_name)
      )
    ]

    if (anyNA(data[[column_name]])) {
      stop(
        paste0(
          "Колонка ",
          column_name,
          " містить нечислові значення в архіві ",
          basename(zip_file),
          "."
        ),
        call. = FALSE
      )
    }
  }

  data[
    ,
    ignore := NULL
  ]

  data[
    ,
    `:=`(
      exchange = "Binance",
      market_type = "spot",
      symbol = "BTCUSDT",
      source_file = basename(
        zip_file
      ),
      downloaded_at = as.POSIXct(
        file.info(zip_file)$mtime,
        tz = "UTC"
      )
    )
  ]

  data.table::setcolorder(
    data,
    c(
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
    )
  )

  data
}

btc_fast_write_parquet = function(
  data,
  output_path
) {
  dir.create(
    dirname(output_path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temporary_path = paste0(
    output_path,
    ".tmp"
  )

  unlink(
    temporary_path,
    force = TRUE
  )

  connection = btc_fast_connect(
    btc_fast_default_config(
      project_root = "."
    )
  )

  on.exit(
    btc_fast_disconnect(
      connection
    ),
    add = TRUE
  )

  duckdb::duckdb_register(
    connection,
    "archive_chunk",
    data
  )

  on.exit(
    try(
      duckdb::duckdb_unregister(
        connection,
        "archive_chunk"
      ),
      silent = TRUE
    ),
    add = TRUE
  )

  DBI::dbExecute(
    connection,
    paste0(
      "COPY archive_chunk TO ",
      btc_fast_sql_string(
        temporary_path
      ),
      " (FORMAT PARQUET, COMPRESSION ZSTD)"
    )
  )

  if (!file.rename(
    temporary_path,
    output_path
  )) {
    copied = file.copy(
      temporary_path,
      output_path,
      overwrite = TRUE
    )

    unlink(
      temporary_path,
      force = TRUE
    )

    if (!isTRUE(copied)) {
      stop(
        paste0(
          "Не вдалося записати Parquet-файл ",
          output_path,
          "."
        ),
        call. = FALSE
      )
    }
  }

  invisible(output_path)
}

btc_fast_convert_one_archive = function(
  config,
  archive
) {
  output_path = btc_fast_archive_parquet_path(
    config,
    archive
  )

  if (
    btc_fast_parquet_current(
      output_path,
      archive$checksum
    )
  ) {
    return(
      data.frame(
        filename = archive$filename,
        checksum = archive$checksum,
        parquet_path = normalizePath(
          output_path,
          winslash = "/",
          mustWork = TRUE
        ),
        converted = FALSE,
        stringsAsFactors = FALSE
      )
    )
  }

  data = btc_fast_read_archive(
    archive$zip_file
  )

  dir.create(
    dirname(output_path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temporary_path = paste0(
    output_path,
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
    btc_fast_disconnect(
      connection
    ),
    add = TRUE
  )

  DBI::dbExecute(
    connection,
    "SET threads = 1"
  )

  duckdb::duckdb_register(
    connection,
    "archive_chunk",
    data
  )

  on.exit(
    try(
      duckdb::duckdb_unregister(
        connection,
        "archive_chunk"
      ),
      silent = TRUE
    ),
    add = TRUE
  )

  DBI::dbExecute(
    connection,
    paste0(
      "COPY archive_chunk TO ",
      btc_fast_sql_string(
        temporary_path
      ),
      " (FORMAT PARQUET, COMPRESSION ZSTD)"
    )
  )

  if (!file.rename(
    temporary_path,
    output_path
  )) {
    copied = file.copy(
      temporary_path,
      output_path,
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
          output_path,
          "."
        ),
        call. = FALSE
      )
    }
  }

  writeLines(
    archive$checksum,
    btc_fast_checksum_sidecar(
      output_path
    ),
    useBytes = TRUE
  )

  data.frame(
    filename = archive$filename,
    checksum = archive$checksum,
    parquet_path = normalizePath(
      output_path,
      winslash = "/",
      mustWork = TRUE
    ),
    converted = TRUE,
    stringsAsFactors = FALSE
  )
}

btc_fast_convert_archives = function(
  config,
  archives
) {
  worker = function(archive) {
    btc_fast_convert_one_archive(
      config,
      archive
    )
  }

  workers = max(
    1L,
    min(
      as.integer(
        config$workers
      ),
      length(archives)
    )
  )

  if (
    workers > 1L &&
    identical(
      .Platform$OS.type,
      "unix"
    )
  ) {
    results = parallel::mclapply(
      archives,
      worker,
      mc.cores = workers,
      mc.preschedule = FALSE
    )
  } else {
    results = lapply(
      archives,
      worker
    )
  }

  failures = vapply(
    results,
    inherits,
    what = "try-error",
    FUN.VALUE = logical(1)
  )

  if (any(failures)) {
    stop(
      "Не вдалося перетворити один або кілька архівів у Parquet.",
      call. = FALSE
    )
  }

  data.table::rbindlist(
    results,
    use.names = TRUE,
    fill = TRUE
  )
}

btc_fast_manifest = function(
  archives,
  parquet_results,
  end_date
) {
  archive_table = data.frame(
    filename = vapply(
      archives,
      function(item) item$filename,
      FUN.VALUE = character(1)
    ),
    period = vapply(
      archives,
      function(item) item$period,
      FUN.VALUE = character(1)
    ),
    archive_date = as.Date(
      vapply(
        archives,
        function(item) as.character(
          item$date
        ),
        FUN.VALUE = character(1)
      )
    ),
    checksum = vapply(
      archives,
      function(item) item$checksum,
      FUN.VALUE = character(1)
    ),
    zip_path = vapply(
      archives,
      function(item) normalizePath(
        item$zip_file,
        winslash = "/",
        mustWork = TRUE
      ),
      FUN.VALUE = character(1)
    ),
    stringsAsFactors = FALSE
  )

  merged = merge(
    archive_table,
    as.data.frame(
      parquet_results
    ),
    by = c(
      "filename",
      "checksum"
    ),
    all.x = TRUE,
    sort = FALSE
  )

  merged$end_date = as.Date(
    end_date
  )

  merged = merged[
    order(
      merged$archive_date,
      merged$filename
    ),
    ,
    drop = FALSE
  ]

  rownames(merged) = NULL
  merged
}

btc_fast_manifest_equal = function(
  old_manifest,
  new_manifest
) {
  if (
    is.null(old_manifest) ||
    is.null(new_manifest)
  ) {
    return(FALSE)
  }

  columns = c(
    "filename",
    "period",
    "archive_date",
    "checksum",
    "parquet_path",
    "end_date"
  )

  if (
    !all(
      columns %in% names(old_manifest)
    ) ||
    !all(
      columns %in% names(new_manifest)
    )
  ) {
    return(FALSE)
  }

  identical(
    old_manifest[
      ,
      columns,
      drop = FALSE
    ],
    new_manifest[
      ,
      columns,
      drop = FALSE
    ]
  )
}

btc_fast_create_views = function(
  connection,
  config,
  manifest,
  end_date
) {
  file_list = btc_fast_sql_file_list(
    manifest$parquet_path
  )

  start_text = format(
    config$start_time,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )

  end_exclusive = as.POSIXct(
    paste(
      as.Date(end_date) + 1L,
      "00:00:00"
    ),
    tz = "UTC"
  )

  end_text = format(
    end_exclusive,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )

  DBI::dbExecute(
    connection,
    paste0(
      "CREATE OR REPLACE TEMP VIEW btc_1m_source AS ",
      "SELECT * FROM read_parquet(",
      file_list,
      ", union_by_name = true) ",
      "WHERE open_time >= TIMESTAMP ",
      btc_fast_sql_string(start_text),
      " AND open_time < TIMESTAMP ",
      btc_fast_sql_string(end_text)
    )
  )

  DBI::dbExecute(
    connection,
    paste0(
      "CREATE OR REPLACE TEMP VIEW btc_1m AS ",
      "SELECT * EXCLUDE (duplicate_rank) FROM (",
      "SELECT *, row_number() OVER (",
      "PARTITION BY open_time ",
      "ORDER BY downloaded_at DESC, source_file DESC",
      ") AS duplicate_rank ",
      "FROM btc_1m_source",
      ") WHERE duplicate_rank = 1"
    )
  )

  invisible(TRUE)
}

btc_fast_validate = function(
  connection,
  config,
  end_date
) {
  metrics = DBI::dbGetQuery(
    connection,
    paste0(
      "SELECT ",
      "count(*) AS rows, ",
      "min(open_time) AS first_open_time, ",
      "max(open_time) AS last_open_time, ",
      "(SELECT count(*) - count(DISTINCT open_time) ",
      "FROM btc_1m_source) AS duplicate_open_times, ",
      "sum(CASE WHEN \"open\" <= 0 OR high <= 0 OR low <= 0 ",
      "OR \"close\" <= 0 THEN 1 ELSE 0 END) AS non_positive_prices, ",
      "sum(CASE WHEN base_volume < 0 OR quote_volume < 0 ",
      "OR taker_buy_base_volume < 0 OR ",
      "taker_buy_quote_volume < 0 THEN 1 ELSE 0 END) ",
      "AS negative_volumes, ",
      "sum(CASE WHEN high < greatest(\"open\", \"close\") ",
      "THEN 1 ELSE 0 END) AS invalid_high, ",
      "sum(CASE WHEN low > least(\"open\", \"close\") ",
      "THEN 1 ELSE 0 END) AS invalid_low, ",
      "sum(CASE WHEN high < low THEN 1 ELSE 0 END) ",
      "AS invalid_range ",
      "FROM btc_1m"
    )
  )

  gaps = DBI::dbGetQuery(
    connection,
    paste0(
      "WITH ordered AS (",
      "SELECT open_time, ",
      "lag(open_time) OVER (ORDER BY open_time) ",
      "AS previous_open_time ",
      "FROM btc_1m",
      ") ",
      "SELECT previous_open_time, ",
      "open_time AS next_open_time, ",
      "CAST(round((",
      "epoch(open_time) - epoch(previous_open_time)",
      ") / 60) - 1 AS BIGINT) AS missing_minutes ",
      "FROM ordered ",
      "WHERE previous_open_time IS NOT NULL ",
      "AND epoch(open_time) - epoch(previous_open_time) > 60 ",
      "ORDER BY previous_open_time"
    )
  )

  expected_last = as.POSIXct(
    paste(
      as.Date(end_date),
      "23:59:00"
    ),
    tz = "UTC"
  )

  report = data.frame(
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
      metrics$rows[[1]],
      format(
        metrics$first_open_time[[1]],
        tz = "UTC",
        usetz = TRUE
      ),
      format(
        metrics$last_open_time[[1]],
        tz = "UTC",
        usetz = TRUE
      ),
      format(
        config$start_time,
        tz = "UTC",
        usetz = TRUE
      ),
      format(
        expected_last,
        tz = "UTC",
        usetz = TRUE
      ),
      metrics$duplicate_open_times[[1]],
      nrow(gaps),
      if (nrow(gaps) > 0L) {
        sum(
          gaps$missing_minutes,
          na.rm = TRUE
        )
      } else {
        0
      },
      metrics$non_positive_prices[[1]],
      metrics$negative_volumes[[1]],
      metrics$invalid_high[[1]],
      metrics$invalid_low[[1]],
      metrics$invalid_range[[1]]
    ),
    stringsAsFactors = FALSE
  )

  critical_ok = isTRUE(
    abs(
      as.numeric(
        metrics$first_open_time[[1]]
      ) -
        as.numeric(
          config$start_time
        )
    ) < 1
  ) &&
    isTRUE(
      abs(
        as.numeric(
          metrics$last_open_time[[1]]
        ) -
        as.numeric(
          expected_last
        )
    ) < 1
  ) &&
    metrics$duplicate_open_times[[1]] == 0 &&
    metrics$non_positive_prices[[1]] == 0 &&
    metrics$negative_volumes[[1]] == 0 &&
    metrics$invalid_high[[1]] == 0 &&
    metrics$invalid_low[[1]] == 0 &&
    metrics$invalid_range[[1]] == 0

  list(
    report = report,
    gaps = gaps,
    critical_ok = critical_ok,
    rows = as.numeric(
      metrics$rows[[1]]
    )
  )
}

btc_fast_api_sample = function(
  connection,
  sample_size
) {
  total_rows = DBI::dbGetQuery(
    connection,
    "SELECT count(*) AS rows FROM btc_1m"
  )$rows[[1]]

  indices = unique(
    as.integer(
      round(
        seq(
          from = 1,
          to = total_rows,
          length.out = min(
            sample_size,
            total_rows
          )
        )
      )
    )
  )

  DBI::dbGetQuery(
    connection,
    paste0(
      "WITH numbered AS (",
      "SELECT *, row_number() OVER (ORDER BY open_time) AS row_id ",
      "FROM btc_1m",
      ") ",
      "SELECT * EXCLUDE (row_id) FROM numbered ",
      "WHERE row_id IN (",
      paste(
        indices,
        collapse = ", "
      ),
      ") ORDER BY open_time"
    )
  )
}

btc_fast_interval_path = function(
  config,
  label
) {
  file.path(
    config$parquet_dir,
    "derived",
    paste0(
      config$symbol,
      "_",
      label,
      ".parquet"
    )
  )
}

btc_fast_aggregation_sql = function(
  seconds,
  label
) {
  expected_minutes = as.integer(
    seconds / 60
  )

  close_microseconds = as.numeric(
    seconds
  ) * 1000000 - 1

  paste0(
    "WITH bucketed AS (",
    "SELECT ",
    "to_timestamp(floor(epoch(open_time) / ",
    seconds,
    ") * ",
    seconds,
    ") AS bucket_time, ",
    "open_time AS source_open_time, ",
    "\"open\", high, low, \"close\", ",
    "base_volume, quote_volume, number_of_trades, ",
    "taker_buy_base_volume, taker_buy_quote_volume ",
    "FROM btc_1m",
    "), aggregated AS (",
    "SELECT ",
    "'Binance' AS exchange, ",
    "'spot' AS market_type, ",
    "'BTCUSDT' AS symbol, ",
    btc_fast_sql_string(label),
    " AS \"interval\", ",
    "bucket_time AS open_time, ",
    "bucket_time + INTERVAL '",
    format(
      close_microseconds,
      scientific = FALSE,
      trim = TRUE
    ),
    " microseconds' AS close_time, ",
    "arg_min(\"open\", source_open_time) AS \"open\", ",
    "max(high) AS high, ",
    "min(low) AS low, ",
    "arg_max(\"close\", source_open_time) AS \"close\", ",
    "sum(base_volume) AS base_volume, ",
    "sum(quote_volume) AS quote_volume, ",
    "sum(number_of_trades) AS number_of_trades, ",
    "sum(taker_buy_base_volume) AS taker_buy_base_volume, ",
    "sum(taker_buy_quote_volume) AS taker_buy_quote_volume, ",
    "count(*) AS observations ",
    "FROM bucketed ",
    "GROUP BY bucket_time",
    "), enriched AS (",
    "SELECT *, ",
    "observations = ",
    expected_minutes,
    " AS is_complete, ",
    "ln(\"close\") AS log_close, ",
    "100 * (ln(\"close\") - ",
    "lag(ln(\"close\")) OVER (ORDER BY open_time)) ",
    "AS log_return, ",
    "100 * (\"close\" / ",
    "lag(\"close\") OVER (ORDER BY open_time) - 1) ",
    "AS simple_return, ",
    "100 * ln(high / low) AS high_low_range, ",
    "CASE WHEN quote_volume > 0 THEN ",
    "taker_buy_quote_volume / quote_volume ",
    "ELSE NULL END AS taker_buy_share ",
    "FROM aggregated",
    ") SELECT * FROM enriched ORDER BY open_time"
  )
}

btc_fast_write_query_parquet = function(
  connection,
  query,
  output_path
) {
  dir.create(
    dirname(output_path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  temporary_path = paste0(
    output_path,
    ".tmp"
  )

  unlink(
    temporary_path,
    force = TRUE
  )

  DBI::dbExecute(
    connection,
    paste0(
      "COPY (",
      query,
      ") TO ",
      btc_fast_sql_string(
        temporary_path
      ),
      " (FORMAT PARQUET, COMPRESSION ZSTD)"
    )
  )

  if (!file.rename(
    temporary_path,
    output_path
  )) {
    copied = file.copy(
      temporary_path,
      output_path,
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
          output_path,
          "."
        ),
        call. = FALSE
      )
    }
  }

  invisible(output_path)
}

btc_fast_build_intervals = function(
  connection,
  config
) {
  interval_table = config$derived_intervals
  results = vector(
    "list",
    nrow(interval_table)
  )

  for (index in seq_len(
    nrow(interval_table)
  )) {
    label = interval_table$label[[index]]
    seconds = interval_table$seconds[[index]]

    output_path = btc_fast_interval_path(
      config,
      label
    )

    message(
      "Формую інтервал ",
      label,
      "."
    )

    query = btc_fast_aggregation_sql(
      seconds = seconds,
      label = label
    )

    btc_fast_write_query_parquet(
      connection = connection,
      query = query,
      output_path = output_path
    )

    rows = DBI::dbGetQuery(
      connection,
      paste0(
        "SELECT count(*) AS rows FROM read_parquet(",
        btc_fast_sql_string(
          output_path
        ),
        ")"
      )
    )$rows[[1]]

    results[[index]] = data.frame(
      interval = label,
      seconds = seconds,
      rows = rows,
      parquet_path = normalizePath(
        output_path,
        winslash = "/",
        mustWork = TRUE
      ),
      stringsAsFactors = FALSE
    )
  }

  data.table::rbindlist(
    results,
    use.names = TRUE
  )
}

btc_fast_save_book_rds = function(
  connection,
  config
) {
  labels = c(
    "1h",
    "4h",
    "1d"
  )

  result = list()

  for (label in labels) {
    parquet_path = btc_fast_interval_path(
      config,
      label
    )

    data = DBI::dbGetQuery(
      connection,
      paste0(
        "SELECT * FROM read_parquet(",
        btc_fast_sql_string(
          parquet_path
        ),
        ") ORDER BY open_time"
      )
    )

    data$open_time = as.POSIXct(
      data$open_time,
      tz = "UTC"
    )

    data$close_time = as.POSIXct(
      data$close_time,
      tz = "UTC"
    )

    output_path = file.path(
      config$processed_dir,
      paste0(
        config$symbol,
        "_",
        label,
        ".rds"
      )
    )

    saveRDS(
      data,
      output_path,
      compress = "gzip"
    )

    complete_data = data[
      data$is_complete %in% TRUE,
      ,
      drop = FALSE
    ]

    saveRDS(
      complete_data,
      file.path(
        config$processed_dir,
        paste0(
          config$symbol,
          "_",
          label,
          "_complete.rds"
        )
      ),
      compress = "gzip"
    )

    result[[label]] = data
  }

  list(
    data_1h = result[["1h"]],
    data_4h = result[["4h"]],
    data_1d = result[["1d"]]
  )
}

btc_fast_outputs_exist = function(
  config
) {
  required_paths = c(
    config$fast_manifest,
    file.path(
      config$processed_dir,
      paste0(
        config$symbol,
        "_1h.rds"
      )
    ),
    file.path(
      config$processed_dir,
      paste0(
        config$symbol,
        "_4h.rds"
      )
    ),
    file.path(
      config$processed_dir,
      paste0(
        config$symbol,
        "_1d.rds"
      )
    )
  )

  if (!all(
    file.exists(
      required_paths
    )
  )) {
    return(FALSE)
  }

  manifest = tryCatch(
    readRDS(
      config$fast_manifest
    ),
    error = function(error) NULL
  )

  if (
    is.null(manifest) ||
    !"parquet_path" %in% names(manifest)
  ) {
    return(FALSE)
  }

  all(
    file.exists(
      manifest$parquet_path
    )
  )
}

btc_fast_load_compact = function(
  config
) {
  if (!btc_fast_outputs_exist(config)) {
    stop(
      paste0(
        "Оптимізовані дані ще не створено. ",
        "Запустіть scripts/03_update_binance_fast.R."
      ),
      call. = FALSE
    )
  }

  list(
    data_1h = readRDS(
      file.path(
        config$processed_dir,
        paste0(
          config$symbol,
          "_1h.rds"
        )
      )
    ),
    data_4h = readRDS(
      file.path(
        config$processed_dir,
        paste0(
          config$symbol,
          "_4h.rds"
        )
      )
    ),
    data_1d = readRDS(
      file.path(
        config$processed_dir,
        paste0(
          config$symbol,
          "_1d.rds"
        )
      )
    ),
    manifest = readRDS(
      config$fast_manifest
    ),
    source = "fast_local_store"
  )
}

btc_fast_write_run_summary = function(
  config,
  start_time,
  end_time,
  validation,
  interval_manifest,
  converted_count
) {
  summary = data.frame(
    metric = c(
      "started_at",
      "finished_at",
      "elapsed_seconds",
      "workers",
      "data_table_threads",
      "duckdb_threads",
      "duckdb_memory_limit",
      "converted_archives",
      "minute_rows",
      "derived_intervals"
    ),
    value = c(
      format(
        start_time,
        tz = "UTC",
        usetz = TRUE
      ),
      format(
        end_time,
        tz = "UTC",
        usetz = TRUE
      ),
      round(
        as.numeric(
          difftime(
            end_time,
            start_time,
            units = "secs"
          )
        ),
        3
      ),
      config$workers,
      config$data_table_threads,
      config$duckdb_threads,
      config$duckdb_memory_limit,
      converted_count,
      validation$rows,
      nrow(interval_manifest)
    ),
    stringsAsFactors = FALSE
  )

  utils::write.csv(
    summary,
    file.path(
      config$validation_dir,
      "fast_pipeline_run.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  utils::write.csv(
    as.data.frame(
      interval_manifest
    ),
    file.path(
      config$validation_dir,
      "fast_interval_manifest.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  invisible(TRUE)
}

btc_fast_update = function(
  config = btc_fast_default_config(),
  update = TRUE
) {
  btc_fast_require_packages()
  btc_fast_make_dirs(config)
  btc_fast_set_threads(config)

  if (!isTRUE(update)) {
    return(
      btc_fast_load_compact(
        config
      )
    )
  }

  started_at = Sys.time()
  end_date = btc_resolve_end_date(
    config
  )

  old_manifest = if (
    file.exists(
      config$fast_manifest
    )
  ) {
    readRDS(
      config$fast_manifest
    )
  } else {
    NULL
  }

  same_end_date = !is.null(
    old_manifest
  ) &&
    "end_date" %in% names(
      old_manifest
    ) &&
    max(
      as.Date(
        old_manifest$end_date
      ),
      na.rm = TRUE
    ) == as.Date(
      end_date
    )

  if (
    isTRUE(
      same_end_date
    ) &&
    btc_fast_outputs_exist(
      config
    ) &&
    !isTRUE(
      config$force_verify_all
    )
  ) {
    message(
      "Нових завершених днів немає. Підключаю швидке локальне сховище."
    )

    return(
      btc_fast_load_compact(
        config
      )
    )
  }

  message(
    "Кінцева дата набору: ",
    end_date
  )

  archives = btc_collect_archives(
    config,
    end_date
  )

  parquet_results = btc_fast_convert_archives(
    config,
    archives
  )

  new_manifest = btc_fast_manifest(
    archives = archives,
    parquet_results = parquet_results,
    end_date = end_date
  )

  connection = btc_fast_connect(
    config
  )

  on.exit(
    btc_fast_disconnect(
      connection
    ),
    add = TRUE
  )

  btc_fast_create_views(
    connection = connection,
    config = config,
    manifest = new_manifest,
    end_date = end_date
  )

  validation = btc_fast_validate(
    connection = connection,
    config = config,
    end_date = end_date
  )

  api_sample = btc_fast_api_sample(
    connection = connection,
    sample_size = config$api_sample_size
  )

  api_check = btc_verify_api_samples(
    api_sample,
    config
  )

  btc_write_validation(
    config,
    validation,
    api_check
  )

  api_failures = sum(
    api_check$checked %in% TRUE &
      api_check$matched %in% FALSE,
    na.rm = TRUE
  )

  if (api_failures > 0L) {
    stop(
      paste0(
        "Деякі вибіркові записи не збігаються з Binance API. ",
        "Перевірте api_sample_check.csv."
      ),
      call. = FALSE
    )
  }

  if (
    isTRUE(
      config$strict_validation
    ) &&
    !isTRUE(
      validation$critical_ok
    )
  ) {
    stop(
      paste0(
        "Критична перевірка даних не пройдена. ",
        "Перевірте quality_report.csv."
      ),
      call. = FALSE
    )
  }

  if (nrow(
    validation$gaps
  ) > 0L) {
    warning(
      paste0(
        "Виявлено пропущені хвилинні інтервали. ",
        "Вони зафіксовані і не заповнюються автоматично."
      ),
      call. = FALSE
    )
  }

  interval_manifest = btc_fast_build_intervals(
    connection = connection,
    config = config
  )

  compact_data = btc_fast_save_book_rds(
    connection = connection,
    config = config
  )

  saveRDS(
    new_manifest,
    config$fast_manifest,
    compress = "gzip"
  )

  finished_at = Sys.time()

  btc_fast_write_run_summary(
    config = config,
    start_time = started_at,
    end_time = finished_at,
    validation = validation,
    interval_manifest = interval_manifest,
    converted_count = sum(
      parquet_results$converted %in% TRUE
    )
  )

  compact_data$manifest = new_manifest
  compact_data$interval_manifest = as.data.frame(
    interval_manifest
  )
  compact_data$source = "fast_updated_store"
  compact_data
}

btc_fast_query_1m = function(
  config = btc_fast_default_config(),
  start_time,
  end_time,
  columns = c(
    "open_time",
    "open",
    "high",
    "low",
    "close",
    "base_volume",
    "quote_volume"
  ),
  limit = Inf
) {
  btc_fast_require_packages()

  if (!file.exists(
    config$fast_manifest
  )) {
    stop(
      paste0(
        "Не знайдено швидкий маніфест. ",
        "Спочатку запустіть scripts/03_update_binance_fast.R."
      ),
      call. = FALSE
    )
  }

  allowed_columns = c(
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
  )

  if (
    length(columns) == 0L ||
    any(
      !columns %in% allowed_columns
    )
  ) {
    stop(
      "Запит містить недозволену назву колонки.",
      call. = FALSE
    )
  }

  start_value = as.POSIXct(
    start_time,
    tz = "UTC"
  )

  end_value = as.POSIXct(
    end_time,
    tz = "UTC"
  )

  if (
    is.na(start_value) ||
    is.na(end_value) ||
    start_value >= end_value
  ) {
    stop(
      "Потрібно вказати коректні start_time і end_time.",
      call. = FALSE
    )
  }

  manifest = readRDS(
    config$fast_manifest
  )

  connection = btc_fast_connect(
    config
  )

  on.exit(
    btc_fast_disconnect(
      connection
    ),
    add = TRUE
  )

  btc_fast_create_views(
    connection = connection,
    config = config,
    manifest = manifest,
    end_date = max(
      as.Date(
        manifest$end_date
      ),
      na.rm = TRUE
    )
  )

  column_sql = paste(
    paste0(
      '"',
      columns,
      '"'
    ),
    collapse = ", "
  )

  limit_sql = if (
    is.finite(limit)
  ) {
    paste0(
      " LIMIT ",
      as.integer(limit)
    )
  } else {
    ""
  }

  result = DBI::dbGetQuery(
    connection,
    paste0(
      "SELECT ",
      column_sql,
      " FROM btc_1m ",
      "WHERE open_time >= TIMESTAMP ",
      btc_fast_sql_string(
        format(
          start_value,
          "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        )
      ),
      " AND open_time < TIMESTAMP ",
      btc_fast_sql_string(
        format(
          end_value,
          "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        )
      ),
      " ORDER BY open_time",
      limit_sql
    )
  )

  if ("open_time" %in% names(result)) {
    result$open_time = as.POSIXct(
      result$open_time,
      tz = "UTC"
    )
  }

  if ("close_time" %in% names(result)) {
    result$close_time = as.POSIXct(
      result$close_time,
      tz = "UTC"
    )
  }

  result
}
