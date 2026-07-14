# Функції для таблиць і інтерактивних графіків у Quarto-книзі
#
# Цей файл не завантажує дані з інтернету.
# Він читає готові локальні RDS-файли, створені конвеєром Binance.

btc_book_require_packages = function() {
  required_packages = c(
    "knitr",
    "plotly"
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
        "Для рендерингу розділу бракує пакетів: ",
        paste(missing_packages, collapse = ", "),
        ". Встановіть їх через renv::install()."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

btc_book_data_paths = function(project_root = ".") {
  root = normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )

  processed_dir = file.path(
    root,
    "data",
    "processed",
    "binance",
    "spot",
    "BTCUSDT"
  )

  validation_dir = file.path(
    root,
    "data",
    "validation",
    "binance",
    "spot",
    "BTCUSDT"
  )

  list(
    processed_dir = processed_dir,
    validation_dir = validation_dir,
    data_1h_complete = file.path(
      processed_dir,
      "BTCUSDT_1h_complete.rds"
    ),
    data_4h_complete = file.path(
      processed_dir,
      "BTCUSDT_4h_complete.rds"
    ),
    data_1d_complete = file.path(
      processed_dir,
      "BTCUSDT_1d_complete.rds"
    ),
    data_1h = file.path(
      processed_dir,
      "BTCUSDT_1h.rds"
    ),
    data_4h = file.path(
      processed_dir,
      "BTCUSDT_4h.rds"
    ),
    data_1d = file.path(
      processed_dir,
      "BTCUSDT_1d.rds"
    ),
    quality_report = file.path(
      validation_dir,
      "quality_report.csv"
    ),
    gap_summary = file.path(
      validation_dir,
      "gap_summary.csv"
    ),
    api_summary = file.path(
      validation_dir,
      "api_summary.csv"
    ),
    incomplete_summary = file.path(
      validation_dir,
      "incomplete_bars_summary.csv"
    )
  )
}

btc_book_read_preferred_rds = function(
  complete_path,
  fallback_path
) {
  selected_path = if (file.exists(complete_path)) {
    complete_path
  } else {
    fallback_path
  }

  if (!file.exists(selected_path)) {
    stop(
      paste0(
        "Не знайдено локальний набір даних: ",
        selected_path,
        ". Спочатку запустіть scripts/01_get_binance_data.R, ",
        "а потім scripts/02_validate_and_build_charts.R."
      ),
      call. = FALSE
    )
  }

  data = readRDS(selected_path)

  if (
    "is_complete" %in% names(data) &&
    !grepl("_complete[.]rds$", selected_path)
  ) {
    data = data[
      data$is_complete %in% TRUE,
      ,
      drop = FALSE
    ]
  }

  rownames(data) = NULL
  data
}

btc_book_load_data = function(project_root = ".") {
  btc_book_require_packages()

  paths = btc_book_data_paths(project_root)

  list(
    paths = paths,
    data_1h = btc_book_read_preferred_rds(
      paths$data_1h_complete,
      paths$data_1h
    ),
    data_4h = btc_book_read_preferred_rds(
      paths$data_4h_complete,
      paths$data_4h
    ),
    data_1d = btc_book_read_preferred_rds(
      paths$data_1d_complete,
      paths$data_1d
    )
  )
}

btc_book_format_time = function(x) {
  format(
    x,
    format = "%Y-%m-%d %H:%M",
    tz = "UTC"
  )
}

btc_book_interval_row = function(
  data,
  interval_name
) {
  data.frame(
    Інтервал = interval_name,
    Спостережень = format(
      nrow(data),
      big.mark = " ",
      scientific = FALSE
    ),
    Початок = btc_book_format_time(
      min(
        data$open_time,
        na.rm = TRUE
      )
    ),
    Кінець = btc_book_format_time(
      max(
        data$open_time,
        na.rm = TRUE
      )
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

btc_book_interval_summary = function(book_data) {
  rbind(
    btc_book_interval_row(
      book_data$data_1h,
      "1 година"
    ),
    btc_book_interval_row(
      book_data$data_4h,
      "4 години"
    ),
    btc_book_interval_row(
      book_data$data_1d,
      "1 день"
    )
  )
}

btc_book_latest_date = function(book_data) {
  format(
    max(
      book_data$data_1d$open_time,
      na.rm = TRUE
    ),
    format = "%Y-%m-%d",
    tz = "UTC"
  )
}

btc_book_first_date = function(book_data) {
  format(
    min(
      book_data$data_1d$open_time,
      na.rm = TRUE
    ),
    format = "%Y-%m-%d",
    tz = "UTC"
  )
}

btc_book_read_csv_if_exists = function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

btc_book_validation_summary = function(book_data) {
  paths = book_data$paths

  gap_summary = btc_book_read_csv_if_exists(
    paths$gap_summary
  )

  api_summary = btc_book_read_csv_if_exists(
    paths$api_summary
  )

  incomplete_summary = btc_book_read_csv_if_exists(
    paths$incomplete_summary
  )

  if (
    is.null(gap_summary) ||
    is.null(api_summary) ||
    is.null(incomplete_summary)
  ) {
    return(
      data.frame(
        Перевірка = "Звіти",
        Результат = paste0(
          "Не всі підсумкові CSV-файли знайдено. ",
          "Запустіть scripts/02_validate_and_build_charts.R."
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
  }

  gap_value = function(metric_name) {
    value = gap_summary$value[
      gap_summary$metric == metric_name
    ]

    if (length(value) == 0L) {
      return(NA_real_)
    }

    as.numeric(value[[1]])
  }

  api_value = function(metric_name) {
    value = api_summary$value[
      api_summary$metric == metric_name
    ]

    if (length(value) == 0L) {
      return(NA_real_)
    }

    as.numeric(value[[1]])
  }

  incomplete_total = sum(
    as.numeric(
      incomplete_summary$incomplete_rows
    ),
    na.rm = TRUE
  )

  data.frame(
    Перевірка = c(
      "Розриви у хвилинному часі",
      "Пропущені хвилини",
      "Неповні агреговані свічки",
      "API-перевірки",
      "Невдалі API-збіги"
    ),
    Результат = c(
      format(
        gap_value("gap_count"),
        big.mark = " ",
        scientific = FALSE
      ),
      format(
        gap_value("missing_minutes"),
        big.mark = " ",
        scientific = FALSE
      ),
      format(
        incomplete_total,
        big.mark = " ",
        scientific = FALSE
      ),
      format(
        api_value("api_checked"),
        big.mark = " ",
        scientific = FALSE
      ),
      format(
        api_value("api_failed"),
        big.mark = " ",
        scientific = FALSE
      )
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

btc_book_filter_period = function(
  data,
  days = NULL
) {
  result = data

  if (
    !is.null(days) &&
    is.finite(days) &&
    days > 0
  ) {
    last_time = max(
      result$open_time,
      na.rm = TRUE
    )

    first_time = last_time -
      as.numeric(days) * 24 * 60 * 60

    result = result[
      result$open_time >= first_time,
      ,
      drop = FALSE
    ]
  }

  rownames(result) = NULL
  result
}

btc_book_range_buttons = function() {
  list(
    list(
      count = 7,
      label = "7 днів",
      step = "day",
      stepmode = "backward"
    ),
    list(
      count = 30,
      label = "30 днів",
      step = "day",
      stepmode = "backward"
    ),
    list(
      count = 90,
      label = "90 днів",
      step = "day",
      stepmode = "backward"
    ),
    list(
      step = "all",
      label = "Увесь період"
    )
  )
}

btc_book_candlestick_volume = function(
  data,
  title,
  days = NULL
) {
  chart_data = btc_book_filter_period(
    data = data,
    days = days
  )

  if (nrow(chart_data) == 0L) {
    stop(
      "Для інтерактивного графіка не залишилося даних.",
      call. = FALSE
    )
  }

  price_chart = plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    type = "candlestick",
    open = ~open,
    high = ~high,
    low = ~low,
    close = ~close,
    name = "BTCUSDT",
    height = 520
  )

  price_chart = plotly::layout(
    price_chart,
    yaxis = list(
      title = "Ціна, USDT"
    ),
    xaxis = list(
      title = "",
      rangeslider = list(
        visible = FALSE
      ),
      rangeselector = list(
        buttons = btc_book_range_buttons()
      )
    )
  )

  volume_chart = plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~quote_volume,
    type = "bar",
    name = "Обсяг, USDT",
    hoverinfo = "x+y",
    height = 200
  )

  volume_chart = plotly::layout(
    volume_chart,
    yaxis = list(
      title = "Обсяг, USDT"
    ),
    xaxis = list(
      title = "Час, UTC"
    )
  )

  combined_chart = plotly::subplot(
    price_chart,
    volume_chart,
    nrows = 2,
    heights = c(
      0.76,
      0.24
    ),
    shareX = TRUE,
    titleY = TRUE,
    margin = 0.03
  )

  combined_chart = plotly::layout(
    combined_chart,
    title = list(
      text = title
    ),
    showlegend = FALSE,
    hovermode = "x unified",
    margin = list(
      l = 75,
      r = 30,
      t = 70,
      b = 55
    )
  )

  combined_chart = plotly::config(
    combined_chart,
    displaylogo = FALSE,
    responsive = TRUE,
    scrollZoom = TRUE
  )

  plotly::partial_bundle(
    combined_chart
  )
}

btc_book_close_history = function(
  data,
  title
) {
  close_chart = plotly::plot_ly(
    data = data,
    x = ~open_time,
    y = ~close,
    type = "scatter",
    mode = "lines",
    name = "Ціна закриття",
    hoverinfo = "x+y",
    height = 520
  )

  close_chart = plotly::layout(
    close_chart,
    title = list(
      text = title
    ),
    xaxis = list(
      title = "Час, UTC",
      rangeslider = list(
        visible = TRUE
      ),
      rangeselector = list(
        buttons = btc_book_range_buttons()
      )
    ),
    yaxis = list(
      title = "Ціна, USDT"
    ),
    showlegend = FALSE,
    hovermode = "x unified",
    margin = list(
      l = 75,
      r = 30,
      t = 70,
      b = 55
    )
  )

  close_chart = plotly::config(
    close_chart,
    displaylogo = FALSE,
    responsive = TRUE,
    scrollZoom = TRUE
  )

  plotly::partial_bundle(
    close_chart
  )
}
