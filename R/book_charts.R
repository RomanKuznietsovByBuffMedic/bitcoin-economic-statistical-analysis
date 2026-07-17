# Функції для таблиць і інтерактивних графіків у Quarto-книзі
#
# Цей файл не завантажує дані з інтернету.
# Він читає готові локальні RDS-файли, створені конвеєром Binance.

btc_book_chart_height = 860L
btc_book_chart_margins = list(
  l = 70,
  r = 20,
  t = 80,
  b = 60
)

btc_book_chart_container = function(widget) {
  widget$width = "100%"
  widget$height = paste0(
    btc_book_chart_height,
    "px"
  )

  if (!is.null(widget$sizingPolicy)) {
    widget$sizingPolicy$browser$fill = FALSE
    widget$sizingPolicy$viewer$fill = FALSE
    widget$sizingPolicy$browser$defaultHeight = btc_book_chart_height
    widget$sizingPolicy$viewer$defaultHeight = btc_book_chart_height
  }

  htmltools::div(
    class = "btc-book-chart",
    style = paste0(
      "width: 100%; ",
      "max-width: 100%; ",
      "height: ",
      btc_book_chart_height,
      "px; ",
      "min-height: ",
      btc_book_chart_height,
      "px; ",
      "overflow: visible; ",
      "box-sizing: border-box;"
    ),
    widget
  )
}


btc_book_require_packages = function() {
  required_packages = c(
    "htmltools",
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
        ". Спочатку запустіть scripts/03_update_binance_fast.R."
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
          "Запустіть scripts/03_update_binance_fast.R."
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
    name = "BTCUSDT"
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
    hoverinfo = "x+y"
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
    height = btc_book_chart_height,
    autosize = TRUE,
    margin = btc_book_chart_margins
  )

  combined_chart = plotly::config(
    combined_chart,
    displaylogo = FALSE,
    responsive = TRUE,
    scrollZoom = TRUE
  )

  btc_book_chart_container(
    plotly::partial_bundle(
      combined_chart
    )
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
    hoverinfo = "x+y"
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
    height = btc_book_chart_height,
    autosize = TRUE,
    margin = btc_book_chart_margins
  )

  close_chart = plotly::config(
    close_chart,
    displaylogo = FALSE,
    responsive = TRUE,
    scrollZoom = TRUE
  )

  btc_book_chart_container(
    plotly::partial_bundle(
      close_chart
    )
  )
}

# Похідні графіки для розділу даних

btc_book_prepare_metrics = function(data) {
  result = data

  if (!"log_return" %in% names(result)) {
    result$log_return = c(
      NA_real_,
      100 * diff(log(result$close))
    )
  }

  if (!"high_low_range" %in% names(result)) {
    result$high_low_range = 100 * log(
      result$high / result$low
    )
  }

  if (!"taker_buy_share" %in% names(result)) {
    result$taker_buy_share = ifelse(
      result$quote_volume > 0,
      result$taker_buy_quote_volume / result$quote_volume,
      NA_real_
    )
  }

  result
}

btc_book_rolling_sd = function(x, window) {
  window = as.integer(window)

  if (!is.finite(window) || window < 2L) {
    stop(
      "Вікно ковзного стандартного відхилення має бути не менше 2.",
      call. = FALSE
    )
  }

  result = rep(
    NA_real_,
    length(x)
  )

  if (length(x) < window) {
    return(result)
  }

  for (index in seq.int(window, length(x))) {
    values = x[seq.int(index - window + 1L, index)]

    if (all(is.finite(values))) {
      result[[index]] = stats::sd(values)
    }
  }

  result
}

btc_book_finalize_derived_chart = function(
  chart,
  title,
  showlegend = FALSE
) {
  chart = plotly::layout(
    chart,
    title = list(
      text = title
    ),
    showlegend = showlegend,
    hovermode = "x unified",
    height = btc_book_chart_height,
    autosize = TRUE,
    margin = btc_book_chart_margins
  )

  chart = plotly::config(
    chart,
    displaylogo = FALSE,
    responsive = TRUE,
    scrollZoom = TRUE
  )

  widget = plotly::partial_bundle(chart)

  if (exists("btc_book_chart_container", mode = "function")) {
    return(
      btc_book_chart_container(widget)
    )
  }

  widget
}

btc_book_return_volatility = function(
  data,
  title,
  window = 30L
) {
  chart_data = btc_book_prepare_metrics(data)
  chart_data$rolling_sd = btc_book_rolling_sd(
    chart_data$log_return,
    window = window
  )
  chart_data$zero = 0

  return_chart = plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~log_return,
    type = "scatter",
    mode = "lines",
    name = "Логарифмічна дохідність",
    hovertemplate = paste0(
      "%{x}<br>",
      "Дохідність: %{y:.3f}%",
      "<extra></extra>"
    )
  )

  return_chart = plotly::add_lines(
    return_chart,
    y = ~zero,
    name = "Нуль",
    line = list(
      dash = "dot",
      width = 1
    ),
    hoverinfo = "skip"
  )

  return_chart = plotly::layout(
    return_chart,
    yaxis = list(
      title = "Дохідність, %"
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

  volatility_chart = plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~rolling_sd,
    type = "scatter",
    mode = "lines",
    name = paste0(
      window,
      "-денне стандартне відхилення"
    ),
    hovertemplate = paste0(
      "%{x}<br>",
      "Стандартне відхилення: %{y:.3f} в.п.",
      "<extra></extra>"
    )
  )

  volatility_chart = plotly::layout(
    volatility_chart,
    yaxis = list(
      title = "Стандартне відхилення, в.п."
    ),
    xaxis = list(
      title = "Час, UTC"
    )
  )

  combined_chart = plotly::subplot(
    return_chart,
    volatility_chart,
    nrows = 2,
    heights = c(
      0.56,
      0.44
    ),
    shareX = TRUE,
    titleY = TRUE,
    margin = 0.05
  )

  btc_book_finalize_derived_chart(
    chart = combined_chart,
    title = title,
    showlegend = FALSE
  )
}

btc_book_range_taker_share = function(
  data,
  title,
  days = 365
) {
  chart_data = btc_book_filter_period(
    btc_book_prepare_metrics(data),
    days = days
  )

  chart_data$taker_buy_share_percent = 100 *
    chart_data$taker_buy_share
  chart_data$share_reference = 50

  range_chart = plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~high_low_range,
    type = "scatter",
    mode = "lines",
    name = "Внутрішній діапазон",
    hovertemplate = paste0(
      "%{x}<br>",
      "Діапазон: %{y:.3f}%",
      "<extra></extra>"
    )
  )

  range_chart = plotly::layout(
    range_chart,
    yaxis = list(
      title = "100 log(H/L), %"
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

  share_chart = plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~taker_buy_share_percent,
    type = "scatter",
    mode = "lines",
    name = "Частка taker buy",
    hovertemplate = paste0(
      "%{x}<br>",
      "Частка: %{y:.2f}%",
      "<extra></extra>"
    )
  )

  share_chart = plotly::add_lines(
    share_chart,
    y = ~share_reference,
    name = "50%",
    line = list(
      dash = "dot",
      width = 1
    ),
    hoverinfo = "skip"
  )

  share_chart = plotly::layout(
    share_chart,
    yaxis = list(
      title = "Частка, %",
      range = c(
        0,
        100
      )
    ),
    xaxis = list(
      title = "Час, UTC"
    )
  )

  combined_chart = plotly::subplot(
    range_chart,
    share_chart,
    nrows = 2,
    heights = c(
      0.50,
      0.50
    ),
    shareX = TRUE,
    titleY = TRUE,
    margin = 0.05
  )

  btc_book_finalize_derived_chart(
    chart = combined_chart,
    title = title,
    showlegend = FALSE
  )
}

btc_book_market_activity = function(
  data,
  title,
  days = 365
) {
  chart_data = btc_book_filter_period(
    data,
    days = days
  )

  volume_chart = plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~quote_volume,
    type = "scatter",
    mode = "lines",
    name = "Оборот у USDT",
    hovertemplate = paste0(
      "%{x}<br>",
      "Оборот: %{y:,.0f} USDT",
      "<extra></extra>"
    )
  )

  volume_chart = plotly::layout(
    volume_chart,
    yaxis = list(
      title = "Оборот, USDT",
      tickformat = "~s"
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

  trades_chart = plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~number_of_trades,
    type = "scatter",
    mode = "lines",
    name = "Кількість угод",
    hovertemplate = paste0(
      "%{x}<br>",
      "Угод: %{y:,.0f}",
      "<extra></extra>"
    )
  )

  trades_chart = plotly::layout(
    trades_chart,
    yaxis = list(
      title = "Кількість угод",
      tickformat = "~s"
    ),
    xaxis = list(
      title = "Час, UTC"
    )
  )

  combined_chart = plotly::subplot(
    volume_chart,
    trades_chart,
    nrows = 2,
    heights = c(
      0.50,
      0.50
    ),
    shareX = TRUE,
    titleY = TRUE,
    margin = 0.05
  )

  btc_book_finalize_derived_chart(
    chart = combined_chart,
    title = title,
    showlegend = FALSE
  )
}

btc_book_bybit_path = function(
  project_root = ".",
  interval = "4h"
) {
  file.path(
    normalizePath(
      project_root,
      winslash = "/",
      mustWork = TRUE
    ),
    "data",
    "processed",
    "bybit",
    "spot",
    "BTCUSDT",
    paste0(
      "BTCUSDT_",
      interval,
      ".rds"
    )
  )
}

btc_book_exchange_comparison_or_note = function(
  binance_data,
  project_root = ".",
  days = 180
) {
  bybit_path = btc_book_bybit_path(
    project_root = project_root,
    interval = "4h"
  )

  if (!file.exists(bybit_path)) {
    return(
      htmltools::div(
        class = "callout callout-style-default callout-note",
        htmltools::div(
          class = "callout-header d-flex align-content-center",
          htmltools::div(
            class = "callout-title-container flex-fill",
            "Порівняльні дані Bybit ще не створені"
          )
        ),
        htmltools::div(
          class = "callout-body-container callout-body",
          htmltools::p(
            paste0(
              "Запустіть scripts/04_get_bybit_comparison.R, ",
              "після чого повторіть quarto render."
            )
          )
        )
      )
    )
  }

  bybit_data = readRDS(bybit_path)

  if (
    "is_complete" %in% names(bybit_data)
  ) {
    bybit_data = bybit_data[
      bybit_data$is_complete %in% TRUE,
      ,
      drop = FALSE
    ]
  }

  binance_recent = btc_book_filter_period(
    data = binance_data,
    days = days
  )

  bybit_recent = btc_book_filter_period(
    data = bybit_data,
    days = days
  )

  comparison = merge(
    binance_recent[
      ,
      c(
        "open_time",
        "close"
      ),
      drop = FALSE
    ],
    bybit_recent[
      ,
      c(
        "open_time",
        "close"
      ),
      drop = FALSE
    ],
    by = "open_time",
    suffixes = c(
      "_binance",
      "_bybit"
    )
  )

  if (nrow(comparison) < 2L) {
    return(
      htmltools::p(
        "Недостатньо спільних спостережень Binance і Bybit."
      )
    )
  }

  comparison$binance_index = 100 *
    comparison$close_binance /
    comparison$close_binance[[1]]

  comparison$bybit_index = 100 *
    comparison$close_bybit /
    comparison$close_bybit[[1]]

  chart = plotly::plot_ly(
    data = comparison,
    x = ~open_time,
    y = ~binance_index,
    type = "scatter",
    mode = "lines",
    name = "Binance"
  )

  chart = plotly::add_lines(
    chart,
    y = ~bybit_index,
    name = "Bybit"
  )

  chart = plotly::layout(
    chart,
    title = list(
      text = paste0(
        "BTCUSDT, Binance і Bybit, 4 години, ",
        days,
        " днів"
      )
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
      title = "Нормована ціна, початок = 100"
    ),
    height = btc_book_chart_height,
    autosize = TRUE,
    margin = btc_book_chart_margins,
    hovermode = "x unified",
    legend = list(
      orientation = "h",
      x = 0,
      y = 1.08
    )
  )

  chart = plotly::config(
    chart,
    displaylogo = FALSE,
    responsive = TRUE,
    scrollZoom = TRUE
  )

  btc_book_chart_container(
    plotly::partial_bundle(
      chart
    )
  )
}
