# Interactive diagnostics for the hourly AR(1) model ---------------------

rolling_root_mean_square <- function(values, window = 168L) {
  values <- as.numeric(values)
  window <- as.integer(window)
  if (window < 2L || length(values) < window) {
    stop("Ковзне вікно має бути меншим за кількість спостережень.")
  }

  result <- rep(NA_real_, length(values))
  squared <- values^2
  cumulative <- c(0, cumsum(squared))
  for (index in seq.int(window, length(values))) {
    window_sum <- cumulative[[index + 1L]] -
      cumulative[[index - window + 1L]]
    result[[index]] <- sqrt(window_sum / window)
  }
  result
}

forecast_calibration_bins <- function(
  forecast,
  actual,
  number_of_bins = 10L
) {
  forecast <- as.numeric(forecast)
  actual <- as.numeric(actual)
  number_of_bins <- as.integer(number_of_bins)

  if (
    length(forecast) != length(actual) ||
      length(forecast) < number_of_bins ||
      anyNA(forecast) ||
      anyNA(actual)
  ) {
    stop("Для калібрування потрібні узгоджені прогнози й факти без NA.")
  }

  ordered_rank <- rank(forecast, ties.method = "first")
  bin <- pmin(
    number_of_bins,
    ceiling(number_of_bins * ordered_rank / length(forecast))
  )
  overall_positive_share <- 100 * mean(actual > 0)

  result <- do.call(
    rbind,
    lapply(seq_len(number_of_bins), function(bin_id) {
      selected <- bin == bin_id
      data.frame(
        decile = bin_id,
        mean_forecast_percent = 100 * mean(forecast[selected]),
        mean_actual_percent = 100 * mean(actual[selected]),
        positive_share_percent = 100 * mean(actual[selected] > 0),
        observations = sum(selected),
        overall_positive_share_percent = overall_positive_share,
        stringsAsFactors = FALSE
      )
    })
  )
  rownames(result) <- NULL
  result
}

monthly_trade_activity <- function(target_time, position) {
  target_time <- as.POSIXct(target_time, tz = "UTC")
  position <- as.integer(position)

  if (
    length(target_time) != length(position) ||
      length(target_time) == 0L ||
      anyNA(position) ||
      any(!position %in% c(0L, 1L))
  ) {
    stop("Для місячної активності потрібні узгоджені позиції 0 або 1.")
  }

  month_label <- format(target_time, "%Y-%m", tz = "UTC")
  switches <- c(FALSE, position[-1L] != position[-length(position)])
  month_indices <- split(seq_along(position), month_label)

  result <- do.call(
    rbind,
    lapply(names(month_indices), function(month_name) {
      selected <- month_indices[[month_name]]
      data.frame(
        month = as.POSIXct(
          paste0(month_name, "-01 00:00:00"),
          tz = "UTC"
        ),
        btc_share_percent = 100 * mean(position[selected]),
        switches = sum(switches[selected]),
        hours = length(selected),
        stringsAsFactors = FALSE
      )
    })
  )
  rownames(result) <- NULL
  result
}

prepare_ar1_chart_data <- function(
  forecasts,
  refits,
  forecast_metrics,
  strategy_paths,
  signal_threshold = 0,
  rolling_window = 168L,
  acf_max_lag = 48L
) {
  require_model_columns(
    forecasts,
    c(
      "target_time",
      "actual_log_return",
      "ar1_log_return_forecast",
      "naive_log_return_forecast",
      "actual_close",
      "ar1_price_forecast",
      "naive_price_forecast"
    ),
    "даних графічної діагностики AR(1)"
  )
  require_model_columns(
    refits,
    c("refit_time", "phi", "phi_standard_error", "phi_p_value"),
    "переоцінювань AR(1)"
  )

  ar1_return_error <-
    forecasts$ar1_log_return_forecast - forecasts$actual_log_return
  naive_return_error <-
    forecasts$naive_log_return_forecast - forecasts$actual_log_return

  refit_data <- refits
  refit_data$phi_lower <-
    refit_data$phi - 1.96 * refit_data$phi_standard_error
  refit_data$phi_upper <-
    refit_data$phi + 1.96 * refit_data$phi_standard_error
  refit_data$phi_ci_excludes_zero <-
    refit_data$phi_lower > 0 | refit_data$phi_upper < 0

  metric_rows <- list()
  metric_names <- c(
    return_mae = "MAE дохідності",
    return_rmse = "RMSE дохідності",
    price_mae = "MAE ціни",
    price_rmse = "RMSE ціни"
  )
  for (metric in names(metric_names)) {
    ar1_value <- forecast_metrics[
      forecast_metrics$method == "AR(1)",
      metric
    ]
    naive_value <- forecast_metrics[
      forecast_metrics$method == "Наївний прогноз",
      metric
    ]
    metric_rows[[length(metric_rows) + 1L]] <- data.frame(
      metric = metric_names[[metric]],
      improvement_percent = 100 * (1 - ar1_value / naive_value),
      stringsAsFactors = FALSE
    )
  }
  metric_improvement <- do.call(rbind, metric_rows)
  metric_improvement$metric <- factor(
    metric_improvement$metric,
    levels = rev(metric_names)
  )

  rolling_data <- data.frame(
    target_time = forecasts$target_time,
    ar1_rmse = rolling_root_mean_square(
      ar1_return_error,
      rolling_window
    ) * 100,
    naive_rmse = rolling_root_mean_square(
      naive_return_error,
      rolling_window
    ) * 100,
    stringsAsFactors = FALSE
  )
  rolling_data$difference <-
    rolling_data$ar1_rmse - rolling_data$naive_rmse

  rolling_day <- as.Date(rolling_data$target_time, tz = "UTC")
  rolling_indices <- split(seq_len(nrow(rolling_data)), rolling_day)
  rolling_daily <- do.call(
    rbind,
    lapply(names(rolling_indices), function(day_name) {
      selected <- rolling_indices[[day_name]]
      values <- rolling_data$difference[selected]
      values <- values[is.finite(values)]
      data.frame(
        date = as.POSIXct(paste0(day_name, " 00:00:00"), tz = "UTC"),
        difference = if (length(values) == 0L) NA_real_ else mean(values),
        stringsAsFactors = FALSE
      )
    })
  )
  rownames(rolling_daily) <- NULL

  acf_object <- stats::acf(
    ar1_return_error,
    lag.max = acf_max_lag,
    plot = FALSE,
    na.action = stats::na.pass
  )
  acf_values <- as.numeric(acf_object$acf)[-1L]
  acf_data <- data.frame(
    lag = seq_along(acf_values),
    autocorrelation = acf_values,
    upper_bound = 1.96 / sqrt(length(ar1_return_error)),
    stringsAsFactors = FALSE
  )

  path_data <- strategy_paths
  path_data <- path_data[order(path_data$strategy, path_data$time), ]
  path_data$running_maximum <- ave(
    path_data$wealth,
    path_data$strategy,
    FUN = cummax
  )
  path_data$drawdown <- path_data$wealth / path_data$running_maximum - 1

  position_values <- as.integer(
    forecasts$ar1_log_return_forecast > signal_threshold
  )

  list(
    refits = refit_data,
    metric_improvement = metric_improvement,
    forecast_pairs = data.frame(
      forecast = forecasts$ar1_log_return_forecast,
      actual = forecasts$actual_log_return,
      stringsAsFactors = FALSE
    ),
    forecast_calibration = forecast_calibration_bins(
      forecast = forecasts$ar1_log_return_forecast,
      actual = forecasts$actual_log_return,
      number_of_bins = 10L
    ),
    rolling_rmse = rolling_daily,
    rolling_window = rolling_window,
    forecast_error_acf = acf_data,
    strategy_paths = path_data,
    trade_activity = monthly_trade_activity(
      target_time = forecasts$target_time,
      position = position_values
    )
  )
}

plot_ar1_phi_stability <- function(data) {
  palette <- book_plot_palette("dark")

  plotly::plot_ly() |>
    plotly::add_segments(
      data = data,
      x = ~refit_time,
      xend = ~refit_time,
      y = ~phi_lower,
      yend = ~phi_upper,
      line = list(color = palette[["neutral"]], width = 3),
      meta = book_trace_meta("neutral"),
      hoverinfo = "skip",
      showlegend = FALSE
    ) |>
    plotly::add_markers(
      data = data,
      x = ~refit_time,
      y = ~phi,
      marker = list(color = palette[["ar1"]], size = 9),
      meta = book_trace_meta("ar1"),
      name = "Оцінка φ₁",
      text = ~paste0(
        "φ₁ = ", formatC(phi, digits = 4, format = "f"),
        "<br>Орієнтовний 95% інтервал: [",
        formatC(phi_lower, digits = 4, format = "f"),
        ", ",
        formatC(phi_upper, digits = 4, format = "f"),
        "]",
        "<br>OLS p-value = ",
        formatC(phi_p_value, digits = 4, format = "f")
      ),
      hovertemplate = "%{x|%Y-%m-%d}<br>%{text}<extra></extra>"
    ) |>
    plotly::layout(
      title = book_plot_title(
        "Коефіцієнт AR(1) під час переоцінювань"
      ),
      xaxis = book_time_axis("Дата переоцінювання", rangeslider = TRUE),
      yaxis = book_axis_style("Коефіцієнт φ₁"),
      shapes = list(
        list(
          type = "line",
          x0 = min(data$refit_time),
          x1 = max(data$refit_time),
          y0 = 0,
          y1 = 0,
          line = list(color = palette[["negative"]], width = 1.2)
        )
      )
    ) |>
    render_book_widget(
      size = "standard",
      margin = book_chart_margin("standard"),
      showlegend = FALSE
    )
}

plot_ar1_forecast_calibration <- function(data) {
  palette <- book_plot_palette("dark")
  baseline <- unique(data$overall_positive_share_percent)[[1L]]

  plotly::plot_ly(
    data = data,
    x = ~decile,
    y = ~positive_share_percent,
    type = "scatter",
    mode = "lines+markers",
    line = list(color = palette[["ar1"]], width = 2.4),
    marker = list(color = palette[["ar1"]], size = 9),
    meta = book_trace_meta("ar1"),
    text = ~paste0(
      "Група прогнозів: ", decile, " із 10",
      "<br>Середній прогноз: ",
      formatC(mean_forecast_percent, digits = 4, format = "f"),
      "%",
      "<br>Середній факт: ",
      formatC(mean_actual_percent, digits = 4, format = "f"),
      "%",
      "<br>Позитивних годин: ",
      formatC(positive_share_percent, digits = 2, format = "f"),
      "%",
      "<br>Спостережень: ", observations
    ),
    hovertemplate = "%{text}<extra></extra>",
    showlegend = FALSE
  ) |>
    plotly::layout(
      title = book_plot_title(
        "Частка позитивних годин за групами прогнозів"
      ),
      xaxis = modifyList(
        book_axis_style("Група прогнозів: від найнижчих до найвищих"),
        list(
          tickmode = "array",
          tickvals = 1:10,
          ticktext = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
          range = c(0.5, 10.5)
        )
      ),
      yaxis = modifyList(
        book_axis_style("Частка позитивних фактичних годин, %"),
        list(ticksuffix = "%")
      ),
      shapes = list(
        list(
          type = "line",
          x0 = 0.5,
          x1 = 10.5,
          y0 = baseline,
          y1 = baseline,
          line = list(
            color = palette[["naive"]],
            width = 1.2,
            dash = "dash"
          )
        )
      ),
      annotations = list(
        list(
          x = 10.4,
          y = baseline,
          text = paste0(
            "Усі години: ",
            formatC(baseline, digits = 1, format = "f"),
            "%"
          ),
          showarrow = FALSE,
          xanchor = "right",
          yanchor = "bottom",
          font = list(size = 12)
        )
      )
    ) |>
    render_book_widget(
      size = "standard",
      margin = book_chart_margin("calibration"),
      showlegend = FALSE
    )
}

plot_ar1_metric_improvement <- function(data) {
  palette <- book_plot_palette("dark")
  maximum <- max(abs(data$improvement_percent), na.rm = TRUE)
  limit <- max(0.05, 1.25 * maximum)

  plotly::plot_ly() |>
    plotly::add_segments(
      data = data,
      x = 0,
      xend = ~improvement_percent,
      y = ~metric,
      yend = ~metric,
      line = list(color = palette[["neutral"]], width = 2),
      meta = book_trace_meta("neutral"),
      hoverinfo = "skip",
      showlegend = FALSE
    ) |>
    plotly::add_trace(
      data = data,
      x = ~improvement_percent,
      y = ~metric,
      type = "scatter",
      mode = "markers+text",
      marker = list(color = palette[["ar1"]], size = 11),
      meta = book_trace_meta("ar1"),
      text = ~paste0(
        ifelse(improvement_percent >= 0, "+", ""),
        formatC(improvement_percent, digits = 4, format = "f"),
        "%"
      ),
      textposition = "middle right",
      textfont = list(size = 12, color = palette[["text"]]),
      hovertemplate = paste0(
        "%{y}",
        "<br>Зменшення похибки: %{x:.4f}%",
        "<extra></extra>"
      ),
      showlegend = FALSE
    ) |>
    plotly::layout(
      title = book_plot_title(
        "Зміна похибки AR(1) відносно наївного прогнозу"
      ),
      xaxis = modifyList(
        book_axis_style("Зменшення похибки, %"),
        list(range = c(-limit, limit), ticksuffix = "%")
      ),
      yaxis = modifyList(
        book_axis_style(NULL),
        list(
          categoryorder = "array",
          categoryarray = levels(data$metric)
        )
      ),
      shapes = list(
        list(
          type = "line",
          x0 = 0,
          x1 = 0,
          y0 = -0.5,
          y1 = 3.5,
          line = list(color = palette[["negative"]], width = 1.1)
        )
      )
    ) |>
    render_book_widget(
      size = "compact",
      margin = book_chart_margin("metric_labels"),
      showlegend = FALSE
    )
}

plot_ar1_rolling_rmse_difference <- function(data, rolling_window) {
  palette <- book_plot_palette("dark")

  plotly::plot_ly(
    data = data,
    x = ~date,
    y = ~difference,
    type = "scatter",
    mode = "lines",
    line = list(color = palette[["accent"]], width = 2.2),
    meta = book_trace_meta("accent"),
    hovertemplate = paste0(
      "%{x|%Y-%m-%d}",
      "<br>RMSE AR(1) мінус наївний: %{y:.5f} в.п.",
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::layout(
      title = book_plot_title(
        paste0(
          "Різниця RMSE у ",
          rolling_window,
          "-годинних вікнах"
        )
      ),
      xaxis = book_time_axis("Дата, UTC", rangeslider = TRUE),
      yaxis = book_axis_style(
        "RMSE AR(1) мінус RMSE наївного прогнозу, в.п."
      ),
      shapes = list(
        list(
          type = "line",
          x0 = min(data$date),
          x1 = max(data$date),
          y0 = 0,
          y1 = 0,
          line = list(color = palette[["naive"]], width = 1.2)
        )
      )
    ) |>
    render_book_widget(
      hovermode = "x unified",
      size = "standard",
      margin = book_chart_margin("rolling"),
      showlegend = FALSE,
      auto_y_on_x = TRUE
    )
}

plot_ar1_forecast_error_acf <- function(data) {
  palette <- book_plot_palette("dark")
  upper <- unique(data$upper_bound)[[1L]]

  plotly::plot_ly(
    data = data,
    x = ~lag,
    y = ~autocorrelation,
    type = "bar",
    marker = list(color = palette[["ar1"]]),
    meta = book_trace_meta("ar1"),
    hovertemplate = "Лаг: %{x} год.<br>ACF: %{y:.4f}<extra></extra>",
    showlegend = FALSE
  ) |>
    plotly::layout(
      title = book_plot_title(
        "Автокореляція похибок прогнозу AR(1)"
      ),
      xaxis = book_axis_style("Лаг, годин"),
      yaxis = book_axis_style("Автокореляція"),
      shapes = list(
        list(
          type = "line",
          x0 = 0,
          x1 = max(data$lag) + 1,
          y0 = upper,
          y1 = upper,
          line = list(color = palette[["naive"]], width = 1)
        ),
        list(
          type = "line",
          x0 = 0,
          x1 = max(data$lag) + 1,
          y0 = -upper,
          y1 = -upper,
          line = list(color = palette[["naive"]], width = 1)
        )
      )
    ) |>
    render_book_widget(
      size = "compact",
      margin = book_chart_margin("compact"),
      showlegend = FALSE
    )
}

plot_strategy_paths <- function(data) {
  colours <- book_strategy_colours("dark")
  roles <- c(
    "AR(1): BTC або USDT" = "ar1",
    "Купи й тримай" = "buy_hold",
    "USDT без торгівлі" = "cash"
  )
  widget <- plotly::plot_ly()

  for (strategy_name in unique(data$strategy)) {
    subset_data <- data[data$strategy == strategy_name, ]
    widget <- widget |>
      plotly::add_trace(
        data = subset_data,
        x = ~time,
        y = ~wealth,
        type = "scatter",
        mode = "lines",
        name = strategy_name,
        line = list(color = colours[[strategy_name]], width = 2.1),
        meta = book_trace_meta(roles[[strategy_name]]),
        hovertemplate = paste0(
          "%{x|%Y-%m-%d %H:%M} UTC",
          "<br>Капітал: %{y:.2f} USDT",
          "<extra>", strategy_name, "</extra>"
        )
      )
  }

  starting_capital <- data$wealth[[1L]]

  widget |>
    plotly::layout(
      title = book_plot_title(
        "Капітал трьох правил після витрат"
      ),
      xaxis = book_time_axis("Дата і час, UTC", rangeslider = TRUE),
      yaxis = book_axis_style("Капітал, USDT"),
      shapes = list(
        list(
          type = "line",
          x0 = min(data$time),
          x1 = max(data$time),
          y0 = starting_capital,
          y1 = starting_capital,
          line = list(
            color = book_plot_palette("dark")[["neutral"]],
            width = 1,
            dash = "dot"
          )
        )
      )
    ) |>
    render_book_widget(
      hovermode = "x unified",
      size = "tall",
      margin = book_chart_margin("strategy"),
      auto_y_on_x = TRUE
    )
}

plot_strategy_drawdowns <- function(data) {
  colours <- book_strategy_colours("dark")
  roles <- c(
    "AR(1): BTC або USDT" = "ar1",
    "Купи й тримай" = "buy_hold",
    "USDT без торгівлі" = "cash"
  )
  widget <- plotly::plot_ly()

  for (strategy_name in unique(data$strategy)) {
    subset_data <- data[data$strategy == strategy_name, ]
    widget <- widget |>
      plotly::add_trace(
        data = subset_data,
        x = ~time,
        y = ~(100 * drawdown),
        type = "scatter",
        mode = "lines",
        name = strategy_name,
        line = list(color = colours[[strategy_name]], width = 2.0),
        meta = book_trace_meta(roles[[strategy_name]]),
        hovertemplate = paste0(
          "%{x|%Y-%m-%d %H:%M} UTC",
          "<br>Просідання: %{y:.2f}%",
          "<extra>", strategy_name, "</extra>"
        )
      )
  }

  minimum_drawdown <- min(100 * data$drawdown, na.rm = TRUE)

  widget |>
    plotly::layout(
      title = book_plot_title(
        "Просідання капіталу для трьох правил"
      ),
      xaxis = book_time_axis("Дата і час, UTC", rangeslider = TRUE),
      yaxis = modifyList(
        book_axis_style("Просідання, %"),
        list(range = c(1.05 * minimum_drawdown, 0), ticksuffix = "%")
      ),
      shapes = list(
        list(
          type = "line",
          x0 = min(data$time),
          x1 = max(data$time),
          y0 = 0,
          y1 = 0,
          line = list(
            color = book_plot_palette("dark")[["neutral"]],
            width = 1
          )
        )
      )
    ) |>
    render_book_widget(
      hovermode = "x unified",
      size = "tall",
      margin = book_chart_margin("strategy"),
      auto_y_on_x = FALSE
    )
}

plot_ar1_trade_activity <- function(data) {
  palette <- book_plot_palette("dark")

  share_plot <- plotly::plot_ly(
    data = data,
    x = ~month,
    y = ~btc_share_percent,
    type = "scatter",
    mode = "lines+markers",
    line = list(color = palette[["ar1"]], width = 2.4),
    marker = list(color = palette[["ar1"]], size = 8),
    meta = book_trace_meta("ar1"),
    hovertemplate = paste0(
      "%{x|%Y-%m}",
      "<br>Час у BTC: %{y:.1f}%",
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::layout(
      xaxis = modifyList(
        book_time_axis(NULL),
        list(showticklabels = FALSE)
      ),
      yaxis = modifyList(
        book_axis_style("Час у BTC, %"),
        list(range = c(0, 100), ticksuffix = "%")
      )
    )

  switch_plot <- plotly::plot_ly(
    data = data,
    x = ~month,
    y = ~switches,
    type = "bar",
    marker = list(color = palette[["naive"]]),
    meta = book_trace_meta("naive"),
    hovertemplate = paste0(
      "%{x|%Y-%m}",
      "<br>Перемикань BTC/USDT: %{y}",
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::layout(
      xaxis = book_time_axis("Місяць, UTC", rangeslider = TRUE),
      yaxis = book_axis_style("Перемикання")
    )

  plotly::subplot(
    share_plot,
    switch_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.55, 0.45),
    margin = 0.08,
    titleX = TRUE,
    titleY = TRUE
  ) |>
    plotly::layout(
      title = book_plot_title(
        "Час у BTC та перемикання правила AR(1)"
      )
    ) |>
    render_book_widget(
      size = "double",
      margin = book_chart_margin("activity"),
      showlegend = FALSE
    )
}
