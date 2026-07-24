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
    window_sum <- cumulative[[index + 1L]] - cumulative[[index - window + 1L]]
    result[[index]] <- sqrt(window_sum / window)
  }
  result
}

compress_position_segments <- function(target_time, position) {
  target_time <- as.POSIXct(target_time, tz = "UTC")
  position <- as.integer(position)
  if (length(target_time) != length(position) || length(target_time) == 0L) {
    stop("Для сегментів позиції потрібні непорожні узгоджені дані.")
  }

  time_step <- stats::median(diff(as.numeric(target_time)))
  if (!is.finite(time_step) || is.na(time_step) || time_step <= 0) {
    time_step <- 3600
  }

  runs <- rle(position)
  run_end <- cumsum(runs$lengths)
  run_start <- c(1L, utils::head(run_end, -1L) + 1L)
  end_time <- target_time[run_end] + time_step

  data.frame(
    start = target_time[run_start],
    end = end_time,
    state = ifelse(runs$values == 1L, "BTC", "USDT"),
    duration_hours = runs$lengths,
    stringsAsFactors = FALSE
  )
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

  ar1_return_error <- forecasts$ar1_log_return_forecast - forecasts$actual_log_return
  naive_return_error <- forecasts$naive_log_return_forecast - forecasts$actual_log_return

  refit_data <- refits
  refit_data$phi_lower <- refit_data$phi - 1.96 * refit_data$phi_standard_error
  refit_data$phi_upper <- refit_data$phi + 1.96 * refit_data$phi_standard_error
  refit_data$phi_ci_excludes_zero <- refit_data$phi_lower > 0 | refit_data$phi_upper < 0

  metric_rows <- list()
  metric_names <- c(
    return_mae = "MAE дохідності",
    return_rmse = "RMSE дохідності",
    price_mae = "MAE ціни",
    price_rmse = "RMSE ціни"
  )
  for (metric in names(metric_names)) {
    ar1_value <- forecast_metrics[forecast_metrics$method == "AR(1)", metric]
    naive_value <- forecast_metrics[forecast_metrics$method == "Наївний прогноз", metric]
    metric_rows[[length(metric_rows) + 1L]] <- data.frame(
      metric = metric_names[[metric]],
      improvement_percent = 100 * (1 - ar1_value / naive_value),
      family = if (grepl("дохідності", metric_names[[metric]])) "Дохідність" else "Ціна",
      short_metric = if (grepl("MAE", metric_names[[metric]])) "MAE" else "RMSE",
      stringsAsFactors = FALSE
    )
  }
  metric_improvement <- do.call(rbind, metric_rows)

  rolling_data <- data.frame(
    target_time = forecasts$target_time,
    ar1_rmse = rolling_root_mean_square(ar1_return_error, rolling_window) * 100,
    naive_rmse = rolling_root_mean_square(naive_return_error, rolling_window) * 100,
    stringsAsFactors = FALSE
  )
  rolling_data$difference <- rolling_data$ar1_rmse - rolling_data$naive_rmse

  loss_advantage <- naive_return_error^2 - ar1_return_error^2
  loss_data <- data.frame(
    target_time = forecasts$target_time,
    cumulative_mean_advantage_pp_squared = cumsum(loss_advantage) / seq_along(loss_advantage) * 100^2,
    stringsAsFactors = FALSE
  )

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
  path_data$running_maximum <- ave(path_data$wealth, path_data$strategy, FUN = cummax)
  path_data$drawdown <- path_data$wealth / path_data$running_maximum - 1

  position_values <- as.integer(forecasts$ar1_log_return_forecast > signal_threshold)

  list(
    refits = refit_data,
    metric_improvement = metric_improvement,
    forecast_density = data.frame(
      forecast = 100 * forecasts$ar1_log_return_forecast,
      actual = 100 * forecasts$actual_log_return,
      stringsAsFactors = FALSE
    ),
    rolling_rmse = rolling_data,
    rolling_window = rolling_window,
    loss_advantage = loss_data,
    forecast_error_acf = acf_data,
    strategy_paths = path_data,
    position = data.frame(target_time = forecasts$target_time, position = position_values, stringsAsFactors = FALSE),
    position_segments = compress_position_segments(forecasts$target_time, position_values)
  )
}

plot_ar1_phi_stability <- function(data) {
  palette <- book_plot_palette()
  colours <- ifelse(data$phi_ci_excludes_zero, palette[["positive"]], palette[["ar1"]])

  plotly::plot_ly() |>
    plotly::add_segments(
      data = data,
      x = ~refit_time,
      xend = ~refit_time,
      y = ~phi_lower,
      yend = ~phi_upper,
      line = list(color = palette[["neutral"]], width = 3),
      hoverinfo = "skip",
      showlegend = FALSE
    ) |>
    plotly::add_markers(
      data = data,
      x = ~refit_time,
      y = ~phi,
      marker = list(color = colours, size = 10),
      name = "Оцінка φ₁",
      text = ~paste0(
        "φ₁ = ", formatC(phi, digits = 4, format = "f"),
        "<br>95% ДІ: [", formatC(phi_lower, digits = 4, format = "f"), ", ", formatC(phi_upper, digits = 4, format = "f"), "]",
        "<br>p-value = ", formatC(phi_p_value, digits = 4, format = "f")
      ),
      hovertemplate = "%{x|%Y-%m-%d}<br>%{text}<extra></extra>"
    ) |>
    plotly::layout(
      title = book_plot_title("Стабільність коефіцієнта AR(1) у часі"),
      xaxis = book_time_axis("Дата переоцінювання"),
      yaxis = book_axis_style("Коефіцієнт φ₁"),
      shapes = list(list(type = "line", x0 = min(data$refit_time), x1 = max(data$refit_time), y0 = 0, y1 = 0, line = list(color = palette[["negative"]], width = 1.2)))
    ) |>
    render_book_widget(height = 700, showlegend = FALSE)
}

plot_ar1_metric_improvement <- function(data) {
  palette <- book_plot_palette()
  colours <- c(
    "Дохідність" = palette[["positive"]],
    "Ціна" = palette[["accent"]]
  )

  mae_data <- data[data$short_metric == "MAE", ]
  rmse_data <- data[data$short_metric == "RMSE", ]

  make_metric_panel <- function(panel_data, panel_title, show_legend) {
    widget <- plotly::plot_ly()

    for (family_name in c("Дохідність", "Ціна")) {
      subset_data <- panel_data[panel_data$family == family_name, ]
      widget <- widget |>
        plotly::add_trace(
          data = subset_data,
          x = ~improvement_percent,
          y = ~family,
          type = "bar",
          orientation = "h",
          name = family_name,
          marker = list(color = colours[[family_name]]),
          text = ~paste0(
            formatC(improvement_percent, digits = 3, format = "f"),
            "%"
          ),
          textposition = "outside",
          cliponaxis = FALSE,
          hovertemplate = paste0(
            family_name,
            "<br>Покращення: %{x:.4f}%",
            "<extra></extra>"
          ),
          showlegend = show_legend
        )
    }

    widget |>
      plotly::layout(
        title = list(
          text = panel_title,
          x = 0.5,
          xanchor = "center",
          font = list(color = palette[["text"]], size = 18)
        ),
        xaxis = book_axis_style("Покращення, %"),
        yaxis = modifyList(
          book_axis_style(NULL),
          list(
            categoryorder = "array",
            categoryarray = c("Ціна", "Дохідність")
          )
        ),
        bargap = 0.35,
        shapes = list(
          list(
            type = "line",
            x0 = 0,
            x1 = 0,
            y0 = -0.5,
            y1 = 1.5,
            line = list(color = palette[["neutral"]], width = 1)
          )
        )
      )
  }

  left_plot <- make_metric_panel(mae_data, "MAE", TRUE)
  right_plot <- make_metric_panel(rmse_data, "RMSE", FALSE)

  plotly::subplot(
    left_plot,
    right_plot,
    nrows = 1,
    shareX = FALSE,
    titleX = TRUE,
    margin = 0.10
  ) |>
    plotly::layout(
      title = book_plot_title(
        "Покращення AR(1) відносно наївного прогнозу"
      )
    ) |>
    render_book_widget(
      height = 760,
      margin = list(l = 90, r = 54, t = 126, b = 86),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        x = 0.5,
        y = 1.06,
        xanchor = "center",
        yanchor = "bottom",
        bgcolor = "rgba(0,0,0,0)"
      )
    )
}

plot_ar1_forecast_density <- function(data) {
  palette <- book_plot_palette()
  x_limit <- stats::quantile(
    abs(data$forecast),
    probs = 0.995,
    na.rm = TRUE
  )
  y_limit <- stats::quantile(
    abs(data$actual),
    probs = 0.995,
    na.rm = TRUE
  )
  x_limit <- min(max(as.numeric(x_limit), 0.01), 1)
  y_limit <- min(max(as.numeric(y_limit), 0.25), 5)
  diagonal_limit <- min(x_limit, y_limit)

  plotly::plot_ly(
    data = data,
    x = ~forecast,
    y = ~actual,
    type = "histogram2d",
    xbins = list(
      start = -x_limit,
      end = x_limit,
      size = (2 * x_limit) / 60
    ),
    ybins = list(
      start = -y_limit,
      end = y_limit,
      size = (2 * y_limit) / 60
    ),
    colorscale = list(c(0, "#13213A"), c(0.25, "#1D4ED8"), c(0.6, "#38BDF8"), c(1, "#FBBF24")),
    colorbar = list(title = "Кількість<br>годин", tickfont = list(color = palette[["text"]]), titlefont = list(color = palette[["text"]])),
    hovertemplate = paste0(
      "Прогноз: %{x:.3f}%",
      "<br>Факт: %{y:.3f}%",
      "<br>Годин у клітинці: %{z}",
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::layout(
      title = book_plot_title("Щільність: прогноз AR(1) проти фактичної дохідності"),
      xaxis = modifyList(
        book_axis_style("Прогнозована лог-дохідність, %"),
        list(range = c(-x_limit, x_limit))
      ),
      yaxis = modifyList(
        book_axis_style("Фактична лог-дохідність, %"),
        list(range = c(-y_limit, y_limit))
      ),
      shapes = list(
        list(
          type = "line",
          x0 = -diagonal_limit,
          x1 = diagonal_limit,
          y0 = -diagonal_limit,
          y1 = diagonal_limit,
          line = list(color = palette[["positive"]], width = 1.4)
        ),
        list(
          type = "line",
          x0 = 0,
          x1 = 0,
          y0 = -y_limit,
          y1 = y_limit,
          line = list(color = palette[["neutral"]], width = 0.8)
        ),
        list(
          type = "line",
          x0 = -x_limit,
          x1 = x_limit,
          y0 = 0,
          y1 = 0,
          line = list(color = palette[["neutral"]], width = 0.8)
        )
      )
    ) |>
    render_book_widget(hovermode = "closest", height = 720, margin = list(l = 92, r = 96, t = 100, b = 82), showlegend = FALSE)
}

plot_ar1_rolling_rmse_difference <- function(data, rolling_window) {
  palette <- book_plot_palette()

  plotly::plot_ly(
    data = data,
    x = ~target_time,
    y = ~difference,
    type = "scattergl",
    mode = "lines",
    line = list(color = palette[["accent"]], width = 1.4),
    fill = "tozeroy",
    fillcolor = "rgba(56,189,248,0.14)",
    hovertemplate = paste0("%{x|%Y-%m-%d %H:%M} UTC", "<br>Різниця RMSE: %{y:.4f} в.п.", "<extra></extra>"),
    showlegend = FALSE
  ) |>
    plotly::layout(
      title = book_plot_title(paste0("Ковзна різниця RMSE за ", rolling_window, " годин")),
      xaxis = book_time_axis("Дата і час, UTC"),
      yaxis = book_axis_style("RMSE AR(1) мінус RMSE benchmark, в.п."),
      shapes = list(list(type = "line", x0 = min(data$target_time), x1 = max(data$target_time), y0 = 0, y1 = 0, line = list(color = palette[["naive"]], width = 1)))
    ) |>
    render_book_widget(hovermode = "x unified", height = 700, showlegend = FALSE, auto_y_on_x = TRUE)
}

plot_ar1_loss_advantage <- function(data) {
  palette <- book_plot_palette()

  plotly::plot_ly(
    data = data,
    x = ~target_time,
    y = ~cumulative_mean_advantage_pp_squared,
    type = "scattergl",
    mode = "lines",
    line = list(color = palette[["positive"]], width = 1.5),
    hovertemplate = paste0("%{x|%Y-%m-%d %H:%M} UTC", "<br>Середня перевага: %{y:.6f} в.п.²", "<extra></extra>"),
    showlegend = FALSE
  ) |>
    plotly::layout(
      title = book_plot_title("Чи накопичується перевага AR(1) над benchmark"),
      xaxis = book_time_axis("Дата і час, UTC"),
      yaxis = book_axis_style("Різниця середніх квадратів похибок, в.п.²"),
      shapes = list(list(type = "line", x0 = min(data$target_time), x1 = max(data$target_time), y0 = 0, y1 = 0, line = list(color = palette[["neutral"]], width = 1)))
    ) |>
    render_book_widget(hovermode = "x unified", height = 700, showlegend = FALSE, auto_y_on_x = TRUE)
}

plot_ar1_forecast_error_acf <- function(data) {
  palette <- book_plot_palette()
  upper <- unique(data$upper_bound)[[1L]]

  plotly::plot_ly(
    data = data,
    x = ~lag,
    y = ~autocorrelation,
    type = "bar",
    marker = list(color = ifelse(abs(data$autocorrelation) > upper, palette[["negative"]], palette[["ar1"]])),
    hovertemplate = "Лаг: %{x} год.<br>ACF: %{y:.4f}<extra></extra>",
    showlegend = FALSE
  ) |>
    plotly::layout(
      title = book_plot_title("ACF позавибіркових похибок AR(1)"),
      xaxis = book_axis_style("Лаг, годин"),
      yaxis = book_axis_style("Автокореляція"),
      shapes = list(
        list(type = "line", x0 = 0, x1 = max(data$lag) + 1, y0 = upper, y1 = upper, line = list(color = palette[["naive"]], width = 1)),
        list(type = "line", x0 = 0, x1 = max(data$lag) + 1, y0 = -upper, y1 = -upper, line = list(color = palette[["naive"]], width = 1))
      )
    ) |>
    render_book_widget(height = 620, showlegend = FALSE)
}

plot_strategy_paths <- function(data) {
  colours <- book_strategy_colours()
  widget <- plotly::plot_ly()
  for (strategy_name in unique(data$strategy)) {
    subset_data <- data[data$strategy == strategy_name, ]
    widget <- widget |>
      plotly::add_trace(
        data = subset_data,
        x = ~time,
        y = ~wealth,
        type = "scattergl",
        mode = "lines",
        name = strategy_name,
        line = list(color = colours[[strategy_name]], width = 1.4),
        hovertemplate = paste0("%{x|%Y-%m-%d %H:%M} UTC", "<br>Капітал: %{y:.2f} USDT", "<extra>", strategy_name, "</extra>")
      )
  }


  widget |>
    plotly::layout(
      title = book_plot_title("Крива капіталу після витрат"),
      xaxis = book_time_axis("Дата і час, UTC"),
      yaxis = book_axis_style("Капітал, USDT")
    ) |>
    render_book_widget(hovermode = "x unified", height = 720, auto_y_on_x = TRUE)
}

plot_strategy_drawdowns <- function(data) {
  colours <- book_strategy_colours()
  widget <- plotly::plot_ly()
  for (strategy_name in unique(data$strategy)) {
    subset_data <- data[data$strategy == strategy_name, ]
    widget <- widget |>
      plotly::add_trace(
        data = subset_data,
        x = ~time,
        y = ~(100 * drawdown),
        type = "scattergl",
        mode = "lines",
        name = strategy_name,
        line = list(color = colours[[strategy_name]], width = 1.3),
        hovertemplate = paste0("%{x|%Y-%m-%d %H:%M} UTC", "<br>Просідання: %{y:.2f}%", "<extra>", strategy_name, "</extra>")
      )
  }


  widget |>
    plotly::layout(
      title = book_plot_title("Просідання від попереднього максимуму"),
      xaxis = book_time_axis("Дата і час, UTC"),
      yaxis = book_axis_style("Просідання, %")
    ) |>
    render_book_widget(hovermode = "x unified", height = 700, auto_y_on_x = TRUE)
}

plot_ar1_position <- function(data) {
  palette <- book_plot_palette()
  colours <- c(BTC = palette[["ar1"]], USDT = palette[["cash"]])

  widget <- plotly::plot_ly()
  for (state_name in c("BTC", "USDT")) {
    subset_data <- data[data$state == state_name, ]
    if (nrow(subset_data) == 0L) next

    subset_data$hover_text <- paste0(
      "Період: ", format(subset_data$start, "%Y-%m-%d %H:%M", tz = "UTC"), " UTC",
      "<br>до ", format(subset_data$end, "%Y-%m-%d %H:%M", tz = "UTC"), " UTC",
      "<br>Стан: ", subset_data$state,
      "<br>Тривалість: ", subset_data$duration_hours, " год."
    )

    widget <- widget |>
      plotly::add_segments(
        data = subset_data,
        x = ~start,
        xend = ~end,
        y = ~state,
        yend = ~state,
        name = state_name,
        line = list(color = colours[[state_name]], width = 22),
        text = ~hover_text,
        hovertemplate = "%{text}<extra></extra>"
      )
  }


  widget |>
    plotly::layout(
      title = book_plot_title("Коли правило тримало BTC, а коли USDT"),
      xaxis = book_time_axis("Дата і час, UTC"),
      yaxis = modifyList(book_axis_style(NULL), list(categoryorder = "array", categoryarray = c("USDT", "BTC"), showgrid = FALSE))
    ) |>
    render_book_widget(height = 520)
}
