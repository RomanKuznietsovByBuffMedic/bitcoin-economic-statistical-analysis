# Interactive charts for the hourly naive forecast ----------------------

prepare_naive_chart_data <- function(forecasts) {
  require_model_columns(
    forecasts,
    c(
      "target_time",
      "actual_close",
      "naive_price_forecast",
      "actual_log_return"
    ),
    "графіків наївного прогнозу"
  )

  hourly <- data.frame(
    target_time = forecasts$target_time,
    actual_close = forecasts$actual_close,
    naive_price_forecast = forecasts$naive_price_forecast,
    price_error = forecasts$naive_price_forecast - forecasts$actual_close,
    absolute_price_error = abs(
      forecasts$naive_price_forecast - forecasts$actual_close
    ),
    actual_return_percent = 100 * forecasts$actual_log_return,
    absolute_return_error_bps = 10000 * abs(forecasts$actual_log_return),
    stringsAsFactors = FALSE
  )

  day <- as.Date(hourly$target_time, tz = "UTC")
  day_indices <- split(seq_len(nrow(hourly)), day)
  daily_error <- do.call(
    rbind,
    lapply(names(day_indices), function(day_name) {
      values <- hourly$absolute_return_error_bps[day_indices[[day_name]]]
      data.frame(
        date = as.POSIXct(paste0(day_name, " 00:00:00"), tz = "UTC"),
        median_error_bps = stats::median(values, na.rm = TRUE),
        p95_error_bps = as.numeric(
          stats::quantile(values, probs = 0.95, na.rm = TRUE)
        ),
        stringsAsFactors = FALSE
      )
    })
  )
  rownames(daily_error) <- NULL

  list(
    hourly = hourly,
    daily_error = daily_error
  )
}

plot_model_time_split <- function(data) {
  require_model_columns(
    data,
    c("part", "role", "start", "end"),
    "графіка часового поділу"
  )

  part_labels <- as.character(data$part)
  part_roles <- as.character(data$role)
  supported_roles <- c("training", "validation", "test")
  if (
    nrow(data) != length(supported_roles) ||
      anyNA(part_labels) ||
      any(!nzchar(part_labels)) ||
      anyDuplicated(part_labels) ||
      anyNA(part_roles) ||
      anyDuplicated(part_roles) ||
      !setequal(part_roles, supported_roles)
  ) {
    stop(
      paste(
        "Графік потребує різних підписів і рівно трьох ролей:",
        paste(supported_roles, collapse = ", "),
        "."
      )
    )
  }

  palette <- book_plot_palette("dark")
  colours <- c(
    training = palette[["neutral"]],
    validation = palette[["ar1"]],
    test = palette[["negative"]]
  )
  trace_roles <- c(
    training = "neutral",
    validation = "ar1",
    test = "negative"
  )

  widget <- plotly::plot_ly()

  for (index in seq_len(nrow(data))) {
    part_name <- part_labels[[index]]
    part_role <- part_roles[[index]]
    hover_text <- paste0(
      part_name,
      "<br>",
      format(data$start[[index]], "%Y-%m-%d %H:%M", tz = "UTC"),
      " UTC",
      "<br>до ",
      format(data$end[[index]], "%Y-%m-%d %H:%M", tz = "UTC"),
      " UTC"
    )

    widget <- widget |>
      plotly::add_trace(
        x = c(data$start[[index]], data$end[[index]]),
        y = c(part_name, part_name),
        type = "scatter",
        mode = "lines",
        line = list(color = colours[[part_role]], width = 24),
        meta = book_trace_meta(trace_roles[[part_role]]),
        text = c(hover_text, hover_text),
        hovertemplate = "%{text}<extra></extra>",
        showlegend = FALSE,
        inherit = FALSE
      )
  }

  boundary_times <- sort(unique(c(data$start, data$end)))
  boundary_times <- boundary_times[
    boundary_times > min(boundary_times) &
      boundary_times < max(boundary_times)
  ]

  shapes <- lapply(boundary_times, function(boundary_time) {
    list(
      type = "line",
      x0 = boundary_time,
      x1 = boundary_time,
      y0 = -0.5,
      y1 = 2.5,
      line = list(color = palette[["neutral"]], width = 1)
    )
  })

  widget |>
    plotly::layout(
      title = book_plot_title(
        "Хронологічний поділ даних"
      ),
      xaxis = book_time_axis("Час, UTC", rangeslider = TRUE),
      yaxis = modifyList(
        book_axis_style(NULL),
        list(
          categoryorder = "array",
          categoryarray = part_labels,
          showgrid = FALSE
        )
      ),
      shapes = shapes
    ) |>
    render_book_widget(
      size = "compact",
      margin = book_chart_margin("time_split"),
      showlegend = FALSE
    )
}

plot_naive_price_forecast <- function(data) {
  hourly <- data$hourly
  palette <- book_plot_palette("dark")
  initial_range <- book_time_range(hourly$target_time, 7L * 24L)

  plotly::plot_ly() |>
    plotly::add_trace(
      data = hourly,
      x = ~target_time,
      y = ~actual_close,
      type = "scatter",
      mode = "lines",
      name = "Фактична ціна",
      line = list(color = palette[["actual"]], width = 2.7),
      meta = book_trace_meta("actual"),
      hovertemplate = paste0(
        "%{x|%Y-%m-%d %H:%M} UTC",
        "<br>Факт: %{y:,.2f} USDT",
        "<extra></extra>"
      )
    ) |>
    plotly::add_trace(
      data = hourly,
      x = ~target_time,
      y = ~naive_price_forecast,
      type = "scatter",
      mode = "lines",
      name = "Наївний прогноз",
      line = list(color = palette[["naive"]], width = 2.0),
      meta = book_trace_meta("naive"),
      hovertemplate = paste0(
        "%{x|%Y-%m-%d %H:%M} UTC",
        "<br>Прогноз: %{y:,.2f} USDT",
        "<extra></extra>"
      )
    ) |>
    plotly::layout(
      xaxis = book_time_axis(
        "Дата і час, UTC",
        range = initial_range,
        rangeslider = TRUE,
        range_buttons = TRUE
      ),
      yaxis = book_axis_style("USDT за 1 BTC")
    ) |>
    render_book_widget(
      hovermode = "x unified",
      size = "tall",
      margin = book_chart_margin("range_controls"),
      showlegend = FALSE,
      auto_y_on_x = TRUE
    )
}

plot_naive_absolute_error <- function(data) {
  daily <- data$daily_error
  palette <- book_plot_palette("dark")

  plotly::plot_ly() |>
    plotly::add_trace(
      data = daily,
      x = ~date,
      y = ~median_error_bps,
      type = "scatter",
      mode = "lines",
      name = "Типова помилка за день",
      line = list(color = palette[["actual"]], width = 2.2),
      meta = book_trace_meta("actual"),
      hovertemplate = paste0(
        "%{x|%Y-%m-%d}",
        "<br>Медіана: %{y:.2f} б.п.",
        "<extra></extra>"
      )
    ) |>
    plotly::add_trace(
      data = daily,
      x = ~date,
      y = ~p95_error_bps,
      type = "scatter",
      mode = "lines",
      name = "Велика помилка за день",
      line = list(color = palette[["negative"]], width = 2.2),
      meta = book_trace_meta("negative"),
      hovertemplate = paste0(
        "%{x|%Y-%m-%d}",
        "<br>95-й процентиль: %{y:.2f} б.п.",
        "<extra></extra>"
      )
    ) |>
    plotly::layout(
      title = book_plot_title(
        "Типова і велика похибка наївного прогнозу"
      ),
      xaxis = book_time_axis("Дата, UTC", rangeslider = TRUE),
      yaxis = book_axis_style("Абсолютна помилка прогнозу, б.п.")
    ) |>
    render_book_widget(
      hovermode = "x unified",
      size = "standard",
      margin = book_chart_margin("wide_axis"),
      auto_y_on_x = TRUE
    )
}

plot_naive_return_distribution <- function(data) {
  hourly <- data$hourly
  palette <- book_plot_palette("dark")
  quantiles <- stats::quantile(
    hourly$actual_return_percent,
    probs = c(0.025, 0.5, 0.975),
    na.rm = TRUE
  )

  plotly::plot_ly(
    data = hourly,
    x = ~actual_return_percent,
    type = "histogram",
    nbinsx = 80,
    marker = list(
      color = palette[["accent"]],
      line = list(color = palette[["neutral"]], width = 0.2)
    ),
    meta = book_trace_meta("accent"),
    hovertemplate = "Дохідність: %{x:.3f}%<br>Годин: %{y}<extra></extra>",
    showlegend = FALSE
  ) |>
    plotly::layout(
      title = book_plot_title(
        "Розподіл годинних логарифмічних дохідностей"
      ),
      bargap = 0.03,
      xaxis = book_axis_style("Логарифмічна дохідність за годину, %"),
      yaxis = book_axis_style("Кількість годин"),
      shapes = list(
        list(
          type = "line",
          x0 = quantiles[[1L]],
          x1 = quantiles[[1L]],
          y0 = 0,
          y1 = 1,
          yref = "paper",
          line = list(color = palette[["neutral"]], width = 1, dash = "dot")
        ),
        list(
          type = "line",
          x0 = quantiles[[2L]],
          x1 = quantiles[[2L]],
          y0 = 0,
          y1 = 1,
          yref = "paper",
          line = list(color = palette[["actual"]], width = 1.2)
        ),
        list(
          type = "line",
          x0 = quantiles[[3L]],
          x1 = quantiles[[3L]],
          y0 = 0,
          y1 = 1,
          yref = "paper",
          line = list(color = palette[["neutral"]], width = 1, dash = "dot")
        )
      )
    ) |>
    render_book_widget(
      size = "standard",
      margin = book_chart_margin("standard"),
      showlegend = FALSE
    )
}
