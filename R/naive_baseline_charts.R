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

  data.frame(
    target_time = forecasts$target_time,
    actual_close = forecasts$actual_close,
    naive_price_forecast = forecasts$naive_price_forecast,
    price_error = forecasts$naive_price_forecast - forecasts$actual_close,
    absolute_price_error = abs(
      forecasts$naive_price_forecast - forecasts$actual_close
    ),
    actual_return_percent = 100 * forecasts$actual_log_return,
    stringsAsFactors = FALSE
  )
}

plot_model_time_split <- function(data) {
  require_model_columns(
    data,
    c("part", "start", "end"),
    "графіка часового поділу"
  )

  palette <- book_plot_palette()
  colours <- c(
    "Навчання" = palette[["neutral"]],
    "Перевірка" = palette[["ar1"]],
    "Закритий тест" = palette[["negative"]]
  )

  widget <- plotly::plot_ly()

  for (index in seq_len(nrow(data))) {
    part_name <- as.character(data$part[[index]])
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
        line = list(color = colours[[part_name]], width = 22),
        text = c(hover_text, hover_text),
        hovertemplate = "%{text}<extra></extra>",
        showlegend = FALSE
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

  annotations <- lapply(boundary_times, function(boundary_time) {
    list(
      x = boundary_time,
      y = 1.06,
      xref = "x",
      yref = "paper",
      text = format(boundary_time, "%Y-%m-%d %H:%M", tz = "UTC"),
      showarrow = FALSE,
      font = list(size = 11)
    )
  })

  widget |>
    plotly::layout(
      title = book_plot_title("Часовий поділ даних"),
      xaxis = book_time_axis("Час, UTC"),
      yaxis = modifyList(
        book_axis_style(NULL),
        list(
          categoryorder = "array",
          categoryarray = rev(levels(data$part)),
          showgrid = FALSE
        )
      ),
      shapes = shapes,
      annotations = annotations
    ) |>
    render_book_widget(
      height = 720,
      margin = list(l = 110, r = 20, t = 108, b = 64),
      showlegend = FALSE
    )
}

plot_naive_price_forecast <- function(data) {
  palette <- book_plot_palette()
  plotly::plot_ly() |>
    plotly::add_trace(
      data = data,
      x = ~target_time,
      y = ~actual_close,
      type = "scattergl",
      mode = "lines",
      name = "Фактична ціна",
      line = list(color = palette[["actual"]], width = 1.4),
      hovertemplate = "%{x|%Y-%m-%d %H:%M} UTC<br>Факт: %{y:,.2f}<extra></extra>"
    ) |>
    plotly::add_trace(
      data = data,
      x = ~target_time,
      y = ~naive_price_forecast,
      type = "scattergl",
      mode = "lines",
      name = "Наївний прогноз",
      line = list(color = palette[["naive"]], width = 1.1),
      hovertemplate = "%{x|%Y-%m-%d %H:%M} UTC<br>Прогноз: %{y:,.2f}<extra></extra>"
    ) |>
    plotly::layout(
      title = book_plot_title("Наївний прогноз ціни на наступну годину"),
      xaxis = book_time_axis("Дата і час, UTC"),
      yaxis = book_axis_style("USDT за 1 BTC")
    ) |>
    render_book_widget(
      hovermode = "x unified",
      height = 720,
      margin = list(l = 84, r = 20, t = 100, b = 70),
      auto_y_on_x = TRUE
    )
}

plot_naive_absolute_error <- function(data) {
  palette <- book_plot_palette()
  plotly::plot_ly(
    data = data,
    x = ~target_time,
    y = ~absolute_price_error,
    type = "scattergl",
    mode = "lines",
    line = list(color = palette[["negative"]], width = 1),
    hovertemplate = paste0(
      "%{x|%Y-%m-%d %H:%M} UTC",
      "<br>Абсолютна похибка: %{y:,.2f} USDT",
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::layout(
      title = book_plot_title("Розмір промаху наївного прогнозу в кожну годину"),
      xaxis = book_time_axis("Дата і час, UTC"),
      yaxis = book_axis_style("Абсолютна похибка, USDT")
    ) |>
    render_book_widget(
      hovermode = "x unified",
      height = 680,
      margin = list(l = 92, r = 20, t = 92, b = 70),
      showlegend = FALSE,
      auto_y_on_x = TRUE
    )
}

plot_naive_return_distribution <- function(data) {
  palette <- book_plot_palette()

  plotly::plot_ly(
    data = data,
    x = ~actual_return_percent,
    type = "histogram",
    nbinsx = 80,
    marker = list(
      color = palette[["accent"]],
      line = list(color = palette[["neutral"]], width = 0.2)
    ),
    hovertemplate = "Дохідність: %{x:.3f}%<br>Годин: %{y}<extra></extra>",
    showlegend = FALSE
  ) |>
    plotly::layout(
      title = book_plot_title("Розподіл фактичних годинних дохідностей"),
      bargap = 0.03,
      xaxis = book_axis_style("Логарифмічна дохідність за годину, %"),
      yaxis = book_axis_style("Кількість годин")
    ) |>
    render_book_widget(
      height = 620,
      margin = list(l = 80, r = 20, t = 92, b = 70),
      showlegend = FALSE
    )
}
