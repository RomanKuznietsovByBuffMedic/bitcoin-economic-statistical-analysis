# Chart for the naive point-forecast benchmark ---------------------------

plot_naive_point_benchmark <- function(data, quote_currency) {
  require_naive_benchmark_columns(
    data,
    c(
      "target_time",
      "actual_close",
      "naive_close_forecast",
      "actual_log_return",
      "naive_log_return_forecast"
    ),
    "графіка наївного прогнозу"
  )
  quote_currency <- as.character(quote_currency)
  if (
    length(quote_currency) != 1L ||
      is.na(quote_currency) ||
      !nzchar(trimws(quote_currency))
  ) {
    stop("Потрібна валюта котирування для графіка прогнозу.")
  }

  palette <- book_plot_palette("dark")
  chart_data <- data
  chart_data$forecast_error_bps <- 10000 * (
    chart_data$actual_log_return -
      chart_data$naive_log_return_forecast
  )

  price_plot <- plotly::plot_ly() |>
    plotly::add_trace(
      data = chart_data,
      x = ~target_time,
      y = ~actual_close,
      type = "scatter",
      mode = "lines",
      name = "Фактична ціна",
      line = list(
        color = palette[["actual"]],
        width = 3
      ),
      meta = book_trace_meta("actual"),
      hovertemplate = paste0(
        "%{x|%Y-%m-%d %H:%M} UTC",
        "<br>Фактична ціна: %{y:,.2f} ",
        quote_currency,
        "<extra></extra>"
      ),
      showlegend = TRUE,
      inherit = FALSE
    ) |>
    plotly::add_trace(
      data = chart_data,
      x = ~target_time,
      y = ~naive_close_forecast,
      type = "scatter",
      mode = "lines",
      name = "Наївний прогноз",
      line = list(
        color = palette[["naive"]],
        width = 2,
        dash = "dot"
      ),
      meta = book_trace_meta("naive"),
      hovertemplate = paste0(
        "%{x|%Y-%m-%d %H:%M} UTC",
        "<br>Прогноз: %{y:,.2f} ",
        quote_currency,
        "<extra></extra>"
      ),
      showlegend = TRUE,
      inherit = FALSE
    ) |>
    plotly::layout(
      xaxis = modifyList(
        book_time_axis(NULL),
        list(showticklabels = FALSE)
      ),
      yaxis = book_axis_style(
        paste0("Ціна, ", quote_currency, " за BTC")
      )
    )

  error_plot <- plotly::plot_ly() |>
    plotly::add_trace(
      data = chart_data,
      x = ~target_time,
      y = ~forecast_error_bps,
      type = "scatter",
      mode = "lines",
      name = "Факт мінус прогноз",
      line = list(
        color = palette[["naive"]],
        width = 1.8
      ),
      meta = book_trace_meta("naive"),
      hovertemplate = paste0(
        "%{x|%Y-%m-%d %H:%M} UTC",
        "<br>Факт мінус прогноз: %{y:.2f} б.п.",
        "<extra></extra>"
      ),
      showlegend = FALSE,
      inherit = FALSE
    ) |>
    plotly::layout(
      xaxis = book_time_axis(
        title = "Дата і час, UTC"
      ),
      yaxis = modifyList(
        book_axis_style("Похибка, б.п."),
        list(
          zeroline = TRUE,
          zerolinecolor = palette[["neutral"]],
          zerolinewidth = 1.3
        )
      )
    )

  plotly::subplot(
    price_plot,
    error_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.62, 0.38),
    margin = 0.08,
    titleX = TRUE,
    titleY = TRUE
  ) |>
    render_book_widget(
      hovermode = "x unified",
      size = "double",
      margin = book_chart_margin("diagnostic_two_panel"),
      showlegend = TRUE
    )
}
