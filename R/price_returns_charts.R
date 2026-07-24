# Interactive price-return charts ----------------------------------------

complete_hourly_price_return_grid <- function(data) {
  if (nrow(data) == 0L) {
    stop("Неможливо побудувати графік для порожнього набору даних.")
  }

  required_columns <- c(
    "open_time",
    "price_quote_per_btc",
    "simple_return_1h",
    "log_return_1h"
  )
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "Для графіка бракує полів: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  observed_values <- data |>
    dplyr::select(dplyr::all_of(required_columns)) |>
    dplyr::arrange(open_time) |>
    dplyr::distinct(open_time, .keep_all = TRUE)

  hourly_grid <- tibble::tibble(
    open_time = seq.POSIXt(
      from = min(observed_values$open_time),
      to = max(observed_values$open_time),
      by = "hour"
    )
  )

  hourly_grid |>
    dplyr::left_join(observed_values, by = "open_time") |>
    dplyr::mutate(
      simple_return_percent = 100 * simple_return_1h,
      log_return_percent = 100 * log_return_1h
    )
}

make_price_widget <- function(
  data,
  quote_currency = "USDT",
  market_label = "BTC/USDT"
) {
  chart_data <- complete_hourly_price_return_grid(data)

  plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~price_quote_per_btc,
    type = "scattergl",
    mode = "lines",
    line = list(color = "#F2A900", width = 1.8),
    hovertemplate = paste0(
      "%{x|%Y-%m-%d %H:%M} UTC",
      "<br>1 BTC: %{y:,.2f} ",
      quote_currency,
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::layout(
      title = book_plot_title(paste("Ціна 1 BTC на ринку", market_label)),
      xaxis = book_time_axis("Дата і час, UTC"),
      yaxis = book_axis_style(paste(quote_currency, "за 1 BTC"))
    ) |>
    render_book_widget(
      hovermode = "x unified",
      height = 760,
      margin = list(l = 92, r = 24, t = 100, b = 82),
      showlegend = FALSE,
      auto_y_on_x = TRUE
    )
}

make_returns_widget <- function(
  data,
  market_label = "BTC/USDT"
) {
  chart_data <- complete_hourly_price_return_grid(data)
  palette <- book_plot_palette()

  plotly::plot_ly() |>
    plotly::add_trace(
      data = chart_data,
      x = ~open_time,
      y = ~simple_return_percent,
      type = "scattergl",
      mode = "lines",
      name = "Звичайна дохідність",
      line = list(color = palette[["ar1"]], width = 1.2),
      hovertemplate = paste0(
        "%{x|%Y-%m-%d %H:%M} UTC",
        "<br>Звичайна дохідність: %{y:.3f}%",
        "<extra></extra>"
      )
    ) |>
    plotly::add_trace(
      data = chart_data,
      x = ~open_time,
      y = ~log_return_percent,
      type = "scattergl",
      mode = "lines",
      name = "Логарифмічна дохідність",
      line = list(color = palette[["negative"]], width = 1.2),
      hovertemplate = paste0(
        "%{x|%Y-%m-%d %H:%M} UTC",
        "<br>Логарифмічна дохідність: %{y:.3f}%",
        "<extra></extra>"
      )
    ) |>
    plotly::layout(
      title = book_plot_title(paste("Годинні дохідності", market_label)),
      xaxis = book_time_axis("Дата і час, UTC"),
      yaxis = book_axis_style("Дохідність за годину, %")
    ) |>
    render_book_widget(
      hovermode = "x unified",
      height = 760,
      margin = list(l = 92, r = 24, t = 112, b = 82),
      auto_y_on_x = TRUE
    )
}
