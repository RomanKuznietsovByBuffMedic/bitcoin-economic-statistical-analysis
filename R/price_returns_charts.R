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
      log_return_percent = 100 * log_return_1h,
      return_difference_basis_points =
        10000 * (simple_return_1h - log_return_1h)
    )
}

make_price_widget <- function(
  data,
  quote_currency = "USDT",
  market_label = "BTC/USDT"
) {
  chart_data <- complete_hourly_price_return_grid(data)
  palette <- book_plot_palette("dark")

  plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~price_quote_per_btc,
    type = "scattergl",
    mode = "lines",
    line = list(color = palette[["price"]], width = 2.1),
    meta = book_trace_meta("price"),
    connectgaps = FALSE,
    hovertemplate = paste0(
      "%{x|%Y-%m-%d %H:%M} UTC",
      "<br>1 BTC: %{y:,.2f} ",
      quote_currency,
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::layout(
      title = book_plot_title(
        paste("Ціна закриття", market_label, "за годинами")
      ),
      xaxis = book_time_axis(
        "Дата і час, UTC",
        rangeslider = TRUE
      ),
      yaxis = book_axis_style(paste(quote_currency, "за 1 BTC"))
    ) |>
    render_book_widget(
      hovermode = "x unified",
      size = "tall",
      margin = book_chart_margin("price"),
      showlegend = FALSE,
      auto_y_on_x = TRUE
    )
}

make_returns_widget <- function(
  data,
  market_label = "BTC/USDT"
) {
  chart_data <- complete_hourly_price_return_grid(data)
  palette <- book_plot_palette("dark")

  return_plot <- plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~log_return_percent,
    type = "scattergl",
    mode = "lines",
    line = list(color = palette[["accent"]], width = 1.35),
    meta = book_trace_meta("accent"),
    connectgaps = FALSE,
    hovertemplate = paste0(
      "%{x|%Y-%m-%d %H:%M} UTC",
      "<br>Логарифмічна дохідність: %{y:.3f}%",
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::layout(
      xaxis = modifyList(
        book_time_axis(NULL),
        list(showticklabels = FALSE)
      ),
      yaxis = book_axis_style("Лог-дохідність, %")
    )

  difference_plot <- plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~return_difference_basis_points,
    type = "scattergl",
    mode = "lines",
    line = list(color = palette[["naive"]], width = 1.35),
    meta = book_trace_meta("naive"),
    connectgaps = FALSE,
    hovertemplate = paste0(
      "%{x|%Y-%m-%d %H:%M} UTC",
      "<br>Звичайна мінус логарифмічна: %{y:.3f} б.п.",
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::layout(
      xaxis = book_time_axis("Дата і час, UTC", rangeslider = TRUE),
      yaxis = book_axis_style("Різниця, б.п.")
    )

  plotly::subplot(
    return_plot,
    difference_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.68, 0.32),
    margin = 0.07,
    titleX = TRUE,
    titleY = TRUE
  ) |>
    plotly::layout(
      title = book_plot_title(
        paste("Годинні дохідності", market_label)
      ),
      annotations = list(
        list(
          x = 0,
          y = 1.01,
          xref = "paper",
          yref = "paper",
          text = paste("Логарифмічна дохідність", market_label),
          showarrow = FALSE,
          xanchor = "left",
          font = list(size = 13)
        ),
        list(
          x = 0,
          y = 0.31,
          xref = "paper",
          yref = "paper",
          text = "Різниця між звичайною і логарифмічною дохідністю",
          showarrow = FALSE,
          xanchor = "left",
          font = list(size = 13)
        )
      )
    ) |>
    render_book_widget(
      hovermode = "x unified",
      size = "double",
      margin = book_chart_margin("two_panel"),
      showlegend = FALSE,
      auto_y_on_x = FALSE
    )
}
