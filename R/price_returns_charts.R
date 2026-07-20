# Price-return charts -----------------------------------------------------
#
# This module contains only charts for prepared price and return data.
# Shared rendering and themes are defined in R/book_charts.R.

complete_hourly_price_grid <- function(data) {
  if (nrow(data) == 0) {
    stop("Неможливо побудувати графік для порожнього набору даних.")
  }

  observed_prices <- data |>
    dplyr::select(open_time, price_quote_per_btc) |>
    dplyr::arrange(open_time) |>
    dplyr::distinct(open_time, .keep_all = TRUE)

  hourly_grid <- tibble::tibble(
    open_time = seq.POSIXt(
      from = min(observed_prices$open_time),
      to = max(observed_prices$open_time),
      by = "hour"
    )
  )

  hourly_grid |>
    dplyr::left_join(observed_prices, by = "open_time")
}

make_price_history_widget <- function(
  data,
  quote_currency = "USDT",
  market_label = "BTC/USDT"
) {
  chart_data <- complete_hourly_price_grid(data)

  chart <- ggplot2::ggplot(
    chart_data,
    ggplot2::aes(x = open_time, y = price_quote_per_btc)
  ) +
    ggplot2::geom_line(
      linewidth = 0.35,
      colour = "#F2A900",
      na.rm = FALSE
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(
        big.mark = " ",
        suffix = paste0(" ", quote_currency)
      )
    ) +
    ggplot2::labs(
      x = NULL,
      y = paste(quote_currency, "за 1 BTC"),
      title = paste("Ціна закриття", market_label),
      subtitle = "Відсутні години не з'єднуються лінією"
    ) +
    book_ggplot_theme(base_size = 15) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )

  render_book_plot(
    chart,
    tooltip = c("x", "y"),
    connect_gaps = FALSE
  )
}
