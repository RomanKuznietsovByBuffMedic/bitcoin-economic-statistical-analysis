# Price-return charts ------------------------------------------------------
#
# This module contains chart construction and shared Plotly settings.

price_returns_plot_height <- 420
plotly_quarto_theme_file <- file.path(
  "assets",
  "plotly-quarto-theme.js"
)

read_plotly_quarto_theme <- function(
  path = plotly_quarto_theme_file
) {
  if (!file.exists(path)) {
    stop("Не знайдено файл теми Plotly: ", path)
  }

  paste(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
}

apply_quarto_plotly_theme <- function(widget) {
  htmlwidgets::onRender(
    widget,
    read_plotly_quarto_theme()
  )
}

apply_book_plotly_layout <- function(widget, height = price_returns_plot_height) {
  widget |>
    plotly::layout(
      autosize = TRUE,
      height = height,
      hovermode = "x unified",
      margin = list(
        l = 80,
        r = 25,
        t = 75,
        b = 55
      ),
      legend = list(
        orientation = "h",
        x = 0,
        y = 1.08
      ),
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      plot_bgcolor = "rgba(0, 0, 0, 0)"
    ) |>
    plotly::config(
      displaylogo = FALSE,
      responsive = TRUE
    ) |>
    apply_quarto_plotly_theme()
}

make_price_history_widget <- function(
  data,
  validation_start,
  final_test_start
) {
  chart <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = open_time,
      y = price_usdt_per_btc
    )
  ) +
    ggplot2::geom_line(
      linewidth = 0.35,
      colour = "#F2A900"
    ) +
    ggplot2::geom_vline(
      xintercept = as.numeric(validation_start),
      linewidth = 0.55,
      linetype = "dashed",
      colour = "#2C7FB8"
    ) +
    ggplot2::geom_vline(
      xintercept = as.numeric(final_test_start),
      linewidth = 0.55,
      linetype = "dashed",
      colour = "#D7301F"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(
        big.mark = " ",
        suffix = " USDT"
      )
    ) +
    ggplot2::labs(
      x = NULL,
      y = "USDT за 1 BTC",
      title = "Ціна закриття BTCUSDT",
      subtitle = paste(
        "Синя межа - внутрішня перевірка;",
        "червона межа - фінальний тест"
      )
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  plotly::ggplotly(
    chart,
    tooltip = c("x", "y")
  ) |>
    apply_book_plotly_layout()
}

make_return_forecast_widget <- function(
  predictions,
  evaluation_name,
  evaluation_end,
  displayed_days = 14
) {
  plot_start <- evaluation_end - as.difftime(
    displayed_days,
    units = "days"
  )

  chart_data <- predictions |>
    dplyr::filter(target_open_time >= plot_start) |>
    dplyr::mutate(
      actual_return_percent =
        100 * target_simple_return_1h,
      predicted_return_percent =
        100 * predicted_simple_return_ar1
    )

  chart <- ggplot2::ggplot(
    chart_data,
    ggplot2::aes(x = target_open_time)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(
        y = actual_return_percent,
        colour = "Фактична"
      ),
      linewidth = 0.40,
      alpha = 0.80
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        y = predicted_return_percent,
        colour = "Прогноз AR(1)"
      ),
      linewidth = 0.65
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "Фактична" = "#52606D",
        "Прогноз AR(1)" = "#D7301F"
      )
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Зміна, %",
      colour = NULL,
      title = "Наступна годинна зміна BTCUSDT",
      subtitle = paste("Період оцінювання:", evaluation_name)
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank()
    )

  plotly::ggplotly(
    chart,
    tooltip = c("x", "y", "colour")
  ) |>
    apply_book_plotly_layout()
}
