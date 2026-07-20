# Shared chart system -----------------------------------------------------
#
# Every chart displayed in the HTML book must pass through
# `render_book_plot()`.  This keeps dimensions, Plotly controls and the
# light/dark appearance consistent across data and model chapters.

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

apply_book_plotly_layout <- function(
  widget,
  hovermode = "x unified"
) {
  widget |>
    plotly::layout(
      autosize = TRUE,
      hovermode = hovermode,
      margin = list(l = 115, r = 30, t = 90, b = 70),
      legend = list(orientation = "h", x = 0, y = 1.08),
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      plot_bgcolor = "rgba(0, 0, 0, 0)"
    ) |>
    plotly::config(
      displaylogo = FALSE,
      responsive = TRUE
    ) |>
    apply_quarto_plotly_theme()
}

disable_plotly_gap_connection <- function(widget) {
  widget$x$data <- lapply(
    widget$x$data,
    function(trace) {
      if (!is.null(trace$mode) && grepl("lines", trace$mode, fixed = TRUE)) {
        trace$connectgaps <- FALSE
      }
      trace
    }
  )
  widget
}

book_ggplot_theme <- function(base_size = 15) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),
      legend.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),
      legend.key = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      )
    )
}

render_book_plot <- function(
  chart,
  tooltip = "all",
  hovermode = "x unified",
  connect_gaps = FALSE
) {
  if (!inherits(chart, "ggplot")) {
    stop("render_book_plot() очікує об'єкт ggplot.")
  }

  widget <- plotly::ggplotly(
    chart,
    tooltip = tooltip
  )

  if (!isTRUE(connect_gaps)) {
    widget <- disable_plotly_gap_connection(widget)
  }

  apply_book_plotly_layout(
    widget,
    hovermode = hovermode
  )
}
