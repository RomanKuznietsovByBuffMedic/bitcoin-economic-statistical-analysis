# Shared chart system -----------------------------------------------------
#
# Every chart displayed in the HTML book must pass through
# `render_book_plot()` or `render_book_widget()`. This keeps dimensions,
# Plotly controls and the light/dark appearance consistent across chapters.

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

apply_quarto_plotly_theme <- function(
  widget,
  height = NULL
) {
  htmlwidgets::onRender(
    widget,
    read_plotly_quarto_theme(),
    data = list(height = height)
  )
}

apply_book_plotly_layout <- function(
  widget,
  hovermode = "x unified",
  margin = list(l = 115, r = 30, t = 90, b = 70),
  legend = list(orientation = "h", x = 0, y = 1.08),
  showlegend = TRUE,
  height = NULL
) {
  widget |>
    plotly::layout(
      autosize = TRUE,
      hovermode = hovermode,
      margin = margin,
      legend = legend,
      showlegend = showlegend,
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      plot_bgcolor = "rgba(0, 0, 0, 0)"
    ) |>
    plotly::config(
      displaylogo = FALSE,
      responsive = TRUE
    ) |>
    apply_quarto_plotly_theme(height = height)
}

disable_plotly_gap_connection <- function(widget) {
  disable_trace_gaps <- function(trace) {
    if (!is.null(trace$mode) && grepl("lines", trace$mode, fixed = TRUE)) {
      trace$connectgaps <- FALSE
    }
    trace
  }

  if (length(widget$x$data) > 0) {
    widget$x$data <- lapply(widget$x$data, disable_trace_gaps)
  }
  if (length(widget$x$attrs) > 0) {
    widget$x$attrs <- lapply(widget$x$attrs, disable_trace_gaps)
  }

  widget
}

render_book_widget <- function(
  widget,
  hovermode = "x unified",
  connect_gaps = FALSE,
  margin = list(l = 115, r = 30, t = 90, b = 70),
  legend = list(orientation = "h", x = 0, y = 1.08),
  showlegend = TRUE,
  height = NULL
) {
  if (!inherits(widget, "plotly")) {
    stop("render_book_widget() очікує об'єкт plotly.")
  }

  if (!isTRUE(connect_gaps)) {
    widget <- disable_plotly_gap_connection(widget)
  }

  apply_book_plotly_layout(
    widget,
    hovermode = hovermode,
    margin = margin,
    legend = legend,
    showlegend = showlegend,
    height = height
  )
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

  render_book_widget(
    widget,
    hovermode = hovermode,
    connect_gaps = connect_gaps
  )
}
