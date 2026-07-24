# Shared chart system -----------------------------------------------------
#
# The book uses one fixed chart theme for all interactive graphics. This makes
# text readable in both light and dark Quarto themes and avoids white-on-white
# or black-on-black combinations.

book_plot_palette <- function() {
  c(
    ar1 = "#3B82F6",
    naive = "#F59E0B",
    buy_hold = "#10B981",
    cash = "#A855F7",
    actual = "#F8FAFC",
    positive = "#22C55E",
    negative = "#F43F5E",
    accent = "#38BDF8",
    neutral = "#94A3B8",
    grid = "#334155",
    background = "rgba(0,0,0,0)",
    panel = "rgba(0,0,0,0)",
    text = "#F8FAFC"
  )
}

book_forecast_colours <- function() {
  palette <- book_plot_palette()
  c(
    "AR(1)" = palette[["ar1"]],
    "Наївний прогноз" = palette[["naive"]]
  )
}

book_strategy_colours <- function() {
  palette <- book_plot_palette()
  c(
    "AR(1): BTC або USDT" = palette[["ar1"]],
    "Купи й тримай" = palette[["buy_hold"]],
    "USDT без торгівлі" = palette[["cash"]]
  )
}

book_plot_title <- function(text) {
  list(
    text = text,
    x = 0,
    xanchor = "left",
    y = 0.985,
    yanchor = "top",
    pad = list(b = 14)
  )
}

book_axis_style <- function(title = NULL) {
  palette <- book_plot_palette()
  list(
    title = list(text = title, font = list(color = palette[["text"]], size = 15)),
    color = palette[["text"]],
    tickfont = list(color = palette[["text"]], size = 13),
    gridcolor = palette[["grid"]],
    zerolinecolor = palette[["neutral"]],
    linecolor = palette[["neutral"]],
    automargin = TRUE,
    fixedrange = FALSE
  )
}

book_time_range <- function(times, hours) {
  times <- sort(as.POSIXct(times, tz = "UTC"))
  if (length(times) == 0L) {
    return(NULL)
  }

  end_time <- utils::tail(times, 1L)
  start_time <- max(
    utils::head(times, 1L),
    end_time - as.difftime(hours, units = "hours")
  )

  format(
    c(start_time, end_time),
    "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
}

book_time_axis <- function(title = "Дата і час, UTC", range = NULL) {
  axis <- book_axis_style(title)
  axis$type <- "date"
  if (!is.null(range)) {
    axis$range <- range
  }
  axis$rangeslider <- list(
    visible = TRUE,
    thickness = 0.10,
    bgcolor = "rgba(148,163,184,0.10)",
    bordercolor = "rgba(148,163,184,0.45)",
    borderwidth = 1
  )
  axis$showspikes <- FALSE
  axis
}

render_book_widget <- function(
  widget,
  hovermode = "closest",
  height = 720,
  margin = list(l = 90, r = 26, t = 108, b = 78),
  legend = list(
    orientation = "h",
    x = 0,
    y = 1.07,
    xanchor = "left",
    yanchor = "bottom",
    bgcolor = "rgba(0,0,0,0)",
    font = list(size = 13)
  ),
  showlegend = TRUE,
  auto_y_on_x = FALSE
) {
  if (!inherits(widget, "plotly")) {
    stop("render_book_widget() очікує об'єкт plotly.")
  }

  palette <- book_plot_palette()

  widget <- widget |>
    plotly::layout(
      autosize = TRUE,
      hovermode = hovermode,
      margin = margin,
      legend = legend,
      showlegend = showlegend,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(
        color = palette[["text"]],
        family = "system-ui, sans-serif",
        size = 15
      ),
      hoverlabel = list(
        bgcolor = "rgba(11,18,32,0.98)",
        bordercolor = palette[["neutral"]],
        font = list(color = palette[["text"]])
      ),
      meta = list(
        book_chart = TRUE,
        auto_y_on_x = isTRUE(auto_y_on_x)
      ),
      dragmode = "zoom"
    ) |>
    plotly::config(
      displaylogo = FALSE,
      responsive = TRUE,
      scrollZoom = FALSE,
      doubleClick = "reset",
      modeBarButtonsToRemove = c(
        "lasso2d",
        "select2d",
        "autoScale2d",
        "toggleSpikelines"
      )
    )

  widget$height <- height
  widget$sizingPolicy <- htmlwidgets::sizingPolicy(
    defaultHeight = height,
    browser.fill = FALSE,
    viewer.fill = FALSE,
    padding = 0
  )

  widget
}

book_ggplot_theme <- function(base_size = 15) {
  palette <- book_plot_palette()

  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = palette[["text"]]),
      axis.text = ggplot2::element_text(colour = palette[["text"]]),
      axis.title = ggplot2::element_text(colour = palette[["text"]]),
      plot.title = ggplot2::element_text(colour = palette[["text"]], face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = palette[["neutral"]]),
      plot.caption = ggplot2::element_text(colour = palette[["neutral"]]),
      legend.text = ggplot2::element_text(colour = palette[["text"]]),
      legend.title = ggplot2::element_text(colour = palette[["text"]]),
      panel.grid.major = ggplot2::element_line(colour = palette[["grid"]], linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = palette[["background"]], colour = palette[["background"]]),
      panel.background = ggplot2::element_rect(fill = palette[["panel"]], colour = palette[["panel"]]),
      legend.background = ggplot2::element_rect(fill = palette[["background"]], colour = NA),
      legend.key = ggplot2::element_rect(fill = palette[["background"]], colour = NA),
      plot.margin = ggplot2::margin(16, 18, 14, 16)
    )
}

render_book_plot <- function(
  chart,
  tooltip = "all",
  hovermode = "closest",
  height = 720
) {
  if (!inherits(chart, "ggplot")) {
    stop("render_book_plot() очікує об'єкт ggplot.")
  }

  plotly::ggplotly(chart, tooltip = tooltip) |>
    render_book_widget(
      hovermode = hovermode,
      height = height
    )
}
