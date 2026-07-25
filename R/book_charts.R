# Shared chart system -----------------------------------------------------
#
# Public charts use one semantic colour system, responsive dimensions and
# consistent interaction rules. Plotly traces carry a role in `meta`; the
# browser switches the actual trace colours together with the Quarto theme.

require_figure_text <- function(value, name) {
  value <- as.character(value)
  if (
    length(value) == 0L ||
      anyNA(value) ||
      any(!nzchar(trimws(value)))
  ) {
    stop("Для опису графіка бракує поля: ", name)
  }

  value
}

book_figure_markdown <- function(
  type,
  title,
  fields,
  css_class
) {
  field_blocks <- unlist(
    lapply(
      fields,
      function(field) {
        values <- require_figure_text(
          field$value,
          field$label
        )
        body <- if (length(values) == 1L) {
          values
        } else {
          c("", paste0("- ", values))
        }

        c(
          paste0("**", field$label, ":**"),
          body,
          ""
        )
      }
    ),
    use.names = FALSE
  )

  markdown <- c(
    paste0(
      "::: {.callout-",
      type,
      " appearance=\"simple\" title=\"",
      title,
      "\" .",
      css_class,
      "}"
    ),
    field_blocks,
    ":::"
  )

  paste(markdown, collapse = "\n")
}

book_figure_callout <- function(
  type,
  title,
  fields,
  css_class
) {
  knitr::asis_output(book_figure_markdown(
    type = type,
    title = title,
    fields = fields,
    css_class = css_class
  ))
}

book_figure_intro <- function(
  question,
  explanation,
  how_to_read,
  caution = NULL
) {
  fields <- list(
    list(label = "Питання", value = question),
    list(label = "Що показано", value = explanation),
    list(label = "Як читати", value = how_to_read)
  )
  if (!is.null(caution)) {
    fields[[length(fields) + 1L]] <- list(
      label = "Важливе обмеження",
      value = caution
    )
  }

  book_figure_callout(
    type = "tip",
    title = "Перед графіком",
    fields = fields,
    css_class = "figure-reading-guide"
  )
}

book_figure_comment <- function(
  observation,
  conclusion,
  follow_up
) {
  book_figure_callout(
    type = "note",
    title = "Висновок після графіка",
    fields = list(
      list(label = "Що видно", value = observation),
      list(label = "Головний висновок", value = conclusion),
      list(
        label = "Що варто перевірити або змінити",
        value = follow_up
      )
    ),
    css_class = "figure-reader-comment"
  )
}

book_figure_review <- function(
  questions,
  source_step = paste(
    "Після локального рендеру зафіксуйте спостереження.",
    "Теоретичне тлумачення й рішення щодо моделей",
    "будуть додані окремо після звірки з джерелами."
  )
) {
  book_figure_callout(
    type = "note",
    title = "Після локального перегляду",
    fields = list(
      list(
        label = "Що потрібно зафіксувати",
        value = questions
      ),
      list(
        label = "Наступний етап",
        value = source_step
      )
    ),
    css_class = "figure-reader-review"
  )
}

book_model_decision <- function(
  model,
  approved,
  reason,
  next_step
) {
  model <- require_figure_text(model, "модель")
  if (
    length(approved) != 1L ||
      is.na(approved) ||
      !is.logical(approved)
  ) {
    stop("approved має бути одним логічним значенням.")
  }

  book_figure_callout(
    type = if (approved) "important" else "warning",
    title = paste(
      "Рішення щодо",
      model
    ),
    fields = list(
      list(
        label = "Статус",
        value = if (approved) {
          "Передумови виконано."
        } else {
          "Передумови не виконано."
        }
      ),
      list(label = "Підстава", value = reason),
      list(label = "Наступна дія", value = next_step)
    ),
    css_class = "model-decision"
  )
}

book_format_number <- function(value, digits = 2L) {
  formatC(
    value,
    format = "f",
    digits = as.integer(digits),
    big.mark = " "
  )
}

book_format_probability <- function(
  value,
  digits = 4L,
  lower_display_limit = 0.0001
) {
  if (value < lower_display_limit) {
    paste0("< ", book_format_number(lower_display_limit, digits))
  } else {
    book_format_number(value, digits)
  }
}

book_plot_palettes <- function() {
  list(
    dark = c(
      ar1 = "#60A5FA",
      naive = "#FBBF24",
      buy_hold = "#14B8A6",
      cash = "#C084FC",
      actual = "#22C55E",
      price = "#F59E0B",
      positive = "#4ADE80",
      negative = "#FB7185",
      accent = "#38BDF8",
      neutral = "#94A3B8",
      grid = "#465266",
      background = "rgba(0,0,0,0)",
      panel = "rgba(0,0,0,0)",
      text = "#F3F4F6"
    ),
    light = c(
      ar1 = "#2563EB",
      naive = "#B45309",
      buy_hold = "#0F766E",
      cash = "#7C3AED",
      actual = "#15803D",
      price = "#B45309",
      positive = "#16A34A",
      negative = "#BE123C",
      accent = "#0369A1",
      neutral = "#64748B",
      grid = "#D1D5DB",
      background = "rgba(0,0,0,0)",
      panel = "rgba(0,0,0,0)",
      text = "#111827"
    )
  )
}

book_plot_palette <- function(theme = c("dark", "light")) {
  theme <- match.arg(theme)
  book_plot_palettes()[[theme]]
}

book_trace_meta <- function(role) {
  list(book_role = as.character(role))
}

book_forecast_colours <- function(theme = "dark") {
  palette <- book_plot_palette(theme)
  c(
    "AR(1)" = palette[["ar1"]],
    "Наївний прогноз" = palette[["naive"]]
  )
}

book_strategy_colours <- function(theme = "dark") {
  palette <- book_plot_palette(theme)
  c(
    "AR(1): BTC або USDT" = palette[["ar1"]],
    "Купи й тримай" = palette[["buy_hold"]],
    "USDT без торгівлі" = palette[["cash"]]
  )
}

book_chart_sizes <- function() {
  list(
    compact = list(
      height = 640L,
      min_height = 540L,
      max_height = 760L,
      aspect_ratio = 0.78,
      mobile_min_height = 540L,
      mobile_max_height = 740L,
      mobile_aspect_ratio = 1.10
    ),
    standard = list(
      height = 740L,
      min_height = 620L,
      max_height = 880L,
      aspect_ratio = 0.86,
      mobile_min_height = 600L,
      mobile_max_height = 840L,
      mobile_aspect_ratio = 1.18
    ),
    tall = list(
      height = 820L,
      min_height = 680L,
      max_height = 960L,
      aspect_ratio = 0.98,
      mobile_min_height = 650L,
      mobile_max_height = 920L,
      mobile_aspect_ratio = 1.28
    ),
    double = list(
      height = 920L,
      min_height = 760L,
      max_height = 1080L,
      aspect_ratio = 1.12,
      mobile_min_height = 760L,
      mobile_max_height = 1040L,
      mobile_aspect_ratio = 1.55
    )
  )
}

book_chart_size <- function(size = "standard") {
  sizes <- book_chart_sizes()
  if (
    length(size) != 1L ||
      is.na(size) ||
      !size %in% names(sizes)
  ) {
    stop(
      "Невідомий розмір графіка: ",
      paste(size, collapse = ", ")
    )
  }

  sizes[[size]]
}

book_size_value <- function(value, defaults, name) {
  if (is.null(value)) {
    return(defaults[[name]])
  }

  value
}

book_plot_margin <- function(
  left = 72,
  right = 18,
  top = 104,
  bottom = 84
) {
  list(
    l = as.integer(left),
    r = as.integer(right),
    t = as.integer(top),
    b = as.integer(bottom)
  )
}

book_chart_margins <- function() {
  list(
    standard = book_plot_margin(bottom = 86),
    price = book_plot_margin(bottom = 90),
    two_panel = book_plot_margin(
      left = 76,
      top = 112,
      bottom = 88
    ),
    time_split = book_plot_margin(
      left = 102,
      top = 100,
      bottom = 82
    ),
    range_controls = book_plot_margin(
      top = 104,
      bottom = 92
    ),
    wide_axis = book_plot_margin(
      left = 78,
      bottom = 86
    ),
    calibration = book_plot_margin(
      left = 78,
      bottom = 90
    ),
    metric_labels = book_plot_margin(
      left = 118
    ),
    rolling = book_plot_margin(
      left = 88,
      bottom = 86
    ),
    compact = book_plot_margin(bottom = 82),
    strategy = book_plot_margin(
      top = 112,
      bottom = 88
    ),
    activity = book_plot_margin(
      left = 76,
      top = 110,
      bottom = 90
    ),
    diagnostic = book_plot_margin(
      left = 78,
      top = 112,
      bottom = 86
    ),
    diagnostic_two_panel = book_plot_margin(
      left = 82,
      top = 116,
      bottom = 90
    ),
    diagnostic_labels = book_plot_margin(
      left = 230,
      top = 112,
      bottom = 86
    )
  )
}

book_chart_margin <- function(profile = "standard") {
  margins <- book_chart_margins()
  if (
    length(profile) != 1L ||
      is.na(profile) ||
      !profile %in% names(margins)
  ) {
    stop(
      "Невідомий профіль відступів графіка: ",
      paste(profile, collapse = ", ")
    )
  }

  margins[[profile]]
}

book_plot_title <- function(text) {
  list(
    text = text,
    x = 0,
    xanchor = "left",
    y = 0.985,
    yanchor = "top",
    font = list(size = 19),
    pad = list(b = 14)
  )
}

book_axis_style <- function(title = NULL) {
  palette <- book_plot_palette("dark")
  list(
    title = list(
      text = title,
      font = list(color = palette[["text"]], size = 15)
    ),
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

book_time_axis <- function(
  title = "Дата і час, UTC",
  range = NULL,
  rangeslider = FALSE,
  range_buttons = FALSE
) {
  axis <- book_axis_style(title)
  axis$type <- "date"

  if (!is.null(range)) {
    axis$range <- range
  }

  if (isTRUE(rangeslider)) {
    axis$rangeslider <- list(
      visible = TRUE,
      thickness = 0.09,
      bgcolor = "rgba(148,163,184,0.10)",
      bordercolor = "rgba(148,163,184,0.45)",
      borderwidth = 1
    )
  }

  if (isTRUE(range_buttons)) {
    axis$rangeselector <- list(
      x = 0,
      y = 1.045,
      xanchor = "left",
      yanchor = "bottom",
      font = list(size = 12, color = "#0F172A"),
      bgcolor = "rgba(255,255,255,0.97)",
      activecolor = "rgba(253,224,71,0.97)",
      bordercolor = "rgba(100,116,139,0.60)",
      borderwidth = 1,
      buttons = list(
        list(count = 7L, label = "7 днів", step = "day", stepmode = "backward"),
        list(count = 30L, label = "30 днів", step = "day", stepmode = "backward"),
        list(step = "all", label = "Увесь період")
      )
    )
  }

  axis$showspikes <- FALSE
  axis
}

render_book_widget <- function(
  widget,
  hovermode = "closest",
  size = "standard",
  height = NULL,
  min_height = NULL,
  max_height = NULL,
  aspect_ratio = NULL,
  mobile_min_height = NULL,
  mobile_max_height = NULL,
  mobile_aspect_ratio = NULL,
  margin = book_chart_margin("standard"),
  legend = list(
    orientation = "h",
    x = 0,
    y = 1.08,
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

  size_defaults <- book_chart_size(size)
  height <- book_size_value(height, size_defaults, "height")
  min_height <- book_size_value(
    min_height,
    size_defaults,
    "min_height"
  )
  max_height <- book_size_value(
    max_height,
    size_defaults,
    "max_height"
  )
  aspect_ratio <- book_size_value(
    aspect_ratio,
    size_defaults,
    "aspect_ratio"
  )
  mobile_min_height <- book_size_value(
    mobile_min_height,
    size_defaults,
    "mobile_min_height"
  )
  mobile_max_height <- book_size_value(
    mobile_max_height,
    size_defaults,
    "mobile_max_height"
  )
  mobile_aspect_ratio <- book_size_value(
    mobile_aspect_ratio,
    size_defaults,
    "mobile_aspect_ratio"
  )

  palette <- book_plot_palette("dark")
  palettes <- lapply(book_plot_palettes(), as.list)

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
        auto_y_on_x = isTRUE(auto_y_on_x),
        palettes = palettes,
        aspect_ratio = as.numeric(aspect_ratio),
        min_height = as.integer(min_height),
        max_height = as.integer(max_height),
        mobile_min_height = as.integer(mobile_min_height),
        mobile_max_height = as.integer(mobile_max_height),
        mobile_aspect_ratio = as.numeric(mobile_aspect_ratio)
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
  palette <- book_plot_palette("dark")

  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = palette[["text"]]),
      axis.text = ggplot2::element_text(colour = palette[["text"]]),
      axis.title = ggplot2::element_text(colour = palette[["text"]]),
      plot.title = ggplot2::element_text(
        colour = palette[["text"]],
        face = "bold",
        size = ggplot2::rel(1.15)
      ),
      plot.subtitle = ggplot2::element_text(colour = palette[["neutral"]]),
      plot.caption = ggplot2::element_text(colour = palette[["neutral"]]),
      legend.text = ggplot2::element_text(colour = palette[["text"]]),
      legend.title = ggplot2::element_text(colour = palette[["text"]]),
      panel.grid.major = ggplot2::element_line(
        colour = palette[["grid"]],
        linewidth = 0.35
      ),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(
        fill = palette[["background"]],
        colour = palette[["background"]]
      ),
      panel.background = ggplot2::element_rect(
        fill = palette[["panel"]],
        colour = palette[["panel"]]
      ),
      legend.background = ggplot2::element_rect(
        fill = palette[["background"]],
        colour = NA
      ),
      legend.key = ggplot2::element_rect(
        fill = palette[["background"]],
        colour = NA
      ),
      plot.margin = ggplot2::margin(16, 18, 14, 16)
    )
}

render_book_plot <- function(
  chart,
  tooltip = "all",
  hovermode = "closest",
  size = "standard",
  margin = book_chart_margin("standard"),
  role = "accent"
) {
  if (!inherits(chart, "ggplot")) {
    stop("render_book_plot() очікує об'єкт ggplot.")
  }

  widget <- plotly::ggplotly(chart, tooltip = tooltip)
  if (length(widget$x$data) > 0L) {
    for (index in seq_along(widget$x$data)) {
      widget$x$data[[index]]$meta <- book_trace_meta(role)
    }
  }

  render_book_widget(
    widget,
    hovermode = hovermode,
    size = size,
    margin = margin
  )
}
