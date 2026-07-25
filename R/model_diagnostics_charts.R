# Interactive charts for training-only model diagnostics ----------------

diagnostic_limit_traces <- function(
  data,
  colour,
  xaxis = "x",
  yaxis = "y"
) {
  maximum_lag <- max(data$lag)
  list(
    list(
      x = c(0, maximum_lag + 1L),
      y = rep(data$upper[[1L]], 2L),
      type = "scatter",
      mode = "lines",
      line = list(color = colour, width = 1.2, dash = "dot"),
      meta = book_trace_meta("naive"),
      hoverinfo = "skip",
      showlegend = FALSE,
      xaxis = xaxis,
      yaxis = yaxis
    ),
    list(
      x = c(0, maximum_lag + 1L),
      y = rep(data$lower[[1L]], 2L),
      type = "scatter",
      mode = "lines",
      line = list(color = colour, width = 1.2, dash = "dot"),
      meta = book_trace_meta("naive"),
      hoverinfo = "skip",
      showlegend = FALSE,
      xaxis = xaxis,
      yaxis = yaxis
    )
  )
}

add_diagnostic_limits <- function(widget, data) {
  palette <- book_plot_palette("dark")
  traces <- diagnostic_limit_traces(
    data,
    colour = palette[["naive"]]
  )
  for (trace in traces) {
    widget <- plotly::add_trace(
      widget,
      x = trace$x,
      y = trace$y,
      type = trace$type,
      mode = trace$mode,
      line = trace$line,
      meta = trace$meta,
      hoverinfo = trace$hoverinfo,
      showlegend = trace$showlegend,
      inherit = FALSE
    )
  }

  widget
}

correlation_panel <- function(data, title, role = "accent") {
  palette <- book_plot_palette("dark")

  widget <- plotly::plot_ly(
    data = data,
    x = ~lag,
    y = ~value,
    type = "bar",
    marker = list(color = palette[[role]]),
    meta = book_trace_meta(role),
    hovertemplate = paste0(
      "Лаг: %{x} год.",
      "<br>Коефіцієнт: %{y:.4f}",
      "<extra></extra>"
    ),
    showlegend = FALSE
  )
  widget <- add_diagnostic_limits(widget, data)

  widget |>
    plotly::layout(
      xaxis = book_axis_style("Лаг, годин"),
      yaxis = book_axis_style(title)
    )
}

plot_training_rolling_moments <- function(data, window_hours) {
  require_model_columns(
    data,
    c(
      "window_end",
      "mean_percent",
      "standard_deviation_percent"
    ),
    "графіка рухомих характеристик"
  )
  palette <- book_plot_palette("dark")

  mean_plot <- plotly::plot_ly(
    data = data,
    x = ~window_end,
    y = ~mean_percent,
    type = "scatter",
    mode = "lines",
    line = list(color = palette[["accent"]], width = 2.1),
    meta = book_trace_meta("accent"),
    hovertemplate = paste0(
      "%{x|%Y-%m-%d}",
      "<br>Середня дохідність: %{y:.4f}%",
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::layout(
      xaxis = modifyList(
        book_time_axis(NULL),
        list(showticklabels = FALSE)
      ),
      yaxis = book_axis_style("Середня, %")
    )

  deviation_plot <- plotly::plot_ly(
    data = data,
    x = ~window_end,
    y = ~standard_deviation_percent,
    type = "scatter",
    mode = "lines",
    line = list(color = palette[["negative"]], width = 2.1),
    meta = book_trace_meta("negative"),
    hovertemplate = paste0(
      "%{x|%Y-%m-%d}",
      "<br>Стандартне відхилення: %{y:.4f}%",
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::layout(
      xaxis = book_time_axis("Кінець рухомого вікна, UTC"),
      yaxis = book_axis_style("Стандартне відхилення, %")
    )

  plotly::subplot(
    mean_plot,
    deviation_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.48, 0.52),
    margin = 0.08,
    titleX = TRUE,
    titleY = TRUE
  ) |>
    plotly::layout(
      title = book_plot_title(
        paste0(
          "Середня та мінливість у ",
          window_hours,
          "-годинних вікнах"
        )
      )
    ) |>
    render_book_widget(
      hovermode = "x unified",
      size = "double",
      margin = book_chart_margin("diagnostic_two_panel"),
      showlegend = FALSE
    )
}

plot_return_acf_pacf <- function(acf_data, pacf_data) {
  acf_plot <- correlation_panel(
    acf_data,
    title = "ACF доходності",
    role = "accent"
  )
  pacf_plot <- correlation_panel(
    pacf_data,
    title = "PACF доходності",
    role = "ar1"
  )

  plotly::subplot(
    acf_plot,
    pacf_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.5, 0.5),
    margin = 0.08,
    titleX = TRUE,
    titleY = TRUE
  ) |>
    plotly::layout(
      title = book_plot_title(
        "Лінійна залежність між годинними доходностями"
      )
    ) |>
    render_book_widget(
      size = "double",
      margin = book_chart_margin("diagnostic_two_panel"),
      showlegend = FALSE
    )
}

plot_squared_return_acf <- function(data) {
  correlation_panel(
    data,
    title = "ACF квадратів доходності",
    role = "negative"
  ) |>
    plotly::layout(
      title = book_plot_title(
        "Залежність у величині годинних рухів"
      )
    ) |>
    render_book_widget(
      size = "standard",
      margin = book_chart_margin("diagnostic"),
      showlegend = FALSE
    )
}

plot_ar_order_selection <- function(data) {
  require_model_columns(
    data,
    c("order", "delta_bic", "selected"),
    "графіка вибору порядку AR"
  )
  palette <- book_plot_palette("dark")
  selected <- data[data$selected, , drop = FALSE]

  plotly::plot_ly(
    data = data,
    x = ~order,
    y = ~delta_bic,
    type = "scatter",
    mode = "lines+markers",
    line = list(color = palette[["neutral"]], width = 1.8),
    marker = list(color = palette[["neutral"]], size = 6),
    meta = book_trace_meta("neutral"),
    hovertemplate = paste0(
      "Порядок AR: %{x}",
      "<br>BIC мінус найкращий BIC: %{y:.2f}",
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::add_trace(
      data = selected,
      x = ~order,
      y = ~delta_bic,
      type = "scatter",
      mode = "markers+text",
      marker = list(
        color = palette[["positive"]],
        size = 12
      ),
      meta = book_trace_meta("positive"),
      text = ~paste0("обрано p = ", order),
      textposition = "top right",
      hovertemplate = paste0(
        "Мінімум BIC",
        "<br>Обраний порядок: %{x}",
        "<extra></extra>"
      ),
      showlegend = FALSE
    ) |>
    plotly::layout(
      title = book_plot_title(
        "BIC для послідовних моделей AR"
      ),
      xaxis = modifyList(
        book_axis_style("Порядок p"),
        list(
          tickmode = "linear",
          tick0 = 0,
          dtick = 4
        )
      ),
      yaxis = book_axis_style(
        "Різниця з найменшим BIC"
      )
    ) |>
    render_book_widget(
      size = "standard",
      margin = book_chart_margin("diagnostic"),
      showlegend = FALSE
    )
}

plot_return_normal_qq <- function(data) {
  require_model_columns(
    data,
    c(
      "normal_expected_percent",
      "empirical_percent"
    ),
    "нормального Q-Q графіка"
  )
  palette <- book_plot_palette("dark")
  limits <- range(
    data$normal_expected_percent,
    data$empirical_percent
  )

  plotly::plot_ly() |>
    plotly::add_trace(
      data = data,
      x = ~normal_expected_percent,
      y = ~empirical_percent,
      type = "scatter",
      mode = "markers",
      marker = list(
        color = palette[["accent"]],
        size = 5,
        opacity = 0.72
      ),
      meta = book_trace_meta("accent"),
      hovertemplate = paste0(
        "Нормальний квантиль: %{x:.3f}%",
        "<br>Емпіричний квантиль: %{y:.3f}%",
        "<extra></extra>"
      ),
      showlegend = FALSE,
      inherit = FALSE
    ) |>
    plotly::add_trace(
      x = limits,
      y = limits,
      type = "scatter",
      mode = "lines",
      line = list(
        color = palette[["neutral"]],
        width = 1.5,
        dash = "dash"
      ),
      meta = book_trace_meta("neutral"),
      hoverinfo = "skip",
      showlegend = FALSE,
      inherit = FALSE
    ) |>
    plotly::layout(
      title = book_plot_title(
        "Квантили доходності проти нормального розподілу"
      ),
      xaxis = book_axis_style(
        "Очікуваний нормальний квантиль, %"
      ),
      yaxis = book_axis_style(
        "Емпіричний квантиль доходності, %"
      )
    ) |>
    render_book_widget(
      size = "standard",
      margin = book_chart_margin("diagnostic"),
      showlegend = FALSE
    )
}

plot_arma_order_selection <- function(data) {
  require_model_columns(
    data,
    c(
      "ar_order",
      "ma_order",
      "delta_bic",
      "selected"
    ),
    "графіка вибору порядку ARMA"
  )
  palette <- book_plot_palette("dark")
  ar_orders <- sort(unique(data$ar_order))
  ma_orders <- sort(unique(data$ma_order))
  delta_bic <- matrix(
    NA_real_,
    nrow = length(ma_orders),
    ncol = length(ar_orders),
    dimnames = list(ma_orders, ar_orders)
  )
  for (index in seq_len(nrow(data))) {
    delta_bic[
      as.character(data$ma_order[[index]]),
      as.character(data$ar_order[[index]])
    ] <- data$delta_bic[[index]]
  }
  selected <- data[data$selected, , drop = FALSE]

  plotly::plot_ly() |>
    plotly::add_trace(
      x = ar_orders,
      y = ma_orders,
      z = delta_bic,
      type = "heatmap",
      colorscale = list(
        c(0, palette[["positive"]]),
        c(0.35, palette[["accent"]]),
        c(1, palette[["negative"]])
      ),
      colorbar = list(
        title = list(text = "ΔBIC"),
        thickness = 14
      ),
      meta = book_trace_meta("accent"),
      hovertemplate = paste0(
        "AR порядок p: %{x}",
        "<br>MA порядок q: %{y}",
        "<br>BIC мінус найкращий: %{z:.2f}",
        "<extra></extra>"
      ),
      inherit = FALSE
    ) |>
    plotly::add_trace(
      data = selected,
      x = ~ar_order,
      y = ~ma_order,
      type = "scatter",
      mode = "markers",
      marker = list(
        color = palette[["text"]],
        size = 14,
        symbol = "x",
        line = list(width = 2)
      ),
      meta = book_trace_meta("actual"),
      hovertemplate = paste0(
        "Обрана модель",
        "<br>ARMA(%{x},%{y})",
        "<extra></extra>"
      ),
      showlegend = FALSE,
      inherit = FALSE
    ) |>
    plotly::layout(
      title = book_plot_title(
        "BIC у сітці моделей середнього ARMA"
      ),
      xaxis = modifyList(
        book_axis_style("AR порядок p"),
        list(
          tickmode = "array",
          tickvals = ar_orders
        )
      ),
      yaxis = modifyList(
        book_axis_style("MA порядок q"),
        list(
          tickmode = "array",
          tickvals = ma_orders
        )
      )
    ) |>
    render_book_widget(
      size = "standard",
      margin = book_chart_margin("diagnostic"),
      showlegend = FALSE
    )
}

plot_volatility_dependence <- function(
  absolute_acf,
  squared_acf
) {
  absolute_plot <- correlation_panel(
    absolute_acf,
    title = "ACF модуля доходності",
    role = "accent"
  )
  squared_plot <- correlation_panel(
    squared_acf,
    title = "ACF квадрата доходності",
    role = "negative"
  )

  plotly::subplot(
    absolute_plot,
    squared_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.5, 0.5),
    margin = 0.08,
    titleX = TRUE,
    titleY = TRUE
  ) |>
    plotly::layout(
      title = book_plot_title(
        "Часова залежність у масштабі годинних рухів"
      )
    ) |>
    render_book_widget(
      size = "double",
      margin = book_chart_margin("diagnostic_two_panel"),
      showlegend = FALSE
    )
}

plot_calendar_volatility <- function(hourly, weekday) {
  require_model_columns(
    hourly,
    c("hour", "mean_absolute_return_percent"),
    "годинного календарного профілю"
  )
  require_model_columns(
    weekday,
    c("weekday_label", "mean_absolute_return_percent"),
    "денного календарного профілю"
  )
  palette <- book_plot_palette("dark")

  hourly_plot <- plotly::plot_ly(
    data = hourly,
    x = ~hour,
    y = ~mean_absolute_return_percent,
    type = "scatter",
    mode = "lines+markers",
    line = list(color = palette[["accent"]], width = 2),
    marker = list(color = palette[["accent"]], size = 6),
    meta = book_trace_meta("accent"),
    hovertemplate = paste0(
      "Година UTC: %{x}:00",
      "<br>Середній |r|: %{y:.3f}%",
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::layout(
      xaxis = modifyList(
        book_axis_style("Година доби, UTC"),
        list(dtick = 2)
      ),
      yaxis = book_axis_style("Середній |r|, %")
    )

  weekday_plot <- plotly::plot_ly(
    data = weekday,
    x = ~weekday_label,
    y = ~mean_absolute_return_percent,
    type = "bar",
    marker = list(color = palette[["negative"]]),
    meta = book_trace_meta("negative"),
    hovertemplate = paste0(
      "%{x}",
      "<br>Середній |r|: %{y:.3f}%",
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::layout(
      xaxis = book_axis_style("День тижня, UTC"),
      yaxis = book_axis_style("Середній |r|, %")
    )

  plotly::subplot(
    hourly_plot,
    weekday_plot,
    nrows = 2,
    heights = c(0.56, 0.44),
    margin = 0.09,
    titleX = TRUE,
    titleY = TRUE
  ) |>
    plotly::layout(
      title = book_plot_title(
        "Календарний профіль абсолютної доходності"
      )
    ) |>
    render_book_widget(
      size = "double",
      margin = book_chart_margin("diagnostic_two_panel"),
      showlegend = FALSE
    )
}

plot_lagged_volatility_signals <- function(data) {
  require_model_columns(
    data,
    c(
      "feature",
      "spearman_with_next_absolute_return"
    ),
    "графіка лагових ознак волатильності"
  )
  palette <- book_plot_palette("dark")
  ordered <- data[
    order(data$spearman_with_next_absolute_return),
    ,
    drop = FALSE
  ]
  ordered$feature <- factor(
    ordered$feature,
    levels = ordered$feature
  )

  plotly::plot_ly(
    data = ordered,
    x = ~spearman_with_next_absolute_return,
    y = ~feature,
    type = "bar",
    orientation = "h",
    marker = list(color = palette[["accent"]]),
    meta = book_trace_meta("accent"),
    hovertemplate = paste0(
      "%{y}",
      "<br>Spearman з наступним |r|: %{x:.3f}",
      "<extra></extra>"
    ),
    showlegend = FALSE
  ) |>
    plotly::layout(
      title = book_plot_title(
        "Лаговані ознаки та масштаб наступного руху"
      ),
      xaxis = book_axis_style(
        "Кореляція Spearman з наступним |r|"
      ),
      yaxis = book_axis_style(NULL)
    ) |>
    render_book_widget(
      size = "standard",
      margin = book_chart_margin("diagnostic_labels"),
      showlegend = FALSE
    )
}
