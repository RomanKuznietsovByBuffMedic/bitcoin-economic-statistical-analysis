# Charts for descriptive data exploration -------------------------------
#
# Every function receives already prepared plotting data. Chart functions
# do not compute model scores or return model decisions.

exploration_custom_text <- function(values, digits = 4L) {
  formatC(
    values,
    format = "f",
    digits = as.integer(digits),
    big.mark = " "
  )
}

plot_annual_market_scale <- function(data) {
  require_exploration_columns(
    data,
    c(
      "year",
      "metric_id",
      "metric",
      "unit",
      "value",
      "index_first_period_100"
    ),
    "графіка річного масштабу"
  )
  palette <- book_plot_palette("dark")
  roles <- c(
    close = "price",
    volume = "accent",
    turnover = "actual",
    intrahour_range = "negative"
  )
  metric_ids <- unique(data$metric_id)
  unknown_ids <- setdiff(metric_ids, names(roles))
  if (length(unknown_ids) > 0L) {
    stop(
      "Невідомі річні показники: ",
      paste(unknown_ids, collapse = ", ")
    )
  }
  if (any(!grepl("^[0-9]{4}$", data$year))) {
    stop(
      "Рік для графіка має бути записаний чотирма цифрами."
    )
  }
  baseline_year <- min(data$year)

  widget <- plotly::plot_ly()
  for (metric_id in metric_ids) {
    selected <- data[
      data$metric_id == metric_id,
      ,
      drop = FALSE
    ]
    role <- roles[[metric_id]]
    selected$year_date <- as.Date(
      paste0(selected$year, "-01-01")
    )
    custom_text <- paste0(
      exploration_custom_text(selected$value),
      " ",
      selected$unit
    )
    widget <- plotly::add_trace(
      widget,
      x = selected$year_date,
      y = selected$index_first_period_100,
      customdata = custom_text,
      type = "scatter",
      mode = "lines+markers",
      name = selected$metric[[1L]],
      line = list(
        color = palette[[role]],
        width = 2.2
      ),
      marker = list(
        color = palette[[role]],
        size = 8
      ),
      meta = book_trace_meta(role),
      hovertemplate = paste0(
        "%{x|%Y}",
        "<br>Індекс: %{y:.1f}",
        "<br>Значення: %{customdata}",
        "<extra>%{fullData.name}</extra>"
      ),
      inherit = FALSE
    )
  }

  widget |>
    plotly::layout(
      title = book_plot_title(
        "Як змінювався типовий масштаб ринкових величин?"
      ),
      xaxis = modifyList(
        book_time_axis(
          "Календарний рік, UTC"
        ),
        list(
          tickformat = "%Y",
          dtick = "M12"
        )
      ),
      yaxis = book_axis_style(
        paste0("Індекс: ", baseline_year, " = 100")
      ),
      shapes = list(
        list(
          type = "line",
          xref = "paper",
          x0 = 0,
          x1 = 1,
          yref = "y",
          y0 = 100,
          y1 = 100,
          line = list(
            color = palette[["neutral"]],
            width = 1.2,
            dash = "dot"
          )
        )
      )
    ) |>
    render_book_widget(
      size = "standard",
      margin = book_chart_margin("diagnostic"),
      showlegend = TRUE
    )
}

plot_rolling_return_description <- function(
  data,
  window_hours
) {
  require_exploration_columns(
    data,
    c(
      "window_end",
      "mean_return_percent",
      "standard_deviation_percent",
      "median_absolute_return_percent"
    ),
    "графіка рухомого опису"
  )
  window_hours <- require_exploration_integer(
    window_hours,
    "Ширина рухомого вікна",
    minimum = 1L
  )
  palette <- book_plot_palette("dark")

  mean_plot <- plotly::plot_ly() |>
    plotly::add_trace(
      data = data,
      x = ~window_end,
      y = ~mean_return_percent,
      type = "scatter",
      mode = "lines",
      name = "Середня дохідність",
      line = list(
        color = palette[["accent"]],
        width = 2
      ),
      meta = book_trace_meta("accent"),
      hovertemplate = paste0(
        "%{x|%Y-%m-%d}",
        "<br>Середня: %{y:.4f}%",
        "<extra></extra>"
      ),
      showlegend = FALSE,
      inherit = FALSE
    ) |>
    plotly::layout(
      xaxis = modifyList(
        book_time_axis(NULL),
        list(showticklabels = FALSE)
      ),
      yaxis = book_axis_style("Середня, %")
    )

  scale_plot <- plotly::plot_ly() |>
    plotly::add_trace(
      data = data,
      x = ~window_end,
      y = ~standard_deviation_percent,
      type = "scatter",
      mode = "lines",
      name = "Стандартне відхилення",
      line = list(
        color = palette[["negative"]],
        width = 2.1
      ),
      meta = book_trace_meta("negative"),
      hovertemplate = paste0(
        "%{x|%Y-%m-%d}",
        "<br>Стандартне відхилення: %{y:.4f}%",
        "<extra></extra>"
      ),
      inherit = FALSE
    ) |>
    plotly::add_trace(
      data = data,
      x = ~window_end,
      y = ~median_absolute_return_percent,
      type = "scatter",
      mode = "lines",
      name = "Медіанний модуль",
      line = list(
        color = palette[["actual"]],
        width = 2.1,
        dash = "dash"
      ),
      meta = book_trace_meta("actual"),
      hovertemplate = paste0(
        "%{x|%Y-%m-%d}",
        "<br>Медіанний модуль дохідності: %{y:.4f}%",
        "<extra></extra>"
      ),
      inherit = FALSE
    ) |>
    plotly::layout(
      xaxis = book_time_axis(
        "Кінець рухомого вікна, UTC",
        rangeslider = TRUE
      ),
      yaxis = book_axis_style("Масштаб руху, %")
    )

  plotly::subplot(
    mean_plot,
    scale_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.44, 0.56),
    margin = 0.08,
    titleX = TRUE,
    titleY = TRUE
  ) |>
    plotly::layout(
      title = book_plot_title(
        paste0(
          "Як змінювалися центр і масштаб у ",
          window_hours,
          "-годинних вікнах?"
        )
      )
    ) |>
    render_book_widget(
      hovermode = "x unified",
      size = "double",
      margin = book_chart_margin("diagnostic_two_panel"),
      showlegend = TRUE
    )
}

plot_return_distribution <- function(histogram, qq) {
  require_exploration_columns(
    histogram,
    c("left", "right", "midpoint", "count"),
    "гістограми дохідності"
  )
  require_exploration_columns(
    qq,
    c(
      "normal_reference_percent",
      "empirical_percent"
    ),
    "Q-Q графіка дохідності"
  )
  palette <- book_plot_palette("dark")

  histogram_plot <- plotly::plot_ly() |>
    plotly::add_trace(
      data = histogram,
      x = ~midpoint,
      y = ~count,
      width = histogram$right - histogram$left,
      customdata = cbind(
        histogram$left,
        histogram$right
      ),
      type = "bar",
      marker = list(
        color = palette[["accent"]],
        line = list(width = 0)
      ),
      meta = book_trace_meta("accent"),
      hovertemplate = paste0(
        "Інтервал: (%{customdata[0]:.3f}%; ",
        "%{customdata[1]:.3f}%]",
        "<br>Годин: %{y}",
        "<extra></extra>"
      ),
      showlegend = FALSE,
      inherit = FALSE
    ) |>
    plotly::layout(
      xaxis = book_axis_style(
        "Годинна логарифмічна дохідність, %"
      ),
      yaxis = modifyList(
        book_axis_style(
          "Кількість годин, логарифмічна шкала"
        ),
        list(type = "log")
      ),
      bargap = 0
    )

  line_range <- range(
    c(
      qq$normal_reference_percent,
      qq$empirical_percent
    )
  )
  qq_plot <- plotly::plot_ly() |>
    plotly::add_trace(
      data = qq,
      x = ~normal_reference_percent,
      y = ~empirical_percent,
      type = "scatter",
      mode = "markers",
      marker = list(
        color = palette[["actual"]],
        size = 5,
        opacity = 0.75
      ),
      meta = book_trace_meta("actual"),
      hovertemplate = paste0(
        "Нормальний орієнтир: %{x:.3f}%",
        "<br>Фактичний квантиль: %{y:.3f}%",
        "<extra></extra>"
      ),
      showlegend = FALSE,
      inherit = FALSE
    ) |>
    plotly::add_trace(
      x = line_range,
      y = line_range,
      type = "scatter",
      mode = "lines",
      line = list(
        color = palette[["neutral"]],
        width = 1.5,
        dash = "dot"
      ),
      meta = book_trace_meta("neutral"),
      hoverinfo = "skip",
      showlegend = FALSE,
      inherit = FALSE
    ) |>
    plotly::layout(
      xaxis = book_axis_style("Квантиль нормального орієнтира, %"),
      yaxis = book_axis_style("Фактичний квантиль, %")
    )

  plotly::subplot(
    histogram_plot,
    qq_plot,
    nrows = 2,
    heights = c(0.5, 0.5),
    margin = 0.09,
    titleX = TRUE,
    titleY = TRUE
  ) |>
    plotly::layout(
      title = book_plot_title(
        "Яку форму має розподіл годинної дохідності?"
      )
    ) |>
    render_book_widget(
      size = "double",
      margin = book_chart_margin("diagnostic_two_panel"),
      showlegend = FALSE
    )
}

correlation_bar_panel <- function(data, y_title, role) {
  require_exploration_columns(
    data,
    c(
      "lag",
      "value",
      "lower_reference",
      "upper_reference"
    ),
    "кореляційної панелі"
  )
  palette <- book_plot_palette("dark")

  plotly::plot_ly() |>
    plotly::add_trace(
      data = data,
      x = ~lag,
      y = ~value,
      type = "bar",
      marker = list(
        color = palette[[role]],
        line = list(width = 0)
      ),
      meta = book_trace_meta(role),
      hovertemplate = paste0(
        "Лаг: %{x} год.",
        "<br>Коефіцієнт: %{y:.4f}",
        "<extra></extra>"
      ),
      showlegend = FALSE,
      inherit = FALSE
    ) |>
    plotly::layout(
      xaxis = book_axis_style("Лаг, годин"),
      yaxis = book_axis_style(y_title)
    )
}

correlation_reference_band <- function(
  data,
  xref,
  yref
) {
  list(
    type = "rect",
    xref = xref,
    x0 = 0.5,
    x1 = max(data$lag) + 0.5,
    yref = yref,
    y0 = data$lower_reference[[1L]],
    y1 = data$upper_reference[[1L]],
    fillcolor = "rgba(148,163,184,0.16)",
    line = list(width = 0),
    layer = "below"
  )
}

plot_return_correlations <- function(acf_data, pacf_data) {
  acf_plot <- correlation_bar_panel(
    acf_data,
    y_title = "ACF дохідності",
    role = "accent"
  )
  pacf_plot <- correlation_bar_panel(
    pacf_data,
    y_title = "PACF дохідності",
    role = "actual"
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
        paste(
          "Чи є лінійна залежність між",
          "дохідностями різних годин?"
        )
      ),
      shapes = list(
        correlation_reference_band(
          acf_data,
          xref = "x",
          yref = "y"
        ),
        correlation_reference_band(
          pacf_data,
          xref = "x2",
          yref = "y2"
        )
      )
    ) |>
    render_book_widget(
      size = "double",
      margin = book_chart_margin("diagnostic_two_panel"),
      showlegend = FALSE
    )
}

plot_return_magnitude_correlations <- function(
  absolute_acf,
  squared_acf
) {
  absolute_plot <- correlation_bar_panel(
    absolute_acf,
    y_title = "ACF модуля дохідності",
    role = "accent"
  )
  squared_plot <- correlation_bar_panel(
    squared_acf,
    y_title = "ACF квадрата дохідності",
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
        "Чи повторюється масштаб годинних рухів у часі?"
      ),
      shapes = list(
        correlation_reference_band(
          absolute_acf,
          xref = "x",
          yref = "y"
        ),
        correlation_reference_band(
          squared_acf,
          xref = "x2",
          yref = "y2"
        )
      )
    ) |>
    render_book_widget(
      size = "double",
      margin = book_chart_margin("diagnostic_two_panel"),
      showlegend = FALSE
    )
}

calendar_value_matrix <- function(data, column) {
  result <- matrix(
    NA_real_,
    nrow = 7L,
    ncol = 24L,
    dimnames = list(
      c("Пн", "Вт", "Ср", "Чт", "Пт", "Сб", "Нд"),
      as.character(0:23)
    )
  )
  for (index in seq_len(nrow(data))) {
    result[
      data$weekday[[index]],
      data$hour[[index]] + 1L
    ] <- data[[column]][[index]]
  }
  if (anyNA(result) || any(!is.finite(result))) {
    stop("Календарна матриця містить пропущені значення.")
  }

  result
}

plot_calendar_return_patterns <- function(data) {
  require_exploration_columns(
    data,
    c(
      "weekday",
      "weekday_label",
      "hour",
      "mean_return_percent",
      "median_absolute_return_percent"
    ),
    "календарного графіка"
  )
  palette <- book_plot_palette("dark")
  hours <- 0:23
  weekdays <- c("Пн", "Вт", "Ср", "Чт", "Пт", "Сб", "Нд")
  mean_matrix <- calendar_value_matrix(
    data,
    "mean_return_percent"
  )
  magnitude_matrix <- calendar_value_matrix(
    data,
    "median_absolute_return_percent"
  )
  mean_limit <- max(abs(mean_matrix))
  if (!is.finite(mean_limit) || mean_limit <= 0) {
    mean_limit <- 1e-6
  }

  mean_plot <- plotly::plot_ly() |>
    plotly::add_trace(
      x = hours,
      y = weekdays,
      z = mean_matrix,
      type = "heatmap",
      zmin = -mean_limit,
      zmax = mean_limit,
      zmid = 0,
      colorscale = list(
        c(0, palette[["negative"]]),
        c(0.5, palette[["neutral"]]),
        c(1, palette[["positive"]])
      ),
      colorbar = list(
        title = list(text = "Середня, %"),
        thickness = 14,
        y = 0.78,
        len = 0.4
      ),
      hovertemplate = paste0(
        "%{y}, %{x}:00 UTC",
        "<br>Середня дохідність: %{z:.4f}%",
        "<extra></extra>"
      ),
      inherit = FALSE
    ) |>
    plotly::layout(
      xaxis = modifyList(
        book_axis_style(NULL),
        list(
          showticklabels = FALSE,
          dtick = 2
        )
      ),
      yaxis = modifyList(
        book_axis_style("День тижня"),
        list(autorange = "reversed")
      )
    )

  magnitude_plot <- plotly::plot_ly() |>
    plotly::add_trace(
      x = hours,
      y = weekdays,
      z = magnitude_matrix,
      type = "heatmap",
      colorscale = list(
        c(0, palette[["neutral"]]),
        c(0.55, palette[["accent"]]),
        c(1, palette[["negative"]])
      ),
      colorbar = list(
        title = list(text = "Медіанний модуль, %"),
        thickness = 14,
        y = 0.22,
        len = 0.4
      ),
      hovertemplate = paste0(
        "%{y}, %{x}:00 UTC",
        "<br>Медіанний модуль дохідності: %{z:.4f}%",
        "<extra></extra>"
      ),
      inherit = FALSE
    ) |>
    plotly::layout(
      xaxis = modifyList(
        book_axis_style("Година доби, UTC"),
        list(dtick = 2)
      ),
      yaxis = modifyList(
        book_axis_style("День тижня"),
        list(autorange = "reversed")
      )
    )

  plotly::subplot(
    mean_plot,
    magnitude_plot,
    nrows = 2,
    shareX = TRUE,
    heights = c(0.5, 0.5),
    margin = 0.08,
    titleX = TRUE,
    titleY = TRUE
  ) |>
    plotly::layout(
      title = book_plot_title(
        "Чи змінюються знак і масштаб рухів за календарем?"
      )
    ) |>
    render_book_widget(
      size = "double",
      margin = book_chart_margin("diagnostic_two_panel"),
      showlegend = FALSE
    )
}

plot_lagged_feature_groups <- function(data) {
  require_exploration_columns(
    data,
    c(
      "feature_id",
      "feature",
      "feature_unit",
      "group",
      "observations",
      "median_feature_value",
      "median_next_absolute_return_percent"
    ),
    "графіка лагованих ознак"
  )
  palette <- book_plot_palette("dark")
  roles <- c(
    previous_absolute_return = "actual",
    intrahour_range = "accent",
    volume = "price",
    turnover = "negative"
  )
  feature_ids <- unique(data$feature_id)
  unknown_ids <- setdiff(feature_ids, names(roles))
  if (length(unknown_ids) > 0L) {
    stop(
      "Невідомі лаговані ознаки: ",
      paste(unknown_ids, collapse = ", ")
    )
  }

  widget <- plotly::plot_ly()
  for (feature_id in feature_ids) {
    selected <- data[
      data$feature_id == feature_id,
      ,
      drop = FALSE
    ]
    role <- roles[[feature_id]]
    customdata <- cbind(
      selected$median_feature_value,
      selected$observations
    )
    widget <- plotly::add_trace(
      widget,
      x = selected$group,
      y = selected$median_next_absolute_return_percent,
      customdata = customdata,
      type = "scatter",
      mode = "lines+markers",
      name = selected$feature[[1L]],
      line = list(
        color = palette[[role]],
        width = 2
      ),
      marker = list(
        color = palette[[role]],
        size = 7
      ),
      meta = book_trace_meta(role),
      hovertemplate = paste0(
        "Група: %{x}",
        "<br>Медіана ознаки: %{customdata[0]:.4f}",
        " ",
        selected$feature_unit[[1L]],
        "<br>Медіанний модуль наступної дохідності: %{y:.4f}%",
        "<br>Годин у групі: %{customdata[1]:.0f}",
        "<extra>%{fullData.name}</extra>"
      ),
      inherit = FALSE
    )
  }

  widget |>
    plotly::layout(
      title = book_plot_title(
        paste(
          "Що видно після групування",
          "попередніх ринкових ознак?"
        )
      ),
      xaxis = modifyList(
        book_axis_style(
          "Група ознаки: від найменших до найбільших значень"
        ),
        list(
          tickmode = "linear",
          dtick = 1
        )
      ),
      yaxis = book_axis_style(
        "Медіанний модуль дохідності наступної години, %"
      )
    ) |>
    render_book_widget(
      size = "standard",
      margin = book_chart_margin("diagnostic"),
      showlegend = TRUE
    )
}
