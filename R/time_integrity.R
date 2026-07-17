# Аналіз часової цілісності локального набору BTCUSDT.
#
# Функції читають уже створені звіти валідації. Інтернет-запити не виконуються.

btc_time_integrity_paths = function(project_root = ".") {
  root = normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )

  validation_dir = file.path(
    root,
    "data",
    "validation",
    "binance",
    "spot",
    "BTCUSDT"
  )

  list(
    validation_dir = validation_dir,
    quality_report = file.path(validation_dir, "quality_report.csv"),
    missing_intervals = file.path(validation_dir, "missing_intervals.csv"),
    incomplete_bars = file.path(validation_dir, "incomplete_bars_summary.csv")
  )
}

btc_time_integrity_read_csv = function(path) {
  if (!file.exists(path)) {
    stop(
      paste0(
        "Не знайдено файл часової перевірки: ",
        path,
        ". Спочатку запустіть scripts/02_validate_and_build_charts.R."
      ),
      call. = FALSE
    )
  }

  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

btc_time_integrity_parse_utc = function(value) {
  if (inherits(value, "POSIXct")) {
    return(as.POSIXct(value, tz = "UTC"))
  }

  text = trimws(as.character(value))
  text = sub(" UTC$", "", text)

  as.POSIXct(
    text,
    tz = "UTC",
    tryFormats = c(
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d %H:%M",
      "%Y-%m-%d"
    )
  )
}

btc_time_integrity_quality_value = function(
  quality_report,
  metric_name,
  default = NA_character_
) {
  if (!all(c("metric", "value") %in% names(quality_report))) {
    return(default)
  }

  selected = quality_report$value[
    quality_report$metric == metric_name
  ]

  if (length(selected) == 0L) {
    return(default)
  }

  selected[[1L]]
}

btc_time_integrity_number = function(value, default = 0) {
  parsed = suppressWarnings(as.numeric(as.character(value)))

  if (length(parsed) != 1L || is.na(parsed) || !is.finite(parsed)) {
    return(default)
  }

  parsed
}

btc_time_integrity_load = function(project_root = ".") {
  paths = btc_time_integrity_paths(project_root)

  quality = btc_time_integrity_read_csv(paths$quality_report)
  gaps = btc_time_integrity_read_csv(paths$missing_intervals)
  intervals = btc_time_integrity_read_csv(paths$incomplete_bars)

  if (nrow(gaps) > 0L) {
    required_gap_columns = c(
      "previous_open_time",
      "next_open_time",
      "missing_minutes"
    )

    if (!all(required_gap_columns %in% names(gaps))) {
      stop(
        paste0(
          "missing_intervals.csv не містить колонок: ",
          paste(
            setdiff(required_gap_columns, names(gaps)),
            collapse = ", "
          ),
          "."
        ),
        call. = FALSE
      )
    }

    gaps$previous_open_time = btc_time_integrity_parse_utc(
      gaps$previous_open_time
    )
    gaps$next_open_time = btc_time_integrity_parse_utc(
      gaps$next_open_time
    )
    gaps$missing_minutes = as.numeric(gaps$missing_minutes)
    gaps$missing_start = gaps$previous_open_time + 60
    gaps$missing_end = gaps$next_open_time - 60
    gaps$duration_hours = gaps$missing_minutes / 60
    gaps$start_year = format(
      gaps$missing_start,
      "%Y",
      tz = "UTC"
    )
    gaps$size_class = cut(
      gaps$missing_minutes,
      breaks = c(-Inf, 1, 5, 60, 1440, Inf),
      labels = c(
        "1 хвилина",
        "2-5 хвилин",
        "6-60 хвилин",
        "61-1440 хвилин",
        "понад 1440 хвилин"
      ),
      right = TRUE
    )
  } else {
    gaps$previous_open_time = as.POSIXct(character(), tz = "UTC")
    gaps$next_open_time = as.POSIXct(character(), tz = "UTC")
    gaps$missing_minutes = numeric()
    gaps$missing_start = as.POSIXct(character(), tz = "UTC")
    gaps$missing_end = as.POSIXct(character(), tz = "UTC")
    gaps$duration_hours = numeric()
    gaps$start_year = character()
    gaps$size_class = factor(character())
  }

  observed_rows = btc_time_integrity_number(
    btc_time_integrity_quality_value(quality, "rows")
  )
  duplicate_rows = btc_time_integrity_number(
    btc_time_integrity_quality_value(
      quality,
      "duplicate_open_times"
    )
  )
  unique_rows = max(0, observed_rows - duplicate_rows)
  missing_minutes = sum(gaps$missing_minutes, na.rm = TRUE)
  expected_rows = unique_rows + missing_minutes
  coverage_percent = if (expected_rows > 0) {
    100 * unique_rows / expected_rows
  } else {
    NA_real_
  }

  first_time = btc_time_integrity_parse_utc(
    btc_time_integrity_quality_value(
      quality,
      "first_open_time"
    )
  )
  last_time = btc_time_integrity_parse_utc(
    btc_time_integrity_quality_value(
      quality,
      "last_open_time"
    )
  )

  largest_gap = if (nrow(gaps) > 0L) {
    max(gaps$missing_minutes, na.rm = TRUE)
  } else {
    0
  }

  summary = data.frame(
    Показник = c(
      "Початок фактичного ряду",
      "Кінець фактичного ряду",
      "Унікальні хвилинні свічки",
      "Очікувані хвилинні свічки",
      "Кількість розривів",
      "Відсутні хвилини",
      "Найбільший розрив, хвилин",
      "Дублікати часу відкриття",
      "Часове покриття, %"
    ),
    Значення = c(
      format(first_time, "%Y-%m-%d %H:%M UTC", tz = "UTC"),
      format(last_time, "%Y-%m-%d %H:%M UTC", tz = "UTC"),
      format(unique_rows, big.mark = " ", scientific = FALSE),
      format(expected_rows, big.mark = " ", scientific = FALSE),
      format(nrow(gaps), big.mark = " ", scientific = FALSE),
      format(missing_minutes, big.mark = " ", scientific = FALSE),
      format(largest_gap, big.mark = " ", scientific = FALSE),
      format(duplicate_rows, big.mark = " ", scientific = FALSE),
      if (is.na(coverage_percent)) {
        "Немає даних"
      } else {
        format(round(coverage_percent, 6), nsmall = 6)
      }
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  if (nrow(gaps) > 0L) {
    order_index = order(
      gaps$missing_minutes,
      decreasing = TRUE
    )
    largest_gaps = gaps[order_index, , drop = FALSE]

    largest_gaps_table = data.frame(
      `Після наявної свічки` = format(
        largest_gaps$previous_open_time,
        "%Y-%m-%d %H:%M UTC",
        tz = "UTC"
      ),
      `Перша наступна свічка` = format(
        largest_gaps$next_open_time,
        "%Y-%m-%d %H:%M UTC",
        tz = "UTC"
      ),
      `Відсутні хвилини` = largest_gaps$missing_minutes,
      `Тривалість, години` = round(
        largest_gaps$duration_hours,
        3
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    yearly_split = split(
      gaps,
      gaps$start_year,
      drop = TRUE
    )

    yearly = do.call(
      rbind,
      lapply(
        names(yearly_split),
        function(year_value) {
          year_data = yearly_split[[year_value]]

          data.frame(
            Рік = year_value,
            Розривів = nrow(year_data),
            `Відсутні хвилини` = sum(
              year_data$missing_minutes,
              na.rm = TRUE
            ),
            `Найбільший розрив, хвилин` = max(
              year_data$missing_minutes,
              na.rm = TRUE
            ),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        }
      )
    )

    yearly = yearly[order(yearly$Рік), , drop = FALSE]
    rownames(yearly) = NULL
  } else {
    largest_gaps_table = data.frame(
      `Після наявної свічки` = character(),
      `Перша наступна свічка` = character(),
      `Відсутні хвилини` = numeric(),
      `Тривалість, години` = numeric(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    yearly = data.frame(
      Рік = character(),
      Розривів = integer(),
      `Відсутні хвилини` = numeric(),
      `Найбільший розрив, хвилин` = numeric(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  interval_table = intervals

  if (nrow(interval_table) > 0L) {
    names(interval_table) = sub(
      "^interval$",
      "Інтервал",
      names(interval_table)
    )
    names(interval_table) = sub(
      "^rows$",
      "Усі свічки",
      names(interval_table)
    )
    names(interval_table) = sub(
      "^complete_rows$",
      "Повні свічки",
      names(interval_table)
    )
    names(interval_table) = sub(
      "^incomplete_rows$",
      "Неповні свічки",
      names(interval_table)
    )
    names(interval_table) = sub(
      "^complete_percent$",
      "Повні свічки, %",
      names(interval_table)
    )
    names(interval_table) = sub(
      "^first_open_time$",
      "Початок",
      names(interval_table)
    )
    names(interval_table) = sub(
      "^last_open_time$",
      "Кінець",
      names(interval_table)
    )
  }

  list(
    paths = paths,
    quality = quality,
    gaps = gaps,
    intervals = intervals,
    summary = summary,
    largest_gaps = largest_gaps_table,
    yearly = yearly,
    interval_table = interval_table,
    coverage_percent = coverage_percent
  )
}

btc_time_integrity_largest_gaps = function(
  integrity,
  n = 10L
) {
  if (nrow(integrity$largest_gaps) == 0L) {
    return(integrity$largest_gaps)
  }

  utils::head(
    integrity$largest_gaps,
    n = as.integer(n)
  )
}

btc_time_integrity_gap_timeline = function(integrity) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Для графіка потрібен пакет plotly.", call. = FALSE)
  }

  gaps = integrity$gaps

  if (nrow(gaps) == 0L) {
    return(
      htmltools::div(
        class = "callout callout-style-default callout-note",
        htmltools::div(
          class = "callout-body-container callout-body",
          htmltools::p("Часових розривів не виявлено.")
        )
      )
    )
  }

  hover_text = paste0(
    "Початок відсутності: ",
    format(gaps$missing_start, "%Y-%m-%d %H:%M UTC", tz = "UTC"),
    "<br>Кінець відсутності: ",
    format(gaps$missing_end, "%Y-%m-%d %H:%M UTC", tz = "UTC"),
    "<br>Відсутні хвилини: ",
    format(gaps$missing_minutes, big.mark = " ", scientific = FALSE)
  )

  figure = plotly::plot_ly(
    data = gaps,
    x = ~missing_start,
    y = ~missing_minutes,
    type = "scatter",
    mode = "markers",
    text = hover_text,
    hoverinfo = "text",
    height = 640,
    source = "btc-time-gaps"
  )

  figure = plotly::layout(
    figure,
    title = list(
      text = "Часові розриви хвилинного ряду BTCUSDT",
      x = 0.5
    ),
    xaxis = list(
      title = "Дата початку розриву",
      rangeslider = list(visible = TRUE)
    ),
    yaxis = list(
      title = "Відсутні хвилини",
      rangemode = "tozero"
    ),
    margin = list(l = 80, r = 30, t = 80, b = 70),
    autosize = TRUE
  )

  plotly::config(
    figure,
    responsive = TRUE,
    displaylogo = FALSE
  )
}

btc_time_integrity_year_plot = function(integrity) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Для графіка потрібен пакет plotly.", call. = FALSE)
  }

  yearly = integrity$yearly

  if (nrow(yearly) == 0L) {
    return(
      htmltools::div(
        class = "callout callout-style-default callout-note",
        htmltools::div(
          class = "callout-body-container callout-body",
          htmltools::p("Немає розривів для розподілу за роками.")
        )
      )
    )
  }

  figure = plotly::plot_ly(
    data = yearly,
    x = ~Рік,
    y = ~`Відсутні хвилини`,
    type = "bar",
    text = ~paste0(
      "Рік: ",
      Рік,
      "<br>Розривів: ",
      Розривів,
      "<br>Відсутні хвилини: ",
      format(
        `Відсутні хвилини`,
        big.mark = " ",
        scientific = FALSE
      )
    ),
    hoverinfo = "text",
    height = 640,
    source = "btc-year-gaps"
  )

  figure = plotly::layout(
    figure,
    title = list(
      text = "Відсутні хвилини за роком початку розриву",
      x = 0.5
    ),
    xaxis = list(title = "Рік"),
    yaxis = list(
      title = "Відсутні хвилини",
      rangemode = "tozero"
    ),
    margin = list(l = 80, r = 30, t = 80, b = 70),
    autosize = TRUE
  )

  plotly::config(
    figure,
    responsive = TRUE,
    displaylogo = FALSE
  )
}
