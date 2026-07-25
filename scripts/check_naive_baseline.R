#!/usr/bin/env Rscript

source("R/project_config.R")
source("R/project_io.R")
source("R/book_charts.R")
source("R/model_data.R")
source("R/naive_baseline.R")
source("R/naive_baseline_charts.R")

config <- read_project_config()
prepared <- read_rds_required(
  config$paths$prepared,
  "підготовлений набір ціни й дохідностей"
)

example_start <- config$study$data_start + 24 * 60 * 60
example_end <- example_start + 14 * 24 * 60 * 60
forecasts <- build_naive_hourly_forecasts(
  hourly_data = prepared,
  evaluation_start = example_start,
  evaluation_end_exclusive = example_end
)
expected_hours <- as.integer(
  difftime(
    example_end,
    example_start,
    units = "hours"
  )
)
if (
  nrow(forecasts) != expected_hours ||
    any(forecasts$target_time >=
      config$evaluation$validation_start) ||
    any(forecasts$naive_log_return_forecast != 0) ||
    any(
      forecasts$naive_price_forecast !=
        forecasts$previous_close
    )
) {
  stop("Train-only контракт наївного прогнозу порушено.")
}

changed_future <- prepared
future_rows <- changed_future$open_time >=
  config$evaluation$validation_start
changed_future$close[future_rows] <-
  changed_future$close[future_rows] * 100
future_forecasts <- build_naive_hourly_forecasts(
  hourly_data = changed_future,
  evaluation_start = example_start,
  evaluation_end_exclusive = example_end
)
if (!identical(forecasts, future_forecasts)) {
  stop("Майбутні дані вплинули на train-ілюстрацію baseline.")
}

chart_data <- prepare_naive_chart_data(forecasts)
time_split_data <- build_model_time_split_chart_data(config)
time_split_chart <- plot_model_time_split(time_split_data)

relabeled_time_split <- time_split_data
relabeled_time_split$part <- c(
  "Початкове навчання",
  "Внутрішня перевірка",
  "Фінальна оцінка"
)
relabeled_time_split_chart <- plot_model_time_split(
  relabeled_time_split
)

trace_colours <- function(chart) {
  vapply(
    chart$x$data,
    function(trace) as.character(trace$line$color),
    character(1)
  )
}
trace_roles <- function(chart) {
  vapply(
    chart$x$data,
    function(trace) as.character(trace$meta$book_role),
    character(1)
  )
}
expected_colours <- unname(c(
  book_plot_palette("dark")[["neutral"]],
  book_plot_palette("dark")[["ar1"]],
  book_plot_palette("dark")[["negative"]]
))
expected_roles <- c("neutral", "ar1", "negative")
if (
  !identical(unname(trace_colours(time_split_chart)), expected_colours) ||
    !identical(
      unname(trace_colours(relabeled_time_split_chart)),
      expected_colours
    ) ||
    !identical(unname(trace_roles(time_split_chart)), expected_roles) ||
    !identical(
      unname(trace_roles(relabeled_time_split_chart)),
      expected_roles
    )
) {
  stop("Колір часового періоду залежить від текстового підпису.")
}

invalid_time_split <- time_split_data
invalid_time_split$role[[2L]] <- "holdout"
invalid_result <- try(
  plot_model_time_split(invalid_time_split),
  silent = TRUE
)
if (!inherits(invalid_result, "try-error")) {
  stop("Невідома машинна роль часового періоду не була відхилена.")
}

charts <- list(
  time_split_chart,
  plot_naive_price_forecast(chart_data)
)
if (!all(vapply(charts, inherits, logical(1), "plotly"))) {
  stop("Не всі графіки baseline є інтерактивними Plotly.")
}

trace_types <- unlist(
  lapply(charts, function(chart) {
    vapply(
      chart$x$data,
      function(trace) {
        if (is.null(trace$type)) "scatter" else trace$type
      },
      character(1)
    )
  }),
  use.names = FALSE
)
if (any(trace_types == "scattergl")) {
  stop("Графіки baseline не повинні залежати від WebGL.")
}

cat("\nКОНТРАКТ НАЇВНОГО ПРОГНОЗУ\n")
cat("Train-ілюстрація:", nrow(forecasts), "годин\n")
cat("Validation і test не використано.\n")
cat("Перевірку наївного правила пройдено.\n")
