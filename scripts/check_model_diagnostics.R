#!/usr/bin/env Rscript

source("R/project_config.R")
source("R/project_io.R")
source("R/book_charts.R")
source("R/model_data.R")
source("R/model_diagnostics.R")
source("R/model_diagnostics_charts.R")

config <- read_project_config()
prepared <- read_rds_required(
  config$paths$prepared,
  "підготовлений набір ціни й дохідностей"
)
analysis <- analyze_training_time_series(
  hourly_data = prepared,
  config = config
)

if (
  max(analysis$training$open_time) >=
    config$evaluation$validation_start ||
    any(analysis$training$open_time >=
      config$evaluation$test_start)
) {
  stop("Аналіз використав validation або test.")
}
if (
  !identical(
    analysis$order_selection$order,
    0:config$analysis$mean_max_lag
  ) ||
    sum(analysis$order_selection$selected) != 1L
) {
  stop("Таблиця вибору порядку AR має некоректну структуру.")
}
expected_arma_rows <- (
  config$analysis$arma_max_ar + 1L
) * (
  config$analysis$arma_max_ma + 1L
)
if (
  nrow(analysis$arma_selection) != expected_arma_rows ||
    sum(analysis$arma_selection$selected) != 1L ||
    anyNA(analysis$arma_selection$selected)
) {
  stop("Таблиця вибору порядку ARMA має некоректну структуру.")
}
if (
  nrow(analysis$readiness$conditions) != 8L ||
    anyNA(analysis$readiness$conditions$passed)
) {
  stop("Контракт передумов AR(1) неповний.")
}
if (
  nrow(analysis$distribution$qq) !=
    config$analysis$normal_qq_points ||
    anyNA(analysis$distribution$qq) ||
    !is.finite(
      analysis$distribution$shape$excess_kurtosis
    )
) {
  stop("Діагностика розподілу має некоректну структуру.")
}
if (
  nrow(analysis$arch_lm) != 1L ||
    analysis$arch_lm$lag[[1L]] != config$analysis$arch_lag ||
    anyNA(analysis$arch_lm)
) {
  stop("ARCH-LM має некоректну структуру.")
}
if (
  nrow(analysis$calendar$hourly) != 24L ||
    nrow(analysis$calendar$weekday) != 7L ||
    nrow(analysis$calendar$strength) != 2L
) {
  stop("Календарна діагностика має некоректну структуру.")
}
if (
  nrow(analysis$lagged_signals) != 4L ||
    anyNA(
      analysis$lagged_signals$
        spearman_with_next_absolute_return
    )
) {
  stop("Таблиця лагованих ознак має некоректну структуру.")
}
if (
  nrow(analysis$annual_market_scale) < 3L ||
    anyNA(analysis$annual_market_scale) ||
    anyDuplicated(analysis$annual_market_scale$year)
) {
  stop("Річна перевірка масштабу OHLCV має некоректну структуру.")
}
if (
  nrow(analysis$suitability) != 13L ||
    anyDuplicated(analysis$suitability$family) ||
    anyNA(analysis$suitability)
) {
  stop("Карта придатності моделей має некоректну структуру.")
}

changed_future <- prepared
future_rows <- changed_future$open_time >=
  config$evaluation$validation_start
changed_future$log_return_1h[future_rows] <-
  changed_future$log_return_1h[future_rows] * 100
changed_future$close[future_rows] <-
  changed_future$close[future_rows] * 2
changed_future$high[future_rows] <-
  changed_future$high[future_rows] * 2
changed_future$low[future_rows] <-
  changed_future$low[future_rows] * 2
changed_future$volume[future_rows] <-
  changed_future$volume[future_rows] * 50
future_analysis <- analyze_training_time_series(
  hourly_data = changed_future,
  config = config
)
future_invariants <- list(
  ar_order = identical(
    analysis$decision$selected_order,
    future_analysis$decision$selected_order
  ),
  arma_order = identical(
    analysis$decision$selected_arma,
    future_analysis$decision$selected_arma
  ),
  stationarity = isTRUE(all.equal(
    analysis$stationarity$table,
    future_analysis$stationarity$table,
    tolerance = 0
  )),
  distribution = isTRUE(all.equal(
    analysis$distribution,
    future_analysis$distribution,
    tolerance = 0
  )),
  dependence = isTRUE(all.equal(
    analysis$dependence,
    future_analysis$dependence,
    tolerance = 0
  )),
  calendar = isTRUE(all.equal(
    analysis$calendar,
    future_analysis$calendar,
    tolerance = 0
  )),
  lagged_signals = isTRUE(all.equal(
    analysis$lagged_signals,
    future_analysis$lagged_signals,
    tolerance = 0
  )),
  annual_market_scale = isTRUE(all.equal(
    analysis$annual_market_scale,
    future_analysis$annual_market_scale,
    tolerance = 0
  )),
  suitability = identical(
    analysis$suitability,
    future_analysis$suitability
  )
)
if (!all(unlist(future_invariants, use.names = FALSE))) {
  stop(
    "Майбутні дані вплинули на: ",
    paste(
      names(future_invariants)[
        !unlist(future_invariants, use.names = FALSE)
      ],
      collapse = ", "
    )
  )
}

set.seed(20260725)
stationary_example <- as.numeric(stats::arima.sim(
  model = list(ar = 0.6),
  n = 5000L
))
random_walk_example <- cumsum(stats::rnorm(5000L))
stationary_adf <- adf_unit_root_test(stationary_example)
stationary_kpss <- kpss_level_test(stationary_example)
random_walk_adf <- adf_unit_root_test(random_walk_example)
random_walk_kpss <- kpss_level_test(random_walk_example)
synthetic_order <- ar_order_bic_table(
  stationary_example,
  maximum_order = 8L
)

if (
  !stationary_adf$reject_null ||
    stationary_kpss$reject_null ||
    random_walk_adf$reject_null ||
    !random_walk_kpss$reject_null ||
    synthetic_order$order[synthetic_order$selected] != 1L
) {
  stop("Синтетична перевірка ADF, KPSS або вибору AR(1) не пройдена.")
}

marker_messages <- character()
invalid_trace_warnings <- character()
record_marker_message <- function(condition) {
  message_text <- conditionMessage(condition)
  is_marker_message <- grepl(
    "marker object has been specified|Adding markers to the mode",
    message_text,
    ignore.case = TRUE
  )
  if (is_marker_message) {
    marker_messages <<- c(marker_messages, message_text)
    invokeRestart("muffleMessage")
  }
}
record_invalid_trace_warning <- function(condition) {
  warning_text <- conditionMessage(condition)
  is_invalid_trace_warning <- grepl(
    "objects don't have these attributes|Valid attributes include",
    warning_text,
    ignore.case = TRUE
  )
  if (is_invalid_trace_warning) {
    invalid_trace_warnings <<- c(
      invalid_trace_warnings,
      warning_text
    )
    invokeRestart("muffleWarning")
  }
}

charts <- withCallingHandlers(
  list(
    plot_return_normal_qq(
      analysis$distribution$qq
    ),
    plot_training_rolling_moments(
      analysis$rolling_moments,
      config$analysis$rolling_window_hours
    ),
    plot_return_acf_pacf(
      analysis$dependence$acf,
      analysis$dependence$pacf
    ),
    plot_arma_order_selection(
      analysis$arma_selection
    ),
    plot_volatility_dependence(
      analysis$dependence$absolute_acf,
      analysis$dependence$squared_acf
    ),
    plot_calendar_volatility(
      analysis$calendar$hourly,
      analysis$calendar$weekday
    ),
    plot_lagged_volatility_signals(
      analysis$lagged_signals
    )
  ),
  message = record_marker_message,
  warning = record_invalid_trace_warning
)
if (!all(vapply(charts, inherits, logical(1), "plotly"))) {
  stop("Не всі діагностичні графіки є Plotly-віджетами.")
}

built_charts <- withCallingHandlers(
  lapply(charts, plotly::plotly_build),
  message = record_marker_message,
  warning = record_invalid_trace_warning
)
if (length(marker_messages) > 0L) {
  stop(
    paste(
      "Plotly додав markers до лінійного діагностичного trace:",
      paste(unique(marker_messages), collapse = " ")
    )
  )
}
if (length(invalid_trace_warnings) > 0L) {
  stop(
    paste(
      "Plotly отримав атрибути несумісного типу trace:",
      paste(unique(invalid_trace_warnings), collapse = " ")
    )
  )
}

diagnostic_traces <- unlist(
  lapply(built_charts, function(chart) chart$x$data),
  recursive = FALSE
)
is_diagnostic_limit <- vapply(
  diagnostic_traces,
  function(trace) {
    identical(trace$type, "scatter") &&
      identical(trace$hoverinfo, "skip") &&
      identical(trace$line$dash, "dot")
  },
  logical(1)
)
diagnostic_limits <- diagnostic_traces[is_diagnostic_limit]
if (length(diagnostic_limits) != 8L) {
  stop(
    "Очікувалося 8 лінійних меж ACF/PACF, отримано: ",
    length(diagnostic_limits),
    "."
  )
}
limits_are_lines <- vapply(
  diagnostic_limits,
  function(trace) identical(trace$mode, "lines"),
  logical(1)
)
if (!all(limits_are_lines)) {
  stop("Plotly змінив режим лінійних меж ACF/PACF.")
}

qq_traces <- built_charts[[1L]]$x$data
is_qq_reference <- vapply(
  qq_traces,
  function(trace) {
    identical(trace$type, "scatter") &&
      identical(trace$mode, "lines") &&
      identical(trace$hoverinfo, "skip") &&
      identical(trace$line$dash, "dash")
  },
  logical(1)
)
if (sum(is_qq_reference) != 1L) {
  stop(
    "Нормальний Q-Q графік не має рівно однієї ",
    "незалежної еталонної лінії."
  )
}

arma_traces <- built_charts[[4L]]$x$data
arma_trace_types <- vapply(
  arma_traces,
  function(trace) {
    if (is.null(trace$type)) "" else trace$type
  },
  character(1)
)
is_arma_heatmap <- arma_trace_types == "heatmap"
is_arma_choice <- vapply(
  arma_traces,
  function(trace) {
    identical(trace$type, "scatter") &&
      identical(trace$mode, "markers")
  },
  logical(1)
)
if (
  sum(is_arma_heatmap) != 1L ||
    sum(is_arma_choice) != 1L
) {
  stop(
    "Графік ARMA має некоректний склад traces."
  )
}
arma_choice <- arma_traces[[which(is_arma_choice)]]
if (
  !is.null(arma_choice$z) ||
    !is.null(arma_choice$colorscale) ||
    !is.null(arma_choice$colorbar)
) {
  stop(
    "Позначка обраної ARMA-моделі успадкувала ",
    "атрибути heatmap."
  )
}

cat("\nПОВНИЙ АНАЛІЗ ПЕРЕД МОДЕЛЛЮ\n")
cat(
  "Train:",
  format_utc(analysis$training_period$start),
  "-",
  format_utc(analysis$training_period$end_exclusive),
  "UTC без включення правої межі\n"
)
cat("Спостережень:", analysis$training_period$observations, "\n")
cat(
  "BIC обрав AR(",
  analysis$decision$selected_order,
  ") і ARMA(",
  analysis$decision$selected_arma[["ar"]],
  ",",
  analysis$decision$selected_arma[["ma"]],
  ")\n",
  sep = ""
)
cat(
  "ARCH-LM p:",
  format(analysis$arch_lm$p_value[[1L]], digits = 4L),
  "\n"
)
cat(analysis$decision$status, "\n\n")
print(
  analysis$suitability[
    ,
    c("family", "target", "status")
  ],
  row.names = FALSE
)
cat("\nПовний аналіз перед вибором моделі перевірено.\n")
