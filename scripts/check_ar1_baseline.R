#!/usr/bin/env Rscript

source("R/project_config.R")
source("R/project_io.R")
source("R/book_charts.R")
source("R/model_data.R")
source("R/model_diagnostics.R")
source("R/naive_baseline.R")
source("R/ar1_baseline.R")
source("R/ar1_baseline_charts.R")

config <- read_project_config()
prepared <- read_rds_required(
  config$paths$prepared,
  "підготовлений набір ціни й дохідностей"
)
diagnostics <- diagnose_ar1_candidate(
  hourly_data = prepared,
  config = config
)
if (!diagnostics$decision$ar1_selected) {
  cat("\nКАНДИДАТ AR(1) НЕ ДОПУЩЕНО\n")
  print(
    diagnostics$readiness$conditions[
      ,
      c("condition", "evidence", "status")
    ],
    row.names = FALSE
  )
  cat(
    "\nValidation і закритий test не використано. ",
    "Блокування AR(1) працює правильно.\n",
    sep = ""
  )
  quit(save = "no", status = 0L)
}

experiment <- run_ar1_development_experiment(
  hourly_data = prepared,
  config = config,
  diagnostics = diagnostics
)

if (isTRUE(experiment$sealed_test$used)) {
  stop("Фінальний test-період не повинен використовуватися.")
}
if (
  any(experiment$forecasts$target_time >=
    experiment$sealed_test$start)
) {
  stop("Validation-прогнози потрапили до фінального test-періоду.")
}
if (
  nrow(experiment$forecasts) != experiment$validation$hours ||
    nrow(experiment$refits) < 1L
) {
  stop("Кількість прогнозів або переоцінювань некоректна.")
}

chart_data <- prepare_ar1_chart_data(
  forecasts = experiment$forecasts,
  refits = experiment$refits,
  forecast_metrics = experiment$forecast_metrics,
  strategy_paths = experiment$strategies$paths,
  signal_threshold = config$trading$signal_threshold_log_return,
  rolling_window = 168L,
  acf_max_lag = 48L
)

if (
  nrow(chart_data$forecast_pairs) != nrow(experiment$forecasts) ||
    nrow(chart_data$forecast_calibration) != 10L ||
    nrow(chart_data$refits) != nrow(experiment$refits) ||
    nrow(chart_data$metric_improvement) != 4L ||
    nrow(chart_data$rolling_rmse) < 1L ||
    nrow(chart_data$forecast_error_acf) != 48L ||
    nrow(chart_data$trade_activity) < 1L
) {
  stop("Дані для графічної діагностики мають некоректний розмір.")
}

charts <- list(
  plot_ar1_phi_stability(chart_data$refits),
  plot_ar1_forecast_calibration(chart_data$forecast_calibration),
  plot_ar1_metric_improvement(chart_data$metric_improvement),
  plot_ar1_rolling_rmse_difference(
    chart_data$rolling_rmse,
    rolling_window = chart_data$rolling_window
  ),
  plot_ar1_forecast_error_acf(chart_data$forecast_error_acf),
  plot_strategy_paths(chart_data$strategy_paths),
  plot_strategy_drawdowns(chart_data$strategy_paths),
  plot_ar1_trade_activity(chart_data$trade_activity)
)
if (!all(vapply(charts, inherits, logical(1), "plotly"))) {
  stop("Не всі графіки AR(1) є інтерактивними Plotly.")
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
  stop("Validation-графіки AR(1) не повинні залежати від WebGL.")
}

cat("\nПЕРША СТАТИСТИЧНА МОДЕЛЬ: AR(1)\n")
cat(
  "Validation:",
  format_utc(experiment$validation$start),
  "-",
  format_utc(experiment$validation$end_exclusive),
  "UTC без включення правої межі\n"
)
cat("Годинних прогнозів:", nrow(experiment$forecasts), "\n")
cat("Переоцінювань моделі:", nrow(experiment$refits), "\n\n")
cat("ЯКІСТЬ ПРОГНОЗУ\n")
print(experiment$forecast_metrics, row.names = FALSE)
cat("\nТОРГОВІ ОРІЄНТИРИ\n")
print(experiment$strategies$summary, row.names = FALSE)
cat("\nПеревірку годинної AR(1) пройдено.\n")
