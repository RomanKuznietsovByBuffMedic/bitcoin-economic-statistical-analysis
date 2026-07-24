#!/usr/bin/env Rscript

source("R/project_config.R")
source("R/project_io.R")
source("R/book_charts.R")
source("R/naive_baseline.R")
source("R/ar1_baseline.R")
source("R/ar1_baseline_charts.R")

config <- read_project_config()
prepared <- read_rds_required(
  config$paths$prepared,
  "підготовлений набір ціни й дохідностей"
)
experiment <- run_ar1_development_experiment(
  hourly_data = prepared,
  config = config
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
  nrow(chart_data$forecast_density) !=
    nrow(experiment$forecasts) ||
    nrow(chart_data$refits) != nrow(experiment$refits) ||
    nrow(chart_data$metric_improvement) != 4L ||
    nrow(chart_data$forecast_error_acf) != 48L
) {
  stop("Дані для графічної діагностики мають некоректний розмір.")
}

charts <- list(
  plot_ar1_phi_stability(chart_data$refits),
  plot_ar1_metric_improvement(chart_data$metric_improvement),
  plot_ar1_forecast_density(chart_data$forecast_density),
  plot_ar1_rolling_rmse_difference(
    chart_data$rolling_rmse,
    rolling_window = chart_data$rolling_window
  ),
  plot_ar1_loss_advantage(chart_data$loss_advantage),
  plot_ar1_forecast_error_acf(chart_data$forecast_error_acf),
  plot_strategy_paths(chart_data$strategy_paths),
  plot_strategy_drawdowns(chart_data$strategy_paths),
  plot_ar1_position(chart_data$position_segments)
)
if (!all(vapply(charts, inherits, logical(1), "plotly"))) {
  stop("Не всі графіки AR(1) є інтерактивними Plotly.")
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
