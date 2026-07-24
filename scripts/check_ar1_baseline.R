#!/usr/bin/env Rscript

# Reproducible check of the first statistical baseline ------------------

source("R/project_config.R")
source("R/project_io.R")
source("R/ar1_baseline.R")

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
  nrow(experiment$forecasts) != experiment$validation$days ||
    nrow(experiment$refits) < 1L
) {
  stop("Кількість прогнозів або переоцінювань некоректна.")
}

cat("\nПЕРША МОДЕЛЬ: AR(1)\n")
cat(
  "Validation:",
  format_utc(experiment$validation$start),
  "-",
  format_utc(experiment$validation$end_exclusive),
  "UTC без включення правої межі\n"
)
cat(
  "Фінальний test залишається закритим:",
  format_utc(experiment$sealed_test$start),
  "-",
  format_utc(experiment$sealed_test$end_exclusive),
  "UTC без включення правої межі\n"
)
cat(
  "Денних прогнозів:",
  nrow(experiment$forecasts),
  "\n"
)
cat(
  "Переоцінювань моделі:",
  nrow(experiment$refits),
  "\n\n"
)

cat("ЯКІСТЬ ПРОГНОЗУ\n")
print(experiment$forecast_metrics, row.names = FALSE)
cat("\nТОРГОВІ ОРІЄНТИРИ\n")
print(experiment$strategies$summary, row.names = FALSE)
cat("\nПеревірку AR(1) пройдено.\n")
