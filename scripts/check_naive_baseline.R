#!/usr/bin/env Rscript

source("R/project_config.R")
source("R/project_io.R")
source("R/book_charts.R")
source("R/naive_baseline.R")
source("R/naive_baseline_charts.R")

config <- read_project_config()
prepared <- read_rds_required(
  config$paths$prepared,
  "підготовлений набір ціни й дохідностей"
)
experiment <- run_naive_baseline_experiment(
  hourly_data = prepared,
  config = config
)

if (isTRUE(experiment$sealed_test$used)) {
  stop("Фінальний test-період не повинен використовуватися.")
}
if (nrow(experiment$forecasts) != experiment$validation$hours) {
  stop("Кількість годинних наївних прогнозів некоректна.")
}

chart_data <- prepare_naive_chart_data(experiment$forecasts)
time_split_data <- data.frame(
  part = factor(
    c("Навчання", "Перевірка", "Закритий тест"),
    levels = rev(c("Навчання", "Перевірка", "Закритий тест"))
  ),
  start = c(
    config$study$data_start,
    experiment$validation$start,
    experiment$sealed_test$start
  ),
  end = c(
    experiment$validation$start,
    experiment$validation$end_exclusive,
    experiment$sealed_test$end_exclusive
  )
)

charts <- list(
  plot_model_time_split(time_split_data),
  plot_naive_price_forecast(chart_data),
  plot_naive_absolute_error(chart_data),
  plot_naive_return_distribution(chart_data)
)
if (!all(vapply(charts, inherits, logical(1), "plotly"))) {
  stop("Не всі графіки наївного прогнозу є інтерактивними Plotly.")
}

cat("\nНАЇВНИЙ ГОДИННИЙ ПРОГНОЗ\n")
cat("Прогнозів:", nrow(experiment$forecasts), "\n")
cat("Інтервал: 1h\n")
print(experiment$forecast_metrics, row.names = FALSE)
cat("\nПеревірку наївного прогнозу пройдено.\n")
