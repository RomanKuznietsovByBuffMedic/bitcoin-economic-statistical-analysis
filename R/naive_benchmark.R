# Naive one-step benchmark for point forecasts ---------------------------
#
# This module fixes the simplest benchmark before candidate models are
# compared. It prepares aligned forecasts and losses, but never chooses a
# model or assigns a suitability status.

require_naive_benchmark_columns <- function(
  data,
  columns,
  description
) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop(description, " має бути непорожнім data.frame.")
  }

  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "Для ",
      description,
      " бракує полів: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  invisible(columns)
}

build_naive_point_benchmark <- function(
  hourly_data,
  start_time,
  end_exclusive
) {
  require_naive_benchmark_columns(
    hourly_data,
    c("open_time", "close"),
    "наївного прогнозу"
  )

  data <- hourly_data[
    order(hourly_data$open_time),
    c("open_time", "close"),
    drop = FALSE
  ]

  if (
    !inherits(data$open_time, "POSIXt") ||
      anyNA(data$open_time) ||
      anyDuplicated(data$open_time)
  ) {
    stop("open_time має містити унікальні моменти POSIXct без NA.")
  }
  if (
    !is.numeric(data$close) ||
      anyNA(data$close) ||
      any(!is.finite(data$close)) ||
      any(data$close <= 0)
  ) {
    stop("close має містити скінченні додатні ціни без NA.")
  }
  if (
    length(start_time) != 1L ||
      length(end_exclusive) != 1L ||
      !inherits(start_time, "POSIXt") ||
      !inherits(end_exclusive, "POSIXt") ||
      is.na(start_time) ||
      is.na(end_exclusive) ||
      start_time >= end_exclusive
  ) {
    stop("Потрібні коректні послідовні межі прогнозного періоду.")
  }

  hour_steps <- diff(as.numeric(data$open_time))
  if (length(hour_steps) > 0L && any(hour_steps != 60 * 60)) {
    stop("Наївний прогноз потребує безперервного годинного ряду.")
  }

  target_indices <- which(
    data$open_time >= start_time &
      data$open_time < end_exclusive
  )
  if (
    length(target_indices) == 0L ||
      min(target_indices) <= 1L
  ) {
    stop(
      paste(
        "Для прогнозного періоду потрібні цільові години",
        "та попереднє спостереження."
      )
    )
  }

  origin_indices <- target_indices - 1L
  actual_log_return <- log(
    data$close[target_indices] /
      data$close[origin_indices]
  )

  benchmark <- data.frame(
    forecast_origin = data$open_time[origin_indices],
    target_time = data$open_time[target_indices],
    origin_close = data$close[origin_indices],
    actual_close = data$close[target_indices],
    naive_close_forecast = data$close[origin_indices],
    actual_log_return = actual_log_return,
    naive_log_return_forecast = 0,
    stringsAsFactors = FALSE
  )

  if (
    anyNA(benchmark) ||
      any(!is.finite(benchmark$actual_log_return))
  ) {
    stop("Наївний прогноз містить NA або нескінченні значення.")
  }

  benchmark
}

score_return_forecast <- function(
  actual_log_return,
  forecast_log_return,
  method
) {
  actual_log_return <- as.numeric(actual_log_return)
  forecast_log_return <- as.numeric(forecast_log_return)
  method <- as.character(method)

  if (
    length(actual_log_return) == 0L ||
      length(actual_log_return) != length(forecast_log_return) ||
      anyNA(actual_log_return) ||
      anyNA(forecast_log_return) ||
      any(!is.finite(actual_log_return)) ||
      any(!is.finite(forecast_log_return))
  ) {
    stop(
      paste(
        "Факт і прогноз мають бути скінченними векторами",
        "однакової ненульової довжини."
      )
    )
  }
  if (
    length(method) != 1L ||
      is.na(method) ||
      !nzchar(trimws(method))
  ) {
    stop("Назва методу має бути одним непорожнім рядком.")
  }

  error <- actual_log_return - forecast_log_return

  data.frame(
    method = method,
    observations = length(error),
    mae_bps = 10000 * mean(abs(error)),
    rmse_bps = 10000 * sqrt(mean(error^2)),
    stringsAsFactors = FALSE
  )
}

compare_return_forecast_to_naive <- function(
  benchmark,
  candidate_log_return_forecast,
  candidate_label
) {
  require_naive_benchmark_columns(
    benchmark,
    c(
      "actual_log_return",
      "naive_log_return_forecast"
    ),
    "порівняння з наївним прогнозом"
  )

  naive_metrics <- score_return_forecast(
    actual_log_return = benchmark$actual_log_return,
    forecast_log_return =
      benchmark$naive_log_return_forecast,
    method = "Наївний прогноз: r[t+1] = 0"
  )
  candidate_metrics <- score_return_forecast(
    actual_log_return = benchmark$actual_log_return,
    forecast_log_return = candidate_log_return_forecast,
    method = candidate_label
  )

  comparison <- rbind(naive_metrics, candidate_metrics)
  comparison$mae_ratio_to_naive <-
    comparison$mae_bps / naive_metrics$mae_bps
  comparison$rmse_ratio_to_naive <-
    comparison$rmse_bps / naive_metrics$rmse_bps
  rownames(comparison) <- NULL

  comparison
}
