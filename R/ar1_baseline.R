# Hourly AR(1) development experiment ------------------------------------
#
# The final test year stays sealed. The model uses one-hour log returns,
# produces one-hour-ahead forecasts, and is compared with the fixed naive
# forecast from R/naive_baseline.R.

fit_ar1_ols <- function(returns, minimum_pairs = 168L) {
  returns <- as.numeric(returns)
  if (length(returns) == 0L || anyNA(returns)) {
    stop("AR(1) потребує непорожнього ряду без NA.")
  }
  if (any(!is.finite(returns))) {
    stop("Дохідності для AR(1) мають бути скінченними.")
  }
  if (length(returns) - 1L < minimum_pairs) {
    stop(
      "Для AR(1) потрібно щонайменше ",
      minimum_pairs,
      " пар годинних спостережень."
    )
  }

  response <- returns[-1L]
  lagged_return <- returns[-length(returns)]
  model <- stats::lm(response ~ lagged_return)
  model_summary <- summary(model)
  coefficients <- model_summary$coefficients
  residuals <- stats::residuals(model)
  phi <- unname(
    coefficients["lagged_return", "Estimate"]
  )
  if (!is.finite(phi) || abs(phi) >= 1) {
    stop(
      paste(
        "Оцінена AR(1) не задовольняє умову стаціонарності",
        "|phi1| < 1."
      )
    )
  }
  ljung_box_lag <- min(24L, max(2L, floor(length(residuals) / 10L)))
  ljung_box <- stats::Box.test(
    residuals,
    lag = ljung_box_lag,
    type = "Ljung-Box",
    fitdf = 1L
  )

  list(
    model = model,
    intercept = unname(coefficients["(Intercept)", "Estimate"]),
    phi = phi,
    intercept_standard_error = unname(
      coefficients["(Intercept)", "Std. Error"]
    ),
    phi_standard_error = unname(
      coefficients["lagged_return", "Std. Error"]
    ),
    intercept_p_value = unname(
      coefficients["(Intercept)", "Pr(>|t|)"]
    ),
    phi_p_value = unname(
      coefficients["lagged_return", "Pr(>|t|)"]
    ),
    r_squared = unname(model_summary$r.squared),
    residual_ljung_box_lag = ljung_box_lag,
    residual_ljung_box_statistic = unname(ljung_box$statistic),
    residual_ljung_box_p_value = unname(ljung_box$p.value),
    n_pairs = length(response)
  )
}

forecast_ar1_one_step <- function(model_fit, previous_return) {
  previous_return <- as.numeric(previous_return)
  if (
    length(previous_return) != 1L ||
      is.na(previous_return) ||
      !is.finite(previous_return)
  ) {
    stop("Попередня дохідність має бути одним скінченним числом.")
  }

  model_fit$intercept + model_fit$phi * previous_return
}

month_serial <- function(value) {
  as.integer(format(value, "%Y", tz = "UTC")) * 12L +
    as.integer(format(value, "%m", tz = "UTC"))
}

walk_forward_ar1 <- function(
  hourly_data,
  validation_start,
  validation_end_exclusive,
  refit_every_months = 1L
) {
  data <- prepare_hourly_model_frame(hourly_data)
  target_indices <- hourly_validation_indices(
    data,
    validation_start,
    validation_end_exclusive
  )

  refit_every_months <- as.integer(refit_every_months)
  if (
    length(refit_every_months) != 1L ||
      is.na(refit_every_months) ||
      refit_every_months < 1L
  ) {
    stop("refit_every_months має бути додатним цілим числом.")
  }

  forecast_rows <- vector("list", length(target_indices))
  refit_rows <- list()
  current_fit <- NULL
  current_refit_id <- 0L
  last_refit_month <- NA_integer_

  for (row_number in seq_along(target_indices)) {
    target_index <- target_indices[[row_number]]
    target_time <- data$open_time[[target_index]]
    target_month <- month_serial(target_time)
    needs_refit <- is.null(current_fit) ||
      target_month - last_refit_month >= refit_every_months

    if (needs_refit) {
      history <- data$log_return_1h[seq_len(target_index - 1L)]
      history <- history[!is.na(history)]
      current_fit <- fit_ar1_ols(history)
      current_refit_id <- current_refit_id + 1L
      last_refit_month <- target_month

      refit_rows[[current_refit_id]] <- data.frame(
        refit_id = current_refit_id,
        refit_time = target_time,
        training_end = data$open_time[[target_index - 1L]],
        n_pairs = current_fit$n_pairs,
        intercept = current_fit$intercept,
        phi = current_fit$phi,
        intercept_standard_error =
          current_fit$intercept_standard_error,
        phi_standard_error = current_fit$phi_standard_error,
        intercept_p_value = current_fit$intercept_p_value,
        phi_p_value = current_fit$phi_p_value,
        r_squared = current_fit$r_squared,
        residual_ljung_box_lag =
          current_fit$residual_ljung_box_lag,
        residual_ljung_box_statistic =
          current_fit$residual_ljung_box_statistic,
        residual_ljung_box_p_value =
          current_fit$residual_ljung_box_p_value,
        stringsAsFactors = FALSE
      )
    }

    previous_return <- data$log_return_1h[[target_index - 1L]]
    ar1_return_forecast <- forecast_ar1_one_step(
      current_fit,
      previous_return
    )
    previous_close <- data$close[[target_index - 1L]]

    forecast_rows[[row_number]] <- data.frame(
      target_time = target_time,
      forecast_origin = data$open_time[[target_index - 1L]],
      execution_time = target_time,
      exit_time = data$open_time[[target_index + 1L]],
      refit_id = current_refit_id,
      previous_close = previous_close,
      actual_close = data$close[[target_index]],
      actual_log_return = data$log_return_1h[[target_index]],
      ar1_log_return_forecast = ar1_return_forecast,
      naive_log_return_forecast = 0,
      ar1_price_forecast = previous_close * exp(ar1_return_forecast),
      naive_price_forecast = previous_close,
      execution_open = data$open[[target_index]],
      exit_open = data$open[[target_index + 1L]],
      asset_return = data$next_open_return_1h[[target_index]],
      stringsAsFactors = FALSE
    )
  }

  forecasts <- do.call(rbind, forecast_rows)
  refits <- do.call(rbind, refit_rows)
  rownames(forecasts) <- NULL
  rownames(refits) <- NULL

  if (
    anyNA(forecasts) ||
      any(!is.finite(forecasts$actual_log_return)) ||
      any(!is.finite(forecasts$asset_return))
  ) {
    stop("Walk-forward результат містить NA або нескінченні значення.")
  }

  list(forecasts = forecasts, refits = refits)
}

forecast_accuracy_table <- function(forecasts) {
  require_model_columns(
    forecasts,
    c(
      "actual_log_return",
      "actual_close",
      "ar1_log_return_forecast",
      "naive_log_return_forecast",
      "ar1_price_forecast",
      "naive_price_forecast"
    ),
    "оцінювання прогнозів"
  )

  ar1_direction <- mean(
    (forecasts$ar1_log_return_forecast > 0) ==
      (forecasts$actual_log_return > 0)
  )

  rbind(
    forecast_metrics_for(
      forecasts = forecasts,
      method = "AR(1)",
      return_forecast = forecasts$ar1_log_return_forecast,
      price_forecast = forecasts$ar1_price_forecast,
      directional_accuracy = ar1_direction
    ),
    forecast_metrics_for(
      forecasts = forecasts,
      method = "Наївний прогноз",
      return_forecast = forecasts$naive_log_return_forecast,
      price_forecast = forecasts$naive_price_forecast
    )
  )
}

run_ar1_development_experiment <- function(
  hourly_data,
  config,
  diagnostics
) {
  assert_ar1_selected(diagnostics)

  if (
    !identical(config$model$family, "ar") ||
      !identical(config$model$order, 1L) ||
      !identical(config$model$target_interval, "1h")
  ) {
    stop("Поточний експеримент зафіксовано як годинний AR(1).")
  }
  if (
    !identical(config$evaluation$forecast_horizon_periods, 1L) ||
      !identical(config$evaluation$execution_lag_periods, 1L) ||
      !identical(config$evaluation$execution_price, "next_open") ||
      !identical(config$evaluation$training_window, "expanding")
  ) {
    stop("Параметри оцінювання не відповідають пілоту AR(1).")
  }

  hourly <- prepare_hourly_model_frame(hourly_data)
  walk_forward <- walk_forward_ar1(
    hourly_data = hourly,
    validation_start = config$evaluation$validation_start,
    validation_end_exclusive =
      config$evaluation$validation_end_exclusive,
    refit_every_months = config$evaluation$refit_every_months
  )
  forecasts <- walk_forward$forecasts
  expected_validation_hours <- as.integer(
    difftime(
      config$evaluation$validation_end_exclusive,
      config$evaluation$validation_start,
      units = "hours"
    )
  )
  if (nrow(forecasts) != expected_validation_hours) {
    stop("Кількість годинних прогнозів не відповідає часовим межам.")
  }
  if (
    any(forecasts$target_time >= config$evaluation$test_start) ||
      any(forecasts$exit_time > config$evaluation$test_start) ||
      max(forecasts$exit_time) != config$evaluation$test_start
  ) {
    stop("Фінальний test-період випадково використано в розробці.")
  }

  threshold <- config$trading$signal_threshold_log_return
  ar1_positions <- as.integer(
    forecasts$ar1_log_return_forecast > threshold
  )
  position_sets <- list(
    list(
      id = "ar1_long_cash",
      label = "AR(1): BTC або USDT",
      values = ar1_positions
    ),
    list(
      id = "buy_and_hold",
      label = "Купи й тримай",
      values = rep(1, nrow(forecasts))
    ),
    list(
      id = "cash",
      label = "USDT без торгівлі",
      values = rep(0, nrow(forecasts))
    )
  )
  backtests <- lapply(
    position_sets,
    function(position_set) {
      backtest_long_cash(
        forecasts = forecasts,
        positions = position_set$values,
        strategy_id = position_set$id,
        strategy_label = position_set$label,
        starting_capital = config$trading$starting_capital_quote,
        cost_rate = config$trading$total_cost_rate,
        periods_per_year = 365 * 24
      )
    }
  )

  strategy_summary <- do.call(
    rbind,
    lapply(backtests, `[[`, "summary")
  )
  strategy_paths <- do.call(
    rbind,
    lapply(backtests, `[[`, "path")
  )
  rownames(strategy_summary) <- NULL
  rownames(strategy_paths) <- NULL

  forecast_metrics <- forecast_accuracy_table(forecasts)
  ar1_metrics <- forecast_metrics[
    forecast_metrics$method == "AR(1)",
    ,
    drop = FALSE
  ]
  naive_metrics <- forecast_metrics[
    forecast_metrics$method == "Наївний прогноз",
    ,
    drop = FALSE
  ]

  list(
    hourly = hourly,
    forecasts = forecasts,
    refits = walk_forward$refits,
    forecast_metrics = forecast_metrics,
    return_rmse_improvement =
      1 - ar1_metrics$return_rmse / naive_metrics$return_rmse,
    price_rmse_improvement =
      1 - ar1_metrics$price_rmse / naive_metrics$price_rmse,
    strategies = list(
      summary = strategy_summary,
      paths = strategy_paths,
      details = backtests
    ),
    validation = list(
      start = config$evaluation$validation_start,
      end_exclusive = config$evaluation$validation_end_exclusive,
      hours = expected_validation_hours
    ),
    sealed_test = list(
      start = config$evaluation$test_start,
      end_exclusive = config$evaluation$test_end_exclusive,
      used = FALSE
    )
  )
}
