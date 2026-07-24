# AR(1) development experiment ------------------------------------------
#
# The final test year stays sealed. This module aggregates the complete
# hourly series to UTC days, evaluates a fixed AR(1) model on the final year
# of the training sample, and compares it with simple forecast and trading
# benchmarks.

require_model_columns <- function(data, columns, description) {
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

aggregate_hourly_to_daily <- function(
  hourly_data,
  timezone = "UTC"
) {
  required_columns <- c(
    "open_time",
    "open",
    "high",
    "low",
    "close",
    "volume"
  )
  require_model_columns(
    hourly_data,
    required_columns,
    "денного агрегування"
  )

  if (!identical(timezone, "UTC")) {
    stop("Поточне денне агрегування підтримує лише UTC.")
  }

  data <- hourly_data[
    order(hourly_data$open_time),
    ,
    drop = FALSE
  ]
  if (
    !inherits(data$open_time, "POSIXt") ||
      anyNA(data$open_time) ||
      any(duplicated(data$open_time))
  ) {
    stop(
      "open_time має містити впорядковані унікальні моменти POSIXct."
    )
  }

  time_steps <- diff(as.numeric(data$open_time))
  if (length(time_steps) > 0L && any(time_steps != 60 * 60)) {
    stop(
      paste(
        "Для денного моделювання потрібен повний",
        "безперервний годинний ряд."
      )
    )
  }

  numeric_columns <- c("open", "high", "low", "close", "volume")
  invalid_numeric <- vapply(
    numeric_columns,
    function(column) {
      values <- data[[column]]
      anyNA(values) || any(!is.finite(values))
    },
    logical(1)
  )
  if (any(invalid_numeric)) {
    stop(
      "Некоректні числові поля: ",
      paste(names(invalid_numeric)[invalid_numeric], collapse = ", ")
    )
  }
  if (
    any(data$open <= 0) ||
      any(data$high <= 0) ||
      any(data$low <= 0) ||
      any(data$close <= 0) ||
      any(data$volume < 0)
  ) {
    stop("OHLC мають бути додатними, а обсяг не може бути від'ємним.")
  }

  day_labels <- format(
    data$open_time,
    "%Y-%m-%d",
    tz = timezone
  )
  row_groups <- split(
    seq_len(nrow(data)),
    day_labels,
    drop = TRUE
  )
  day_counts <- lengths(row_groups)
  if (any(day_counts != 24L)) {
    bad_days <- names(day_counts)[day_counts != 24L]
    stop(
      "Неповні UTC-дні в основному ряді: ",
      paste(utils::head(bad_days, 5L), collapse = ", ")
    )
  }

  complete_day <- vapply(
    row_groups,
    function(indices) {
      identical(
        format(
          data$open_time[indices],
          "%H",
          tz = timezone
        ),
        sprintf("%02d", 0:23)
      )
    },
    logical(1)
  )
  if (any(!complete_day)) {
    stop("Щонайменше один UTC-день не містить годин від 00 до 23.")
  }

  first_value <- function(column) {
    vapply(
      row_groups,
      function(indices) data[[column]][indices[[1L]]],
      numeric(1)
    )
  }
  last_value <- function(column) {
    vapply(
      row_groups,
      function(indices) data[[column]][indices[[length(indices)]]],
      numeric(1)
    )
  }
  aggregate_value <- function(column, function_name) {
    aggregate_function <- match.fun(function_name)
    vapply(
      row_groups,
      function(indices) {
        aggregate_function(data[[column]][indices])
      },
      numeric(1)
    )
  }

  day_start <- as.POSIXct(
    paste(names(row_groups), "00:00:00"),
    format = "%Y-%m-%d %H:%M:%S",
    tz = timezone
  )
  daily <- data.frame(
    day_start = day_start,
    open = first_value("open"),
    high = aggregate_value("high", "max"),
    low = aggregate_value("low", "min"),
    close = last_value("close"),
    volume = aggregate_value("volume", "sum"),
    hours = as.integer(day_counts),
    stringsAsFactors = FALSE
  )

  if ("turnover" %in% names(data)) {
    if (
      anyNA(data$turnover) ||
        any(!is.finite(data$turnover)) ||
        any(data$turnover < 0)
    ) {
      stop("Поле turnover має містити невід'ємні скінченні числа.")
    }
    daily$turnover <- aggregate_value("turnover", "sum")
  }

  daily$log_return_1d <- c(
    NA_real_,
    diff(log(daily$close))
  )
  daily$simple_return_1d <- exp(daily$log_return_1d) - 1
  daily$next_open_return_1d <- c(
    daily$open[-1L] /
      daily$open[-nrow(daily)] - 1,
    NA_real_
  )

  daily
}

fit_ar1_ols <- function(returns, minimum_pairs = 30L) {
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
      " пар спостережень."
    )
  }

  response <- returns[-1L]
  lagged_return <- returns[-length(returns)]
  model <- stats::lm(response ~ lagged_return)
  model_summary <- summary(model)
  coefficients <- model_summary$coefficients
  residuals <- stats::residuals(model)
  ljung_box_lag <- min(10L, max(2L, floor(length(residuals) / 5L)))
  ljung_box <- stats::Box.test(
    residuals,
    lag = ljung_box_lag,
    type = "Ljung-Box",
    fitdf = 1L
  )

  list(
    model = model,
    intercept = unname(coefficients["(Intercept)", "Estimate"]),
    phi = unname(coefficients["lagged_return", "Estimate"]),
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
  daily_data,
  validation_start,
  validation_end_exclusive,
  refit_every_months = 1L
) {
  require_model_columns(
    daily_data,
    c(
      "day_start",
      "open",
      "close",
      "log_return_1d",
      "next_open_return_1d"
    ),
    "walk-forward оцінювання"
  )

  refit_every_months <- as.integer(refit_every_months)
  if (
    length(refit_every_months) != 1L ||
      is.na(refit_every_months) ||
      refit_every_months < 1L
  ) {
    stop("refit_every_months має бути додатним цілим числом.")
  }

  target_indices <- which(
    daily_data$day_start >= validation_start &
      daily_data$day_start < validation_end_exclusive
  )
  if (length(target_indices) == 0L) {
    stop("У validation-періоді немає денних спостережень.")
  }
  if (
    min(target_indices) <= 2L ||
      max(target_indices) >= nrow(daily_data)
  ) {
    stop(
      paste(
        "Для кожного validation-прогнозу потрібні",
        "попередні дані та наступна ціна відкриття."
      )
    )
  }

  forecast_rows <- vector("list", length(target_indices))
  refit_rows <- list()
  current_fit <- NULL
  current_refit_id <- 0L
  last_refit_month <- NA_integer_

  for (row_number in seq_along(target_indices)) {
    target_index <- target_indices[[row_number]]
    target_time <- daily_data$day_start[[target_index]]
    target_month <- month_serial(target_time)
    needs_refit <- is.null(current_fit) ||
      target_month - last_refit_month >= refit_every_months

    if (needs_refit) {
      history <- daily_data$log_return_1d[
        seq_len(target_index - 1L)
      ]
      history <- history[!is.na(history)]
      current_fit <- fit_ar1_ols(history)
      current_refit_id <- current_refit_id + 1L
      last_refit_month <- target_month

      refit_rows[[current_refit_id]] <- data.frame(
        refit_id = current_refit_id,
        refit_time = target_time,
        training_end = daily_data$day_start[[target_index - 1L]],
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

    previous_return <- daily_data$log_return_1d[[
      target_index - 1L
    ]]
    ar1_return_forecast <- forecast_ar1_one_step(
      current_fit,
      previous_return
    )
    previous_close <- daily_data$close[[target_index - 1L]]

    forecast_rows[[row_number]] <- data.frame(
      target_time = target_time,
      forecast_origin = daily_data$day_start[[
        target_index - 1L
      ]],
      execution_time = target_time,
      exit_time = daily_data$day_start[[target_index + 1L]],
      refit_id = current_refit_id,
      previous_close = previous_close,
      actual_close = daily_data$close[[target_index]],
      actual_log_return = daily_data$log_return_1d[[
        target_index
      ]],
      ar1_log_return_forecast = ar1_return_forecast,
      naive_log_return_forecast = 0,
      ar1_price_forecast =
        previous_close * exp(ar1_return_forecast),
      naive_price_forecast = previous_close,
      execution_open = daily_data$open[[target_index]],
      exit_open = daily_data$open[[target_index + 1L]],
      asset_return = daily_data$next_open_return_1d[[
        target_index
      ]],
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

  list(
    forecasts = forecasts,
    refits = refits
  )
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

  metrics_for <- function(
    method,
    return_forecast,
    price_forecast,
    directional_accuracy
  ) {
    return_error <- return_forecast - forecasts$actual_log_return
    price_error <- price_forecast - forecasts$actual_close

    data.frame(
      method = method,
      return_mae = mean(abs(return_error)),
      return_rmse = sqrt(mean(return_error^2)),
      price_mae = mean(abs(price_error)),
      price_rmse = sqrt(mean(price_error^2)),
      directional_accuracy = directional_accuracy,
      stringsAsFactors = FALSE
    )
  }

  ar1_direction <- mean(
    (forecasts$ar1_log_return_forecast > 0) ==
      (forecasts$actual_log_return > 0)
  )

  rbind(
    metrics_for(
      method = "AR(1)",
      return_forecast = forecasts$ar1_log_return_forecast,
      price_forecast = forecasts$ar1_price_forecast,
      directional_accuracy = ar1_direction
    ),
    metrics_for(
      method = "Поточна ціна",
      return_forecast = forecasts$naive_log_return_forecast,
      price_forecast = forecasts$naive_price_forecast,
      directional_accuracy = NA_real_
    )
  )
}

backtest_long_cash <- function(
  forecasts,
  positions,
  strategy_id,
  strategy_label,
  starting_capital,
  cost_rate
) {
  require_model_columns(
    forecasts,
    c("execution_time", "exit_time", "asset_return"),
    "перевірки торгового правила"
  )

  positions <- as.numeric(positions)
  if (
    length(positions) != nrow(forecasts) ||
      anyNA(positions) ||
      any(!positions %in% c(0, 1))
  ) {
    stop("Позиція має дорівнювати 0 або 1 для кожного прогнозу.")
  }
  if (
    length(starting_capital) != 1L ||
      is.na(starting_capital) ||
      !is.finite(starting_capital) ||
      starting_capital <= 0
  ) {
    stop("Початковий капітал має бути додатним числом.")
  }
  if (
    length(cost_rate) != 1L ||
      is.na(cost_rate) ||
      !is.finite(cost_rate) ||
      cost_rate < 0 ||
      cost_rate >= 1
  ) {
    stop("Ставка витрат має належати проміжку від 0 до 1.")
  }

  previous_positions <- c(0, utils::head(positions, -1L))
  opening_turnover <- abs(positions - previous_positions)
  terminal_turnover <- abs(utils::tail(positions, 1L))
  wealth <- starting_capital
  wealth_values <- numeric(length(positions) + 1L)
  wealth_values[[1L]] <- wealth
  period_returns <- numeric(length(positions))
  paid_costs <- numeric(length(positions) + 1L)

  for (index in seq_along(positions)) {
    starting_wealth <- wealth
    paid_costs[[index]] <-
      starting_wealth * cost_rate * opening_turnover[[index]]
    wealth_after_trade <- starting_wealth - paid_costs[[index]]
    wealth <- wealth_after_trade *
      (1 + positions[[index]] * forecasts$asset_return[[index]])
    period_returns[[index]] <- wealth / starting_wealth - 1
    wealth_values[[index + 1L]] <- wealth
  }

  terminal_cost <- wealth * cost_rate * terminal_turnover
  paid_costs[[length(paid_costs)]] <- terminal_cost
  if (terminal_cost > 0) {
    wealth <- wealth - terminal_cost
    wealth_values[[length(wealth_values)]] <- wealth
    previous_wealth <- wealth_values[[length(wealth_values) - 1L]]
    period_returns[[length(period_returns)]] <-
      wealth / previous_wealth - 1
  }

  running_maximum <- cummax(wealth_values)
  drawdown <- wealth_values / running_maximum - 1
  return_standard_deviation <- stats::sd(period_returns)
  annualized_volatility <- return_standard_deviation * sqrt(365)
  annualized_sharpe <- if (
    is.na(return_standard_deviation) ||
      return_standard_deviation == 0
  ) {
    NA_real_
  } else {
    sqrt(365) * mean(period_returns) / return_standard_deviation
  }
  gross_return <- prod(
    1 + positions * forecasts$asset_return
  ) - 1
  validation_days <- length(positions)

  summary <- data.frame(
    strategy_id = strategy_id,
    strategy = strategy_label,
    starting_capital = starting_capital,
    final_capital = wealth,
    gross_return = gross_return,
    net_return = wealth / starting_capital - 1,
    annualized_return =
      (wealth / starting_capital)^(365 / validation_days) - 1,
    annualized_volatility = annualized_volatility,
    sharpe_zero_rate = annualized_sharpe,
    maximum_drawdown = min(drawdown),
    time_in_market = mean(positions),
    turnover = sum(opening_turnover) + terminal_turnover,
    orders = sum(opening_turnover > 0) +
      as.integer(terminal_turnover > 0),
    paid_costs = sum(paid_costs),
    stringsAsFactors = FALSE
  )
  path <- data.frame(
    time = c(
      forecasts$execution_time[[1L]],
      forecasts$exit_time
    ),
    strategy_id = strategy_id,
    strategy = strategy_label,
    wealth = wealth_values,
    stringsAsFactors = FALSE
  )

  list(
    summary = summary,
    path = path,
    positions = positions,
    period_returns = period_returns,
    paid_costs = paid_costs
  )
}

run_ar1_development_experiment <- function(hourly_data, config) {
  if (
    !identical(config$model$family, "ar") ||
      !identical(config$model$order, 1L) ||
      !identical(config$model$target_interval, "1d")
  ) {
    stop("Поточний експеримент зафіксовано як денний AR(1).")
  }
  if (
    !identical(config$evaluation$forecast_horizon_periods, 1L) ||
      !identical(config$evaluation$execution_lag_periods, 1L) ||
      !identical(config$evaluation$execution_price, "next_open") ||
      !identical(config$evaluation$training_window, "expanding")
  ) {
    stop("Параметри оцінювання не відповідають пілоту AR(1).")
  }

  daily <- aggregate_hourly_to_daily(
    hourly_data,
    timezone = config$study$timezone
  )
  validation_start <- config$evaluation$validation_start
  validation_end <- config$evaluation$validation_end_exclusive
  test_start <- config$evaluation$test_start

  required_boundaries <- c(validation_start, validation_end, test_start)
  boundary_present <- vapply(
    required_boundaries,
    function(boundary) {
      any(as.numeric(daily$day_start) == as.numeric(boundary))
    },
    logical(1)
  )
  if (any(!boundary_present)) {
    stop("Денні дані не містять усіх меж validation і test.")
  }

  walk_forward <- walk_forward_ar1(
    daily_data = daily,
    validation_start = validation_start,
    validation_end_exclusive = validation_end,
    refit_every_months = config$evaluation$refit_every_months
  )
  forecasts <- walk_forward$forecasts
  expected_validation_days <- as.integer(
    difftime(validation_end, validation_start, units = "days")
  )
  if (nrow(forecasts) != expected_validation_days) {
    stop(
      "Кількість validation-прогнозів не відповідає часовим межам."
    )
  }
  if (
    any(forecasts$target_time >= test_start) ||
      any(forecasts$exit_time > test_start) ||
      max(forecasts$exit_time) != test_start
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
        starting_capital =
          config$trading$starting_capital_quote,
        cost_rate = config$trading$total_cost_rate
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
    forecast_metrics$method == "Поточна ціна",
    ,
    drop = FALSE
  ]

  list(
    daily = daily,
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
      start = validation_start,
      end_exclusive = validation_end,
      days = expected_validation_days
    ),
    sealed_test = list(
      start = test_start,
      end_exclusive = config$evaluation$test_end_exclusive,
      used = FALSE
    )
  )
}
