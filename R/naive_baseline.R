# Hourly naive forecast and common evaluation helpers --------------------

build_naive_hourly_forecasts <- function(
  hourly_data,
  evaluation_start,
  evaluation_end_exclusive
) {
  data <- prepare_hourly_model_frame(hourly_data)
  indices <- hourly_validation_indices(
    data,
    evaluation_start,
    evaluation_end_exclusive
  )

  previous_indices <- indices - 1L

  forecasts <- data.frame(
    target_time = data$open_time[indices],
    forecast_origin = data$open_time[previous_indices],
    execution_time = data$open_time[indices],
    exit_time = data$open_time[indices + 1L],
    previous_close = data$close[previous_indices],
    actual_close = data$close[indices],
    actual_log_return = data$log_return_1h[indices],
    naive_log_return_forecast = 0,
    naive_price_forecast = data$close[previous_indices],
    execution_open = data$open[indices],
    exit_open = data$open[indices + 1L],
    asset_return = data$next_open_return_1h[indices],
    stringsAsFactors = FALSE
  )

  if (
    anyNA(forecasts) ||
      any(!is.finite(forecasts$actual_log_return)) ||
      any(!is.finite(forecasts$asset_return))
  ) {
    stop("Наївний прогноз містить NA або нескінченні значення.")
  }

  forecasts
}

forecast_metrics_for <- function(
  forecasts,
  method,
  return_forecast,
  price_forecast,
  directional_accuracy = NA_real_
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

naive_forecast_metrics <- function(forecasts) {
  forecast_metrics_for(
    forecasts = forecasts,
    method = "Наївний прогноз",
    return_forecast = forecasts$naive_log_return_forecast,
    price_forecast = forecasts$naive_price_forecast
  )
}

backtest_long_cash <- function(
  forecasts,
  positions,
  strategy_id,
  strategy_label,
  starting_capital,
  cost_rate,
  periods_per_year = 365 * 24
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
    stop("Позиція має дорівнювати 0 або 1 для кожної години.")
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
  annualized_volatility <- return_standard_deviation *
    sqrt(periods_per_year)
  annualized_sharpe <- if (
    is.na(return_standard_deviation) ||
      return_standard_deviation == 0
  ) {
    NA_real_
  } else {
    sqrt(periods_per_year) *
      mean(period_returns) / return_standard_deviation
  }
  gross_return <- prod(
    1 + positions * forecasts$asset_return
  ) - 1
  period_count <- length(positions)

  summary <- data.frame(
    strategy_id = strategy_id,
    strategy = strategy_label,
    starting_capital = starting_capital,
    final_capital = wealth,
    gross_return = gross_return,
    net_return = wealth / starting_capital - 1,
    annualized_return =
      (wealth / starting_capital)^(periods_per_year / period_count) - 1,
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
