# Hourly naive forecast and common evaluation helpers --------------------

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

prepare_hourly_model_frame <- function(hourly_data) {
  require_model_columns(
    hourly_data,
    c("open_time", "open", "close", "log_return_1h"),
    "годинного моделювання"
  )

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
    stop("open_time має містити унікальні моменти POSIXct без NA.")
  }

  time_steps <- diff(as.numeric(data$open_time))
  if (length(time_steps) > 0L && any(time_steps != 60 * 60)) {
    stop("Для моделі 1h потрібен безперервний годинний ряд.")
  }

  numeric_columns <- c("open", "close", "log_return_1h")
  invalid_numeric <- vapply(
    numeric_columns,
    function(column) {
      values <- data[[column]]
      any(!is.na(values) & !is.finite(values))
    },
    logical(1)
  )
  if (any(invalid_numeric)) {
    stop(
      "Некоректні числові поля: ",
      paste(names(invalid_numeric)[invalid_numeric], collapse = ", ")
    )
  }
  if (any(data$open <= 0) || any(data$close <= 0)) {
    stop("Ціни відкриття і закриття мають бути додатними.")
  }

  data$next_open_return_1h <- c(
    data$open[-1L] / data$open[-nrow(data)] - 1,
    NA_real_
  )

  data
}

hourly_validation_indices <- function(
  hourly_data,
  validation_start,
  validation_end_exclusive
) {
  indices <- which(
    hourly_data$open_time >= validation_start &
      hourly_data$open_time < validation_end_exclusive
  )

  if (length(indices) == 0L) {
    stop("У validation-періоді немає годинних спостережень.")
  }
  if (min(indices) <= 2L || max(indices) >= nrow(hourly_data)) {
    stop(
      paste(
        "Для кожного прогнозу потрібна попередня година",
        "і наступна ціна відкриття."
      )
    )
  }

  indices
}

build_naive_hourly_forecasts <- function(
  hourly_data,
  validation_start,
  validation_end_exclusive
) {
  data <- prepare_hourly_model_frame(hourly_data)
  indices <- hourly_validation_indices(
    data,
    validation_start,
    validation_end_exclusive
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

run_naive_baseline_experiment <- function(hourly_data, config) {
  if (!identical(config$model$target_interval, "1h")) {
    stop("Наївний прогноз має використовувати інтервал 1h.")
  }

  forecasts <- build_naive_hourly_forecasts(
    hourly_data = hourly_data,
    validation_start = config$evaluation$validation_start,
    validation_end_exclusive =
      config$evaluation$validation_end_exclusive
  )

  expected_hours <- as.integer(
    difftime(
      config$evaluation$validation_end_exclusive,
      config$evaluation$validation_start,
      units = "hours"
    )
  )
  if (nrow(forecasts) != expected_hours) {
    stop("Кількість годинних прогнозів не відповідає часовим межам.")
  }
  if (
    any(forecasts$target_time >= config$evaluation$test_start) ||
      any(forecasts$exit_time > config$evaluation$test_start) ||
      max(forecasts$exit_time) != config$evaluation$test_start
  ) {
    stop("Фінальний test-період випадково використано в розробці.")
  }

  list(
    hourly = prepare_hourly_model_frame(hourly_data),
    forecasts = forecasts,
    forecast_metrics = naive_forecast_metrics(forecasts),
    validation = list(
      start = config$evaluation$validation_start,
      end_exclusive = config$evaluation$validation_end_exclusive,
      hours = expected_hours
    ),
    sealed_test = list(
      start = config$evaluation$test_start,
      end_exclusive = config$evaluation$test_end_exclusive,
      used = FALSE
    )
  )
}
