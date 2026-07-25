#!/usr/bin/env Rscript

# Small deterministic contract checks ------------------------------------
#
# These checks use only synthetic data. They do not read project RDS files,
# contact exchanges or choose a statistical model.

source("R/project_config.R")
source("R/project_io.R")
source("R/hourly_ohlc_quality.R")
source("R/price_returns.R")
source("R/time_split.R")
source("R/naive_benchmark.R")

expect_error <- function(expression, pattern) {
  error <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = identity
  )
  if (
    is.null(error) ||
      !grepl(pattern, conditionMessage(error), ignore.case = TRUE)
  ) {
    stop("Не отримано очікуваної помилки: ", pattern)
  }
  invisible(error)
}

utc <- function(value) {
  as.POSIXct(value, tz = "UTC")
}

# Three explicit, complete and non-overlapping time blocks.
split_times <- seq(
  utc("2024-01-01 00:00:00"),
  by = "hour",
  length.out = 6L
)
split_data <- data.frame(
  open_time = split_times,
  value = seq_along(split_times)
)
split <- split_time_series(
  data = split_data,
  exploration_start = split_times[[1L]],
  validation_start = split_times[[3L]],
  test_start = split_times[[5L]],
  test_end_exclusive = split_times[[6L]] + 60 * 60
)
stopifnot(
  nrow(split$exploration) == 2L,
  nrow(split$validation) == 2L,
  nrow(split$test) == 2L,
  identical(
    split$data$sample_role,
    rep(c("exploration", "validation", "test"), each = 2L)
  )
)

# Forecast roles are assigned by target_time, not forecast_origin.
forecast_index <- build_one_step_forecast_index(
  data = split_data,
  exploration_start = split_times[[1L]],
  validation_start = split_times[[3L]],
  test_start = split_times[[5L]],
  test_end_exclusive = split_times[[6L]] + 60 * 60
)
stopifnot(
  nrow(forecast_index$index) == 5L,
  identical(
    as.numeric(forecast_index$index$forecast_origin),
    as.numeric(split_times[1:5])
  ),
  identical(
    as.numeric(forecast_index$index$target_time),
    as.numeric(split_times[2:6])
  ),
  identical(
    forecast_index$index$sample_role,
    c(
      "exploration",
      "validation",
      "validation",
      "test",
      "test"
    )
  ),
  identical(
    as.integer(
      forecast_index$summary$`Прогнозних випадків`
    ),
    c(1L, 2L, 2L)
  )
)

# Exact duplicate candles are reported and collapsed; conflicts stop.
candle_times <- split_times[1:3]
candles <- data.frame(
  open_time = candle_times,
  open = c(10, 11, 12),
  high = c(11, 12, 13),
  low = c(9, 10, 11),
  close = c(10.5, 11.5, 12.5),
  volume = c(1, 2, 3)
)
exact_duplicate <- rbind(candles, candles[2L, , drop = FALSE])
duplicate_check <- validate_hourly_ohlc(
  exact_duplicate,
  start_time = candle_times[[1L]],
  end_time = candle_times[[3L]] + 60 * 60
)
duplicate_count <- duplicate_check$summary$Значення[
  duplicate_check$summary$Перевірка == "Повторені години"
]
stopifnot(
  nrow(duplicate_check$data) == 3L,
  identical(as.numeric(duplicate_count), 1)
)

conflict <- candles[2L, , drop = FALSE]
conflict$close <- 11.25
expect_error(
  validate_hourly_ohlc(
    rbind(candles, conflict),
    start_time = candle_times[[1L]],
    end_time = candle_times[[3L]] + 60 * 60
  ),
  "суперечлив"
)

# A return is defined only across adjacent hours.
gap_prices <- data.frame(
  open_time = split_times[c(1L, 2L, 4L)],
  close = c(100, 110, 121)
)
gap_returns <- build_price_return_features(gap_prices)
stopifnot(
  is.na(gap_returns$log_return_1h[[1L]]),
  isTRUE(all.equal(
    gap_returns$log_return_1h[[2L]],
    log(1.1)
  )),
  is.na(gap_returns$log_return_1h[[3L]])
)

# Candidate forecasts must use exactly the benchmark target-time keys.
forecast_times <- seq(
  utc("2024-02-01 00:00:00"),
  by = "hour",
  length.out = 7L
)
prices <- data.frame(
  open_time = forecast_times,
  close = c(100, 101, 100, 102, 104, 103, 105)
)
benchmark <- build_naive_point_benchmark(
  hourly_data = prices,
  start_time = forecast_times[[2L]],
  end_exclusive = forecast_times[[7L]]
)
candidate <- data.frame(
  target_time = benchmark$target_time,
  forecast_log_return = rep(0, nrow(benchmark))
)
comparison <- compare_return_forecast_to_naive(
  benchmark = benchmark,
  candidate_forecast = candidate,
  candidate_label = "Синтетичний кандидат"
)
stopifnot(
  nrow(comparison) == 2L,
  all(comparison$observations == nrow(benchmark))
)

expect_error(
  compare_return_forecast_to_naive(
    benchmark,
    candidate[nrow(candidate):1L, , drop = FALSE],
    "Переставлений кандидат"
  ),
  "ті самі target_time"
)
expect_error(
  compare_return_forecast_to_naive(
    benchmark,
    candidate[-1L, , drop = FALSE],
    "Неповний кандидат"
  ),
  "ті самі target_time"
)
duplicate_keys <- candidate
duplicate_keys$target_time[[2L]] <- duplicate_keys$target_time[[1L]]
expect_error(
  compare_return_forecast_to_naive(
    benchmark,
    duplicate_keys,
    "Кандидат із повтором"
  ),
  "унікальні моменти"
)

cat("Детерміновані контракти пройдено.\n")
