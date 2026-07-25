# Shared model-data contract ----------------------------------------------
#
# Every statistical model and every pre-model diagnostic uses the same
# ordered, complete hourly frame.  Keeping this contract outside any one
# model prevents the naive benchmark from owning shared validation logic.

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

build_model_time_split_chart_data <- function(
  config,
  labels = c("Train", "Validation", "Закритий test")
) {
  if (
    !is.list(config) ||
      !is.list(config$study) ||
      !is.list(config$evaluation)
  ) {
    stop("Для часового поділу потрібна повна конфігурація проєкту.")
  }

  roles <- c("training", "validation", "test")
  labels <- as.character(labels)
  if (
    length(labels) != length(roles) ||
      anyNA(labels) ||
      any(!nzchar(labels)) ||
      anyDuplicated(labels)
  ) {
    stop("Часовий поділ потребує трьох різних непорожніх підписів.")
  }

  period_times <- list(
    training_start = config$study$data_start,
    validation_start = config$evaluation$validation_start,
    validation_end_exclusive =
      config$evaluation$validation_end_exclusive,
    test_start = config$evaluation$test_start,
    test_end_exclusive = config$evaluation$test_end_exclusive
  )
  valid_times <- vapply(
    period_times,
    function(value) {
      length(value) == 1L &&
        inherits(value, "POSIXt") &&
        !is.na(value)
    },
    logical(1)
  )
  if (!all(valid_times)) {
    stop("Межі train, validation і test мають бути моментами POSIXct.")
  }

  numeric_times <- vapply(period_times, as.numeric, numeric(1))
  if (
    numeric_times[["training_start"]] >=
      numeric_times[["validation_start"]] ||
      numeric_times[["validation_start"]] >=
        numeric_times[["validation_end_exclusive"]] ||
      numeric_times[["validation_end_exclusive"]] !=
        numeric_times[["test_start"]] ||
      numeric_times[["test_start"]] >=
        numeric_times[["test_end_exclusive"]]
  ) {
    stop("Періоди train, validation і test мають бути послідовними.")
  }

  data.frame(
    part = labels,
    role = roles,
    start = c(
      period_times$training_start,
      period_times$validation_start,
      period_times$test_start
    ),
    end = c(
      period_times$validation_start,
      period_times$validation_end_exclusive,
      period_times$test_end_exclusive
    ),
    stringsAsFactors = FALSE
  )
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

training_model_frame <- function(hourly_data, config) {
  data <- prepare_hourly_model_frame(hourly_data)
  training <- data[
    data$open_time >= config$study$data_start &
      data$open_time < config$evaluation$validation_start,
    ,
    drop = FALSE
  ]

  if (nrow(training) == 0L) {
    stop("Початковий навчальний період для вибору моделі порожній.")
  }
  if (
    max(training$open_time) >= config$evaluation$validation_start ||
      any(training$open_time >= config$evaluation$test_start)
  ) {
    stop("Діагностика вибору моделі випадково використала майбутні дані.")
  }

  usable_returns <- !is.na(training$log_return_1h)
  if (!all(usable_returns[-1L])) {
    stop("Усередині навчального періоду є пропущені доходності.")
  }

  training <- training[usable_returns, , drop = FALSE]
  if (
    nrow(training) < 1000L ||
      any(!is.finite(training$log_return_1h))
  ) {
    stop(
      paste(
        "Для діагностики потрібні щонайменше 1000",
        "скінченних навчальних доходностей."
      )
    )
  }

  training$log_price <- log(training$close)
  training
}
