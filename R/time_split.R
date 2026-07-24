# Chronological training and test split ----------------------------------
#
# Financial time series must remain ordered. The final test period is one
# continuous block after the training period. No row is shuffled.

split_time_series <- function(
  data,
  training_start,
  test_start,
  test_end_exclusive,
  interval_seconds = 60 * 60,
  time_column = "open_time"
) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("Для часового поділу потрібен непорожній data.frame.")
  }
  if (!time_column %in% names(data)) {
    stop("У даних немає часової змінної: ", time_column)
  }
  if (
    any(vapply(
      list(training_start, test_start, test_end_exclusive),
      function(value) {
        length(value) != 1L || is.na(value)
      },
      logical(1)
    )) ||
      training_start >= test_start ||
      test_start >= test_end_exclusive
  ) {
    stop("Некоректні межі навчального й тестового періодів.")
  }
  if (
    length(interval_seconds) != 1L ||
      is.na(interval_seconds) ||
      !is.finite(interval_seconds) ||
      interval_seconds <= 0
  ) {
    stop("interval_seconds має бути додатним числом.")
  }

  time_values <- data[[time_column]]
  if (!inherits(time_values, "POSIXt") || any(is.na(time_values))) {
    stop("Часова змінна має містити коректні POSIXct-значення.")
  }
  if (any(duplicated(time_values))) {
    stop("Перед часовим поділом потрібно усунути повторені моменти.")
  }

  ordered_data <- data[order(time_values), , drop = FALSE]
  ordered_times <- ordered_data[[time_column]]
  in_period <- ordered_times >= training_start &
    ordered_times < test_end_exclusive
  period_data <- ordered_data[in_period, , drop = FALSE]

  if (nrow(period_data) == 0L) {
    stop("У заданих межах немає даних для часового поділу.")
  }

  period_times <- period_data[[time_column]]
  sample_role <- ifelse(
    period_times < test_start,
    "training",
    "test"
  )
  period_data$sample_role <- sample_role

  training <- period_data[sample_role == "training", , drop = FALSE]
  test <- period_data[sample_role == "test", , drop = FALSE]
  if (nrow(training) == 0L || nrow(test) == 0L) {
    stop("Навчальна або тестова частина виявилася порожньою.")
  }
  if (max(training[[time_column]]) >= min(test[[time_column]])) {
    stop("Навчальна й тестова частини перекриваються.")
  }

  expected_test_first <- test_start
  expected_test_last <- test_end_exclusive - interval_seconds
  if (
    !same_instant(min(test[[time_column]]), expected_test_first) ||
      !same_instant(max(test[[time_column]]), expected_test_last)
  ) {
    stop(
      paste(
        "Тестовий набір не має першої або останньої очікуваної",
        "свічки. Перевірте межі та пропуски."
      )
    )
  }

  expected_training <- as.numeric(
    difftime(test_start, training_start, units = "secs")
  ) / interval_seconds
  expected_test <- as.numeric(
    difftime(test_end_exclusive, test_start, units = "secs")
  ) / interval_seconds

  summary <- tibble::tibble(
    `Частина` = c("Навчальна", "Тестова"),
    `Використання` = c(
      "Побудова та внутрішня walk-forward перевірка",
      "Підсумкова оцінка моделі та правила"
    ),
    `Початок, UTC` = c(
      format_utc(training_start),
      format_utc(test_start)
    ),
    `Кінець без включення, UTC` = c(
      format_utc(test_start),
      format_utc(test_end_exclusive)
    ),
    `Очікувано рядків` = c(expected_training, expected_test),
    `Наявні рядки` = c(nrow(training), nrow(test))
  )

  list(
    data = period_data,
    training = training,
    test = test,
    summary = summary
  )
}
