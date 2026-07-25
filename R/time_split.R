# Chronological exploration, validation and test split -------------------
#
# Financial time series remain ordered. Each part is one continuous,
# non-overlapping block, and the final test period stays untouched.

split_time_series <- function(
  data,
  exploration_start,
  validation_start,
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

  boundaries <- list(
    exploration_start,
    validation_start,
    test_start,
    test_end_exclusive
  )
  valid_boundary <- vapply(
    boundaries,
    function(value) {
      inherits(value, "POSIXt") &&
        length(value) == 1L &&
        !is.na(value) &&
        is.finite(as.numeric(value))
    },
    logical(1)
  )
  if (
    !all(valid_boundary) ||
      exploration_start >= validation_start ||
      validation_start >= test_start ||
      test_start >= test_end_exclusive
  ) {
    stop("Некоректні межі дослідницького, validation або test періоду.")
  }
  if (
    length(interval_seconds) != 1L ||
      is.na(interval_seconds) ||
      !is.finite(interval_seconds) ||
      interval_seconds <= 0
  ) {
    stop("interval_seconds має бути додатним числом.")
  }

  boundary_steps <- diff(vapply(boundaries, as.numeric, numeric(1))) /
    interval_seconds
  if (any(boundary_steps != floor(boundary_steps))) {
    stop("Межі часового поділу мають відповідати заданому інтервалу.")
  }

  time_values <- data[[time_column]]
  if (!inherits(time_values, "POSIXt") || anyNA(time_values)) {
    stop("Часова змінна має містити коректні POSIXct-значення.")
  }
  if (anyDuplicated(time_values)) {
    stop("Перед часовим поділом потрібно усунути повторені моменти.")
  }

  ordered_data <- data[order(time_values), , drop = FALSE]
  ordered_times <- ordered_data[[time_column]]
  in_period <- ordered_times >= exploration_start &
    ordered_times < test_end_exclusive
  period_data <- ordered_data[in_period, , drop = FALSE]
  if (nrow(period_data) == 0L) {
    stop("У заданих межах немає даних для часового поділу.")
  }

  period_times <- period_data[[time_column]]
  sample_role <- ifelse(
    period_times < validation_start,
    "exploration",
    ifelse(period_times < test_start, "validation", "test")
  )
  period_data$sample_role <- sample_role

  exploration <- period_data[
    sample_role == "exploration",
    ,
    drop = FALSE
  ]
  validation <- period_data[
    sample_role == "validation",
    ,
    drop = FALSE
  ]
  test <- period_data[sample_role == "test", , drop = FALSE]
  parts <- list(
    exploration = exploration,
    validation = validation,
    test = test
  )
  if (any(vapply(parts, nrow, integer(1)) == 0L)) {
    stop("Одна з трьох частин часового поділу виявилася порожньою.")
  }

  starts <- list(exploration_start, validation_start, test_start)
  ends <- list(validation_start, test_start, test_end_exclusive)
  expected_rows <- vapply(
    seq_along(starts),
    function(index) {
      as.numeric(difftime(
        ends[[index]],
        starts[[index]],
        units = "secs"
      )) / interval_seconds
    },
    numeric(1)
  )
  actual_rows <- vapply(parts, nrow, integer(1))
  first_times <- unname(vapply(
    parts,
    function(part) as.numeric(min(part[[time_column]])),
    numeric(1)
  ))
  last_times <- unname(vapply(
    parts,
    function(part) as.numeric(max(part[[time_column]])),
    numeric(1)
  ))
  expected_first <- vapply(starts, as.numeric, numeric(1))
  expected_last <- vapply(ends, as.numeric, numeric(1)) -
    interval_seconds

  if (
    !identical(as.numeric(actual_rows), expected_rows) ||
      !identical(first_times, expected_first) ||
      !identical(last_times, expected_last)
  ) {
    stop(
      paste(
        "Часовий поділ не має повного погодинного покриття.",
        "Перевірте межі та пропуски."
      )
    )
  }

  summary <- tibble::tibble(
    `Частина` = c(
      "Дослідницька",
      "Внутрішня перевірка",
      "Фінальний тест"
    ),
    `Використання` = c(
      "Опис даних і постановка прогнозного питання",
      "Вибір і налаштування зафіксованого кандидата",
      "Одноразова підсумкова оцінка"
    ),
    `Початок, UTC` = vapply(starts, format_utc, character(1)),
    `Кінець без включення, UTC` = vapply(
      ends,
      format_utc,
      character(1)
    ),
    `Очікувано рядків` = expected_rows,
    `Наявні рядки` = actual_rows
  )

  list(
    data = period_data,
    exploration = exploration,
    validation = validation,
    test = test,
    summary = summary
  )
}
