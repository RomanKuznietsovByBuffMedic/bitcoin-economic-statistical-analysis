# Generic hourly OHLC quality checks --------------------------------------

find_hourly_gaps <- function(data) {
  open_times <- sort(unique(data$open_time))
  if (length(open_times) < 2L) {
    return(tibble::tibble(
      previous_observation = as.POSIXct(character(), tz = "UTC"),
      next_observation = as.POSIXct(character(), tz = "UTC"),
      interval_hours = numeric(),
      missing_hours = numeric()
    ))
  }

  step_hours <- as.numeric(diff(open_times), units = "hours")
  gap_indices <- which(step_hours != 1)

  tibble::tibble(
    previous_observation = open_times[gap_indices],
    next_observation = open_times[gap_indices + 1L],
    interval_hours = step_hours[gap_indices],
    missing_hours = step_hours[gap_indices] - 1
  )
}

validate_hourly_ohlc <- function(data, start_time, end_time) {
  if (
    length(start_time) != 1L ||
      length(end_time) != 1L ||
      is.na(start_time) ||
      is.na(end_time) ||
      start_time >= end_time
  ) {
    stop("Некоректні часові межі для перевірки OHLC.")
  }

  required_columns <- c("open_time", "open", "high", "low", "close")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "Для перевірки OHLC бракує стовпців: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (any(is.na(data$open_time))) {
    stop("Виявлено порожні або некоректні часові позначки.")
  }

  period_data <- data |>
    dplyr::filter(open_time >= start_time, open_time < end_time)

  if (nrow(period_data) == 0L) {
    stop("У заданому періоді немає жодної OHLC-свічки.")
  }

  duplicate_rows <- sum(duplicated(period_data$open_time))
  clean_data <- period_data |>
    dplyr::arrange(open_time) |>
    dplyr::distinct(open_time, .keep_all = TRUE)

  utc_parts <- as.POSIXlt(clean_data$open_time, tz = "UTC")
  misaligned_hours <- sum(
    utc_parts$min != 0 |
      floor(utc_parts$sec) != 0
  )
  if (misaligned_hours > 0) {
    stop("Виявлено свічки, що не починаються на межі години UTC.")
  }

  gaps <- find_hourly_gaps(clean_data)
  invalid_prices <- sum(
    !is.finite(clean_data$open) |
      !is.finite(clean_data$high) |
      !is.finite(clean_data$low) |
      !is.finite(clean_data$close) |
      clean_data$open <= 0 |
      clean_data$high <= 0 |
      clean_data$low <= 0 |
      clean_data$close <= 0
  )
  if (invalid_prices > 0) {
    stop("Виявлено недодатні або нечислові OHLC-ціни.")
  }

  invalid_ohlc_structure <- sum(
    clean_data$high < clean_data$low |
      clean_data$high < clean_data$open |
      clean_data$high < clean_data$close |
      clean_data$low > clean_data$open |
      clean_data$low > clean_data$close
  )
  if (invalid_ohlc_structure > 0) {
    stop("Виявлено порушення логічних співвідношень OHLC.")
  }

  invalid_volume <- 0
  if ("volume" %in% names(clean_data)) {
    invalid_volume <- sum(
      !is.finite(clean_data$volume) |
        clean_data$volume < 0
    )
    if (invalid_volume > 0) {
      stop("Виявлено від'ємний або нечисловий обсяг.")
    }
  }

  expected_rows <- as.numeric(difftime(end_time, start_time, units = "hours"))
  missing_hours <- max(expected_rows - nrow(clean_data), 0)
  completeness <- 100 * nrow(clean_data) / expected_rows

  summary <- tibble::tibble(
    `Перевірка` = c(
      "Очікувана кількість годин",
      "Фактична кількість свічок",
      "Пропущені години",
      "Повнота, %",
      "Повторені години",
      "Часові розриви",
      "Свічки поза межею години UTC",
      "Недодатні або нечислові OHLC-ціни",
      "Порушення співвідношень OHLC",
      "Від'ємний або нечисловий обсяг"
    ),
    `Значення` = c(
      expected_rows,
      nrow(clean_data),
      missing_hours,
      round(completeness, 4),
      duplicate_rows,
      nrow(gaps),
      misaligned_hours,
      invalid_prices,
      invalid_ohlc_structure,
      invalid_volume
    )
  )

  list(data = clean_data, gaps = gaps, summary = summary)
}

require_bounded_hourly_ohlc <- function(
  data,
  start_time,
  end_time,
  source_label = "Ринковий ряд",
  allow_internal_gaps = FALSE
) {
  result <- validate_hourly_ohlc(
    data = data,
    start_time = start_time,
    end_time = end_time
  )

  expected_rows <- as.numeric(
    difftime(end_time, start_time, units = "hours")
  )
  expected_last <- end_time - 60 * 60
  duplicate_rows <- result$summary$Значення[
    result$summary$Перевірка == "Повторені години"
  ]
  boundaries_match <- identical(
    as.numeric(min(result$data$open_time)),
    as.numeric(start_time)
  ) && identical(
    as.numeric(max(result$data$open_time)),
    as.numeric(expected_last)
  )

  if (
    duplicate_rows != 0 ||
      !boundaries_match ||
      (
        !isTRUE(allow_internal_gaps) &&
          (
            nrow(result$data) != expected_rows ||
              nrow(result$gaps) != 0L
          )
      )
  ) {
    stop(
      source_label,
      if (isTRUE(allow_internal_gaps)) {
        paste(
          " не має правильних меж",
          "або містить повторені години."
        )
      } else {
        " не має повного погодинного покриття заданого періоду."
      }
    )
  }

  result
}

require_complete_hourly_ohlc <- function(
  data,
  start_time,
  end_time,
  source_label = "Ринковий ряд"
) {
  require_bounded_hourly_ohlc(
    data = data,
    start_time = start_time,
    end_time = end_time,
    source_label = source_label,
    allow_internal_gaps = FALSE
  )
}
