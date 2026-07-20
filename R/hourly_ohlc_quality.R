# Generic hourly OHLC quality checks --------------------------------------

sha256_file <- function(path) {
  executable <- Sys.which("sha256sum")
  if (!nzchar(executable)) {
    stop("Для контрольної суми потрібна системна команда sha256sum.")
  }
  output <- system2(executable, path, stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    stop("Не вдалося обчислити SHA-256 для файлу: ", path)
  }
  strsplit(trimws(output[[1]]), "[[:space:]]+")[[1]][[1]]
}

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
