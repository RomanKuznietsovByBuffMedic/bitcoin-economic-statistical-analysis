# Cross-exchange comparison -----------------------------------------------
#
# Bybit and Bitstamp are compared only at common hourly timestamps.
# Prices from one exchange are never inserted into the other exchange.

compare_hourly_exchanges <- function(
  primary,
  reference,
  primary_name = "Bybit",
  reference_name = "Bitstamp"
) {
  required_columns <- c("open_time", "close")
  missing_primary <- setdiff(required_columns, names(primary))
  missing_reference <- setdiff(required_columns, names(reference))

  if (length(missing_primary) > 0L) {
    stop(
      "В основному наборі бракує полів: ",
      paste(missing_primary, collapse = ", ")
    )
  }
  if (length(missing_reference) > 0L) {
    stop(
      "У контрольному наборі бракує полів: ",
      paste(missing_reference, collapse = ", ")
    )
  }

  primary_prices <- primary |>
    dplyr::select(open_time, close) |>
    dplyr::rename(primary_close = close)

  reference_prices <- reference |>
    dplyr::select(open_time, close) |>
    dplyr::rename(reference_close = close)

  common <- primary_prices |>
    dplyr::inner_join(reference_prices, by = "open_time") |>
    dplyr::arrange(open_time) |>
    dplyr::mutate(
      hours_from_previous = as.numeric(
        difftime(
          open_time,
          dplyr::lag(open_time),
          units = "hours"
        )
      ),
      price_difference_percent =
        100 * (primary_close / reference_close - 1),
      primary_log_return = dplyr::if_else(
        hours_from_previous == 1,
        log(primary_close / dplyr::lag(primary_close)),
        NA_real_
      ),
      reference_log_return = dplyr::if_else(
        hours_from_previous == 1,
        log(reference_close / dplyr::lag(reference_close)),
        NA_real_
      )
    )

  if (nrow(common) == 0L) {
    stop("Основний і контрольний ринки не мають спільних годин.")
  }

  valid_returns <- common |>
    dplyr::filter(
      !is.na(primary_log_return),
      !is.na(reference_log_return)
    )

  if (nrow(valid_returns) < 2L) {
    stop("Недостатньо спільних суміжних дохідностей для порівняння.")
  }

  summary <- tibble::tibble(
    `Перевірка` = c(
      "Спільні годинні ціни",
      "Спільні суміжні дохідності",
      "Медіанна абсолютна різниця цін, %",
      "95-й процентиль абсолютної різниці цін, %",
      "Максимальна абсолютна різниця цін, %",
      "Кореляція логарифмічних дохідностей"
    ),
    `Значення` = c(
      nrow(common),
      nrow(valid_returns),
      stats::median(abs(common$price_difference_percent)),
      as.numeric(stats::quantile(
        abs(common$price_difference_percent),
        probs = 0.95,
        names = FALSE
      )),
      max(abs(common$price_difference_percent)),
      stats::cor(
        valid_returns$primary_log_return,
        valid_returns$reference_log_return
      )
    )
  )

  names(common)[names(common) == "primary_close"] <-
    paste0(tolower(primary_name), "_close")
  names(common)[names(common) == "reference_close"] <-
    paste0(tolower(reference_name), "_close")

  list(data = common, summary = summary)
}
