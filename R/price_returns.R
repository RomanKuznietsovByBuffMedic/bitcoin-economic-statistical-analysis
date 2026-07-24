# Price-return data preparation -------------------------------------------
#
# This module is responsible only for preparing price and return variables.
# It does not download data or perform statistical analysis.

build_price_return_features <- function(
  data,
  price_column = "close"
) {
  required_columns <- c("open_time", price_column)
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "Для побудови дохідностей бракує полів: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  if (nrow(data) == 0L) {
    stop("Неможливо побудувати дохідності для порожнього набору.")
  }
  selected_price <- data[[price_column]]
  if (any(!is.finite(selected_price) | selected_price <= 0)) {
    stop("Ціна закриття має бути додатним числом.")
  }

  ordered_data <- data |>
    dplyr::arrange(open_time) |>
    dplyr::mutate(
      price_quote_per_btc = .data[[price_column]],

      hours_from_previous = as.numeric(
        difftime(
          open_time,
          dplyr::lag(open_time),
          units = "hours"
        )
      ),

      simple_return_1h = dplyr::if_else(
        hours_from_previous == 1,
        price_quote_per_btc /
          dplyr::lag(price_quote_per_btc) - 1,
        NA_real_
      ),

      log_return_1h = dplyr::if_else(
        hours_from_previous == 1,
        log(
          price_quote_per_btc /
            dplyr::lag(price_quote_per_btc)
        ),
        NA_real_
      )
    )

  ordered_data
}

maximum_return_conversion_error <- function(data) {
  valid_data <- data |>
    dplyr::filter(
      !is.na(simple_return_1h),
      !is.na(log_return_1h)
    )

  if (nrow(valid_data) == 0L) {
    stop("Немає дохідностей для перевірки перетворення.")
  }

  valid_data |>
    dplyr::summarise(
      maximum_error = max(
        abs(
          simple_return_1h -
            (exp(log_return_1h) - 1)
        )
      )
    ) |>
    dplyr::pull(maximum_error)
}

make_return_examples <- function() {
  tibble::tibble(
    `Звичайна дохідність, %` = c(-10, -5, -1, 1, 5, 10)
  ) |>
    dplyr::mutate(
      `Логарифмічна дохідність, %` =
        100 * log(1 + `Звичайна дохідність, %` / 100)
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ round(.x, 3)
      )
    )
}
