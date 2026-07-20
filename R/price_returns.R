# Price-return data preparation -------------------------------------------
#
# This module is responsible only for preparing analytical variables.
# It does not download data, fit models, or evaluate forecasts.

build_price_return_features <- function(data) {
  data |>
    dplyr::arrange(open_time) |>
    dplyr::mutate(
      price_quote_per_btc = close,

      hours_from_previous = as.numeric(
        difftime(
          open_time,
          dplyr::lag(open_time),
          units = "hours"
        )
      ),

      hours_to_next = as.numeric(
        difftime(
          dplyr::lead(open_time),
          open_time,
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
      ),

      target_open_time = dplyr::if_else(
        hours_to_next == 1,
        dplyr::lead(open_time),
        as.POSIXct(NA, tz = "UTC")
      ),

      actual_next_close_quote_per_btc = dplyr::if_else(
        hours_to_next == 1,
        dplyr::lead(price_quote_per_btc),
        NA_real_
      ),

      target_simple_return_1h = dplyr::if_else(
        hours_to_next == 1,
        dplyr::lead(price_quote_per_btc) /
          price_quote_per_btc - 1,
        NA_real_
      ),

      target_log_return_1h = dplyr::if_else(
        hours_to_next == 1,
        log(
          dplyr::lead(price_quote_per_btc) /
            price_quote_per_btc
        ),
        NA_real_
      ),

      target_up_1h = dplyr::case_when(
        is.na(target_log_return_1h) ~ NA,
        target_log_return_1h > 0 ~ TRUE,
        TRUE ~ FALSE
      )
    )
}

maximum_return_conversion_error <- function(data) {
  data |>
    dplyr::filter(
      !is.na(simple_return_1h),
      !is.na(log_return_1h)
    ) |>
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
