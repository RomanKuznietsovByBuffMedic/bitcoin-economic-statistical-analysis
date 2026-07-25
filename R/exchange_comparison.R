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

hourly_return_frame <- function(data, source_id) {
  if (
    !is.data.frame(data) ||
      !all(c("open_time", "close") %in% names(data)) ||
      nrow(data) < 2L
  ) {
    stop(
      "Для перевірки екстремальних доходностей джерело ",
      source_id,
      " повинно містити open_time і close."
    )
  }
  ordered <- data[
    order(data$open_time),
    c("open_time", "close"),
    drop = FALSE
  ]
  if (
    !inherits(ordered$open_time, "POSIXt") ||
      anyNA(ordered$open_time) ||
      anyDuplicated(ordered$open_time) ||
      anyNA(ordered$close) ||
      any(!is.finite(ordered$close)) ||
      any(ordered$close <= 0)
  ) {
    stop("Некоректні часові мітки або ціни в джерелі ", source_id, ".")
  }

  consecutive <- c(
    FALSE,
    diff(as.numeric(ordered$open_time)) == 60 * 60
  )
  returns <- c(
    NA_real_,
    diff(log(ordered$close))
  )
  returns[!consecutive] <- NA_real_
  result <- data.frame(
    open_time = ordered$open_time,
    value = returns,
    stringsAsFactors = FALSE
  )
  names(result)[[2L]] <- source_id
  result
}

compare_extreme_hourly_returns <- function(
  sources,
  primary_id,
  start_time,
  end_time,
  top_n = 10L
) {
  if (
    !is.list(sources) ||
      length(sources) < 2L ||
      is.null(names(sources)) ||
      any(!nzchar(names(sources))) ||
      !primary_id %in% names(sources)
  ) {
    stop("sources має бути іменованим списком щонайменше двох джерел.")
  }
  top_n <- as.integer(top_n)
  if (length(top_n) != 1L || is.na(top_n) || top_n < 1L) {
    stop("top_n має бути додатним цілим числом.")
  }

  frames <- lapply(
    names(sources),
    function(source_id) {
      hourly_return_frame(
        sources[[source_id]],
        source_id
      )
    }
  )
  common <- Reduce(
    function(left, right) {
      merge(
        left,
        right,
        by = "open_time",
        all = FALSE,
        sort = TRUE
      )
    },
    frames
  )
  common <- common[
    common$open_time >= start_time &
      common$open_time < end_time,
    ,
    drop = FALSE
  ]
  complete <- stats::complete.cases(common)
  common <- common[complete, , drop = FALSE]
  if (nrow(common) < top_n) {
    stop("Недостатньо спільних доходностей для перевірки екстремумів.")
  }

  primary_return <- common[[primary_id]]
  extreme_indices <- utils::head(
    order(abs(primary_return), decreasing = TRUE),
    top_n
  )
  controls <- setdiff(names(sources), primary_id)
  rows <- lapply(
    controls,
    function(control_id) {
      differences <- 100 * (
        primary_return - common[[control_id]]
      )
      data.frame(
        source = control_id,
        common_return_hours = nrow(common),
        return_correlation = stats::cor(
          primary_return,
          common[[control_id]]
        ),
        median_absolute_difference_pp =
          stats::median(abs(differences)),
        maximum_absolute_difference_pp =
          max(abs(differences)),
        same_sign_among_primary_extremes = sum(
          sign(primary_return[extreme_indices]) ==
            sign(common[[control_id]][extreme_indices])
        ),
        checked_extremes = top_n,
        stringsAsFactors = FALSE
      )
    }
  )

  list(
    common = common,
    summary = do.call(rbind, rows),
    extremes = common[
      extreme_indices,
      ,
      drop = FALSE
    ]
  )
}
