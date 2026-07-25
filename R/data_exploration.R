# Descriptive exploration of hourly market data -------------------------
#
# This module prepares tables and plotting data only. It does not select,
# approve, reject, fit, or rank statistical models.

require_exploration_columns <- function(
  data,
  columns,
  description
) {
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

require_exploration_integer <- function(
  value,
  description,
  minimum = 1L,
  maximum = Inf
) {
  numeric_value <- suppressWarnings(as.numeric(value))
  integer_value <- suppressWarnings(as.integer(value))
  if (
    length(value) != 1L ||
      is.na(numeric_value) ||
      !is.finite(numeric_value) ||
      is.na(integer_value) ||
      numeric_value != integer_value ||
      integer_value < minimum ||
      integer_value > maximum
  ) {
    stop(description, " має бути цілим числом у заданих межах.")
  }

  integer_value
}

require_exploration_time <- function(value, description) {
  if (
    length(value) != 1L ||
      !inherits(value, "POSIXt") ||
      is.na(value)
  ) {
    stop(description, " має бути одним моментом POSIXct.")
  }

  as.POSIXct(value, tz = "UTC")
}

prepare_hourly_exploration_data <- function(
  hourly_data,
  start_time,
  end_exclusive
) {
  required_columns <- c(
    "open_time",
    "open",
    "high",
    "low",
    "close",
    "volume",
    "turnover",
    "log_return_1h"
  )
  require_exploration_columns(
    hourly_data,
    required_columns,
    "дослідницького аналізу"
  )

  start_time <- require_exploration_time(
    start_time,
    "Початок дослідницького періоду"
  )
  end_exclusive <- require_exploration_time(
    end_exclusive,
    "Кінець дослідницького періоду"
  )
  if (start_time >= end_exclusive) {
    stop("Початок дослідницького періоду має передувати кінцю.")
  }

  ordered <- hourly_data[
    order(as.numeric(hourly_data$open_time)),
    ,
    drop = FALSE
  ]
  selected <- ordered[
    ordered$open_time >= start_time &
      ordered$open_time < end_exclusive,
    ,
    drop = FALSE
  ]
  if (nrow(selected) < 1000L) {
    stop(
      paste(
        "Для дослідницьких графіків потрібно щонайменше",
        "1000 годинних свічок."
      )
    )
  }
  if (
    !inherits(selected$open_time, "POSIXt") ||
      anyNA(selected$open_time) ||
      anyDuplicated(selected$open_time)
  ) {
    stop("open_time має містити унікальні моменти POSIXct без NA.")
  }

  time_steps <- diff(as.numeric(selected$open_time))
  if (length(time_steps) > 0L && any(time_steps != 60 * 60)) {
    stop("У дослідницькому періоді порушено годинну часову сітку.")
  }

  market_columns <- c(
    "open",
    "high",
    "low",
    "close",
    "volume",
    "turnover"
  )
  invalid_market_columns <- vapply(
    market_columns,
    function(column) {
      values <- selected[[column]]
      anyNA(values) || any(!is.finite(values))
    },
    logical(1)
  )
  if (any(invalid_market_columns)) {
    stop(
      "Некоректні числові поля: ",
      paste(
        names(invalid_market_columns)[invalid_market_columns],
        collapse = ", "
      )
    )
  }
  if (
    any(selected$open <= 0) ||
      any(selected$high <= 0) ||
      any(selected$low <= 0) ||
      any(selected$close <= 0) ||
      any(selected$volume < 0) ||
      any(selected$turnover < 0)
  ) {
    stop("OHLC мають бути додатними, а обсяг і оборот - невід'ємними.")
  }

  return_values <- selected$log_return_1h
  if (any(!is.na(return_values) & !is.finite(return_values))) {
    stop("log_return_1h містить нескінченне значення.")
  }
  missing_returns <- which(is.na(return_values))
  if (
    length(missing_returns) > 1L ||
      (
        length(missing_returns) == 1L &&
          missing_returns[[1L]] != 1L
      )
  ) {
    stop(
      paste(
        "Усередині дослідницького періоду є пропущені",
        "годинні дохідності."
      )
    )
  }

  selected$intrahour_range_percent <-
    100 * log(selected$high / selected$low)
  selected$hour_utc <- as.integer(format(
    selected$open_time,
    "%H",
    tz = "UTC"
  ))
  selected$weekday_utc <- as.integer(format(
    selected$open_time,
    "%u",
    tz = "UTC"
  ))
  selected$year_utc <- format(
    selected$open_time,
    "%Y",
    tz = "UTC"
  )

  returns <- selected[
    !is.na(selected$log_return_1h),
    ,
    drop = FALSE
  ]
  returns$return_percent <- 100 * returns$log_return_1h
  returns$absolute_return_percent <- abs(returns$return_percent)
  returns$squared_log_return <- returns$log_return_1h^2

  list(
    candles = selected,
    returns = returns,
    period = list(
      start = start_time,
      end_exclusive = end_exclusive,
      last_observation = max(selected$open_time),
      candle_observations = nrow(selected),
      return_observations = nrow(returns),
      missing_returns = length(missing_returns)
    )
  )
}

exploration_period_table <- function(period) {
  data.frame(
    characteristic = c(
      "Початок",
      "Кінець без включення",
      "Остання наявна година",
      "Годинних свічок",
      "Годинних дохідностей",
      "Пропущених дохідностей"
    ),
    value = c(
      format(period$start, "%Y-%m-%d %H:%M UTC", tz = "UTC"),
      format(
        period$end_exclusive,
        "%Y-%m-%d %H:%M UTC",
        tz = "UTC"
      ),
      format(
        period$last_observation,
        "%Y-%m-%d %H:%M UTC",
        tz = "UTC"
      ),
      format(period$candle_observations, big.mark = " "),
      format(period$return_observations, big.mark = " "),
      format(period$missing_returns, big.mark = " ")
    ),
    stringsAsFactors = FALSE
  )
}

return_shape_table <- function(return_percent) {
  return_percent <- as.numeric(return_percent)
  if (
    length(return_percent) < 1000L ||
      anyNA(return_percent) ||
      any(!is.finite(return_percent))
  ) {
    stop("Для опису розподілу потрібні скінченні дохідності без NA.")
  }

  centre <- mean(return_percent)
  centered <- return_percent - centre
  observation_count <- length(return_percent)
  scale <- sqrt(
    sum(centered^2) / (observation_count - 1)
  )
  if (!is.finite(scale) || scale <= 0) {
    stop("Стандартне відхилення дохідності має бути додатним.")
  }

  data.frame(
    characteristic = c(
      "Кількість",
      "Середня",
      "Медіана",
      "Стандартне відхилення",
      "Асиметрія",
      "Ексцес"
    ),
    value = c(
      length(return_percent),
      centre,
      stats::median(return_percent),
      scale,
      sum(centered^3) /
        ((observation_count - 1) * scale^3),
      sum(centered^4) /
        ((observation_count - 1) * scale^4) - 3
    ),
    unit = c(
      "годин",
      "%",
      "%",
      "%",
      "",
      ""
    ),
    stringsAsFactors = FALSE
  )
}

return_quantile_table <- function(return_percent) {
  probabilities <- c(
    0,
    0.001,
    0.01,
    0.05,
    0.25,
    0.5,
    0.75,
    0.95,
    0.99,
    0.999,
    1
  )
  labels <- c(
    "Мінімум",
    "0.1%",
    "1%",
    "5%",
    "25%",
    "Медіана",
    "75%",
    "95%",
    "99%",
    "99.9%",
    "Максимум"
  )

  data.frame(
    quantile = labels,
    probability = probabilities,
    return_percent = as.numeric(stats::quantile(
      return_percent,
      probs = probabilities,
      names = FALSE,
      type = 8
    )),
    stringsAsFactors = FALSE
  )
}

rolling_return_description <- function(
  returns,
  window_hours,
  step_hours
) {
  require_exploration_columns(
    returns,
    c("open_time", "return_percent", "absolute_return_percent"),
    "рухомого опису дохідності"
  )
  window_hours <- require_exploration_integer(
    window_hours,
    "Ширина рухомого вікна",
    minimum = 168L,
    maximum = nrow(returns)
  )
  step_hours <- require_exploration_integer(
    step_hours,
    "Крок рухомого вікна",
    minimum = 24L,
    maximum = window_hours
  )

  end_indices <- seq.int(
    from = window_hours,
    to = nrow(returns),
    by = step_hours
  )
  if (utils::tail(end_indices, 1L) != nrow(returns)) {
    end_indices <- c(end_indices, nrow(returns))
  }

  do.call(
    rbind,
    lapply(
      end_indices,
      function(end_index) {
        start_index <- end_index - window_hours + 1L
        values <- returns$return_percent[
          start_index:end_index
        ]
        data.frame(
          window_start = returns$open_time[[start_index]],
          window_end = returns$open_time[[end_index]],
          mean_return_percent = mean(values),
          standard_deviation_percent = stats::sd(values),
          median_absolute_return_percent =
            stats::median(abs(values)),
          stringsAsFactors = FALSE
        )
      }
    )
  )
}

return_distribution_plot_data <- function(
  return_percent,
  histogram_bins = 120L,
  qq_points = 501L
) {
  histogram_bins <- require_exploration_integer(
    histogram_bins,
    "Кількість інтервалів гістограми",
    minimum = 30L,
    maximum = 400L
  )
  qq_points <- require_exploration_integer(
    qq_points,
    "Кількість точок Q-Q",
    minimum = 101L,
    maximum = length(return_percent)
  )

  value_range <- range(return_percent)
  if (
    any(!is.finite(value_range)) ||
      value_range[[1L]] == value_range[[2L]]
  ) {
    stop("Дохідності не мають придатного діапазону для гістограми.")
  }
  breaks <- seq(
    value_range[[1L]],
    value_range[[2L]],
    length.out = histogram_bins + 1L
  )
  histogram <- graphics::hist(
    return_percent,
    breaks = breaks,
    plot = FALSE,
    include.lowest = TRUE,
    right = TRUE
  )
  histogram_data <- data.frame(
    left = utils::head(histogram$breaks, -1L),
    right = utils::tail(histogram$breaks, -1L),
    midpoint = histogram$mids,
    count = histogram$counts,
    stringsAsFactors = FALSE
  )
  histogram_data <- histogram_data[
    histogram_data$count > 0L,
    ,
    drop = FALSE
  ]

  probabilities <- seq(
    from = 0.001,
    to = 0.999,
    length.out = qq_points
  )
  empirical <- as.numeric(stats::quantile(
    return_percent,
    probs = probabilities,
    names = FALSE,
    type = 8
  ))
  centre <- mean(return_percent)
  scale <- stats::sd(return_percent)
  normal_reference <- centre +
    scale * stats::qnorm(probabilities)

  list(
    histogram = histogram_data,
    qq = data.frame(
      probability = probabilities,
      normal_reference_percent = normal_reference,
      empirical_percent = empirical,
      stringsAsFactors = FALSE
    )
  )
}

single_correlation_view <- function(
  values,
  maximum_lag,
  partial = FALSE
) {
  values <- as.numeric(values)
  if (
    length(values) < 1000L ||
      anyNA(values) ||
      any(!is.finite(values))
  ) {
    stop("Для кореляційного графіка потрібні скінченні значення без NA.")
  }
  maximum_lag <- require_exploration_integer(
    maximum_lag,
    "Максимальний лаг",
    minimum = 1L,
    maximum = floor(length(values) / 5)
  )

  correlation <- if (isTRUE(partial)) {
    stats::pacf(
      values,
      lag.max = maximum_lag,
      plot = FALSE,
      na.action = stats::na.pass
    )$acf
  } else {
    stats::acf(
      values,
      lag.max = maximum_lag,
      plot = FALSE,
      demean = TRUE,
      na.action = stats::na.pass
    )$acf[-1L]
  }
  reference_bound <- 1.96 / sqrt(length(values))

  data.frame(
    lag = seq_len(maximum_lag),
    value = as.numeric(correlation),
    lower_reference = -reference_bound,
    upper_reference = reference_bound,
    stringsAsFactors = FALSE
  )
}

return_correlation_views <- function(
  returns,
  return_maximum_lag,
  magnitude_maximum_lag
) {
  require_exploration_columns(
    returns,
    c(
      "log_return_1h",
      "absolute_return_percent",
      "squared_log_return"
    ),
    "кореляційного опису"
  )

  list(
    return_acf = single_correlation_view(
      returns$log_return_1h,
      maximum_lag = return_maximum_lag
    ),
    return_pacf = single_correlation_view(
      returns$log_return_1h,
      maximum_lag = return_maximum_lag,
      partial = TRUE
    ),
    absolute_return_acf = single_correlation_view(
      returns$absolute_return_percent,
      maximum_lag = magnitude_maximum_lag
    ),
    squared_return_acf = single_correlation_view(
      returns$squared_log_return,
      maximum_lag = magnitude_maximum_lag
    )
  )
}

calendar_return_grid <- function(returns) {
  require_exploration_columns(
    returns,
    c(
      "hour_utc",
      "weekday_utc",
      "return_percent",
      "absolute_return_percent"
    ),
    "календарного опису"
  )
  weekday_labels <- c(
    "Пн",
    "Вт",
    "Ср",
    "Чт",
    "Пт",
    "Сб",
    "Нд"
  )

  rows <- lapply(
    1:7,
    function(weekday) {
      do.call(
        rbind,
        lapply(
          0:23,
          function(hour) {
            selected <-
              returns$weekday_utc == weekday &
              returns$hour_utc == hour
            data.frame(
              weekday = weekday,
              weekday_label = weekday_labels[[weekday]],
              hour = hour,
              observations = sum(selected),
              mean_return_percent =
                mean(returns$return_percent[selected]),
              median_absolute_return_percent =
                stats::median(
                  returns$absolute_return_percent[selected]
                ),
              stringsAsFactors = FALSE
            )
          }
        )
      )
    }
  )
  result <- do.call(rbind, rows)
  if (
    nrow(result) != 7L * 24L ||
      any(result$observations == 0L) ||
      any(!is.finite(result$mean_return_percent)) ||
      any(!is.finite(result$median_absolute_return_percent))
  ) {
    stop("Не вдалося побудувати повну календарну сітку 7 x 24.")
  }

  result
}

equal_frequency_group <- function(values, groups) {
  ranks <- rank(values, ties.method = "average")
  result <- ceiling(groups * ranks / length(values))
  pmin(groups, pmax(1L, as.integer(result)))
}

lagged_feature_groups <- function(
  returns,
  groups = 10L,
  quote_currency
) {
  require_exploration_columns(
    returns,
    c(
      "absolute_return_percent",
      "intrahour_range_percent",
      "volume",
      "turnover"
    ),
    "опису попередніх ринкових ознак"
  )
  groups <- require_exploration_integer(
    groups,
    "Кількість груп лагованих ознак",
    minimum = 5L,
    maximum = 20L
  )
  if (nrow(returns) <= groups * 20L) {
    stop("Для групування лагованих ознак бракує спостережень.")
  }
  quote_currency <- as.character(quote_currency)
  if (
    length(quote_currency) != 1L ||
      is.na(quote_currency) ||
      !nzchar(trimws(quote_currency))
  ) {
    stop("Для обороту потрібна валюта котирування.")
  }

  predictors <- list(
    previous_absolute_return = list(
      label = "Попередній модуль дохідності",
      unit = "%",
      values = returns$absolute_return_percent
    ),
    intrahour_range = list(
      label = paste(
        "Логарифмічний",
        "внутрішньогодинний діапазон"
      ),
      unit = "%",
      values = returns$intrahour_range_percent
    ),
    volume = list(
      label = "Обсяг BTC",
      unit = "BTC",
      values = returns$volume
    ),
    turnover = list(
      label = "Оборот",
      unit = quote_currency,
      values = returns$turnover
    )
  )
  next_absolute_return <-
    returns$absolute_return_percent[-1L]

  do.call(
    rbind,
    lapply(
      names(predictors),
      function(feature_id) {
        feature <- predictors[[feature_id]]
        values <- utils::head(feature$values, -1L)
        group <- equal_frequency_group(values, groups)

        do.call(
          rbind,
          lapply(
            sort(unique(group)),
            function(current_group) {
              selected <- group == current_group
              data.frame(
                feature_id = feature_id,
                feature = feature$label,
                feature_unit = feature$unit,
                group = current_group,
                observations = sum(selected),
                median_feature_value =
                  stats::median(values[selected]),
                median_next_absolute_return_percent =
                  stats::median(next_absolute_return[selected]),
                stringsAsFactors = FALSE
              )
            }
          )
        )
      }
    )
  )
}

annual_market_scale <- function(candles, quote_currency) {
  require_exploration_columns(
    candles,
    c(
      "year_utc",
      "close",
      "volume",
      "turnover",
      "intrahour_range_percent"
    ),
    "річного опису масштабу ринку"
  )
  quote_currency <- as.character(quote_currency)
  if (
    length(quote_currency) != 1L ||
      is.na(quote_currency) ||
      !nzchar(quote_currency)
  ) {
    stop("Потрібна валюта котирування для річного опису.")
  }

  metrics <- list(
    close = list(
      label = "Медіанна ціна закриття",
      unit = paste0(quote_currency, " за BTC"),
      values = candles$close
    ),
    volume = list(
      label = "Медіанний обсяг",
      unit = "BTC за годину",
      values = candles$volume
    ),
    turnover = list(
      label = "Медіанний оборот",
      unit = paste0(quote_currency, " за годину"),
      values = candles$turnover
    ),
    intrahour_range = list(
      label = "Медіанний логарифмічний діапазон",
      unit = "%",
      values = candles$intrahour_range_percent
    )
  )
  years <- sort(unique(candles$year_utc))

  result <- do.call(
    rbind,
    lapply(
      names(metrics),
      function(metric_id) {
        metric <- metrics[[metric_id]]
        rows <- do.call(
          rbind,
          lapply(
            years,
            function(year) {
              selected <- candles$year_utc == year
              data.frame(
                year = year,
                observations = sum(selected),
                metric_id = metric_id,
                metric = metric$label,
                unit = metric$unit,
                value = stats::median(metric$values[selected]),
                stringsAsFactors = FALSE
              )
            }
          )
        )
        baseline <- rows$value[[1L]]
        if (!is.finite(baseline) || baseline <= 0) {
          stop("Базовий річний масштаб має бути додатним.")
        }
        rows$index_first_period_100 <- 100 * rows$value / baseline
        rows
      }
    )
  )

  result
}

build_hourly_data_exploration <- function(
  hourly_data,
  start_time,
  end_exclusive,
  quote_currency,
  rolling_window_hours = 720L,
  rolling_step_hours = 168L,
  return_maximum_lag = 48L,
  magnitude_maximum_lag = 168L,
  histogram_bins = 120L,
  qq_points = 501L,
  relationship_groups = 10L
) {
  prepared <- prepare_hourly_exploration_data(
    hourly_data = hourly_data,
    start_time = start_time,
    end_exclusive = end_exclusive
  )
  returns <- prepared$returns

  result <- list(
    candles = prepared$candles,
    returns = returns,
    period = prepared$period,
    period_table = exploration_period_table(prepared$period),
    return_shape = return_shape_table(returns$return_percent),
    return_quantiles = return_quantile_table(
      returns$return_percent
    ),
    annual_scale = annual_market_scale(
      prepared$candles,
      quote_currency = quote_currency
    ),
    rolling = rolling_return_description(
      returns,
      window_hours = rolling_window_hours,
      step_hours = rolling_step_hours
    ),
    distribution = return_distribution_plot_data(
      returns$return_percent,
      histogram_bins = histogram_bins,
      qq_points = qq_points
    ),
    correlations = return_correlation_views(
      returns,
      return_maximum_lag = return_maximum_lag,
      magnitude_maximum_lag = magnitude_maximum_lag
    ),
    calendar = calendar_return_grid(returns),
    lagged_features = lagged_feature_groups(
      returns,
      groups = relationship_groups,
      quote_currency = quote_currency
    ),
    settings = list(
      rolling_window_hours = rolling_window_hours,
      rolling_step_hours = rolling_step_hours,
      return_maximum_lag = return_maximum_lag,
      magnitude_maximum_lag = magnitude_maximum_lag,
      histogram_bins = histogram_bins,
      qq_points = qq_points,
      relationship_groups = relationship_groups
    )
  )
  class(result) <- c("btc_data_exploration", class(result))
  result
}
