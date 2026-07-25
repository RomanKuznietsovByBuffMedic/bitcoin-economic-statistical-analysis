# Training-only diagnostics before choosing an AR model ------------------
#
# The validation year and the sealed test year are deliberately excluded.
# A model is not allowed to run merely because its code exists: the
# diagnostic result is passed into the model as an explicit precondition.

require_diagnostic_series <- function(values, description) {
  values <- as.numeric(values)
  if (
    length(values) < 1000L ||
      anyNA(values) ||
      any(!is.finite(values))
  ) {
    stop(
      description,
      " має містити щонайменше 1000 скінченних значень без NA."
    )
  }

  values
}

require_probability <- function(value, description) {
  value <- as.numeric(value)
  if (
    length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value <= 0 ||
      value >= 1
  ) {
    stop(description, " має належати відкритому інтервалу (0; 1).")
  }

  value
}

require_positive_diagnostic_integer <- function(value, description) {
  numeric_value <- as.numeric(value)
  integer_value <- suppressWarnings(as.integer(value))
  if (
    length(value) != 1L ||
      is.na(numeric_value) ||
      !is.finite(numeric_value) ||
      is.na(integer_value) ||
      numeric_value != integer_value ||
      integer_value < 1L
  ) {
    stop(description, " має бути додатним цілим числом.")
  }

  integer_value
}

adf_default_max_lag <- function(number_of_observations) {
  number_of_observations <- require_positive_diagnostic_integer(
    number_of_observations,
    "Кількість спостережень для ADF"
  )

  proposed <- floor(
    12 * (number_of_observations / 100)^0.25
  )
  upper_limit <- floor((number_of_observations - 3L) / 3L)

  as.integer(max(0L, min(proposed, upper_limit)))
}

adf_regression_data <- function(values, maximum_lag) {
  changes <- diff(values)
  target_indices <- seq.int(
    from = maximum_lag + 1L,
    to = length(changes)
  )

  result <- data.frame(
    change = changes[target_indices],
    level = values[target_indices],
    stringsAsFactors = FALSE
  )

  for (lag in seq_len(maximum_lag)) {
    result[[paste0("change_lag_", lag)]] <-
      changes[target_indices - lag]
  }

  result
}

linear_model_bic <- function(response, design) {
  fit <- stats::lm.fit(
    x = design,
    y = response
  )
  if (fit$rank != ncol(design)) {
    stop("Регресія для вибору лагу має неповний ранг.")
  }

  residual_sum_of_squares <- sum(fit$residuals^2)
  if (
    !is.finite(residual_sum_of_squares) ||
      residual_sum_of_squares <= 0
  ) {
    stop("Не вдалося обчислити додатну суму квадратів залишків.")
  }

  observations <- length(response)
  observations * log(residual_sum_of_squares / observations) +
    ncol(design) * log(observations)
}

adf_unit_root_test <- function(
  values,
  maximum_lag = NULL,
  alpha = 0.05
) {
  values <- require_diagnostic_series(values, "Ряд для ADF")
  alpha <- require_probability(alpha, "Рівень значущості ADF")
  if (!identical(alpha, 0.05)) {
    stop("Поточна реалізація ADF зафіксована для alpha = 0.05.")
  }

  if (is.null(maximum_lag)) {
    maximum_lag <- adf_default_max_lag(length(values))
  } else {
    maximum_lag <- as.integer(maximum_lag)
  }
  if (
    length(maximum_lag) != 1L ||
      is.na(maximum_lag) ||
      maximum_lag < 0L ||
      maximum_lag > floor((length(values) - 3L) / 3L)
  ) {
    stop("maximum_lag для ADF має некоректне значення.")
  }

  regression_data <- adf_regression_data(values, maximum_lag)
  lag_names <- paste0("change_lag_", seq_len(maximum_lag))
  lag_orders <- 0:maximum_lag
  bic_values <- vapply(
    lag_orders,
    function(order) {
      predictors <- c(
        "level",
        utils::head(lag_names, order)
      )
      design <- cbind(
        intercept = 1,
        as.matrix(
          regression_data[, predictors, drop = FALSE]
        )
      )
      linear_model_bic(
        response = regression_data$change,
        design = design
      )
    },
    numeric(1)
  )
  selected_lag <- lag_orders[[which.min(bic_values)]]
  selected_predictors <- c(
    "level",
    utils::head(lag_names, selected_lag)
  )
  selected_formula <- stats::reformulate(
    selected_predictors,
    response = "change"
  )
  selected_model <- stats::lm(
    formula = selected_formula,
    data = regression_data
  )
  coefficient_table <- summary(selected_model)$coefficients
  test_statistic <- unname(
    coefficient_table["level", "t value"]
  )

  critical_values <- c(
    `1%` = -3.43,
    `5%` = -2.86,
    `10%` = -2.57
  )
  five_percent_critical <- unname(critical_values[["5%"]])

  list(
    test = "ADF",
    null_hypothesis = "наявний одиничний корінь",
    alternative_hypothesis = "ряд стаціонарний за рівнем",
    statistic = test_statistic,
    critical_value = five_percent_critical,
    alpha = alpha,
    selected_lag = selected_lag,
    maximum_lag = maximum_lag,
    critical_values = critical_values,
    reject_null = test_statistic < five_percent_critical,
    observations = nrow(regression_data)
  )
}

kpss_level_test <- function(values, alpha = 0.05) {
  values <- require_diagnostic_series(values, "Ряд для KPSS")
  alpha <- require_probability(alpha, "Рівень значущості KPSS")
  if (!identical(alpha, 0.05)) {
    stop("Поточна реалізація KPSS зафіксована для alpha = 0.05.")
  }

  observations <- length(values)
  residuals <- values - mean(values)
  cumulative_residuals <- cumsum(residuals)
  numerator <- sum(cumulative_residuals^2) / observations^2
  bandwidth <- as.integer(
    floor(4 * (observations / 100)^0.25)
  )
  bandwidth <- max(1L, min(bandwidth, observations - 1L))

  long_run_variance <- sum(residuals^2) / observations
  for (lag in seq_len(bandwidth)) {
    covariance <- sum(
      residuals[(lag + 1L):observations] *
        residuals[seq_len(observations - lag)]
    ) / observations
    bartlett_weight <- 1 - lag / (bandwidth + 1)
    long_run_variance <- long_run_variance +
      2 * bartlett_weight * covariance
  }
  if (
    !is.finite(long_run_variance) ||
      long_run_variance <= 0
  ) {
    stop("KPSS не зміг оцінити додатну довгострокову дисперсію.")
  }

  statistic <- numerator / long_run_variance
  critical_values <- c(
    `10%` = 0.347,
    `5%` = 0.463,
    `2.5%` = 0.574,
    `1%` = 0.739
  )
  five_percent_critical <- unname(critical_values[["5%"]])

  list(
    test = "KPSS",
    null_hypothesis = "ряд стаціонарний за рівнем",
    alternative_hypothesis = "ряд має одиничний корінь",
    statistic = statistic,
    critical_value = five_percent_critical,
    alpha = alpha,
    bandwidth = bandwidth,
    critical_values = critical_values,
    reject_null = statistic > five_percent_critical,
    observations = observations
  )
}

stationarity_test_table <- function(
  price_adf,
  price_kpss,
  return_adf,
  return_kpss
) {
  test_rows <- list(
    list(series = "Логарифм ціни", result = price_adf),
    list(series = "Логарифм ціни", result = price_kpss),
    list(series = "Лог-дохідність", result = return_adf),
    list(series = "Лог-дохідність", result = return_kpss)
  )

  do.call(
    rbind,
    lapply(
      test_rows,
      function(row) {
        result <- row$result
        data.frame(
          series = row$series,
          test = result$test,
          null_hypothesis = result$null_hypothesis,
          statistic = result$statistic,
          critical_value = result$critical_value,
          decision = if (isTRUE(result$reject_null)) {
            "Відхилено H0"
          } else {
            "Не відхилено H0"
          },
          stringsAsFactors = FALSE
        )
      }
    )
  )
}

correlation_diagnostic_data <- function(
  values,
  maximum_lag = 48L
) {
  values <- require_diagnostic_series(
    values,
    "Ряд для ACF і PACF"
  )
  maximum_lag <- require_positive_diagnostic_integer(
    maximum_lag,
    "Максимальний лаг ACF і PACF"
  )
  if (maximum_lag >= length(values) / 5) {
    stop("Максимальний лаг завеликий для довжини ряду.")
  }

  acf_result <- stats::acf(
    values,
    lag.max = maximum_lag,
    plot = FALSE,
    demean = TRUE,
    na.action = stats::na.pass
  )
  pacf_result <- stats::pacf(
    values,
    lag.max = maximum_lag,
    plot = FALSE,
    na.action = stats::na.pass
  )
  absolute_acf_result <- stats::acf(
    abs(values),
    lag.max = maximum_lag,
    plot = FALSE,
    demean = TRUE,
    na.action = stats::na.pass
  )
  squared_acf_result <- stats::acf(
    values^2,
    lag.max = maximum_lag,
    plot = FALSE,
    demean = TRUE,
    na.action = stats::na.pass
  )
  reference_bound <- 1.96 / sqrt(length(values))

  list(
    acf = data.frame(
      lag = seq_len(maximum_lag),
      value = as.numeric(acf_result$acf)[-1L],
      lower = -reference_bound,
      upper = reference_bound,
      stringsAsFactors = FALSE
    ),
    pacf = data.frame(
      lag = seq_len(maximum_lag),
      value = as.numeric(pacf_result$acf),
      lower = -reference_bound,
      upper = reference_bound,
      stringsAsFactors = FALSE
    ),
    absolute_acf = data.frame(
      lag = seq_len(maximum_lag),
      value = as.numeric(absolute_acf_result$acf)[-1L],
      lower = -reference_bound,
      upper = reference_bound,
      stringsAsFactors = FALSE
    ),
    squared_acf = data.frame(
      lag = seq_len(maximum_lag),
      value = as.numeric(squared_acf_result$acf)[-1L],
      lower = -reference_bound,
      upper = reference_bound,
      stringsAsFactors = FALSE
    ),
    reference_bound = reference_bound
  )
}

ar_order_bic_table <- function(values, maximum_order = 48L) {
  values <- require_diagnostic_series(
    values,
    "Ряд для вибору порядку AR"
  )
  maximum_order <- require_positive_diagnostic_integer(
    maximum_order,
    "Максимальний порядок AR"
  )
  if (length(values) <= maximum_order + 100L) {
    stop("Для вибору порядку AR бракує спостережень.")
  }

  target_indices <- seq.int(
    from = maximum_order + 1L,
    to = length(values)
  )
  response <- values[target_indices]
  lag_columns <- lapply(
    seq_len(maximum_order),
    function(lag) values[target_indices - lag]
  )
  names(lag_columns) <- paste0("lag_", seq_len(maximum_order))
  orders <- 0:maximum_order
  bic <- vapply(
    orders,
    function(order) {
      predictors <- utils::head(lag_columns, order)
      design <- if (order == 0L) {
        matrix(
          1,
          nrow = length(response),
          ncol = 1L,
          dimnames = list(NULL, "intercept")
        )
      } else {
        cbind(
          intercept = 1,
          do.call(cbind, predictors)
        )
      }
      linear_model_bic(response, design)
    },
    numeric(1)
  )
  selected_order <- orders[[which.min(bic)]]

  data.frame(
    order = orders,
    bic = bic,
    delta_bic = bic - min(bic),
    selected = orders == selected_order,
    stringsAsFactors = FALSE
  )
}

rolling_return_moments <- function(
  training,
  window_hours = 720L,
  step_hours = 168L
) {
  window_hours <- require_positive_diagnostic_integer(
    window_hours,
    "Ширина рухомого вікна"
  )
  step_hours <- require_positive_diagnostic_integer(
    step_hours,
    "Крок рухомого вікна"
  )
  if (nrow(training) < window_hours) {
    stop("Навчальний період коротший за рухоме вікно.")
  }

  end_indices <- seq.int(
    from = window_hours,
    to = nrow(training),
    by = step_hours
  )
  if (utils::tail(end_indices, 1L) != nrow(training)) {
    end_indices <- c(end_indices, nrow(training))
  }

  do.call(
    rbind,
    lapply(
      end_indices,
      function(end_index) {
        start_index <- end_index - window_hours + 1L
        values <- training$log_return_1h[
          start_index:end_index
        ]
        data.frame(
          window_start = training$open_time[[start_index]],
          window_end = training$open_time[[end_index]],
          mean_percent = 100 * mean(values),
          standard_deviation_percent = 100 * stats::sd(values),
          stringsAsFactors = FALSE
        )
      }
    )
  )
}

ljung_box_diagnostic <- function(values, lag, label) {
  values <- require_diagnostic_series(
    values,
    paste("Ряд для Ljung-Box:", label)
  )
  lag <- require_positive_diagnostic_integer(
    lag,
    "Лаг Ljung-Box"
  )
  if (lag >= length(values) / 5) {
    stop("Лаг Ljung-Box завеликий для довжини ряду.")
  }

  result <- stats::Box.test(
    values,
    lag = lag,
    type = "Ljung-Box",
    fitdf = 0L
  )

  data.frame(
    series = label,
    lag = lag,
    statistic = unname(result$statistic),
    p_value = unname(result$p.value),
    null_hypothesis = "автокореляції до заданого лагу дорівнюють нулю",
    stringsAsFactors = FALSE
  )
}

format_diagnostic_number <- function(value, digits = 4L) {
  formatC(value, format = "f", digits = digits)
}

sample_shape_statistics <- function(values) {
  values <- require_diagnostic_series(
    values,
    "Ряд для опису розподілу"
  )
  observations <- length(values)
  centre <- mean(values)
  scale <- stats::sd(values)
  if (!is.finite(scale) || scale <= 0) {
    stop("Стандартне відхилення доходності має бути додатним.")
  }

  centred <- values - centre
  skewness <- sum(centred^3) /
    ((observations - 1) * scale^3)
  excess_kurtosis <- sum(centred^4) /
    ((observations - 1) * scale^4) - 3

  list(
    observations = observations,
    mean = centre,
    median = stats::median(values),
    standard_deviation = scale,
    minimum = min(values),
    maximum = max(values),
    skewness = skewness,
    excess_kurtosis = excess_kurtosis
  )
}

return_distribution_diagnostics <- function(
  values,
  alpha = 0.05,
  qq_points = 501L
) {
  values <- require_diagnostic_series(
    values,
    "Ряд для аналізу розподілу"
  )
  alpha <- require_probability(
    alpha,
    "Рівень значущості аналізу розподілу"
  )
  qq_points <- require_positive_diagnostic_integer(
    qq_points,
    "Кількість точок Q-Q"
  )
  if (qq_points > length(values)) {
    stop("Кількість точок Q-Q перевищує довжину ряду.")
  }

  shape <- sample_shape_statistics(values)
  jarque_bera <- shape$observations * (
    shape$skewness^2 / 6 +
      shape$excess_kurtosis^2 / 24
  )
  jarque_bera_p_value <- stats::pchisq(
    jarque_bera,
    df = 2,
    lower.tail = FALSE
  )
  probabilities <- seq(
    from = 0.001,
    to = 0.999,
    length.out = qq_points
  )
  empirical <- as.numeric(stats::quantile(
    values,
    probs = probabilities,
    names = FALSE,
    type = 8
  ))
  normal_expected <- shape$mean +
    shape$standard_deviation * stats::qnorm(probabilities)
  tail_probabilities <- c(
    0.001,
    0.01,
    0.05,
    0.5,
    0.95,
    0.99,
    0.999
  )

  list(
    shape = shape,
    counts = data.frame(
      direction = c("Додатні", "Від'ємні", "Нульові"),
      observations = c(
        sum(values > 0),
        sum(values < 0),
        sum(values == 0)
      ),
      stringsAsFactors = FALSE
    ),
    quantiles = data.frame(
      probability = tail_probabilities,
      return_percent = 100 * as.numeric(stats::quantile(
        values,
        probs = tail_probabilities,
        names = FALSE,
        type = 8
      )),
      stringsAsFactors = FALSE
    ),
    qq = data.frame(
      probability = probabilities,
      normal_expected_percent = 100 * normal_expected,
      empirical_percent = 100 * empirical,
      stringsAsFactors = FALSE
    ),
    normality = data.frame(
      test = "Jarque-Bera",
      null_hypothesis = "розподіл має нульову асиметрію і ексцес",
      statistic = jarque_bera,
      p_value = jarque_bera_p_value,
      rejected = jarque_bera_p_value < alpha,
      stringsAsFactors = FALSE
    )
  )
}

arma_order_bic_table <- function(
  values,
  maximum_ar = 3L,
  maximum_ma = 3L
) {
  values <- require_diagnostic_series(
    values,
    "Ряд для вибору порядку ARMA"
  )
  maximum_ar <- as.integer(maximum_ar)
  maximum_ma <- as.integer(maximum_ma)
  if (
    length(maximum_ar) != 1L ||
      length(maximum_ma) != 1L ||
      is.na(maximum_ar) ||
      is.na(maximum_ma) ||
      maximum_ar < 0L ||
      maximum_ma < 0L ||
      maximum_ar > 10L ||
      maximum_ma > 10L ||
      (maximum_ar == 0L && maximum_ma == 0L)
  ) {
    stop("Сітка ARMA має некоректні межі.")
  }

  grid <- expand.grid(
    ar_order = 0:maximum_ar,
    ma_order = 0:maximum_ma,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  fits <- lapply(
    seq_len(nrow(grid)),
    function(index) {
      ar_order <- grid$ar_order[[index]]
      ma_order <- grid$ma_order[[index]]
      fit <- tryCatch(
        suppressWarnings(stats::arima(
          values,
          order = c(ar_order, 0L, ma_order),
          include.mean = TRUE,
          transform.pars = TRUE,
          method = "ML"
        )),
        error = identity
      )

      if (inherits(fit, "error")) {
        return(list(
          bic = NA_real_,
          converged = FALSE,
          message = conditionMessage(fit)
        ))
      }

      parameters <- ar_order + ma_order + 2L
      list(
        bic = -2 * fit$loglik +
          log(length(values)) * parameters,
        converged = identical(fit$code, 0L),
        message = if (identical(fit$code, 0L)) {
          ""
        } else {
          paste0("код оптимізації ", fit$code)
        }
      )
    }
  )
  grid$bic <- vapply(fits, `[[`, numeric(1), "bic")
  grid$converged <- vapply(
    fits,
    `[[`,
    logical(1),
    "converged"
  )
  grid$message <- vapply(
    fits,
    `[[`,
    character(1),
    "message"
  )
  usable <- is.finite(grid$bic) & grid$converged
  if (!any(usable)) {
    stop("Жодна модель у сітці ARMA не збіглася.")
  }
  best_bic <- min(grid$bic[usable])
  grid$delta_bic <- grid$bic - best_bic
  grid$selected <- FALSE
  grid$selected[[which.min(ifelse(
    usable,
    grid$bic,
    Inf
  ))]] <- TRUE

  grid[order(grid$ma_order, grid$ar_order), , drop = FALSE]
}

arch_lm_diagnostic <- function(values, lag, alpha = 0.05) {
  values <- require_diagnostic_series(
    values,
    "Залишки для ARCH-LM"
  )
  lag <- require_positive_diagnostic_integer(
    lag,
    "Лаг ARCH-LM"
  )
  alpha <- require_probability(
    alpha,
    "Рівень значущості ARCH-LM"
  )
  if (lag >= length(values) / 5) {
    stop("Лаг ARCH-LM завеликий для довжини ряду.")
  }

  squared <- values^2
  target_indices <- seq.int(lag + 1L, length(squared))
  response <- squared[target_indices]
  lagged <- vapply(
    seq_len(lag),
    function(current_lag) {
      squared[target_indices - current_lag]
    },
    numeric(length(target_indices))
  )
  design <- cbind(intercept = 1, lagged)
  fit <- stats::lm.fit(design, response)
  total_sum_of_squares <- sum(
    (response - mean(response))^2
  )
  residual_sum_of_squares <- sum(fit$residuals^2)
  r_squared <- 1 -
    residual_sum_of_squares / total_sum_of_squares
  statistic <- length(response) * r_squared
  p_value <- stats::pchisq(
    statistic,
    df = lag,
    lower.tail = FALSE
  )

  data.frame(
    test = "ARCH-LM",
    lag = lag,
    null_hypothesis = "умовна дисперсія стала",
    statistic = statistic,
    p_value = p_value,
    rejected = p_value < alpha,
    stringsAsFactors = FALSE
  )
}

calendar_explained_fraction <- function(
  values,
  hour,
  weekday
) {
  values <- as.numeric(values)
  hour <- as.integer(hour)
  weekday <- as.integer(weekday)
  if (
    length(values) != length(hour) ||
      length(values) != length(weekday) ||
      anyNA(values) ||
      anyNA(hour) ||
      anyNA(weekday) ||
      any(!is.finite(values))
  ) {
    stop("Дані для оцінки календарного ефекту некоректні.")
  }

  design <- stats::model.matrix(
    ~ factor(hour) + factor(weekday)
  )
  fit <- stats::lm.fit(design, values)
  total <- sum((values - mean(values))^2)
  if (!is.finite(total) || total <= 0) {
    stop("Загальна сума квадратів календарного ряду не додатна.")
  }

  1 - sum(fit$residuals^2) / total
}

median_profile_correlation <- function(
  values,
  period,
  group
) {
  period <- as.character(period)
  group <- as.character(group)
  profile <- tapply(values, list(period, group), mean)
  if (is.null(dim(profile)) || nrow(profile) < 2L) {
    return(NA_real_)
  }
  correlations <- stats::cor(
    t(profile),
    use = "pairwise.complete.obs",
    method = "spearman"
  )
  pair_values <- correlations[upper.tri(correlations)]
  pair_values <- pair_values[is.finite(pair_values)]
  if (length(pair_values) == 0L) {
    return(NA_real_)
  }

  stats::median(pair_values)
}

calendar_return_diagnostics <- function(training) {
  require_model_columns(
    training,
    c("open_time", "log_return_1h"),
    "календарної діагностики"
  )
  returns <- training$log_return_1h
  absolute_returns <- abs(returns)
  hour <- as.integer(format(
    training$open_time,
    "%H",
    tz = "UTC"
  ))
  weekday <- as.integer(format(
    training$open_time,
    "%u",
    tz = "UTC"
  ))
  year <- format(training$open_time, "%Y", tz = "UTC")
  weekday_labels <- c(
    "Пн",
    "Вт",
    "Ср",
    "Чт",
    "Пт",
    "Сб",
    "Нд"
  )

  hourly_rows <- lapply(
    0:23,
    function(current_hour) {
      selected <- hour == current_hour
      data.frame(
        hour = current_hour,
        observations = sum(selected),
        mean_return_percent = 100 * mean(returns[selected]),
        mean_absolute_return_percent =
          100 * mean(absolute_returns[selected]),
        median_absolute_return_percent =
          100 * stats::median(absolute_returns[selected]),
        stringsAsFactors = FALSE
      )
    }
  )
  weekday_rows <- lapply(
    1:7,
    function(current_weekday) {
      selected <- weekday == current_weekday
      data.frame(
        weekday = current_weekday,
        weekday_label = weekday_labels[[current_weekday]],
        observations = sum(selected),
        mean_return_percent = 100 * mean(returns[selected]),
        mean_absolute_return_percent =
          100 * mean(absolute_returns[selected]),
        median_absolute_return_percent =
          100 * stats::median(absolute_returns[selected]),
        stringsAsFactors = FALSE
      )
    }
  )

  list(
    hourly = do.call(rbind, hourly_rows),
    weekday = do.call(rbind, weekday_rows),
    strength = data.frame(
      outcome = c(
        "Годинна дохідність",
        "Модуль годинної дохідності"
      ),
      hour_and_weekday_r_squared = c(
        calendar_explained_fraction(
          returns,
          hour,
          weekday
        ),
        calendar_explained_fraction(
          absolute_returns,
          hour,
          weekday
        )
      ),
      hourly_profile_year_stability = c(
        median_profile_correlation(
          returns,
          year,
          hour
        ),
        median_profile_correlation(
          absolute_returns,
          year,
          hour
        )
      ),
      stringsAsFactors = FALSE
    )
  )
}

lagged_volatility_signal_table <- function(training) {
  require_model_columns(
    training,
    c(
      "high",
      "low",
      "volume",
      "turnover",
      "log_return_1h"
    ),
    "аналізу лагових ознак"
  )
  if (
    any(training$high <= 0) ||
      any(training$low <= 0) ||
      any(training$volume < 0) ||
      any(training$turnover < 0)
  ) {
    stop("Ціни й торгові обсяги мають некоректні значення.")
  }

  predictors <- list(
    `Модуль попередньої дохідності` =
      abs(training$log_return_1h),
    `Внутрішньогодинний log(high/low)` =
      log(training$high / training$low),
    `log(1 + обсяг BTC)` =
      log1p(training$volume),
    `log(1 + оборот USDT)` =
      log1p(training$turnover)
  )
  next_absolute_return <- abs(
    training$log_return_1h[-1L]
  )
  correlations <- vapply(
    predictors,
    function(values) {
      stats::cor(
        values[-length(values)],
        next_absolute_return,
        method = "spearman"
      )
    },
    numeric(1)
  )

  data.frame(
    feature = names(correlations),
    spearman_with_next_absolute_return =
      as.numeric(correlations),
    information_time = "кінець попередньої години",
    stringsAsFactors = FALSE
  )
}

annual_market_scale_table <- function(training) {
  require_model_columns(
    training,
    c(
      "open_time",
      "high",
      "low",
      "close",
      "volume",
      "turnover"
    ),
    "річної перевірки масштабу OHLCV"
  )
  if (
    any(training$high <= 0) ||
      any(training$low <= 0) ||
      any(training$close <= 0) ||
      any(training$volume < 0) ||
      any(training$turnover < 0)
  ) {
    stop("OHLCV містить недопустимі значення.")
  }

  years <- format(training$open_time, "%Y", tz = "UTC")
  rows <- lapply(
    unique(years),
    function(year) {
      selected <- years == year
      data.frame(
        year = year,
        observations = sum(selected),
        median_close = stats::median(
          training$close[selected]
        ),
        median_volume_btc = stats::median(
          training$volume[selected]
        ),
        median_turnover_quote = stats::median(
          training$turnover[selected]
        ),
        median_intrahour_range_percent =
          100 * stats::median(log(
            training$high[selected] /
              training$low[selected]
          )),
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, rows)
}

model_suitability_table <- function(
  stationarity,
  order_selection,
  arma_selection,
  distribution,
  arch_lm,
  calendar,
  lagged_signals,
  rolling_moments,
  readiness,
  alpha
) {
  selected_ar <- order_selection$order[
    order_selection$selected
  ][[1L]]
  selected_arma <- arma_selection[
    arma_selection$selected,
    ,
    drop = FALSE
  ]
  return_stationarity_supported <-
    stationarity$return_adf$reject_null &&
    !stationarity$return_kpss$reject_null
  price_stationarity_supported <-
    stationarity$price_adf$reject_null &&
    !stationarity$price_kpss$reject_null
  heavy_tails <-
    distribution$shape$excess_kurtosis > 1 &&
    distribution$normality$p_value[[1L]] < alpha
  arch_effect <- isTRUE(arch_lm$rejected[[1L]])
  calendar_return <- calendar$strength[
    calendar$strength$outcome == "Годинна дохідність",
    ,
    drop = FALSE
  ]
  calendar_absolute <- calendar$strength[
    calendar$strength$outcome ==
      "Модуль годинної дохідності",
    ,
    drop = FALSE
  ]
  range_signal <- lagged_signals[
    lagged_signals$feature ==
      "Внутрішньогодинний log(high/low)",
    "spearman_with_next_absolute_return"
  ][[1L]]
  rolling_sd_ratio <- max(
    rolling_moments$standard_deviation_percent
  ) / min(rolling_moments$standard_deviation_percent)

  data.frame(
    family = c(
      "Наївний прогноз ціни",
      "AR/ARMA для рівня ціни",
      "AR(1) для доходності",
      "ARMA для середнього доходності",
      "Сезонна модель середнього",
      "Календарний baseline волатильності",
      "GARCH для умовної дисперсії",
      "GARCH зі Student-t інноваціями",
      "Модель на основі OHLC-діапазону",
      "EGARCH/GJR або режимна модель",
      "VAR/VECM",
      "ML для напряму або доходності",
      "Мікроструктурна модель книги заявок"
    ),
    target = c(
      "ціна наступної години",
      "рівень ціни",
      "середня дохідність",
      "середня дохідність",
      "середня дохідність",
      "масштаб наступного руху",
      "умовна дисперсія",
      "умовна дисперсія і хвости",
      "масштаб наступного руху",
      "асиметрія або режими ризику",
      "кілька взаємопов'язаних рядів",
      "напрям або дохідність",
      "ліквідність і короткочасний рух"
    ),
    status = c(
      "Допустимий baseline",
      if (price_stationarity_supported) {
        "Потребує повторного обґрунтування"
      } else {
        "Не застосовувати"
      },
      if (readiness$selected) {
        "Допущено до validation"
      } else {
        "Відхилено"
      },
      if (
        selected_arma$ar_order[[1L]] == 0L &&
          selected_arma$ma_order[[1L]] == 0L
      ) {
        "Не підтримано зараз"
      } else {
        "Кандидат для validation"
      },
      if (
        calendar_return$hour_and_weekday_r_squared[[1L]] <
          0.005
      ) {
        "Не підтримано зараз"
      } else {
        "Умовний кандидат"
      },
      if (
        calendar_absolute$hour_and_weekday_r_squared[[1L]] >=
          0.01 &&
          calendar_absolute[[
            "hourly_profile_year_stability"
          ]][[1L]] >= 0.4
      ) {
        "Наступний baseline"
      } else {
        "Потребує додаткової перевірки"
      },
      if (return_stationarity_supported && arch_effect) {
        "Наступний кандидат"
      } else {
        "Передумови не виконано"
      },
      if (arch_effect && heavy_tails) {
        "Пріоритетний кандидат"
      } else {
        "Порівняти з Gaussian GARCH"
      },
      if (range_signal >= 0.2) {
        "Наступний baseline"
      } else {
        "Не підтримано зараз"
      },
      "Відкласти до перевірки простого GARCH",
      "Неможливо з поточним набором",
      "Відкласти до часових baseline",
      "Неможливо з годинними OHLCV"
    ),
    evidence = c(
      "не оцінює параметрів і задає мінімальний орієнтир",
      paste0(
        "ADF/KPSS для log-ціни: узгоджена стаціонарність = ",
        if (price_stationarity_supported) "так" else "ні"
      ),
      paste0(
        "BIC серед AR обрав p = ",
        selected_ar,
        "; PACF(1) = ",
        format_diagnostic_number(readiness$lag_one_pacf, 4L)
      ),
      paste0(
        "BIC серед ARMA обрав (p,q) = (",
        selected_arma$ar_order[[1L]],
        ",",
        selected_arma$ma_order[[1L]],
        ")"
      ),
      paste0(
        "календар пояснює ",
        format_diagnostic_number(
          100 * calendar_return[[
            "hour_and_weekday_r_squared"
          ]][[1L]],
          2L
        ),
        "% варіації доходності"
      ),
      paste0(
        "календар пояснює ",
        format_diagnostic_number(
          100 * calendar_absolute[[
            "hour_and_weekday_r_squared"
          ]][[1L]],
          2L
        ),
        "% варіації |r|"
      ),
      paste0(
        "ARCH-LM p = ",
        format_diagnostic_number(arch_lm$p_value[[1L]], 4L),
        "; рухома sd змінюється у ",
        format_diagnostic_number(rolling_sd_ratio, 2L),
        " раза"
      ),
      paste0(
        "ексцес = ",
        format_diagnostic_number(
          distribution$shape$excess_kurtosis,
          2L
        ),
        "; нормальність відхилено"
      ),
      paste0(
        "Spearman з наступним |r| = ",
        format_diagnostic_number(range_signal, 3L)
      ),
      "потрібні асиметрія, стабільність параметрів або невдалі залишки GARCH",
      "біржові копії BTC є контролем даних, а не незалежними драйверами",
      "поточний набір містить переважно лаги одного ринку",
      "немає угод, спреду та станів книги заявок"
    ),
    next_requirement = c(
      "порівнювати на тих самих годинах",
      "моделювати різницю log-ціни, тобто дохідність",
      "не відкривати validation",
      "новий mean-кандидат лише за нової стійкої структури",
      "показати стабільний практичний ефект у часі",
      "зафіксувати ціль і loss для волатильності",
      "перевірити обмеження, стандартизовані залишки й validation",
      "порівняти розподіли на validation, не лише in-sample BIC",
      "використовувати лише лагований діапазон",
      "спершу показати, чого не описує простий GARCH",
      "додати економічно відмінні синхронні ряди",
      "часовий nested validation і перевага над простими моделями",
      "отримати trade/order-book дані"
    ),
    stringsAsFactors = FALSE
  )
}

ar1_assumption_table <- function(
  training,
  return_adf,
  return_kpss,
  dependence,
  order_selection,
  arch_lag,
  alpha
) {
  returns <- training$log_return_1h
  lagged_model <- stats::lm(
    returns[-1L] ~ returns[-length(returns)]
  )
  phi <- unname(stats::coef(lagged_model)[[2L]])
  diagnostic_lag <- max(order_selection$order)
  residual_ljung_box <- stats::Box.test(
    stats::residuals(lagged_model),
    lag = diagnostic_lag,
    type = "Ljung-Box",
    fitdf = 1L
  )
  residual_arch_lm <- arch_lm_diagnostic(
    stats::residuals(lagged_model),
    lag = arch_lag,
    alpha = alpha
  )
  selected_order <- order_selection$order[
    order_selection$selected
  ][[1L]]
  lag_one_pacf <- dependence$pacf$value[
    dependence$pacf$lag == 1L
  ][[1L]]
  reference_bound <- dependence$reference_bound

  conditions <- data.frame(
    condition_id = c(
      "training_only",
      "return_adf",
      "return_kpss",
      "stationary_root",
      "residual_white_noise",
      "residual_constant_variance",
      "lag_one_signal",
      "bic_order"
    ),
    condition = c(
      "Дозволено використати лише початковий навчальний період",
      "ADF має відхилити одиничний корінь у доходностях",
      "KPSS не має відхилити стаціонарність доходностей",
      "Оцінка AR(1) повинна задовольняти |phi1| < 1",
      "Залишки AR(1) не повинні мати спільної автокореляції",
      "Залишки простої AR(1) не повинні мати ARCH-ефекту",
      "PACF лагу 1 має вийти за орієнтовну 95% межу",
      "BIC має обрати AR(1) серед AR(0)...AR(max)"
    ),
    passed = c(
      max(training$open_time) <
        attr(training, "validation_start"),
      isTRUE(return_adf$reject_null),
      !isTRUE(return_kpss$reject_null),
      abs(phi) < 1,
      unname(residual_ljung_box$p.value) >= alpha,
      !isTRUE(residual_arch_lm$rejected[[1L]]),
      abs(lag_one_pacf) > reference_bound,
      selected_order == 1L
    ),
    evidence = c(
      paste0(
        "остання година: ",
        format(
          max(training$open_time),
          "%Y-%m-%d %H:%M UTC",
          tz = "UTC"
        )
      ),
      paste0(
        "ADF = ",
        format_diagnostic_number(return_adf$statistic, 3L),
        "; критичне = ",
        format_diagnostic_number(return_adf$critical_value, 2L)
      ),
      paste0(
        "KPSS = ",
        format_diagnostic_number(return_kpss$statistic, 3L),
        "; критичне = ",
        format_diagnostic_number(return_kpss$critical_value, 3L)
      ),
      paste0(
        "phi1 = ",
        format_diagnostic_number(phi, 4L)
      ),
      paste0(
        "Ljung-Box(",
        diagnostic_lag,
        ") p-value = ",
        format_diagnostic_number(
          unname(residual_ljung_box$p.value),
          4L
        )
      ),
      paste0(
        "ARCH-LM(",
        arch_lag,
        ") p-value = ",
        format_diagnostic_number(
          residual_arch_lm$p_value[[1L]],
          4L
        )
      ),
      paste0(
        "PACF(1) = ",
        format_diagnostic_number(lag_one_pacf, 4L),
        "; межа = +/-",
        format_diagnostic_number(reference_bound, 4L)
      ),
      paste0("обрано p = ", selected_order)
    ),
    required = TRUE,
    stringsAsFactors = FALSE
  )
  conditions$status <- ifelse(
    conditions$passed,
    "Виконано",
    "Не виконано"
  )

  list(
    conditions = conditions,
    selected = all(conditions$passed),
    selected_order = selected_order,
    phi = phi,
    residual_ljung_box_p_value =
      unname(residual_ljung_box$p.value),
    residual_arch_lm_p_value =
      residual_arch_lm$p_value[[1L]],
    lag_one_pacf = lag_one_pacf,
    reference_bound = reference_bound,
    alpha = alpha
  )
}

analyze_training_time_series <- function(hourly_data, config) {
  training <- training_model_frame(hourly_data, config)
  attr(training, "validation_start") <-
    config$evaluation$validation_start
  alpha <- config$analysis$alpha
  mean_maximum_lag <- config$analysis$mean_max_lag
  dependence_maximum_lag <-
    config$analysis$dependence_max_lag

  price_adf <- adf_unit_root_test(
    training$log_price,
    alpha = alpha
  )
  price_kpss <- kpss_level_test(
    training$log_price,
    alpha = alpha
  )
  return_adf <- adf_unit_root_test(
    training$log_return_1h,
    alpha = alpha
  )
  return_kpss <- kpss_level_test(
    training$log_return_1h,
    alpha = alpha
  )
  dependence <- correlation_diagnostic_data(
    training$log_return_1h,
    maximum_lag = dependence_maximum_lag
  )
  order_selection <- ar_order_bic_table(
    training$log_return_1h,
    maximum_order = mean_maximum_lag
  )
  arma_selection <- arma_order_bic_table(
    training$log_return_1h,
    maximum_ar = config$analysis$arma_max_ar,
    maximum_ma = config$analysis$arma_max_ma
  )
  rolling_moments <- rolling_return_moments(
    training,
    window_hours = config$analysis$rolling_window_hours,
    step_hours = config$analysis$rolling_step_hours
  )
  distribution <- return_distribution_diagnostics(
    training$log_return_1h,
    alpha = alpha,
    qq_points = config$analysis$normal_qq_points
  )
  portmanteau <- rbind(
    ljung_box_diagnostic(
      training$log_return_1h,
      lag = mean_maximum_lag,
      label = "Лог-дохідність"
    ),
    ljung_box_diagnostic(
      abs(training$log_return_1h),
      lag = dependence_maximum_lag,
      label = "Модуль лог-дохідності"
    ),
    ljung_box_diagnostic(
      training$log_return_1h^2,
      lag = dependence_maximum_lag,
      label = "Квадрат лог-дохідності"
    )
  )
  mean_residuals <- training$log_return_1h -
    mean(training$log_return_1h)
  arch_lm <- arch_lm_diagnostic(
    mean_residuals,
    lag = config$analysis$arch_lag,
    alpha = alpha
  )
  calendar <- calendar_return_diagnostics(training)
  lagged_signals <- lagged_volatility_signal_table(training)
  annual_market_scale <- annual_market_scale_table(training)
  readiness <- ar1_assumption_table(
    training = training,
    return_adf = return_adf,
    return_kpss = return_kpss,
    dependence = dependence,
    order_selection = order_selection,
    arch_lag = config$analysis$arch_lag,
    alpha = alpha
  )
  stationarity <- list(
    price_adf = price_adf,
    price_kpss = price_kpss,
    return_adf = return_adf,
    return_kpss = return_kpss,
    table = stationarity_test_table(
      price_adf,
      price_kpss,
      return_adf,
      return_kpss
    )
  )
  suitability <- model_suitability_table(
    stationarity = stationarity,
    order_selection = order_selection,
    arma_selection = arma_selection,
    distribution = distribution,
    arch_lm = arch_lm,
    calendar = calendar,
    lagged_signals = lagged_signals,
    rolling_moments = rolling_moments,
    readiness = readiness,
    alpha = alpha
  )
  failed <- readiness$conditions$condition[
    !readiness$conditions$passed
  ]
  selected_arma <- arma_selection[
    arma_selection$selected,
    c("ar_order", "ma_order"),
    drop = FALSE
  ]
  volatility_clustering <-
    isTRUE(arch_lm$rejected[[1L]]) &&
    portmanteau$p_value[
      portmanteau$series == "Квадрат лог-дохідності"
    ][[1L]] < alpha
  heavy_tails <-
    distribution$normality$rejected[[1L]] &&
    distribution$shape$excess_kurtosis > 1

  result <- list(
    training = training,
    training_period = list(
      start = min(training$open_time),
      end = max(training$open_time),
      end_exclusive = config$evaluation$validation_start,
      observations = nrow(training)
    ),
    stationarity = stationarity,
    distribution = distribution,
    dependence = dependence,
    order_selection = order_selection,
    arma_selection = arma_selection,
    rolling_moments = rolling_moments,
    portmanteau = portmanteau,
    arch_lm = arch_lm,
    calendar = calendar,
    lagged_signals = lagged_signals,
    annual_market_scale = annual_market_scale,
    readiness = readiness,
    suitability = suitability,
    decision = list(
      ar1_selected = readiness$selected,
      selected_order = readiness$selected_order,
      selected_arma = c(
        ar = selected_arma$ar_order[[1L]],
        ma = selected_arma$ma_order[[1L]]
      ),
      failed_conditions = failed,
      volatility_clustering = volatility_clustering,
      heavy_tails = heavy_tails,
      next_target = if (volatility_clustering) {
        "умовна дисперсія наступної годинної дохідності"
      } else {
        "середня годинна дохідність"
      },
      status = if (readiness$selected) {
        "AR(1) допущено до внутрішньої перевірки"
      } else {
        paste(
          "AR(1) відхилено до внутрішньої перевірки;",
          if (volatility_clustering) {
            "наступна обґрунтована ціль — волатильність"
          } else {
            "новий кандидат потребує окремої підстави"
          }
        )
      }
    )
  )
  class(result) <- c("btc_time_series_analysis", class(result))
  result
}

diagnose_ar1_candidate <- function(hourly_data, config) {
  result <- analyze_training_time_series(hourly_data, config)
  class(result) <- c("ar1_candidate_diagnostics", class(result))
  result
}

assert_ar1_selected <- function(diagnostics) {
  if (!inherits(diagnostics, "ar1_candidate_diagnostics")) {
    stop(
      paste(
        "AR(1) потребує результату diagnose_ar1_candidate(),",
        "обчисленого лише на навчальному періоді."
      )
    )
  }
  if (!isTRUE(diagnostics$decision$ar1_selected)) {
    stop(
      "AR(1) не пройшла передумови: ",
      paste(
        diagnostics$decision$failed_conditions,
        collapse = "; "
      ),
      ". Validation і закритий test не відкриваються."
    )
  }

  invisible(diagnostics)
}
