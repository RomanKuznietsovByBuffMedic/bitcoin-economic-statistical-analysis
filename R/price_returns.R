# Price-return analysis ----------------------------------------------------
#
# This module is responsible only for:
# - constructing return variables and the next-hour target;
# - separating training, validation, and final-test periods;
# - fitting and evaluating the simple AR(1) baseline.

build_price_return_features <- function(data) {
  data |>
    dplyr::arrange(open_time) |>
    dplyr::mutate(
      price_usdt_per_btc = close,

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
        price_usdt_per_btc /
          dplyr::lag(price_usdt_per_btc) - 1,
        NA_real_
      ),

      log_return_1h = dplyr::if_else(
        hours_from_previous == 1,
        log(
          price_usdt_per_btc /
            dplyr::lag(price_usdt_per_btc)
        ),
        NA_real_
      ),

      target_open_time = dplyr::if_else(
        hours_to_next == 1,
        dplyr::lead(open_time),
        as.POSIXct(NA, tz = "UTC")
      ),

      actual_next_close_usdt_per_btc = dplyr::if_else(
        hours_to_next == 1,
        dplyr::lead(price_usdt_per_btc),
        NA_real_
      ),

      target_simple_return_1h = dplyr::if_else(
        hours_to_next == 1,
        dplyr::lead(price_usdt_per_btc) /
          price_usdt_per_btc - 1,
        NA_real_
      ),

      target_log_return_1h = dplyr::if_else(
        hours_to_next == 1,
        log(
          dplyr::lead(price_usdt_per_btc) /
            price_usdt_per_btc
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

resolve_evaluation_period <- function(
  run_final_test,
  validation_start,
  final_test_start,
  final_test_end
) {
  if (isTRUE(run_final_test)) {
    return(list(
      name = "Фінальний тест",
      start = final_test_start,
      end = final_test_end
    ))
  }

  list(
    name = "Внутрішня перевірка",
    start = validation_start,
    end = final_test_start
  )
}

split_model_samples <- function(
  data,
  evaluation_period,
  final_test_start,
  final_test_end
) {
  training <- data |>
    dplyr::filter(
      !is.na(target_open_time),
      target_open_time < evaluation_period$start
    )

  evaluation <- data |>
    dplyr::filter(
      !is.na(target_open_time),
      target_open_time >= evaluation_period$start,
      target_open_time < evaluation_period$end
    )

  final_test_rows <- sum(
    !is.na(data$target_open_time) &
      data$target_open_time >= final_test_start &
      data$target_open_time < final_test_end
  )

  summary <- tibble::tibble(
    `Частина` = c(
      "Навчання",
      evaluation_period$name,
      "Зарезервований фінальний тест"
    ),
    `Початок` = c(
      format(min(training$open_time), tz = "UTC"),
      format(evaluation_period$start, tz = "UTC"),
      format(final_test_start, tz = "UTC")
    ),
    `Кінець без включення межі` = c(
      format(evaluation_period$start, tz = "UTC"),
      format(evaluation_period$end, tz = "UTC"),
      format(final_test_end, tz = "UTC")
    ),
    `Кількість цільових годин` = c(
      nrow(training),
      nrow(evaluation),
      final_test_rows
    )
  )

  list(
    training = training,
    evaluation = evaluation,
    summary = summary
  )
}

calculate_error_metrics <- function(actual, predicted, model_name) {
  tibble::tibble(
    Модель = model_name,
    `MAE, базисні пункти` =
      mean(abs(actual - predicted)) * 10000,
    `RMSE, базисні пункти` =
      sqrt(mean((actual - predicted)^2)) * 10000
  )
}

prepare_ar1_samples <- function(training, evaluation) {
  model_training <- training |>
    dplyr::filter(
      !is.na(log_return_1h),
      !is.na(target_log_return_1h)
    )

  model_evaluation <- evaluation |>
    dplyr::filter(
      !is.na(log_return_1h),
      !is.na(target_log_return_1h),
      !is.na(actual_next_close_usdt_per_btc)
  )

  if (nrow(model_training) == 0 || nrow(model_evaluation) == 0) {
    stop(
      paste(
        "Після підготовки не залишилося даних",
        "для моделі."
      )
    )
  }

  list(
    training = model_training,
    evaluation = model_evaluation
  )
}

add_ar1_predictions <- function(model, evaluation) {
  evaluation |>
    dplyr::mutate(
      predicted_log_return_ar1 = as.numeric(
        stats::predict(
          model,
          newdata = evaluation
        )
      ),
      predicted_log_return_zero = 0,
      predicted_simple_return_ar1 =
        exp(predicted_log_return_ar1) - 1,
      predicted_close_ar1 =
        price_usdt_per_btc *
        exp(predicted_log_return_ar1),
      predicted_close_zero = price_usdt_per_btc
    )
}

build_prediction_error_table <- function(predictions) {
  dplyr::bind_rows(
    calculate_error_metrics(
      actual = predictions$target_log_return_1h,
      predicted = predictions$predicted_log_return_zero,
      model_name = "Нульова зміна"
    ),
    calculate_error_metrics(
      actual = predictions$target_log_return_1h,
      predicted = predictions$predicted_log_return_ar1,
      model_name = "AR(1)"
    )
  )
}

calculate_oos_r_squared <- function(predictions) {
  actual <- predictions$target_log_return_1h
  predicted <- predictions$predicted_log_return_ar1

  1 - sum((actual - predicted)^2) / sum(actual^2)
}

build_direction_table <- function(training, predictions) {
  majority_up <- mean(
    training$target_log_return_1h > 0
  ) >= 0.5

  actual_up <- predictions$target_log_return_1h > 0
  predicted_up <- predictions$predicted_log_return_ar1 > 0

  tibble::tibble(
    Модель = c(
      "Переважний напрям у навчальних даних",
      "AR(1)"
    ),
    `Правильний напрям, %` = 100 * c(
      mean(actual_up == majority_up),
      mean(actual_up == predicted_up)
    )
  )
}

extract_ar1_coefficients <- function(model) {
  summary(model)$coefficients |>
    as.data.frame() |>
    tibble::rownames_to_column("Параметр") |>
    dplyr::transmute(
      Параметр,
      `Оцінка` = Estimate,
      `Стандартна похибка` = `Std. Error`,
      `t-статистика` = `t value`,
      `p-значення` = `Pr(>|t|)`
    )
}

run_ar1_baseline <- function(training, evaluation) {
  samples <- prepare_ar1_samples(
    training = training,
    evaluation = evaluation
  )

  model <- stats::lm(
    target_log_return_1h ~ log_return_1h,
    data = samples$training
  )

  predictions <- add_ar1_predictions(
    model = model,
    evaluation = samples$evaluation
  )

  list(
    model = model,
    predictions = predictions,
    coefficients = extract_ar1_coefficients(model),
    error_metrics = build_prediction_error_table(predictions),
    direction_metrics = build_direction_table(
      training = samples$training,
      predictions = predictions
    ),
    oos_r_squared = calculate_oos_r_squared(predictions)
  )
}

make_price_forecast_examples <- function(predictions, n = 8) {
  predictions |>
    dplyr::select(
      target_open_time,
      price_usdt_per_btc,
      actual_next_close_usdt_per_btc,
      predicted_simple_return_ar1,
      predicted_close_ar1
    ) |>
    dplyr::slice_head(n = n) |>
    dplyr::mutate(
      `Прогнозована зміна, %` =
        100 * predicted_simple_return_ar1
    ) |>
    dplyr::transmute(
      `Цільова година UTC` = format(
        target_open_time,
        "%Y-%m-%d %H:%M",
        tz = "UTC"
      ),
      `Поточна ціна` = price_usdt_per_btc,
      `Фактична наступна ціна` =
        actual_next_close_usdt_per_btc,
      `Прогнозована зміна, %`,
      `Прогнозована наступна ціна` =
        predicted_close_ar1
    )
}
