# Підготовка денних даних BTCUSDT для моделювання і бектесту
#
# Головне правило: кожен рядок містить лише інформацію, яка була
# доступна до моменту рішення. Рішення приймається на open поточного дня,
# тому всі ознаки зсуваються щонайменше на один завершений день назад.
# Ціль є дохідністю від open поточного дня до open наступного дня.

BTC_MODEL_DATA_VERSION <- "1"

btc_model_default_config <- function(project_root = ".") {
  root <- normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )

  processed_dir <- file.path(
    root,
    "data",
    "processed",
    "binance",
    "spot",
    "BTCUSDT"
  )

  list(
    project_root = root,
    input_complete = file.path(
      processed_dir,
      "BTCUSDT_1d_complete.rds"
    ),
    input_fallback = file.path(
      processed_dir,
      "BTCUSDT_1d.rds"
    ),
    output_dir = file.path(
      root,
      "data",
      "model",
      "binance",
      "spot",
      "BTCUSDT"
    ),
    interval_seconds = 86400,
    train_fraction = 0.60,
    validation_fraction = 0.20,
    minimum_rows = 365L,
    fee_bps = 10,
    half_spread_bps = 2,
    slippage_bps = 3
  )
}

btc_model_paths <- function(config) {
  list(
    data_rds = file.path(
      config$output_dir,
      "BTCUSDT_1d_model.rds"
    ),
    data_csv = file.path(
      config$output_dir,
      "BTCUSDT_1d_model.csv"
    ),
    metadata_csv = file.path(
      config$output_dir,
      "BTCUSDT_1d_model_metadata.csv"
    ),
    dictionary_csv = file.path(
      config$output_dir,
      "BTCUSDT_1d_model_dictionary.csv"
    ),
    split_summary_csv = file.path(
      config$output_dir,
      "BTCUSDT_1d_split_summary.csv"
    ),
    quality_summary_csv = file.path(
      config$output_dir,
      "BTCUSDT_1d_model_quality.csv"
    )
  )
}

btc_model_lag <- function(x, periods = 1L) {
  periods <- as.integer(periods)
  n <- length(x)
  result <- x
  result[] <- NA

  if (periods < 0L) {
    stop("periods не може бути від'ємним.", call. = FALSE)
  }

  if (periods == 0L) {
    return(x)
  }

  if (periods >= n) {
    return(result)
  }

  result[seq.int(periods + 1L, n)] <- x[seq_len(n - periods)]
  result
}

btc_model_lead <- function(x, periods = 1L) {
  periods <- as.integer(periods)
  n <- length(x)
  result <- x
  result[] <- NA

  if (periods < 0L) {
    stop("periods не може бути від'ємним.", call. = FALSE)
  }

  if (periods == 0L) {
    return(x)
  }

  if (periods >= n) {
    return(result)
  }

  result[seq_len(n - periods)] <- x[seq.int(periods + 1L, n)]
  result
}

btc_model_rolling <- function(x, window, statistic) {
  window <- as.integer(window)
  n <- length(x)
  result <- rep(NA_real_, n)

  if (window < 2L || n < window) {
    return(result)
  }

  for (index in seq.int(window, n)) {
    values <- x[seq.int(index - window + 1L, index)]

    if (all(is.finite(values))) {
      result[[index]] <- statistic(values)
    }
  }

  result
}

btc_model_read_daily <- function(config) {
  input_path <- if (file.exists(config$input_complete)) {
    config$input_complete
  } else {
    config$input_fallback
  }

  if (!file.exists(input_path)) {
    stop(
      paste0(
        "Не знайдено денних даних. Спочатку запустіть ",
        "scripts/01_get_binance_data.R або ",
        "scripts/03_update_binance_fast.R."
      ),
      call. = FALSE
    )
  }

  data <- readRDS(input_path)

  if (!is.data.frame(data)) {
    stop("Денний RDS не містить data.frame.", call. = FALSE)
  }

  required_columns <- c(
    "open_time",
    "close_time",
    "open",
    "high",
    "low",
    "close",
    "quote_volume",
    "taker_buy_quote_volume"
  )

  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "У денних даних відсутні колонки: ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  input_rows <- nrow(data)

  if ("is_complete" %in% names(data)) {
    data <- data[data$is_complete %in% TRUE, , drop = FALSE]
  }

  complete_rows <- nrow(data)

  finite_prices <- is.finite(data$open) &
    is.finite(data$high) &
    is.finite(data$low) &
    is.finite(data$close) &
    data$open > 0 &
    data$high > 0 &
    data$low > 0 &
    data$close > 0 &
    data$high >= pmax(data$open, data$close) &
    data$low <= pmin(data$open, data$close) &
    data$high >= data$low

  finite_activity <- is.finite(data$quote_volume) &
    is.finite(data$taker_buy_quote_volume) &
    data$quote_volume >= 0 &
    data$taker_buy_quote_volume >= 0 &
    data$taker_buy_quote_volume <= data$quote_volume * (1 + 1e-12)

  data <- data[finite_prices & finite_activity, , drop = FALSE]
  valid_rows <- nrow(data)

  data <- data[order(data$open_time), , drop = FALSE]
  duplicate_rows <- duplicated(data$open_time)
  duplicate_count <- sum(duplicate_rows)
  data <- data[!duplicate_rows, , drop = FALSE]
  rownames(data) <- NULL

  time_differences <- diff(as.numeric(data$open_time))
  non_daily_steps <- sum(time_differences != config$interval_seconds)
  missing_days <- sum(
    pmax(
      floor(time_differences / config$interval_seconds) - 1,
      0
    )
  )

  quality <- data.frame(
    metric = c(
      "input_rows",
      "removed_incomplete_rows",
      "removed_invalid_rows",
      "removed_duplicate_rows",
      "non_daily_time_steps",
      "missing_days",
      "usable_daily_rows"
    ),
    value = c(
      input_rows,
      input_rows - complete_rows,
      complete_rows - valid_rows,
      duplicate_count,
      non_daily_steps,
      missing_days,
      nrow(data)
    ),
    stringsAsFactors = FALSE
  )

  list(
    data = data,
    input_path = normalizePath(
      input_path,
      winslash = "/",
      mustWork = TRUE
    ),
    quality = quality
  )
}

btc_model_prepare <- function(daily, config) {
  data <- daily[order(daily$open_time), , drop = FALSE]
  n <- nrow(data)

  if (n < config$minimum_rows) {
    stop(
      paste0(
        "Замало денних спостережень: ",
        n,
        ". Потрібно щонайменше ",
        config$minimum_rows,
        "."
      ),
      call. = FALSE
    )
  }

  interval_seconds <- as.numeric(config$interval_seconds)
  open_seconds <- as.numeric(data$open_time)
  regular_from_previous <- c(
    FALSE,
    diff(open_seconds) == interval_seconds
  )
  regular_to_next <- c(
    diff(open_seconds) == interval_seconds,
    FALSE
  )

  close_return <- rep(NA_real_, n)
  close_return[regular_from_previous] <- 100 * log(
    data$close[regular_from_previous] /
      btc_model_lag(data$close)[regular_from_previous]
  )

  range_pct <- 100 * log(data$high / data$low)

  volume_change <- rep(NA_real_, n)
  previous_volume <- btc_model_lag(data$quote_volume)
  valid_volume <- regular_from_previous &
    data$quote_volume > 0 &
    previous_volume > 0
  volume_change[valid_volume] <- log(
    data$quote_volume[valid_volume] /
      previous_volume[valid_volume]
  )

  taker_buy_share <- ifelse(
    data$quote_volume > 0,
    data$taker_buy_quote_volume / data$quote_volume,
    NA_real_
  )

  rolling_mean_7 <- btc_model_rolling(
    close_return,
    7L,
    mean
  )
  rolling_mean_30 <- btc_model_rolling(
    close_return,
    30L,
    mean
  )
  rolling_volatility_7 <- btc_model_rolling(
    close_return,
    7L,
    stats::sd
  )
  rolling_volatility_30 <- btc_model_rolling(
    close_return,
    30L,
    stats::sd
  )

  target_return <- rep(NA_real_, n)
  target_return[regular_to_next] <- 100 * log(
    btc_model_lead(data$open)[regular_to_next] /
      data$open[regular_to_next]
  )

  model <- data.frame(
    decision_time = data$open_time,
    information_time = btc_model_lag(data$close_time),
    target_end_time = btc_model_lead(data$open_time),
    execution_price = data$open,
    target_end_price = btc_model_lead(data$open),
    target_log_return_pct = target_return,
    return_lag_1 = btc_model_lag(close_return, 1L),
    return_lag_2 = btc_model_lag(close_return, 2L),
    return_lag_7 = btc_model_lag(close_return, 7L),
    mean_return_7 = btc_model_lag(rolling_mean_7, 1L),
    mean_return_30 = btc_model_lag(rolling_mean_30, 1L),
    volatility_7 = btc_model_lag(rolling_volatility_7, 1L),
    volatility_30 = btc_model_lag(rolling_volatility_30, 1L),
    range_lag_1 = btc_model_lag(range_pct, 1L),
    log_volume_change_lag_1 = btc_model_lag(volume_change, 1L),
    taker_buy_share_lag_1 = btc_model_lag(taker_buy_share, 1L),
    stringsAsFactors = FALSE
  )

  feature_columns <- c(
    "return_lag_1",
    "return_lag_2",
    "return_lag_7",
    "mean_return_7",
    "mean_return_30",
    "volatility_7",
    "volatility_30",
    "range_lag_1",
    "log_volume_change_lag_1",
    "taker_buy_share_lag_1"
  )

  valid_features <- vapply(
    model[feature_columns],
    is.finite,
    FUN.VALUE = logical(nrow(model))
  )

  keep <- is.finite(model$target_log_return_pct) &
    is.finite(model$execution_price) &
    is.finite(model$target_end_price) &
    rowSums(valid_features) == length(feature_columns) &
    !is.na(model$information_time) &
    !is.na(model$decision_time) &
    !is.na(model$target_end_time) &
    model$information_time < model$decision_time &
    model$decision_time < model$target_end_time

  model <- model[keep, , drop = FALSE]
  rownames(model) <- NULL

  row_count <- nrow(model)

  if (row_count < config$minimum_rows) {
    stop(
      paste0(
        "Після побудови ознак залишилося замало рядків: ",
        row_count,
        "."
      ),
      call. = FALSE
    )
  }

  train_end <- floor(row_count * config$train_fraction)
  validation_end <- floor(
    row_count * (
      config$train_fraction +
        config$validation_fraction
    )
  )

  if (train_end < 1L || validation_end <= train_end ||
      validation_end >= row_count) {
    stop("Некоректні частки часових вибірок.", call. = FALSE)
  }

  model$sample <- "test"
  model$sample[seq_len(train_end)] <- "train"
  model$sample[seq.int(train_end + 1L, validation_end)] <- "validation"
  model$sample <- factor(
    model$sample,
    levels = c("train", "validation", "test")
  )

  model
}

btc_model_split_summary <- function(model) {
  samples <- levels(model$sample)

  do.call(
    rbind,
    lapply(
      samples,
      function(sample_name) {
        subset <- model[
          model$sample == sample_name,
          ,
          drop = FALSE
        ]

        data.frame(
          sample = sample_name,
          rows = nrow(subset),
          first_decision_time = min(subset$decision_time),
          last_decision_time = max(subset$decision_time),
          stringsAsFactors = FALSE
        )
      }
    )
  )
}

btc_model_metadata <- function(config, input_path) {
  one_way_cost_bps <- config$fee_bps +
    config$half_spread_bps +
    config$slippage_bps

  project_prefix <- paste0(
    config$project_root,
    "/"
  )

  relative_input_path <- if (startsWith(
    input_path,
    project_prefix
  )) {
    substring(
      input_path,
      nchar(project_prefix) + 1L
    )
  } else {
    input_path
  }

  data.frame(
    field = c(
      "model_data_version",
      "source_file",
      "market",
      "interval",
      "information_cutoff",
      "decision_and_execution",
      "target",
      "split_rule",
      "fee_bps_per_order",
      "half_spread_bps_per_order",
      "slippage_bps_per_order",
      "assumed_one_way_cost_bps"
    ),
    value = c(
      BTC_MODEL_DATA_VERSION,
      relative_input_path,
      "Binance Spot BTCUSDT",
      "1d UTC",
      "previous completed daily bar",
      "current daily open",
      "log return from current open to next open",
      "first 60% train, next 20% validation, final 20% test",
      config$fee_bps,
      config$half_spread_bps,
      config$slippage_bps,
      one_way_cost_bps
    ),
    stringsAsFactors = FALSE
  )
}

btc_model_dictionary <- function() {
  data.frame(
    column = c(
      "decision_time",
      "information_time",
      "target_end_time",
      "execution_price",
      "target_end_price",
      "target_log_return_pct",
      "return_lag_1",
      "return_lag_2",
      "return_lag_7",
      "mean_return_7",
      "mean_return_30",
      "volatility_7",
      "volatility_30",
      "range_lag_1",
      "log_volume_change_lag_1",
      "taker_buy_share_lag_1",
      "sample"
    ),
    role = c(
      "decision_time",
      "information_cutoff",
      "target_time",
      "execution",
      "target",
      "target",
      rep("feature", 10L),
      "split"
    ),
    available_at = c(
      "current daily open",
      "before decision",
      "after decision",
      "current daily open",
      "next daily open",
      "next daily open",
      rep("before decision", 10L),
      "before modelling"
    ),
    unit = c(
      "UTC",
      "UTC",
      "UTC",
      "USDT per BTC",
      "USDT per BTC",
      "percent",
      "percent",
      "percent",
      "percent",
      "percent",
      "percent",
      "percent",
      "percent",
      "percent",
      "log ratio",
      "share from 0 to 1",
      "category"
    ),
    description = c(
      "Момент умовного рішення та виконання.",
      "Кінець останньої завершеної свічки, доступної моделі.",
      "Момент завершення прогнозного періоду.",
      "Умовна ціна виконання на відкритті поточного дня.",
      "Ціна відкриття наступного дня.",
      "Логарифмічна дохідність від поточного open до наступного open.",
      "Дохідність close-to-close останньої завершеної доби.",
      "Дохідність close-to-close із лагом дві доби.",
      "Дохідність close-to-close із лагом сім діб.",
      "Середня close-to-close дохідність за сім завершених діб.",
      "Середня close-to-close дохідність за тридцять завершених діб.",
      "Стандартне відхилення close-to-close дохідності за сім діб.",
      "Стандартне відхилення close-to-close дохідності за тридцять діб.",
      "Логарифмічний діапазон high до low попередньої доби.",
      "Зміна обороту попередньої доби відносно доби перед нею.",
      "Частка taker buy в обороті попередньої доби.",
      "Хронологічна вибірка train, validation або test."
    ),
    stringsAsFactors = FALSE
  )
}

btc_build_model_data <- function(config = btc_model_default_config()) {
  if (config$train_fraction <= 0 ||
      config$validation_fraction <= 0 ||
      config$train_fraction + config$validation_fraction >= 1) {
    stop("Частки train, validation і test задано некоректно.", call. = FALSE)
  }

  cost_values <- c(
    config$fee_bps,
    config$half_spread_bps,
    config$slippage_bps
  )

  if (any(!is.finite(cost_values)) || any(cost_values < 0)) {
    stop(
      "Припущення про торгові витрати мають бути невід'ємними числами.",
      call. = FALSE
    )
  }

  dir.create(
    config$output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  source_data <- btc_model_read_daily(config)
  model <- btc_model_prepare(source_data$data, config)
  paths <- btc_model_paths(config)
  split_summary <- btc_model_split_summary(model)
  metadata <- btc_model_metadata(config, source_data$input_path)
  dictionary <- btc_model_dictionary()

  if (!setequal(dictionary$column, names(model))) {
    stop(
      "Словник колонок не відповідає модельній таблиці.",
      call. = FALSE
    )
  }
  model_quality <- rbind(
    source_data$quality,
    data.frame(
      metric = c(
        "removed_warmup_gap_or_target_rows",
        "final_model_rows"
      ),
      value = c(
        nrow(source_data$data) - nrow(model),
        nrow(model)
      ),
      stringsAsFactors = FALSE
    )
  )

  saveRDS(
    model,
    paths$data_rds,
    compress = "gzip"
  )

  utils::write.csv(
    model,
    paths$data_csv,
    row.names = FALSE,
    na = ""
  )

  utils::write.csv(
    metadata,
    paths$metadata_csv,
    row.names = FALSE,
    na = ""
  )

  utils::write.csv(
    dictionary,
    paths$dictionary_csv,
    row.names = FALSE,
    na = ""
  )

  utils::write.csv(
    split_summary,
    paths$split_summary_csv,
    row.names = FALSE,
    na = ""
  )

  utils::write.csv(
    model_quality,
    paths$quality_summary_csv,
    row.names = FALSE,
    na = ""
  )

  list(
    data = model,
    split_summary = split_summary,
    metadata = metadata,
    dictionary = dictionary,
    quality = model_quality,
    paths = paths
  )
}
