# Перевірка даних Binance і створення інтерактивних графіків
#
# Файл потрібно зберегти у:
# scripts/02_validate_and_build_charts.R
#
# Запуск із кореня RStudio Project:
# source("scripts/02_validate_and_build_charts.R", encoding = "UTF-8")
#
# Скрипт:
# 1. Оновлює швидке Parquet-сховище лише за наявності нових архівів.
# 2. Читає компактні 1h, 4h і 1d ряди через DuckDB.
# 3. Не завантажує весь хвилинний набір у пам'ять.
# 4. Читає звіти якості.
# 5. Створює підсумки пропусків і неповних свічок.
# 6. Зберігає окремі набори лише з повними свічками.
# 7. Створює інтерактивні графіки ціни та обсягу.
# 8. Зберігає графіки локально у data/visualization.
#
# Скрипт нічого не встановлює автоматично.

options(stringsAsFactors = FALSE)

chart_height = 860L
chart_margins = list(
  l = 80,
  r = 35,
  t = 80,
  b = 60
)


required_packages = c(
  "data.table",
  "DBI",
  "digest",
  "duckdb",
  "jsonlite",
  "plotly",
  "htmlwidgets"
)

missing_packages = required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "Відсутні пакети: ",
      paste(missing_packages, collapse = ", "),
      ". Встановіть їх через renv::install(missing_packages), ",
      "після чого запустіть скрипт повторно."
    ),
    call. = FALSE
  )
}

project_root = normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

base_pipeline_file = file.path(
  project_root,
  "R",
  "binance_data.R"
)

fast_pipeline_file = file.path(
  project_root,
  "R",
  "binance_data_fast.R"
)

if (!all(file.exists(c(base_pipeline_file, fast_pipeline_file)))) {
  stop(
    paste0(
      "Не знайдено R/binance_data.R або R/binance_data_fast.R. ",
      "Відкрийте правильний RStudio Project."
    ),
    call. = FALSE
  )
}

source(
  base_pipeline_file,
  encoding = "UTF-8"
)

source(
  fast_pipeline_file,
  encoding = "UTF-8"
)

config = btc_fast_default_config(
  project_root = project_root
)

latest_available_date = btc_resolve_end_date(
  config
)

btc_fast = btc_fast_update(
  config = config,
  update = TRUE
)

btc_1h = btc_fast$data_1h
btc_4h = btc_fast$data_4h
btc_1d = btc_fast$data_1d

local_end_date = max(
  as.Date(btc_fast$manifest$end_date),
  na.rm = TRUE
)

update_required = identical(
  btc_fast$source,
  "fast_updated_store"
)

validation_dir = config$validation_dir

quality_file = file.path(
  validation_dir,
  "quality_report.csv"
)

gaps_file = file.path(
  validation_dir,
  "missing_intervals.csv"
)

api_file = file.path(
  validation_dir,
  "api_sample_check.csv"
)

required_validation_files = c(
  quality_file,
  gaps_file,
  api_file
)

if (!all(file.exists(required_validation_files))) {
  stop(
    paste0(
      "Не знайдено всі звіти перевірки у ",
      validation_dir,
      ". Спочатку запустіть scripts/03_update_binance_fast.R."
    ),
    call. = FALSE
  )
}

quality_report = utils::read.csv(
  quality_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

missing_intervals = utils::read.csv(
  gaps_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

api_sample_check = utils::read.csv(
  api_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if (
  nrow(missing_intervals) > 0L &&
  "missing_minutes" %in% names(missing_intervals)
) {
  missing_intervals$missing_minutes = as.numeric(
    missing_intervals$missing_minutes
  )
}

gap_summary = data.frame(
  metric = c(
    "gap_count",
    "missing_minutes",
    "largest_gap_minutes"
  ),
  value = c(
    nrow(missing_intervals),
    if (nrow(missing_intervals) > 0L) {
      sum(
        missing_intervals$missing_minutes,
        na.rm = TRUE
      )
    } else {
      0
    },
    if (nrow(missing_intervals) > 0L) {
      max(
        missing_intervals$missing_minutes,
        na.rm = TRUE
      )
    } else {
      0
    }
  ),
  stringsAsFactors = FALSE
)

api_checked = if ("checked" %in% names(api_sample_check)) {
  sum(
    api_sample_check$checked %in% TRUE,
    na.rm = TRUE
  )
} else {
  0L
}

api_matched = if (
  all(c("checked", "matched") %in% names(api_sample_check))
) {
  sum(
    api_sample_check$checked %in% TRUE &
      api_sample_check$matched %in% TRUE,
    na.rm = TRUE
  )
} else {
  0L
}

api_failed = api_checked - api_matched

api_summary = data.frame(
  metric = c(
    "api_checked",
    "api_matched",
    "api_failed"
  ),
  value = c(
    api_checked,
    api_matched,
    api_failed
  ),
  stringsAsFactors = FALSE
)

make_interval_summary = function(
  data,
  interval_name
) {
  complete = data$is_complete %in% TRUE

  data.frame(
    interval = interval_name,
    rows = nrow(data),
    complete_rows = sum(
      complete,
      na.rm = TRUE
    ),
    incomplete_rows = sum(
      !complete,
      na.rm = TRUE
    ),
    complete_percent = round(
      100 * mean(
        complete,
        na.rm = TRUE
      ),
      6
    ),
    first_open_time = format(
      min(
        data$open_time,
        na.rm = TRUE
      ),
      tz = "UTC",
      usetz = TRUE
    ),
    last_open_time = format(
      max(
        data$open_time,
        na.rm = TRUE
      ),
      tz = "UTC",
      usetz = TRUE
    ),
    stringsAsFactors = FALSE
  )
}

interval_summary = rbind(
  make_interval_summary(
    btc_1h,
    "1h"
  ),
  make_interval_summary(
    btc_4h,
    "4h"
  ),
  make_interval_summary(
    btc_1d,
    "1d"
  )
)

btc_1h_complete = btc_1h[
  btc_1h$is_complete %in% TRUE,
  ,
  drop = FALSE
]

btc_4h_complete = btc_4h[
  btc_4h$is_complete %in% TRUE,
  ,
  drop = FALSE
]

btc_1d_complete = btc_1d[
  btc_1d$is_complete %in% TRUE,
  ,
  drop = FALSE
]

utils::write.csv(
  gap_summary,
  file.path(
    validation_dir,
    "gap_summary.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

utils::write.csv(
  api_summary,
  file.path(
    validation_dir,
    "api_summary.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

utils::write.csv(
  interval_summary,
  file.path(
    validation_dir,
    "incomplete_bars_summary.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

saveRDS(
  btc_1h_complete,
  file.path(
    config$processed_dir,
    "BTCUSDT_1h_complete.rds"
  ),
  compress = "gzip"
)

saveRDS(
  btc_4h_complete,
  file.path(
    config$processed_dir,
    "BTCUSDT_4h_complete.rds"
  ),
  compress = "gzip"
)

saveRDS(
  btc_1d_complete,
  file.path(
    config$processed_dir,
    "BTCUSDT_1d_complete.rds"
  ),
  compress = "gzip"
)

visualization_dir = file.path(
  project_root,
  "data",
  "visualization",
  "binance",
  "spot",
  "BTCUSDT"
)

dir.create(
  visualization_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

filter_recent_complete = function(
  data,
  days = NULL
) {
  result = data[
    data$is_complete %in% TRUE,
    ,
    drop = FALSE
  ]

  if (
    !is.null(days) &&
    is.finite(days) &&
    days > 0
  ) {
    last_time = max(
      result$open_time,
      na.rm = TRUE
    )

    first_time = last_time -
      as.numeric(days) * 24 * 60 * 60

    result = result[
      result$open_time >= first_time,
      ,
      drop = FALSE
    ]
  }

  rownames(result) = NULL
  result
}

build_candlestick_volume_chart = function(
  data,
  title,
  output_file,
  days = NULL
) {
  chart_data = filter_recent_complete(
    data = data,
    days = days
  )

  if (nrow(chart_data) == 0L) {
    stop(
      "Після фільтрації не залишилося даних для графіка: ",
      title,
      call. = FALSE
    )
  }

  price_chart = plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    type = "candlestick",
    open = ~open,
    high = ~high,
    low = ~low,
    close = ~close,
    name = "BTCUSDT"
  )

  price_chart = plotly::layout(
    price_chart,
    yaxis = list(
      title = "Ціна, USDT"
    ),
    xaxis = list(
      title = "",
      rangeslider = list(
        visible = FALSE
      ),
      rangeselector = list(
        buttons = list(
          list(
            count = 7,
            label = "7 днів",
            step = "day",
            stepmode = "backward"
          ),
          list(
            count = 30,
            label = "30 днів",
            step = "day",
            stepmode = "backward"
          ),
          list(
            count = 90,
            label = "90 днів",
            step = "day",
            stepmode = "backward"
          ),
          list(
            step = "all",
            label = "Увесь період"
          )
        )
      )
    )
  )

  volume_chart = plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~quote_volume,
    type = "bar",
    name = "Обсяг, USDT",
    hoverinfo = "x+y"
  )

  volume_chart = plotly::layout(
    volume_chart,
    yaxis = list(
      title = "Обсяг, USDT"
    ),
    xaxis = list(
      title = "Час, UTC"
    )
  )

  combined_chart = plotly::subplot(
    price_chart,
    volume_chart,
    nrows = 2,
    heights = c(
      0.76,
      0.24
    ),
    shareX = TRUE,
    titleY = TRUE,
    margin = 0.03
  )

  combined_chart = plotly::layout(
    combined_chart,
    title = list(
      text = title
    ),
    showlegend = FALSE,
    hovermode = "x unified",
    height = chart_height,
    autosize = TRUE,
    margin = chart_margins
  )

  combined_chart = plotly::config(
    combined_chart,
    displaylogo = FALSE,
    responsive = TRUE,
    scrollZoom = TRUE
  )

  combined_chart = plotly::partial_bundle(
    combined_chart
  )

  output_path = file.path(
    visualization_dir,
    output_file
  )

  library_dir = paste0(
    tools::file_path_sans_ext(
      basename(output_path)
    ),
    "_files"
  )

  htmlwidgets::saveWidget(
    widget = combined_chart,
    file = output_path,
    selfcontained = FALSE,
    libdir = library_dir,
    title = title
  )

  output_path
}

build_close_chart = function(
  data,
  title,
  output_file
) {
  chart_data = filter_recent_complete(
    data = data,
    days = NULL
  )

  close_chart = plotly::plot_ly(
    data = chart_data,
    x = ~open_time,
    y = ~close,
    type = "scatter",
    mode = "lines",
    name = "Ціна закриття",
    hoverinfo = "x+y"
  )

  close_chart = plotly::layout(
    close_chart,
    title = list(
      text = title
    ),
    xaxis = list(
      title = "Час, UTC",
      rangeslider = list(
        visible = TRUE
      )
    ),
    yaxis = list(
      title = "Ціна, USDT"
    ),
    showlegend = FALSE,
    hovermode = "x unified",
    height = chart_height,
    autosize = TRUE,
    margin = chart_margins
  )

  close_chart = plotly::config(
    close_chart,
    displaylogo = FALSE,
    responsive = TRUE,
    scrollZoom = TRUE
  )

  close_chart = plotly::partial_bundle(
    close_chart
  )

  output_path = file.path(
    visualization_dir,
    output_file
  )

  library_dir = paste0(
    tools::file_path_sans_ext(
      basename(output_path)
    ),
    "_files"
  )

  htmlwidgets::saveWidget(
    widget = close_chart,
    file = output_path,
    selfcontained = FALSE,
    libdir = library_dir,
    title = title
  )

  output_path
}

chart_files = c(
  build_candlestick_volume_chart(
    data = btc_1h,
    title = "BTCUSDT, годинні свічки за останні 90 днів",
    output_file = "BTCUSDT_1h_90d.html",
    days = 90
  ),
  build_candlestick_volume_chart(
    data = btc_4h,
    title = "BTCUSDT, чотиригодинні свічки за останній рік",
    output_file = "BTCUSDT_4h_365d.html",
    days = 365
  ),
  build_candlestick_volume_chart(
    data = btc_1d,
    title = "BTCUSDT, денні свічки за весь період",
    output_file = "BTCUSDT_1d_full.html",
    days = NULL
  ),
  build_close_chart(
    data = btc_1d,
    title = "BTCUSDT, повна історія ціни закриття",
    output_file = "BTCUSDT_1d_close_full.html"
  )
)

relative_chart_files = basename(
  chart_files
)

index_lines = c(
  "<!doctype html>",
  "<html lang=\"uk\">",
  "<head>",
  "  <meta charset=\"utf-8\">",
  "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
  "  <title>BTCUSDT, інтерактивні графіки</title>",
  "</head>",
  "<body>",
  "  <h1>BTCUSDT, інтерактивні графіки</h1>",
  "  <ol>",
  paste0(
    "    <li><a href=\"",
    relative_chart_files,
    "\">",
    relative_chart_files,
    "</a></li>"
  ),
  "  </ol>",
  "</body>",
  "</html>"
)

index_file = file.path(
  visualization_dir,
  "index.html"
)

writeLines(
  index_lines,
  con = index_file,
  useBytes = TRUE
)

run_summary = data.frame(
  metric = c(
    "latest_available_date",
    "local_end_date",
    "data_updated",
    "rows_1h",
    "rows_4h",
    "rows_1d",
    "complete_rows_1h",
    "complete_rows_4h",
    "complete_rows_1d",
    "gap_count",
    "missing_minutes",
    "api_failed",
    "chart_index"
  ),
  value = c(
    as.character(latest_available_date),
    as.character(
      max(
        as.Date(
          btc_fast$manifest$end_date
        ),
        na.rm = TRUE
      )
    ),
    as.character(update_required),
    as.character(nrow(btc_1h)),
    as.character(nrow(btc_4h)),
    as.character(nrow(btc_1d)),
    as.character(nrow(btc_1h_complete)),
    as.character(nrow(btc_4h_complete)),
    as.character(nrow(btc_1d_complete)),
    as.character(gap_summary$value[
      gap_summary$metric == "gap_count"
    ]),
    as.character(gap_summary$value[
      gap_summary$metric == "missing_minutes"
    ]),
    as.character(api_failed),
    normalizePath(
      index_file,
      winslash = "/",
      mustWork = TRUE
    )
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  run_summary,
  file.path(
    validation_dir,
    "visualization_run_summary.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

message("")
message("Перевірку завершено.")
message("Підсумок інтервалів:")
print(
  interval_summary,
  row.names = FALSE
)

message("")
message("Підсумок пропусків:")
print(
  gap_summary,
  row.names = FALSE
)

message("")
message("Підсумок API-перевірки:")
print(
  api_summary,
  row.names = FALSE
)

message("")
message("Графіки збережено у:")
message(
  normalizePath(
    visualization_dir,
    winslash = "/",
    mustWork = TRUE
  )
)

message("")
message("Головна сторінка графіків:")
message(
  normalizePath(
    index_file,
    winslash = "/",
    mustWork = TRUE
  )
)

if (interactive()) {
  utils::browseURL(
    index_file
  )
}

message("")
message("Об'єкти, доступні в RStudio:")
message(
  "btc_1h, btc_4h, btc_1d, ",
  "btc_1h_complete, btc_4h_complete, btc_1d_complete, ",
  "quality_report, missing_intervals, api_sample_check, ",
  "gap_summary, api_summary, interval_summary"
)
