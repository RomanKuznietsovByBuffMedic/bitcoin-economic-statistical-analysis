# Швидке оновлення Binance BTCUSDT
#
# Перший запуск:
# 1. Перетворює перевірені ZIP-архіви на окремі Parquet-файли.
# 2. Використовує два процеси для незалежних архівів.
# 3. Формує 5m, 15m, 30m, 1h, 2h, 4h, 6h, 12h і 1d.
# 4. Зберігає 1h, 4h і 1d також як RDS для Quarto-книги.
#
# Наступні запуски:
# 1. Якщо нових завершених днів немає, лише підключають локальні дані.
# 2. Якщо нові архіви є, перетворюють лише відсутні Parquet-частини.
#
# Пакети автоматично не встановлюються.

source(
  file.path(
    "R",
    "binance_data.R"
  ),
  encoding = "UTF-8"
)

source(
  file.path(
    "R",
    "binance_data_fast.R"
  ),
  encoding = "UTF-8"
)

config = btc_fast_default_config(
  project_root = "."
)

config$end_date = "latest"
config$workers = 2L
config$data_table_threads = 2L
config$duckdb_threads = 2L
config$duckdb_memory_limit = "4GB"
config$force_verify_all = FALSE

btc_fast = btc_fast_update(
  config = config,
  update = TRUE
)

btc_1h = btc_fast$data_1h
btc_4h = btc_fast$data_4h
btc_1d = btc_fast$data_1d

btc_query_1m = function(
  start_time,
  end_time,
  columns = c(
    "open_time",
    "open",
    "high",
    "low",
    "close",
    "base_volume",
    "quote_volume"
  ),
  limit = Inf
) {
  btc_fast_query_1m(
    config = config,
    start_time = start_time,
    end_time = end_time,
    columns = columns,
    limit = limit
  )
}

message("")
message(
  "Джерело підключення: ",
  btc_fast$source
)

message(
  "1h: ",
  format(
    nrow(btc_1h),
    big.mark = " "
  ),
  " рядків"
)

message(
  "4h: ",
  format(
    nrow(btc_4h),
    big.mark = " "
  ),
  " рядків"
)

message(
  "1d: ",
  format(
    nrow(btc_1d),
    big.mark = " "
  ),
  " рядків"
)

message("")
message(
  "Хвилинні дані не завантажено повністю в пам'ять."
)

message(
  "Для вибірки використовуйте btc_query_1m()."
)
