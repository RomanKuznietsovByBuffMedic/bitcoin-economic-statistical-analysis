# Запуск отримання або підключення даних Binance BTCUSDT

source(file.path("R", "binance_data.R"), encoding = "UTF-8")

config <- btc_default_config(project_root = ".")

# Значення latest означає останній доступний повний день.
# Для фіксованого зрізу можна вказати, наприклад:
# config$end_date <- as.Date("2026-06-30")
config$end_date <- "latest"

# TRUE:
# перевіряє наявність нових архівів;
# не завантажує повторно вже перевірені архіви;
# підключає готові RDS, якщо змін немає.
#
# FALSE:
# не звертається до мережі;
# підключає вже створені локальні RDS.
btc <- btc_load_or_update(
  config = config,
  update = TRUE
)

btc_1m <- btc$data_1m
btc_1h <- btc$data_1h
btc_4h <- btc$data_4h
btc_1d <- btc$data_1d

message("Джерело підключення: ", btc$source)
message("1m: ", nrow(btc_1m), " рядків")
message("1h: ", nrow(btc_1h), " рядків")
message("4h: ", nrow(btc_4h), " рядків")
message("1d: ", nrow(btc_1d), " рядків")
