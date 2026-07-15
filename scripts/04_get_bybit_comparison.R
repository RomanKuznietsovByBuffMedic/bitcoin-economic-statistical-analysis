# Завантаження контрольних рядів Bybit BTCUSDT

source(
  file.path(
    "R",
    "bybit_data.R"
  ),
  encoding = "UTF-8"
)

config = bybit_default_config(
  project_root = "."
)

bybit = bybit_update(
  config
)

bybit_4h = bybit$data_4h
bybit_1d = bybit$data_1d

message("")
message(
  "Джерело: ",
  bybit$source
)

message(
  "4h: ",
  format(
    nrow(bybit_4h),
    big.mark = " "
  ),
  " рядків"
)

message(
  "1d: ",
  format(
    nrow(bybit_1d),
    big.mark = " "
  ),
  " рядків"
)

message("")
message(
  "Після завершення виконайте quarto render, ",
  "щоб у книзі з'явився порівняльний графік."
)
