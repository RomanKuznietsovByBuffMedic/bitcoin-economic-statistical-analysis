# Побудова чесної денної таблиці для моделювання і бектесту
#
# Перед запуском мають існувати оброблені денні дані Binance.
# Їх створює scripts/01_get_binance_data.R або
# scripts/03_update_binance_fast.R.

source(
  file.path("R", "model_data.R"),
  encoding = "UTF-8"
)

config = btc_model_default_config(
  project_root = "."
)

# Це початкові припущення, а не підтверджені фактичні витрати.
# Перед реальним тестом їх потрібно замінити умовами власного акаунта.
config$fee_bps = 10
config$half_spread_bps = 2
config$slippage_bps = 3

result = btc_build_model_data(config)
btc_model_data = result$data

message("")
message(
  "Створено модельний набір: ",
  format(nrow(btc_model_data), big.mark = " "),
  " рядків."
)

print(
  result$split_summary,
  row.names = FALSE
)

message("")
message(
  "RDS: ",
  result$paths$data_rds
)
message(
  "CSV: ",
  result$paths$data_csv
)
message(
  "Словник колонок: ",
  result$paths$dictionary_csv
)
