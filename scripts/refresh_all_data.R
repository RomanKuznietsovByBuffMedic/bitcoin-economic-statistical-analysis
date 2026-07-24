#!/usr/bin/env Rscript

steps <- c(
  "Binance для початкового порівняння" =
    "scripts/refresh_binance_cache.R",
  "Bybit як основне джерело" =
    "scripts/refresh_bybit_cache.R",
  "Bitstamp як контрольне джерело" =
    "scripts/refresh_bitstamp_cache.R",
  "Підготовка аналітичного набору" =
    "scripts/prepare_price_returns.R",
  "Фінальна перевірка" =
    "scripts/check_data_and_split.R"
)

rscript <- file.path(R.home("bin"), "Rscript")

for (step_index in seq_along(steps)) {
  script <- unname(steps[[step_index]])
  cat(
    "\n[",
    step_index,
    "/",
    length(steps),
    "] ",
    names(steps)[[step_index]],
    "\n",
    sep = ""
  )
  status <- system2(rscript, script)
  if (!isTRUE(status == 0L)) {
    stop("Скрипт завершився з помилкою: ", script)
  }
  cat("[ГОТОВО] ", names(steps)[[step_index]], "\n", sep = "")
}

cat("\nУсі ринкові дані оновлено й перевірено.\n")
