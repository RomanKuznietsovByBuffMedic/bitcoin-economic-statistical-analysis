#!/usr/bin/env Rscript

# Validate the project before rendering ----------------------------------
#
# Rendering is read-only: it does not restore packages, download market data
# or rewrite the local manifest.

options(warn = 1)

required_root_files <- c(
  "_quarto.yml",
  "DESCRIPTION",
  "renv.lock",
  file.path("renv", "activate.R")
)
missing_root_files <- required_root_files[
  !file.exists(required_root_files)
]
if (length(missing_root_files) > 0L) {
  stop(
    "Запустіть quarto render з кореня проєкту. Не знайдено: ",
    paste(missing_root_files, collapse = ", ")
  )
}

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)
Sys.setenv(RENV_PROJECT = project_root)

cat("\nПЕРЕВІРКА ПРОЄКТУ ПЕРЕД РЕНДЕРОМ\n")
cat("1/3. R-пакети.\n")

if (!requireNamespace("renv", quietly = TRUE)) {
  sys.source(
    file.path("renv", "activate.R"),
    envir = globalenv()
  )
}
if (!requireNamespace("renv", quietly = TRUE)) {
  stop(
    paste(
      "Не знайдено renv.",
      "Виконайте Rscript -e 'renv::restore(prompt = FALSE)'."
    )
  )
}
renv::load(project = project_root)

imports <- read.dcf("DESCRIPTION", fields = "Imports")[1L, 1L]
required_packages <- trimws(strsplit(imports, ",", fixed = TRUE)[[1L]])
required_packages <- sub(
  "[[:space:]]*\\(.*\\)$",
  "",
  required_packages
)
missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]
if (length(missing_packages) > 0L) {
  stop(
    "Бракує пакетів: ",
    paste(missing_packages, collapse = ", "),
    ". Виконайте Rscript -e 'renv::restore(prompt = FALSE)'."
  )
}

run_check <- function(path) {
  status <- system2(file.path(R.home("bin"), "Rscript"), path)
  if (!isTRUE(status == 0L)) {
    stop("Перевірка завершилася з помилкою: ", path)
  }
}

cat("2/3. Детерміновані контракти.\n")
run_check("scripts/check_contracts.R")

cat("3/3. Дані, manifest і часовий поділ.\n")
run_check("scripts/check_data_and_split.R")

cat("\nПроєкт готовий. Починається рендер книги.\n\n")
