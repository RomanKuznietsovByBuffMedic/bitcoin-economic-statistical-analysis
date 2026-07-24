#!/usr/bin/env Rscript

# First-render bootstrap --------------------------------------------------
#
# Quarto runs this script before executing any book chapter. It uses only
# base R until the exact project library has been restored from renv.lock.

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
    paste(
      "Запустіть quarto render з кореня проєкту.",
      "Не знайдено:"
    ),
    " ",
    paste(missing_root_files, collapse = ", ")
  )
}

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)
Sys.setenv(RENV_PROJECT = project_root)

install_workers <- suppressWarnings(as.integer(Sys.getenv(
  "PROJECT_INSTALL_WORKERS",
  "2"
)))
if (is.na(install_workers) || install_workers < 1L) {
  stop(
    "PROJECT_INSTALL_WORKERS має бути додатним цілим числом."
  )
}
Sys.setenv(MAKEFLAGS = paste0("-j", install_workers))

cat("\nПІДГОТОВКА ПРОЄКТУ ПЕРЕД РЕНДЕРОМ\n")
cat("1/3. Перевірка renv і R-пакетів.\n")

if (!requireNamespace("renv", quietly = TRUE)) {
  sys.source(
    file.path("renv", "activate.R"),
    envir = globalenv()
  )
}
if (!requireNamespace("renv", quietly = TRUE)) {
  stop(
    paste(
      "Не вдалося автоматично встановити renv.",
      "Перевірте доступ до https://cloud.r-project.org."
    )
  )
}

renv::load(project = project_root)
renv::restore(
  project = project_root,
  prompt = FALSE
)

description <- read.dcf(
  "DESCRIPTION",
  fields = "Imports"
)[1L, 1L]
required_packages <- trimws(
  strsplit(description, ",", fixed = TRUE)[[1L]]
)
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
    "Після renv::restore() бракує пакетів: ",
    paste(missing_packages, collapse = ", ")
  )
}

cat(
  "2/3. Перевірка та підготовка ринкових даних.\n"
)
sys.source(
  file.path("scripts", "ensure_project_data.R"),
  envir = new.env(parent = globalenv())
)

cat(
  paste(
    "3/3. Середовище й дані готові.",
    "Починається рендер книги.\n\n"
  )
)
