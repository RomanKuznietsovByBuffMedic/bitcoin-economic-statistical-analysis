# Quarto book setup -------------------------------------------------------

load_book_environment <- function(
  packages,
  modules,
  envir = parent.frame()
) {
  missing_packages <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages) > 0L) {
    stop(
      "Не встановлено пакети: ",
      paste(missing_packages, collapse = ", "),
      ". Виконайте renv::restore()."
    )
  }

  missing_modules <- modules[!file.exists(modules)]
  if (length(missing_modules) > 0L) {
    stop(
      "Не знайдено модулі: ",
      paste(missing_modules, collapse = ", ")
    )
  }

  for (module in modules) {
    sys.source(module, envir = envir)
  }

  invisible(modules)
}
