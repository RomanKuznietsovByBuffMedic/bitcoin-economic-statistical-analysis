# Project file input and output -------------------------------------------

sha256_file <- function(path) {
  if (!file.exists(path)) {
    stop("Не знайдено файл для SHA-256: ", path)
  }

  executable <- Sys.which("sha256sum")
  if (!nzchar(executable)) {
    stop("Для контрольної суми потрібна системна команда sha256sum.")
  }

  output <- system2(executable, path, stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    stop("Не вдалося обчислити SHA-256 для файлу: ", path)
  }

  strsplit(trimws(output[[1]]), "[[:space:]]+")[[1]][[1]]
}

read_rds_required <- function(path, description = "RDS-файл") {
  if (!file.exists(path)) {
    stop(
      "Не знайдено ",
      description,
      ": ",
      path
    )
  }

  readRDS(path)
}

save_rds_atomic <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  temporary_file <- tempfile(
    pattern = paste0(basename(path), "."),
    tmpdir = dirname(path),
    fileext = ".rds"
  )
  on.exit(unlink(temporary_file), add = TRUE)

  saveRDS(
    object,
    temporary_file,
    compress = "gzip",
    version = 3
  )

  verification_copy <- readRDS(temporary_file)
  if (!identical(verification_copy, object)) {
    stop("Перевірка нового RDS-файлу завершилася помилкою: ", path)
  }

  if (!file.rename(temporary_file, path)) {
    stop("Не вдалося атомарно замінити RDS-файл: ", path)
  }

  invisible(path)
}
