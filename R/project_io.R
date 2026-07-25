# Project file input and output -------------------------------------------

normalize_sha256 <- function(value, description = "SHA-256") {
  if (is.null(value)) {
    stop("Не знайдено значення ", description, ".")
  }

  components <- unlist(
    value,
    recursive = TRUE,
    use.names = FALSE
  )
  components <- unclass(as.character(components))
  normalized <- tolower(gsub(
    "[[:space:]:]",
    "",
    paste0(components, collapse = "")
  ))

  if (
    length(normalized) != 1L ||
      is.na(normalized) ||
      !grepl("^[0-9a-f]{64}$", normalized)
  ) {
    stop(
      description,
      " має бути 64-символьним шістнадцятковим SHA-256."
    )
  }

  unname(as.character(normalized))
}

sha256_file <- function(path) {
  if (!file.exists(path)) {
    stop("Не знайдено файл для SHA-256: ", path)
  }
  if (!requireNamespace("openssl", quietly = TRUE)) {
    stop(
      "Для SHA-256 потрібен пакет openssl. ",
      "Виконайте renv::restore()."
    )
  }

  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)

  normalize_sha256(
    openssl::sha256(connection),
    paste("SHA-256 файла", path)
  )
}

validate_time_range <- function(
  start_time,
  end_time,
  context = "завантаження"
) {
  valid_time <- function(value) {
    inherits(value, "POSIXt") &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(as.numeric(value))
  }

  if (
    !valid_time(start_time) ||
      !valid_time(end_time) ||
      start_time >= end_time
  ) {
    stop("Некоректні часові межі для ", context, ".")
  }

  invisible(list(
    start_time = start_time,
    end_time = end_time
  ))
}

with_file_lock <- function(
  target_path,
  action,
  timeout_seconds = 300,
  stale_after_seconds = 4 * 60 * 60,
  poll_seconds = 0.2
) {
  if (!is.function(action)) {
    stop("with_file_lock() очікує функцію без аргументів.")
  }
  target_path <- as.character(target_path)
  if (
    length(target_path) != 1L ||
      is.na(target_path) ||
      !nzchar(target_path)
  ) {
    stop("Шлях до файла для блокування має бути одним непорожнім рядком.")
  }

  lock_parameter_values <- list(
    timeout_seconds,
    stale_after_seconds,
    poll_seconds
  )
  lock_parameters <- suppressWarnings(as.numeric(
    unlist(lock_parameter_values, use.names = FALSE)
  ))
  if (
    any(lengths(lock_parameter_values) != 1L) ||
      length(lock_parameters) != 3L ||
      anyNA(lock_parameters) ||
      any(!is.finite(lock_parameters)) ||
      lock_parameters[[1L]] < 0 ||
      lock_parameters[[2L]] <= 0 ||
      lock_parameters[[3L]] <= 0
  ) {
    stop("Некоректні часові параметри файлового блокування.")
  }
  timeout_seconds <- lock_parameters[[1L]]
  stale_after_seconds <- lock_parameters[[2L]]
  poll_seconds <- lock_parameters[[3L]]

  lock_path <- paste0(target_path, ".lock")
  dir.create(dirname(lock_path), recursive = TRUE, showWarnings = FALSE)

  started_at <- Sys.time()
  owner_token <- paste(
    Sys.getpid(),
    format(started_at, "%Y%m%d%H%M%OS6", tz = "UTC"),
    sep = "-"
  )
  owner_file <- file.path(lock_path, "owner")
  acquired <- FALSE

  release_lock <- function() {
    if (!acquired || !dir.exists(lock_path)) {
      return(invisible(FALSE))
    }

    recorded_owner <- tryCatch(
      readLines(owner_file, n = 1L, warn = FALSE),
      error = function(error) character()
    )
    if (identical(recorded_owner, owner_token)) {
      unlink(lock_path, recursive = TRUE, force = TRUE)
      return(invisible(TRUE))
    }

    invisible(FALSE)
  }
  on.exit(release_lock(), add = TRUE)

  repeat {
    acquired <- dir.create(lock_path, showWarnings = FALSE)
    if (acquired) {
      owner_write <- tryCatch(
        {
          writeLines(owner_token, owner_file, useBytes = TRUE)
          NULL
        },
        error = identity
      )
      if (inherits(owner_write, "error")) {
        unlink(lock_path, recursive = TRUE, force = TRUE)
        acquired <- FALSE
        stop(
          "Не вдалося записати власника блокування для файла ",
          target_path,
          ": ",
          conditionMessage(owner_write)
        )
      }
      break
    }

    lock_info <- file.info(lock_path)
    lock_age <- as.numeric(
      difftime(Sys.time(), lock_info$mtime, units = "secs")
    )
    if (
      dir.exists(lock_path) &&
        is.finite(lock_age) &&
        lock_age > stale_after_seconds
    ) {
      unlink(lock_path, recursive = TRUE, force = TRUE)
      next
    }

    elapsed <- as.numeric(
      difftime(Sys.time(), started_at, units = "secs")
    )
    if (elapsed >= timeout_seconds) {
      stop(
        "Не вдалося отримати блокування для файлу: ",
        target_path
      )
    }

    Sys.sleep(poll_seconds)
  }

  action()
}

with_file_locks <- function(target_paths, action, ...) {
  if (!is.function(action)) {
    stop("with_file_locks() очікує функцію без аргументів.")
  }

  target_paths <- sort(unique(as.character(target_paths)))
  target_paths <- target_paths[nzchar(target_paths)]
  if (length(target_paths) == 0L) {
    return(action())
  }

  acquire_next <- function(index) {
    if (index > length(target_paths)) {
      return(action())
    }

    with_file_lock(
      target_path = target_paths[[index]],
      action = function() acquire_next(index + 1L),
      ...
    )
  }

  acquire_next(1L)
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
