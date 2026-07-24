# Simple console progress for data downloads ------------------------------

format_download_progress <- function(
  label,
  current,
  total,
  unit,
  width = 24L
) {
  ratio <- current / total
  filled <- if (current >= total) {
    width
  } else {
    floor(width * ratio)
  }
  bar <- paste0(
    strrep("\u2588", filled),
    strrep("\u2591", width - filled)
  )

  sprintf(
    "%s  [%s] %3d%%  %d/%d %s",
    label,
    bar,
    floor(100 * ratio),
    current,
    total,
    unit
  )
}

download_progress_lapply <- function(
  values,
  function_to_apply,
  workers = 1L,
  label = "Завантаження",
  unit = "частин"
) {
  total <- length(values)
  if (total == 0L) {
    return(list())
  }

  workers <- suppressWarnings(as.integer(workers))
  if (length(workers) != 1L || is.na(workers) || workers < 1L) {
    stop("Кількість паралельних процесів має бути додатною.")
  }
  workers <- min(workers, total)

  terminal_output <- isTRUE(tryCatch(
    isatty(stdout()),
    error = function(error) FALSE
  ))
  last_milestone <- -1L
  terminal_line_open <- FALSE

  show_progress <- function(current, force = FALSE) {
    percent <- floor(100 * current / total)
    milestone <- floor(percent / 10L) * 10L
    line <- format_download_progress(
      label = label,
      current = current,
      total = total,
      unit = unit
    )

    if (terminal_output) {
      cat("\r", line, sep = "")
      flush.console()
      terminal_line_open <<- TRUE
    } else if (
      isTRUE(force) ||
        milestone > last_milestone ||
        current >= total
    ) {
      cat(line, "\n", sep = "")
      flush.console()
      last_milestone <<- milestone
    }
  }

  show_progress(0L, force = TRUE)
  on.exit(
    if (terminal_line_open) {
      cat("\n")
    },
    add = TRUE
  )

  results <- vector("list", total)
  indices <- seq_len(total)
  waves <- split(indices, ceiling(indices / workers))

  for (wave_indices in waves) {
    wave_values <- values[wave_indices]
    wave_results <- if (
      .Platform$OS.type == "unix" &&
        workers > 1L &&
        length(wave_indices) > 1L
    ) {
      parallel::mclapply(
        wave_values,
        function_to_apply,
        mc.cores = min(workers, length(wave_indices)),
        mc.preschedule = FALSE
      )
    } else {
      lapply(wave_values, function_to_apply)
    }

    results[wave_indices] <- wave_results
    show_progress(max(wave_indices))
  }

  if (terminal_output) {
    cat("\n")
    terminal_line_open <- FALSE
  }
  results
}
