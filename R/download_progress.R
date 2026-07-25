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

download_json_with_retry <- function(
  url,
  attempts = 3L,
  initial_pause_seconds = 0.5,
  maximum_pause_seconds = 8,
  context = "HTTP-запит",
  simplify_vector = TRUE,
  simplify_data_frame = TRUE,
  simplify_matrix = FALSE,
  response_check = NULL
) {
  url <- as.character(url)
  if (
    length(url) != 1L ||
      is.na(url) ||
      !nzchar(trimws(url))
  ) {
    stop("URL має бути одним непорожнім рядком.")
  }
  attempts <- suppressWarnings(as.numeric(attempts))
  if (
    length(attempts) != 1L ||
      is.na(attempts) ||
      !is.finite(attempts) ||
      attempts < 1L ||
      attempts != floor(attempts)
  ) {
    stop("Кількість повторних спроб має бути додатною.")
  }
  attempts <- as.integer(attempts)

  pauses <- suppressWarnings(as.numeric(c(
    initial_pause_seconds,
    maximum_pause_seconds
  )))
  if (
    length(pauses) != 2L ||
      anyNA(pauses) ||
      any(!is.finite(pauses)) ||
      any(pauses < 0)
  ) {
    stop("Паузи між повторними спробами мають бути невід'ємними.")
  }
  initial_pause_seconds <- pauses[[1L]]
  maximum_pause_seconds <- pauses[[2L]]
  if (!is.null(response_check) && !is.function(response_check)) {
    stop("response_check має бути функцією або NULL.")
  }

  last_error <- "невідома помилка"
  for (attempt in seq_len(attempts)) {
    retry_after <- NA_real_
    response <- tryCatch(
      httr::GET(url, httr::timeout(30)),
      error = identity
    )

    if (inherits(response, "error")) {
      retryable <- TRUE
      last_error <- conditionMessage(response)
    } else {
      status <- httr::status_code(response)
      retry_header <- httr::headers(response)[["retry-after"]]
      if (!is.null(retry_header)) {
        retry_after <- suppressWarnings(as.numeric(retry_header))
        if (
          length(retry_after) != 1L ||
            is.na(retry_after) ||
            !is.finite(retry_after) ||
            retry_after < 0
        ) {
          retry_after <- NA_real_
        }
      }

      if (status >= 200L && status < 300L) {
        parsed <- tryCatch(
          jsonlite::fromJSON(
            httr::content(
              response,
              as = "text",
              encoding = "UTF-8"
            ),
            simplifyVector = simplify_vector,
            simplifyDataFrame = simplify_data_frame,
            simplifyMatrix = simplify_matrix
          ),
          error = identity
        )
        if (inherits(parsed, "error")) {
          retryable <- TRUE
          last_error <- paste(
            "некоректний JSON:",
            conditionMessage(parsed)
          )
        } else {
          check <- if (is.null(response_check)) {
            NULL
          } else {
            response_check(parsed)
          }
          if (is.null(check)) {
            return(parsed)
          }
          if (
            !is.list(check) ||
              !is.logical(check$retryable) ||
              length(check$retryable) != 1L ||
              is.na(check$retryable) ||
              length(check$message) != 1L ||
              is.na(check$message) ||
              !nzchar(as.character(check$message))
          ) {
            stop(
              paste(
                "response_check має повернути NULL або список",
                "з полями retryable і message."
              )
            )
          }
          retryable <- check$retryable
          last_error <- as.character(check$message)
        }
      } else {
        retryable <- status == 429L || status >= 500L
        status_text <- tryCatch(
          httr::http_status(response)$message,
          error = function(error) ""
        )
        last_error <- trimws(paste("HTTP", status, status_text))
      }
    }

    if (!isTRUE(retryable)) {
      stop(context, ": ", last_error)
    }
    if (attempt < attempts) {
      pause <- if (is.finite(retry_after)) {
        retry_after
      } else {
        min(
          maximum_pause_seconds,
          initial_pause_seconds * 2^(attempt - 1L)
        )
      }
      if (pause > 0) {
        Sys.sleep(pause)
      }
    }
  }

  stop(
    context,
    " після ",
    attempts,
    " спроб: ",
    last_error
  )
}

download_task_value <- function(values, index) {
  if (is.list(values)) {
    return(values[[index]])
  }

  values[index]
}

run_download_task <- function(index, value, function_to_apply) {
  tryCatch(
    list(
      ok = TRUE,
      index = index,
      value = function_to_apply(value),
      error = NULL
    ),
    error = function(error) {
      list(
        ok = FALSE,
        index = index,
        value = NULL,
        error = conditionMessage(error)
      )
    }
  )
}

download_task_description <- function(value, max_characters = 120L) {
  description <- paste(
    capture.output(str(value, give.attr = FALSE)),
    collapse = " "
  )
  description <- gsub("[[:space:]]+", " ", trimws(description))

  if (nchar(description) > max_characters) {
    paste0(substr(description, 1L, max_characters - 3L), "...")
  } else {
    description
  }
}

stop_for_download_failures <- function(
  task_results,
  values,
  label,
  task_indices = seq_along(task_results),
  maximum_examples = 5L
) {
  failed <- which(!vapply(
    task_results,
    function(result) is.list(result) && isTRUE(result$ok),
    logical(1)
  ))
  if (length(failed) == 0L) {
    return(invisible(FALSE))
  }

  examples <- failed[seq_len(min(length(failed), maximum_examples))]
  details <- vapply(
    examples,
    function(position) {
      result <- task_results[[position]]
      index <- if (is.list(result) && !is.null(result$index)) {
        result$index
      } else {
        task_indices[[position]]
      }
      error_text <- if (
        is.list(result) &&
          !is.null(result$error)
      ) {
        result$error
      } else {
        paste(as.character(result), collapse = " ")
      }
      value <- download_task_description(
        download_task_value(values, index)
      )

      paste0(
        "- елемент ",
        index,
        " (",
        value,
        "): ",
        error_text
      )
    },
    character(1)
  )

  stop(
    label,
    ": не виконано ",
    length(failed),
    " елементів у поточній хвилі.\n",
    paste(details, collapse = "\n")
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
  progress_state <- new.env(parent = emptyenv())
  progress_state$last_milestone <- -1L
  progress_state$terminal_line_open <- FALSE

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
      progress_state$terminal_line_open <- TRUE
    } else if (
      isTRUE(force) ||
        milestone > progress_state$last_milestone ||
        current >= total
    ) {
      cat(line, "\n", sep = "")
      flush.console()
      progress_state$last_milestone <- milestone
    }
  }

  show_progress(0L, force = TRUE)
  on.exit(
    if (progress_state$terminal_line_open) {
      cat("\n")
    },
    add = TRUE
  )

  results <- vector("list", total)
  indices <- seq_len(total)
  waves <- split(indices, ceiling(indices / workers))

  for (wave_indices in waves) {
    wave_tasks <- lapply(
      wave_indices,
      function(index) {
        list(
          index = index,
          value = download_task_value(values, index)
        )
      }
    )
    run_task <- function(task) {
      run_download_task(
        index = task$index,
        value = task$value,
        function_to_apply = function_to_apply
      )
    }

    wave_results <- if (
      .Platform$OS.type == "unix" &&
        workers > 1L &&
        length(wave_indices) > 1L
    ) {
      parallel::mclapply(
        wave_tasks,
        run_task,
        mc.cores = min(workers, length(wave_indices)),
        mc.preschedule = FALSE
      )
    } else {
      lapply(wave_tasks, run_task)
    }

    stop_for_download_failures(
      task_results = wave_results,
      values = values,
      label = label,
      task_indices = wave_indices
    )
    results[wave_indices] <- lapply(
      wave_results,
      `[[`,
      "value"
    )
    show_progress(max(wave_indices))
  }

  if (terminal_output) {
    cat("\n")
    progress_state$terminal_line_open <- FALSE
  }
  results
}
