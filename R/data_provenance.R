# Data-source provenance and local integrity ------------------------------
#
# Source metadata records what was requested from each exchange. A separate
# manifest binds those metadata to exact local files through SHA-256.

exchange_endpoints <- function(exchange) {
  endpoint_fields <- intersect(
    c("archive_base_url", "rest_endpoint", "endpoint"),
    names(exchange)
  )
  endpoints <- unlist(
    exchange[endpoint_fields],
    recursive = TRUE,
    use.names = TRUE
  )
  stats::setNames(as.character(endpoints), names(endpoints))
}

expected_source_metadata <- function(config, exchange_id) {
  if (!exchange_id %in% names(config$exchanges)) {
    stop("Невідома біржа для метаданих: ", exchange_id)
  }

  exchange <- config$exchanges[[exchange_id]]
  list(
    schema_version = "1",
    exchange_id = exchange$id,
    exchange_name = exchange$name,
    market_type = config$study$market_type,
    symbol = exchange$symbol,
    base_currency = exchange$base_currency,
    quote_currency = exchange$quote_currency,
    interval = config$study$interval,
    timezone = config$study$timezone,
    requested_start_utc = format_utc(
      config$study$data_start,
      include_seconds = TRUE
    ),
    available_start_utc = format_utc(
      exchange$available_start,
      include_seconds = TRUE
    ),
    data_end_exclusive_utc = format_utc(
      config$study$data_end_exclusive,
      include_seconds = TRUE
    ),
    endpoints = exchange_endpoints(exchange)
  )
}

attach_source_metadata <- function(
  data,
  config,
  exchange_id,
  verification_level
) {
  verification_level <- require_config_value(
    verification_level,
    "verification_level"
  )
  metadata <- expected_source_metadata(config, exchange_id)
  metadata$verification_level <- verification_level
  metadata$recorded_at_utc <- format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S UTC",
    tz = "UTC"
  )
  attr(data, "source_metadata") <- metadata
  data
}

canonical_metadata_value <- function(value) {
  if (is.null(value)) {
    return("<missing>")
  }
  value <- unlist(value, recursive = TRUE, use.names = TRUE)
  if (length(value) == 0L) {
    return("")
  }
  value_names <- names(value)
  if (is.null(value_names)) {
    value_names <- rep("", length(value))
  }
  paste(value_names, as.character(value), sep = "=", collapse = "|")
}

check_source_metadata <- function(data, config, exchange_id) {
  expected <- expected_source_metadata(config, exchange_id)
  actual <- attr(data, "source_metadata")

  field_matches <- vapply(
    names(expected),
    function(field) {
      identical(
        canonical_metadata_value(actual[[field]]),
        canonical_metadata_value(expected[[field]])
      )
    },
    logical(1)
  )

  list(
    matches = !is.null(actual) && all(field_matches),
    field_matches = field_matches,
    verification_level = if (is.null(actual$verification_level)) {
      "missing"
    } else {
      as.character(actual$verification_level)
    }
  )
}

source_metadata_summary <- function(config, data_by_exchange) {
  exchange_ids <- names(data_by_exchange)
  unknown_ids <- setdiff(exchange_ids, names(config$exchanges))
  if (length(unknown_ids) > 0L) {
    stop(
      "У перевірці є невідомі біржі: ",
      paste(unknown_ids, collapse = ", ")
    )
  }

  rows <- lapply(
    exchange_ids,
    function(exchange_id) {
      exchange <- config$exchanges[[exchange_id]]
      check <- check_source_metadata(
        data = data_by_exchange[[exchange_id]],
        config = config,
        exchange_id = exchange_id
      )

      verification_label <- switch(
        check$verification_level,
        verified_on_download = "Перевірено під час завантаження",
        legacy_cache =
          "Старий кеш, ідентичність відповіді API не записано",
        missing = "Метаданих немає",
        check$verification_level
      )

      tibble::tibble(
        `Джерело` = exchange$name,
        `Ринок` = config$study$market_type,
        `Пара` = paste0(
          exchange$base_currency,
          "/",
          exchange$quote_currency
        ),
        `Символ API` = exchange$symbol,
        `Інтервал` = config$study$interval,
        `Офіційні адреси` = paste(
          exchange_endpoints(exchange),
          collapse = "\n"
        ),
        `Метадані відповідають config.yml` = check$matches,
        `Походження кешу` = verification_label
      )
    }
  )

  result <- dplyr::bind_rows(rows)
  if (!all(result$`Метадані відповідають config.yml`)) {
    stop(
      paste(
        "Метадані локального кешу не відповідають config.yml.",
        "Оновіть дані перед рендером."
      )
    )
  }

  result
}

manifest_data_spec <- function(config) {
  list(
    market_type = config$study$market_type,
    interval = config$study$interval,
    timezone = config$study$timezone,
    price_field = config$study$price_field,
    data_start_utc = format_utc(
      config$study$data_start,
      include_seconds = TRUE
    ),
    data_end_exclusive_utc = format_utc(
      config$study$data_end_exclusive,
      include_seconds = TRUE
    ),
    data_years = config$study$data_years,
    candidate_exchange = config$candidate$id,
    primary_exchange = config$primary$id,
    reference_exchange = config$reference$id
  )
}

build_data_manifest <- function(config) {
  cache_entries <- lapply(
    names(config$paths$cache),
    function(exchange_id) {
      path <- config$paths$cache[[exchange_id]]
      data <- read_rds_required(path, paste("кеш", exchange_id))
      metadata <- attr(data, "source_metadata")

      list(
        role = "raw",
        exchange_id = exchange_id,
        path = path,
        sha256 = sha256_file(path),
        verification_level = metadata$verification_level
      )
    }
  )
  names(cache_entries) <- names(config$paths$cache)

  prepared <- read_rds_required(
    config$paths$prepared,
    "підготовлений набір"
  )
  prepared_entry <- list(
    role = "prepared",
    path = config$paths$prepared,
    sha256 = sha256_file(config$paths$prepared),
    raw_sha256 = attr(prepared, "raw_sha256")
  )

  list(
    schema_version = 1L,
    generated_at_utc = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S UTC",
      tz = "UTC"
    ),
    data_spec = manifest_data_spec(config),
    files = c(cache_entries, list(prepared = prepared_entry))
  )
}

write_data_manifest <- function(
  config,
  path = "data-manifest.yml"
) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Для створення manifest.yml потрібен пакет yaml.")
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary_file <- tempfile(
    pattern = paste0(basename(path), "."),
    tmpdir = dirname(path),
    fileext = ".yml"
  )
  on.exit(unlink(temporary_file), add = TRUE)

  yaml::write_yaml(
    build_data_manifest(config),
    temporary_file
  )
  yaml::read_yaml(temporary_file)

  if (!file.rename(temporary_file, path)) {
    stop("Не вдалося атомарно замінити маніфест: ", path)
  }

  invisible(path)
}

validate_data_manifest <- function(
  config,
  path = "data-manifest.yml"
) {
  if (!file.exists(path)) {
    stop(
      "Не знайдено ",
      path,
      ". Виконайте Rscript scripts/prepare_price_returns.R."
    )
  }

  manifest <- yaml::read_yaml(path)
  expected_spec <- manifest_data_spec(config)
  if (
    !identical(
      canonical_metadata_value(manifest$data_spec),
      canonical_metadata_value(expected_spec)
    )
  ) {
    stop(
      paste(
        "Параметри даних у manifest.yml не відповідають config.yml.",
        "Перебудуйте підготовлений набір."
      )
    )
  }

  expected_paths <- c(
    config$paths$cache,
    list(prepared = config$paths$prepared)
  )
  checks <- lapply(
    names(expected_paths),
    function(file_id) {
      expected_path <- expected_paths[[file_id]]
      manifest_entry <- manifest$files[[file_id]]
      file_exists <- file.exists(expected_path)
      path_matches <- !is.null(manifest_entry$path) &&
        identical(as.character(manifest_entry$path), expected_path)
      sha_matches <- file_exists &&
        !is.null(manifest_entry$sha256) &&
        identical(
          as.character(manifest_entry$sha256),
          sha256_file(expected_path)
        )

      tibble::tibble(
        `Файл` = file_id,
        `Шлях` = expected_path,
        `Існує` = file_exists,
        `Шлях відповідає маніфесту` = path_matches,
        `SHA-256 відповідає маніфесту` = sha_matches
      )
    }
  )
  result <- dplyr::bind_rows(checks)

  if (
    !all(result$`Існує`) ||
      !all(result$`Шлях відповідає маніфесту`) ||
      !all(result$`SHA-256 відповідає маніфесту`)
  ) {
    stop("Перевірка локальних файлів за manifest.yml не пройдена.")
  }

  result
}
