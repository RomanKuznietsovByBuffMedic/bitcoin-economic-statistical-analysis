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

expected_prepared_metadata <- function(config, raw_sha256) {
  list(
    data_source = config$primary$market_label,
    primary_exchange_id = config$primary$id,
    market_symbol = config$primary$symbol,
    market_type = config$study$market_type,
    interval = config$study$interval,
    price_field = config$study$price_field,
    period_start_utc = format_utc(
      config$study$data_start,
      include_seconds = TRUE
    ),
    period_end_exclusive_utc = format_utc(
      config$study$data_end_exclusive,
      include_seconds = TRUE
    ),
    raw_sha256 = normalize_sha256(
      raw_sha256,
      "SHA-256 основного кешу"
    ),
    test_start_utc = format_utc(
      config$evaluation$test_start,
      include_seconds = TRUE
    ),
    test_end_exclusive_utc = format_utc(
      config$evaluation$test_end_exclusive,
      include_seconds = TRUE
    )
  )
}

attach_prepared_metadata <- function(
  data,
  config,
  raw_sha256
) {
  metadata <- expected_prepared_metadata(config, raw_sha256)
  for (field in names(metadata)) {
    attr(data, field) <- metadata[[field]]
  }

  data
}

check_prepared_metadata <- function(data, config, raw_sha256) {
  expected <- expected_prepared_metadata(config, raw_sha256)
  field_matches <- vapply(
    names(expected),
    function(field) {
      identical(
        canonical_metadata_value(attr(data, field, exact = TRUE)),
        canonical_metadata_value(expected[[field]])
      )
    },
    logical(1)
  )

  list(
    matches = all(field_matches),
    field_matches = field_matches
  )
}

require_prepared_metadata <- function(data, config, raw_sha256) {
  check <- check_prepared_metadata(data, config, raw_sha256)
  if (!isTRUE(check$matches)) {
    mismatched_fields <- names(check$field_matches)[
      !check$field_matches
    ]
    stop(
      paste(
        "Метадані підготовленого набору не відповідають",
        "поточному config.yml або основному кешу."
      ),
      " Невідповідні поля: ",
      paste(mismatched_fields, collapse = ", "),
      ". Виконайте Rscript scripts/prepare_price_returns.R."
    )
  }

  invisible(check)
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
  primary_file <- config$paths$cache[[config$primary$id]]
  primary_raw_sha256 <- sha256_file(primary_file)
  require_prepared_metadata(
    data = prepared,
    config = config,
    raw_sha256 = primary_raw_sha256
  )
  prepared_entry <- list(
    role = "prepared",
    path = config$paths$prepared,
    sha256 = sha256_file(config$paths$prepared),
    raw_sha256 = primary_raw_sha256
  )

  list(
    schema_version = 1L,
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
  if (
    length(manifest$schema_version) != 1L ||
      !identical(as.integer(manifest$schema_version), 1L)
  ) {
    stop("Непідтримувана schema_version у manifest.yml.")
  }
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

  primary_file <- config$paths$cache[[config$primary$id]]
  primary_raw_sha256 <- sha256_file(primary_file)
  prepared <- read_rds_required(
    config$paths$prepared,
    "підготовлений набір"
  )
  require_prepared_metadata(
    data = prepared,
    config = config,
    raw_sha256 = primary_raw_sha256
  )
  if (
    !identical(
      normalize_sha256(
        manifest$files$prepared$raw_sha256,
        "SHA-256 вхідного кешу в manifest.yml"
      ),
      primary_raw_sha256
    )
  ) {
    stop(
      paste(
        "SHA-256 вхідного кешу в manifest.yml",
        "не відповідає поточному основному кешу."
      )
    )
  }

  expected_paths <- c(
    config$paths$cache,
    list(prepared = config$paths$prepared)
  )
  if (
    !identical(
      sort(names(manifest$files)),
      sort(names(expected_paths))
    )
  ) {
    stop("Склад файлів у manifest.yml не відповідає проєкту.")
  }
  checks <- lapply(
    names(expected_paths),
    function(file_id) {
      expected_path <- expected_paths[[file_id]]
      manifest_entry <- manifest$files[[file_id]]
      raw_entry <- !identical(file_id, "prepared")
      file_exists <- file.exists(expected_path)
      role_matches <- identical(
        as.character(manifest_entry$role),
        if (raw_entry) "raw" else "prepared"
      )
      exchange_matches <- !raw_entry || identical(
        as.character(manifest_entry$exchange_id),
        file_id
      )
      verification_matches <- !raw_entry || identical(
        as.character(manifest_entry$verification_level),
        "verified_on_download"
      )
      path_matches <- !is.null(manifest_entry$path) &&
        identical(as.character(manifest_entry$path), expected_path)
      manifest_sha256 <- if (is.null(manifest_entry$sha256)) {
        NA_character_
      } else {
        tryCatch(
          normalize_sha256(
            manifest_entry$sha256,
            paste("SHA-256 файла", file_id, "у manifest.yml")
          ),
          error = function(error) NA_character_
        )
      }
      sha_matches <- file_exists &&
        !is.na(manifest_sha256) &&
        identical(
          manifest_sha256,
          sha256_file(expected_path)
        )

      tibble::tibble(
        `Файл` = file_id,
        `Шлях` = expected_path,
        `Існує` = file_exists,
        `Роль коректна` = role_matches,
        `Біржа коректна` = exchange_matches,
        `Походження перевірено` = verification_matches,
        `Шлях відповідає маніфесту` = path_matches,
        `SHA-256 відповідає маніфесту` = sha_matches
      )
    }
  )
  result <- dplyr::bind_rows(checks)

  if (
    !all(result$`Існує`) ||
      !all(result$`Роль коректна`) ||
      !all(result$`Біржа коректна`) ||
      !all(result$`Походження перевірено`) ||
      !all(result$`Шлях відповідає маніфесту`) ||
      !all(result$`SHA-256 відповідає маніфесту`)
  ) {
    stop("Перевірка локальних файлів за manifest.yml не пройдена.")
  }

  result
}

data_lineage_table <- function(config) {
  source_description <- function(exchange) {
    paste(
      exchange$market_label,
      config$study$interval,
      config$study$timezone
    )
  }

  tibble::tibble(
    `Етап` = c(
      "1. Початковий аудит",
      "2. Основне джерело",
      "3. Незалежний контроль",
      "4. Підготовка",
      "5. Часовий поділ",
      "6. Прогнозний випадок"
    ),
    `Звідки` = c(
      source_description(config$candidate),
      source_description(config$primary),
      source_description(config$reference),
      config$paths$cache[[config$primary$id]],
      config$paths$prepared,
      "Ознаки, відомі на кінець години t"
    ),
    `Що відбувається` = c(
      "Завантаження й перевірка OHLCV та часової сітки",
      "Завантаження й повна перевірка OHLCV та часової сітки",
      "Окрема перевірка загального руху ціни",
      paste(
        "Обчислення turnover, simple_return_1h",
        "і log_return_1h; перевірка SHA-256"
      ),
      paste(
        "Послідовний поділ без перемішування;",
        "межі беруться з config.yml"
      ),
      paste(
        "Прогноз цілі для t+1;",
        "роль вибірки визначає target_time"
      )
    ),
    `Результат і роль` = c(
      paste(
        config$paths$cache[[config$candidate$id]],
        "- лише аудит, не вхід моделі"
      ),
      paste(
        config$paths$cache[[config$primary$id]],
        "- єдиний ряд, що йде далі"
      ),
      paste(
        config$paths$cache[[config$reference$id]],
        "- лише контроль, не вхід моделі"
      ),
      paste(
        config$paths$prepared,
        "- єдиний підготовлений набір"
      ),
      "exploration / validation / test",
      "target_time + прогноз; оцінка лише на однакових ключах"
    )
  )
}
