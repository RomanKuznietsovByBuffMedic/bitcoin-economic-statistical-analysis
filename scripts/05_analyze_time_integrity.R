project_root = normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

source(
  file.path("R", "time_integrity.R"),
  encoding = "UTF-8"
)

integrity = btc_time_integrity_load(project_root)
output_dir = integrity$paths$validation_dir

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

utils::write.csv(
  integrity$summary,
  file.path(output_dir, "time_integrity_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

utils::write.csv(
  integrity$largest_gaps,
  file.path(output_dir, "largest_time_gaps.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

utils::write.csv(
  integrity$yearly,
  file.path(output_dir, "time_gaps_by_year.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

utils::write.csv(
  integrity$interval_table,
  file.path(output_dir, "time_integrity_intervals.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("Аналіз часової цілісності завершено.\n")
cat("Розривів:", nrow(integrity$gaps), "\n")
cat(
  "Відсутніх хвилин:",
  sum(integrity$gaps$missing_minutes, na.rm = TRUE),
  "\n"
)
cat(
  "Часове покриття, %:",
  round(integrity$coverage_percent, 6),
  "\n"
)
cat("Звіти:", output_dir, "\n")
