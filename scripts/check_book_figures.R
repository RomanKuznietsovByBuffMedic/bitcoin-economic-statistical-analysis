#!/usr/bin/env Rscript

# Structural checks for public figures ----------------------------------
#
# This script checks only the book structure. It does not inspect market
# values and does not decide whether any statistical model is suitable.

book_files <- c(
  "02-data-acquisition.qmd",
  "03-price-and-returns.qmd",
  "04-before-model.qmd",
  "05-naive-benchmark.qmd"
)

count_matches <- function(text, pattern) {
  matches <- gregexpr(pattern, text, perl = TRUE)[[1L]]
  if (identical(matches[[1L]], -1L)) 0L else length(matches)
}

read_book_text <- function(path) {
  paste(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
}

results <- lapply(
  book_files,
  function(path) {
    text <- read_book_text(path)
    data.frame(
      file = path,
      figures = count_matches(text, "#\\| label: fig-"),
      introductions = count_matches(
        text,
        "book_figure_intro\\("
      ),
      author_comments = count_matches(
        text,
        "book_figure_comment\\("
      ),
      review_blocks = count_matches(
        text,
        "book_figure_review\\("
      ),
      obsolete_includes = count_matches(
        text,
        "reader-figure-comment\\.qmd"
      ),
      stringsAsFactors = FALSE
    )
  }
)
results <- do.call(rbind, results)
results$response_blocks <-
  results$author_comments + results$review_blocks

if (any(results$figures != results$introductions)) {
  stop("Не кожен публічний графік має book_figure_intro().")
}
if (any(results$figures != results$response_blocks)) {
  stop(
    paste(
      "Після кожного публічного графіка має бути рівно один",
      "авторський коментар або блок локального перегляду."
    )
  )
}
if (any(results$obsolete_includes != 0L)) {
  stop("Залишилися застарілі include-блоки коментарів.")
}

exploration_text <- read_book_text("04-before-model.qmd")
exploration_row <- results[
  results$file == "04-before-model.qmd",
  ,
  drop = FALSE
]
if (
  exploration_row$figures == 0L ||
    exploration_row$author_comments != 0L ||
    exploration_row$review_blocks != exploration_row$figures
) {
  stop(
    paste(
      "Дослідницький розділ має завершувати кожен графік",
      "питаннями для локального перегляду, а не готовим висновком."
    )
  )
}

benchmark_row <- results[
  results$file == "05-naive-benchmark.qmd",
  ,
  drop = FALSE
]
if (
  benchmark_row$figures == 0L ||
    benchmark_row$author_comments != 0L ||
    benchmark_row$review_blocks != benchmark_row$figures
) {
  stop(
    paste(
      "Розділ наївного еталона має пояснювати механізм",
      "і завершувати графік питаннями, а не вибором моделі."
    )
  )
}

forbidden_calls <- c(
  "book_model_decision\\(",
  "analyze_training_time_series\\(",
  "diagnose_ar1_candidate\\(",
  "model_suitability_table\\(",
  "plot_ar_order_selection\\(",
  "plot_arma_order_selection\\("
)
present_forbidden <- forbidden_calls[vapply(
  forbidden_calls,
  function(pattern) {
    grepl(pattern, exploration_text, perl = TRUE)
  },
  logical(1)
)]
if (length(present_forbidden) > 0L) {
  stop(
    "Дослідницький розділ містить автоматичний модельний виклик: ",
    paste(present_forbidden, collapse = ", ")
  )
}

label_matches <- gregexpr(
  "#\\| label: ([^\\n]+)",
  exploration_text,
  perl = TRUE
)[[1L]]
labels <- if (identical(label_matches[[1L]], -1L)) {
  character()
} else {
  sub(
    "^#\\| label: ",
    "",
    regmatches(exploration_text, list(label_matches))[[1L]]
  )
}
if (length(labels) == 0L || anyDuplicated(labels)) {
  stop("Мітки дослідницького розділу порожні або повторюються.")
}

quarto_config <- yaml::read_yaml("_quarto.yml")
expected_chapters <- c(
  "index.qmd",
  "01-how-it-works.qmd",
  "02-data-acquisition.qmd",
  "03-price-and-returns.qmd",
  "04-before-model.qmd",
  "05-naive-benchmark.qmd",
  "references.qmd"
)
if (!identical(quarto_config$book$chapters, expected_chapters)) {
  stop(
    paste(
      "Склад розділів у _quarto.yml не відповідає",
      "поточній структурі книги."
    )
  )
}

print(
  results[
    ,
    c(
      "file",
      "figures",
      "introductions",
      "author_comments",
      "review_blocks"
    )
  ],
  row.names = FALSE
)
cat(
  paste(
    "\nСтруктуру графіків перевірено.",
    "Рішень щодо придатності моделей не обчислювалося.\n"
  )
)
