#!/usr/bin/env Rscript

# Visible end-to-end report. All validation logic lives in R/data_pipeline.R.

source("R/project_config.R")
source("R/project_io.R")
source("R/data_provenance.R")
source("R/hourly_ohlc_quality.R")
source("R/time_split.R")
source("R/data_pipeline.R")

config <- read_project_config()
checks <- collect_project_data_checks(config)

cat("\nПАРАМЕТРИ\n")
print(checks$parameters, n = Inf, width = Inf)
cat("\nШЛЯХ ДАНИХ\n")
print(checks$data_lineage, n = Inf, width = Inf)
cat("\nМЕТАДАНІ ДЖЕРЕЛ\n")
print(checks$provenance, n = Inf, width = Inf)
cat("\nЦІЛІСНІСТЬ ФАЙЛІВ\n")
print(checks$manifest, n = Inf, width = Inf)
cat("\nЧАСОВИЙ ПОДІЛ РЯДКІВ\n")
print(checks$time_split, n = Inf, width = Inf)
cat("\nОДНОКРОКОВІ ПРОГНОЗНІ ВИПАДКИ\n")
print(checks$forecast_split, n = Inf, width = Inf)
cat("\nУсі перевірки даних і часового поділу пройдено.\n")
