# Phase 13 — Batch reference workflow validation
#
# Purpose:
# Validate a small, deterministic batch workflow using synthetic
# annual-maximum-like data for multiple stations.
#
# This script is a validation artifact, not a user-facing package feature.
# It does not change the public API or package scope.

suppressPackageStartupMessages({
  library(ALEA)
})

# ---- Setup ------------------------------------------------------------------

output_dir <- file.path("validation", "reference_outputs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file <- file.path(output_dir, "phase13_batch_reference_summary.csv")

# ---- Synthetic data ----------------------------------------------------------

station_a <- c(
  42.1, 39.4, 51.7, 48.3, 55.2,
  60.1, 46.8, 53.9, 58.4, 62.7,
  49.5, 57.8, 64.3, 52.6, 59.9,
  61.5, 67.2, 54.8, 63.1, 69.4
)

station_b <- c(
  75.2, 81.4, 79.8, 88.1, 91.5,
  84.7, 95.3, 89.6, 93.2, 97.8,
  86.5, 92.1, 99.4, 94.6, 101.2,
  90.7, 96.8, 103.5, 98.9, 105.1
)

station_c <- c(
  28.5, 32.1, 35.4, 31.8, 37.6,
  40.2, 36.5, 39.7, 42.3, 38.9,
  41.1, 44.6, 43.2, 45.8, 47.5,
  46.1, 49.3, 48.7, 50.5, 52.2
)

batch_data <- data.frame(
  station = rep(c("A", "B", "C"), each = 20),
  year = rep(2001:2020, times = 3),
  value = c(station_a, station_b, station_c)
)

return_period <- c(10, 50, 100)

# ---- Batch workflow ----------------------------------------------------------

batch <- alea_batch_fit(
  data = batch_data,
  station = "station",
  time = "year",
  value = "value",
  distributions = c("gev", "gum"),
  methods = c("lmom"),
  return_period = return_period,
  gof = TRUE,
  diagnostics = TRUE,
  select = "ai",
  quiet = TRUE
)

stations <- alea_results(batch, "stations")
fits <- alea_results(batch, "fits")
return_levels <- alea_results(batch, "return_levels")
gof <- alea_results(batch, "gof")
diagnostics <- alea_results(batch, "diagnostics")
selection <- alea_results(batch, "selection")
selected_models <- alea_results(batch, "selected_models")
errors <- alea_results(batch, "errors")

# ---- Validation checks -------------------------------------------------------

stopifnot(inherits(batch, "alea_batch"))

stopifnot(is.data.frame(stations))
stopifnot(is.data.frame(fits))
stopifnot(is.data.frame(return_levels))
stopifnot(is.data.frame(gof))
stopifnot(is.data.frame(diagnostics))
stopifnot(is.data.frame(selection))
stopifnot(is.data.frame(selected_models))
stopifnot(is.data.frame(errors))

stopifnot(nrow(stations) == 3)
stopifnot(setequal(stations$station, c("A", "B", "C")))

# 3 stations x 2 distributions x 1 method
stopifnot(nrow(fits) == 6)
stopifnot(all(fits$status == "ok"))
stopifnot(setequal(fits$station, c("A", "B", "C")))
stopifnot(setequal(fits$distribution, c("gev", "gum")))
stopifnot(setequal(fits$method, "lmom"))

# 3 stations x 2 fitted models x 3 return periods
stopifnot(nrow(return_levels) == 18)
stopifnot(setequal(return_levels$return_period, return_period))
stopifnot(all(is.finite(return_levels$return_level)))

# 3 stations x 2 fitted models x 6 GOF statistics
stopifnot(nrow(gof) == 36)
stopifnot(setequal(gof$statistic, c("ks", "cvm", "ad", "loglik", "aic", "bic")))
stopifnot(all(is.finite(gof$estimate)))

# Diagnostics should be non-empty and station-tagged.
stopifnot(nrow(diagnostics) > 0)
stopifnot(setequal(diagnostics$station, c("A", "B", "C")))

# AI selection is station-level.
stopifnot(nrow(selection) == 3)
stopifnot(setequal(selection$station, c("A", "B", "C")))
stopifnot(all(selection$selected_distribution %in% c("gev", "gpa", "pe3", "ln2", "ln3", "gum")))

# Selected models are available only when AI-selected distribution was among
# the fitted candidate distributions. Since this validation fits only GEV/GUM,
# selected_models may have fewer than 3 rows.
stopifnot(all(selected_models$station %in% c("A", "B", "C")))
stopifnot(all(selected_models$distribution %in% c("gev", "gum")))

# No errors are expected for this controlled synthetic case.
stopifnot(nrow(errors) == 0)
stopifnot(identical(
  names(errors),
  c("station", "step", "distribution", "method", "message", "class")
))

# ---- Compact reference output ------------------------------------------------

reference <- data.frame(
  case_id = "P13-BATCH-001",
  check = c(
    "batch_class",
    "stations_rows",
    "fits_rows",
    "return_levels_rows",
    "gof_rows",
    "diagnostics_non_empty",
    "selection_rows",
    "selected_models_valid",
    "errors_rows",
    "errors_columns"
  ),
  observed = c(
    inherits(batch, "alea_batch"),
    nrow(stations),
    nrow(fits),
    nrow(return_levels),
    nrow(gof),
    nrow(diagnostics),
    nrow(selection),
    all(selected_models$distribution %in% c("gev", "gum")),
    nrow(errors),
    paste(names(errors), collapse = ",")
  ),
  expected = c(
    "TRUE",
    "3",
    "6",
    "18",
    "36",
    ">0",
    "3",
    "TRUE",
    "0",
    "station,step,distribution,method,message,class"
  ),
  passed = c(
    inherits(batch, "alea_batch"),
    nrow(stations) == 3,
    nrow(fits) == 6,
    nrow(return_levels) == 18,
    nrow(gof) == 36,
    nrow(diagnostics) > 0,
    nrow(selection) == 3,
    all(selected_models$distribution %in% c("gev", "gum")),
    nrow(errors) == 0,
    identical(names(errors), c("station", "step", "distribution", "method", "message", "class"))
  )
)

stopifnot(all(reference$passed))

write.csv(
  reference,
  file = output_file,
  row.names = FALSE
)

message("Phase 13 batch reference workflow validation completed successfully.")
message("Reference output written to: ", output_file)

print(reference)