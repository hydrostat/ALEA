# Phase 13 — Independent lmom reference comparisons
#
# Purpose:
# Validate selected ALEA-R distribution wrappers and fitted return-level
# calculations against direct lmom reference calculations.
#
# This script is a validation artifact, not a user-facing package feature.
# It does not change the public API or package scope.

suppressPackageStartupMessages({
  library(ALEA)
  library(lmom)
})

# ---- Setup ------------------------------------------------------------------

output_dir <- file.path("validation", "reference_outputs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file <- file.path(output_dir, "phase13_lmom_reference_comparisons.csv")

tolerance <- 1e-8

return_period <- c(2, 5, 10, 25, 50, 100)
probability <- 1 - 1 / return_period

# Deterministic synthetic annual-maximum-like sample.
x <- c(
  42.1, 39.4, 51.7, 48.3, 55.2,
  60.1, 46.8, 53.9, 58.4, 62.7,
  49.5, 57.8, 64.3, 52.6, 59.9,
  61.5, 67.2, 54.8, 63.1, 69.4
)

# ---- Helper -----------------------------------------------------------------

compare_lmom_reference <- function(distribution, fit_fun, qua_fun) {
  fit <- alea_fit(
    x,
    distribution = distribution,
    method = "lmom"
  )
  
  rl <- alea_return_level(
    fit,
    return_period = return_period
  )
  
  lmoments <- lmom::samlmu(x)
  para <- fit_fun(lmoments)
  
  reference <- qua_fun(probability, para)
  
  data.frame(
    case_id = "P13-LMOM-REF-001",
    distribution = distribution,
    method = "lmom",
    return_period = return_period,
    probability_alea = rl$probability,
    probability_reference = probability,
    return_level_alea = rl$return_level,
    return_level_reference = reference,
    absolute_difference = abs(rl$return_level - reference),
    tolerance = tolerance,
    passed = abs(rl$return_level - reference) <= tolerance
  )
}

# ---- Comparisons -------------------------------------------------------------
#
# These comparisons are intentionally restricted to stable lmom-supported
# distributions with direct pel*/qua* reference functions.

comparisons <- do.call(
  rbind,
  list(
    compare_lmom_reference(
      distribution = "gev",
      fit_fun = lmom::pelgev,
      qua_fun = lmom::quagev
    ),
    compare_lmom_reference(
      distribution = "gpa",
      fit_fun = lmom::pelgpa,
      qua_fun = lmom::quagpa
    ),
    compare_lmom_reference(
      distribution = "gum",
      fit_fun = lmom::pelgum,
      qua_fun = lmom::quagum
    ),
    compare_lmom_reference(
      distribution = "pe3",
      fit_fun = lmom::pelpe3,
      qua_fun = lmom::quape3
    )
  )
)

# ---- Validation checks -------------------------------------------------------

stopifnot(all(comparisons$passed))
stopifnot(all(is.finite(comparisons$return_level_alea)))
stopifnot(all(is.finite(comparisons$return_level_reference)))
stopifnot(all(comparisons$absolute_difference <= comparisons$tolerance))

# Within each distribution, return levels should increase with return period
# for this fitted annual-maximum-like sample.
by_distribution <- split(comparisons, comparisons$distribution)

increasing_checks <- vapply(
  by_distribution,
  function(z) all(diff(z$return_level_alea) > 0),
  logical(1)
)

stopifnot(all(increasing_checks))

# ---- Write reference output --------------------------------------------------

write.csv(
  comparisons,
  file = output_file,
  row.names = FALSE
)

message("Phase 13 lmom reference validation completed successfully.")
message("Reference output written to: ", output_file)

print(comparisons)