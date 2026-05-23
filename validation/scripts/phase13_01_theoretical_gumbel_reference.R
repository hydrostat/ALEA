
## 2. `validation/scripts/phase13_01_theoretical_gumbel_reference.R`


# Phase 13 — Theoretical Gumbel return-level validation
#
# Purpose:
# Validate ALEA-R Gumbel return-level calculations against the
# closed-form Gumbel quantile formula.
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

output_file <- file.path(output_dir, "phase13_gumbel_reference.csv")

# Deterministic synthetic annual-maximum-like sample.
#
# The sample is intentionally simple and small enough for validation review.
# The fitted parameters are obtained through ALEA-R, then return levels are
# checked against the closed-form Gumbel quantile formula using those same
# fitted parameters.

x <- c(
  42.1, 39.4, 51.7, 48.3, 55.2,
  60.1, 46.8, 53.9, 58.4, 62.7,
  49.5, 57.8, 64.3, 52.6, 59.9,
  61.5, 67.2, 54.8, 63.1, 69.4
)

return_period <- c(2, 5, 10, 25, 50, 100)

tolerance <- 1e-10

# ---- Fit Gumbel model -------------------------------------------------------

fit <- alea_fit(
  x,
  distribution = "gum",
  method = "lmom"
)

rl <- alea_return_level(
  fit,
  return_period = return_period
)

params <- coef(fit)

xi <- unname(params[["xi"]])
alpha <- unname(params[["alpha"]])

# ---- Closed-form theoretical reference --------------------------------------
#
# ALEA-R uses the return-period probability convention:
#
#   p = 1 - 1 / T
#
# For the Gumbel distribution in the Hosking/lmom parameterization:
#
#   Q(p) = xi - alpha * log(-log(p))
#
# Therefore:
#
#   z_T = xi - alpha * log(-log(1 - 1 / T))

probability_reference <- 1 - 1 / return_period

return_level_reference <- xi - alpha * log(-log(probability_reference))

comparison <- data.frame(
  case_id = "P13-GUM-THEORY-001",
  distribution = "gum",
  method = "lmom",
  return_period = return_period,
  probability_alea = rl$probability,
  probability_reference = probability_reference,
  return_level_alea = rl$return_level,
  return_level_reference = return_level_reference,
  absolute_difference = abs(rl$return_level - return_level_reference),
  tolerance = tolerance,
  passed = abs(rl$return_level - return_level_reference) <= tolerance
)

# ---- Validation checks -------------------------------------------------------

stopifnot(inherits(fit, "alea_fit"))
stopifnot(inherits(rl, "alea_return_level"))

stopifnot(identical(as.character(rl$distribution), rep("gum", length(return_period))))
stopifnot(identical(as.character(rl$method), rep("lmom", length(return_period))))

stopifnot(all.equal(
  rl$probability,
  probability_reference,
  tolerance = tolerance,
  check.attributes = FALSE
))

stopifnot(all(comparison$passed))

# Return levels should increase with return period for this fitted Gumbel model.
stopifnot(all(diff(comparison$return_level_alea) > 0))

# ---- Write reference output --------------------------------------------------

write.csv(
  comparison,
  file = output_file,
  row.names = FALSE
)

message("Phase 13 Gumbel theoretical validation completed successfully.")
message("Reference output written to: ", output_file)

print(comparison)