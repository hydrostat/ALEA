# Phase 13 — LN2/LN3 zero-threshold validation
#
# Purpose:
# Validate the ALEA-R convention that LN2 behaves as the LN3 model
# with fixed lower bound zeta = 0.
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

output_file <- file.path(output_dir, "phase13_ln2_ln3_zero_threshold_reference.csv")

tolerance <- 1e-10

return_period <- c(2, 5, 10, 25, 50, 100)
probability <- 1 - 1 / return_period

# Positive deterministic annual-maximum-like sample.
x <- c(
  42.1, 39.4, 51.7, 48.3, 55.2,
  60.1, 46.8, 53.9, 58.4, 62.7,
  49.5, 57.8, 64.3, 52.6, 59.9,
  61.5, 67.2, 54.8, 63.1, 69.4
)

# ---- Fit LN2 ----------------------------------------------------------------

fit_ln2 <- alea_fit(
  x,
  distribution = "ln2",
  method = "lmom"
)

rl_ln2 <- alea_return_level(
  fit_ln2,
  return_period = return_period
)

params_ln2 <- coef(fit_ln2)

mu <- unname(params_ln2[["mu"]])
sigma <- unname(params_ln2[["sigma"]])

# ---- Theoretical reference ---------------------------------------------------
#
# ALEA-R treats LN2 as an LN3 model with:
#
#   zeta = 0
#
# Therefore, the quantile is:
#
#   Q(p) = zeta + exp(mu + sigma * qnorm(p))
#
# and with zeta = 0:
#
#   Q(p) = exp(mu + sigma * qnorm(p))

zeta_reference <- 0

return_level_reference <- zeta_reference + exp(
  mu + sigma * stats::qnorm(probability)
)

comparison <- data.frame(
  case_id = "P13-LN2-LN3-001",
  distribution = "ln2",
  method = "lmom",
  return_period = return_period,
  probability_alea = rl_ln2$probability,
  probability_reference = probability,
  zeta_reference = zeta_reference,
  return_level_alea = rl_ln2$return_level,
  return_level_reference = return_level_reference,
  absolute_difference = abs(rl_ln2$return_level - return_level_reference),
  tolerance = tolerance,
  passed = abs(rl_ln2$return_level - return_level_reference) <= tolerance
)

# ---- Validation checks -------------------------------------------------------

stopifnot(inherits(fit_ln2, "alea_fit"))
stopifnot(inherits(rl_ln2, "alea_return_level"))

stopifnot(identical(as.character(rl_ln2$distribution), rep("ln2", length(return_period))))
stopifnot(identical(as.character(rl_ln2$method), rep("lmom", length(return_period))))

stopifnot(is.finite(mu))
stopifnot(is.finite(sigma))
stopifnot(sigma > 0)

stopifnot(all.equal(
  rl_ln2$probability,
  probability,
  tolerance = tolerance,
  check.attributes = FALSE
))

stopifnot(all(comparison$passed))
stopifnot(all(is.finite(comparison$return_level_alea)))
stopifnot(all(diff(comparison$return_level_alea) > 0))

# ---- Write reference output --------------------------------------------------

write.csv(
  comparison,
  file = output_file,
  row.names = FALSE
)

message("Phase 13 LN2/LN3 zero-threshold validation completed successfully.")
message("Reference output written to: ", output_file)

print(comparison)