# Phase 13 — GOF reference recalculation
#
# Purpose:
# Validate ALEA-R goodness-of-fit statistics by recalculating
# selected statistics directly from fitted CDF and density values.
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

output_file <- file.path(output_dir, "phase13_gof_reference_recalculation.csv")

tolerance <- 1e-10

x <- c(
  42.1, 39.4, 51.7, 48.3, 55.2,
  60.1, 46.8, 53.9, 58.4, 62.7,
  49.5, 57.8, 64.3, 52.6, 59.9,
  61.5, 67.2, 54.8, 63.1, 69.4
)

# ---- Fit model and compute ALEA-R GOF ---------------------------------------

fit <- alea_fit(
  x,
  distribution = "gum",
  method = "lmom"
)

gof <- alea_gof(
  fit,
  statistics = c("ks", "cvm", "ad", "loglik", "aic", "bic")
)

params <- coef(fit)

xi <- unname(params[["xi"]])
alpha <- unname(params[["alpha"]])

n <- length(x)
n_parameters <- length(params)

# ---- Direct reference recalculation -----------------------------------------
#
# Gumbel CDF:
#
#   F(x) = exp(-exp(-(x - xi) / alpha))
#
# Gumbel density:
#
#   f(x) = (1 / alpha) * exp(-z - exp(-z))
#
# where:
#
#   z = (x - xi) / alpha

x_sorted <- sort(x)

z_sorted <- (x_sorted - xi) / alpha
u <- exp(-exp(-z_sorted))

# Clamp probabilities for Anderson-Darling numerical stability.
eps <- .Machine$double.eps
u_clamped <- pmin(pmax(u, eps), 1 - eps)

i <- seq_len(n)

ks_reference <- max(
  max(i / n - u),
  max(u - (i - 1) / n)
)

cvm_reference <- sum((u - (2 * i - 1) / (2 * n))^2) + 1 / (12 * n)

ad_reference <- -n - mean(
  (2 * i - 1) * (
    log(u_clamped) + rev(log1p(-u_clamped))
  )
)

z <- (x - xi) / alpha
density <- (1 / alpha) * exp(-z - exp(-z))

loglik_reference <- sum(log(density))

aic_reference <- -2 * loglik_reference + 2 * n_parameters
bic_reference <- -2 * loglik_reference + log(n) * n_parameters

reference <- data.frame(
  statistic = c("ks", "cvm", "ad", "loglik", "aic", "bic"),
  reference_estimate = c(
    ks_reference,
    cvm_reference,
    ad_reference,
    loglik_reference,
    aic_reference,
    bic_reference
  )
)

comparison <- merge(
  as.data.frame(gof),
  reference,
  by = "statistic",
  all.x = TRUE,
  sort = FALSE
)

comparison$case_id <- "P13-GOF-REF-001"
comparison$absolute_difference <- abs(
  comparison$estimate - comparison$reference_estimate
)
comparison$tolerance <- tolerance
comparison$passed <- comparison$absolute_difference <= comparison$tolerance

comparison <- comparison[
  ,
  c(
    "case_id",
    "distribution",
    "method",
    "statistic",
    "estimate",
    "reference_estimate",
    "absolute_difference",
    "tolerance",
    "passed",
    "n",
    "n_parameters",
    "higher_is_better",
    "description"
  )
]

# ---- Validation checks -------------------------------------------------------

stopifnot(inherits(fit, "alea_fit"))
stopifnot(inherits(gof, "alea_gof"))

stopifnot(all(comparison$statistic %in% c("ks", "cvm", "ad", "loglik", "aic", "bic")))
stopifnot(all(comparison$passed))
stopifnot(all(is.finite(comparison$estimate)))
stopifnot(all(is.finite(comparison$reference_estimate)))

stopifnot(identical(as.character(comparison$distribution), rep("gum", 6)))
stopifnot(identical(as.character(comparison$method), rep("lmom", 6)))

# ---- Write reference output --------------------------------------------------

write.csv(
  comparison,
  file = output_file,
  row.names = FALSE
)

message("Phase 13 GOF reference recalculation completed successfully.")
message("Reference output written to: ", output_file)

print(comparison)