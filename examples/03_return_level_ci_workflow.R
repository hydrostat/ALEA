# ALEA-R example 03
# Return-level confidence interval workflow
#
# Purpose:
#   This teaching script estimates return levels and bootstrap confidence
#   intervals for one fitted model.
#
# Data:
#   Public annual maximum mean daily flow data for the Paraopeba River at
#   P. N. Paraopeba, Brazil.
#
# Notes:
#   - ALEA-R currently implements percentile bootstrap confidence intervals
#     for return levels.
#   - This script uses a small n_boot value to keep the example fast.
#   - Production analyses should use larger n_boot values.
#   - Run this script from the ALEA package root directory:
#       source("examples/03_return_level_ci_workflow.R")

cat("\n============================================================\n")
cat("ALEA-R example 03: Return-level confidence intervals\n")
cat("============================================================\n\n")

cat("--- 1. Loading package and data ---\n")
library(ALEA)

data_file <- file.path("examples", "data", "paraopeba_annual_max_flow.csv")

if (!file.exists(data_file)) {
  stop(
    "Data file not found: ", data_file, "\n",
    "Please run this script from the ALEA package root directory."
  )
}

paraopeba_flow <- read.csv(data_file, stringsAsFactors = FALSE)

if (!"flow_m3s" %in% names(paraopeba_flow)) {
  stop("Expected column 'flow_m3s' was not found in the data file.")
}

x <- paraopeba_flow$flow_m3s
x <- x[is.finite(x)]

cat("Finite observations:", length(x), "\n\n")

cat("--- 2. Fitting a GEV distribution by L-moments ---\n")

fit_gev <- alea_fit(
  x,
  distribution = "gev",
  method = "lmom"
)

print(fit_gev)
cat("\n")

cat("--- 3. Estimating return levels ---\n")

return_periods <- c(2, 5, 10, 25, 50, 100, 200)

return_levels <- alea_return_level(
  fit_gev,
  return_period = return_periods
)

cat("Point estimates:\n")
print(return_levels)
cat("\n")

cat("--- 4. Estimating bootstrap confidence intervals ---\n")

# Small teaching value for fast execution. Increase n_boot for real analyses.
ci_return_levels <- confint(
  fit_gev,
  parm = "return_level",
  return_period = return_periods,
  level = 0.95,
  method = "bootstrap",
  n_boot = 50,
  seed = 123
)

cat("Bootstrap confidence intervals:\n")
print(ci_return_levels)
cat("\n")

cat("Bootstrap success summary:\n")
print(as.data.frame(ci_return_levels)[
  ,
  c("return_period", "n_boot", "n_success", "n_failed")
])
cat("\n")

cat(
  "Teaching note: this script uses n_boot = 50 only to keep the example fast.\n",
  "For production analysis, increase n_boot and inspect n_success and n_failed.\n",
  "Wide intervals are common for long return periods and short hydrological\n",
  "records.\n\n",
  sep = ""
)

cat("--- 5. Plotting return levels with confidence intervals ---\n")

p_ci <- plot(ci_return_levels)

cat("Printing return-level confidence interval plot...\n")
print(p_ci)

cat("\n============================================================\n")
cat("Example 03 completed successfully.\n")
cat("============================================================\n")

