# ALEA-R example 01
# Single-site basic workflow
#
# Purpose:
#   This teaching script demonstrates a minimal at-site frequency-analysis
#   workflow with ALEA-R for one hydrological series.
#
# Data:
#   Public annual maximum mean daily flow data for the Paraopeba River at
#   P. N. Paraopeba, Brazil, used here as a teaching dataset.
#
# Teaching reference:
#   Naghettini, M. (ed.) (2017). Fundamentals of Statistical Hydrology.
#
# Notes:
#   - This script uses only the ALEA-R public API.
#   - The fitted distribution is GEV, estimated by L-moments.
#   - Long return periods are shown for teaching purposes and should be
#     interpreted with caution.
#   - Run this script from the ALEA package root directory:
#       source("examples/01_single_site_basic_workflow.R")

cat("\n============================================================\n")
cat("ALEA-R example 01: Single-site basic workflow\n")
cat("============================================================\n\n")

cat("--- 1. Loading package ---\n")
library(ALEA)
cat("ALEA loaded successfully.\n\n")

cat("--- 2. Reading Paraopeba annual maximum flow data ---\n")
data_file <- file.path("examples", "data", "paraopeba_annual_max_flow.csv")

if (!file.exists(data_file)) {
  stop(
    "Data file not found: ", data_file, "\n",
    "Please run this script from the ALEA package root directory and check ",
    "that the Paraopeba CSV files are available in examples/data/."
  )
}

paraopeba_flow <- read.csv(data_file, stringsAsFactors = FALSE)

cat("Data file:", data_file, "\n")
cat("Number of rows:", nrow(paraopeba_flow), "\n")
cat("Column names:", paste(names(paraopeba_flow), collapse = ", "), "\n\n")

cat("First rows of the dataset:\n")
print(utils::head(paraopeba_flow))
cat("\n")

cat("--- 3. Preparing the hydrological sample ---\n")

if (!"flow_m3s" %in% names(paraopeba_flow)) {
  stop("Expected column 'flow_m3s' was not found in the data file.")
}

x <- paraopeba_flow$flow_m3s
x <- x[is.finite(x)]

cat("Finite observations:", length(x), "\n")
cat("Minimum flow:", min(x), "m3/s\n")
cat("Maximum flow:", max(x), "m3/s\n")
cat("Mean flow:", mean(x), "m3/s\n")
cat("Standard deviation:", stats::sd(x), "m3/s\n\n")

cat("--- 4. Fitting a GEV distribution by L-moments ---\n")

fit_gev <- alea_fit(
  x,
  distribution = "gev",
  method = "lmom"
)

cat("Fitted model:\n")
print(fit_gev)
cat("\n")

cat("Estimated parameters:\n")
print(coef(fit_gev))
cat("\n")

cat("--- 5. Estimating return levels ---\n")

# Include a return period larger than the largest empirical plotting position
# so that observed points can be displayed in the return-level plot without
# being truncated. Long return periods are shown for teaching purposes and
# should be interpreted with caution.
return_periods <- c(2, 5, 10, 25, 50, 100, 200)

return_levels <- alea_return_level(
  fit_gev,
  return_period = return_periods
)

cat("Return-level estimates:\n")
print(return_levels)
cat("\n")

cat(
  "Teaching note: the 100-year and 200-year return levels are extrapolations\n",
  "beyond the central part of the observed record. They should be interpreted\n",
  "together with uncertainty, diagnostics, and hydrological judgement.\n\n",
  sep = ""
)

cat("--- 6. Computing goodness-of-fit statistics ---\n")

gof_gev <- alea_gof(fit_gev, statistics = "all")

cat("Goodness-of-fit statistics:\n")
print(gof_gev)
cat("\n")

cat(
  "Teaching note: ALEA-R currently reports GOF statistics and information\n",
  "criteria, not calibrated GOF p-values. These values should be used as\n",
  "evidence for model comparison, not as automatic accept/reject rules.\n\n",
  sep = ""
)

cat("--- 7. Computing sample diagnostics ---\n")

diagnostics_gev <- alea_diagnostics(fit_gev, diagnostics = "all")

cat("Sample diagnostics:\n")
print(diagnostics_gev)
cat("\n")

cat(
  "Teaching note: diagnostic warnings do not automatically invalidate a fitted\n",
  "model. They flag issues that should be reviewed together with hydrological\n",
  "knowledge and the intended application.\n\n",
  sep = ""
)

cat("--- 8. Creating plots ---\n")

p_density <- plot(fit_gev, type = "density")
p_cdf <- plot(fit_gev, type = "cdf")
p_qq <- plot(fit_gev, type = "qq")
p_pp <- plot(fit_gev, type = "pp")
p_return_level <- plot(
  fit_gev,
  type = "return_level",
  return_period = return_periods
)

cat("Printing density plot...\n")
print(p_density)

cat("Printing CDF plot...\n")
print(p_cdf)

cat("Printing Q-Q plot...\n")
print(p_qq)

cat("Printing P-P plot...\n")
print(p_pp)

cat("Printing return-level plot...\n")
print(p_return_level)

cat("\nPlots were created as ggplot objects:\n")
cat("  p_density\n")
cat("  p_cdf\n")
cat("  p_qq\n")
cat("  p_pp\n")
cat("  p_return_level\n\n")

cat("============================================================\n")
cat("Example 01 completed successfully.\n")
cat("============================================================\n")
