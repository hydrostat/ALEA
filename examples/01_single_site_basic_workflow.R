# ALEA-R example 01: Single-site basic workflow
#
# This script demonstrates the core one-model workflow:
# one hydrological series -> one fitted model -> quantiles, GOF,
# diagnostics, AI-assisted distribution-selection support, and plots.

suppressPackageStartupMessages({
  library(ALEA)
})

cat("\n============================================================\n")
cat("Example 01: Single-site basic workflow\n")
cat("============================================================\n")

# Locate teaching data whether the script is run from the package root or from
# inside the examples/ folder.
data_dir <- if (dir.exists("examples/data")) "examples/data" else "data"
flow_file <- file.path(data_dir, "paraopeba_annual_max_flow.csv")

flow_data <- read.csv(flow_file)
x <- flow_data$flow_m3s

cat("\nData summary\n")
print(summary(x))

cat("\nSupported distributions and parameter roles\n")
print(alea_dist())

cat("\nGEV parameter mapping\n")
print(alea_dist("gev"))

cat("\nFit one model with alea_fit()\n")
fit <- alea_fit(
  x,
  distribution = "gev",
  method = "lmom"
)
print(fit)

cat("\nClass of the fitted object\n")
print(class(fit))

cat("\nUser-facing coefficients\n")
print(coef(fit))

cat("\nDistribution-specific internal coefficients\n")
print(coef(fit, type = "internal"))

cat("\nSample diagnostics\n")
diag <- alea_diagnostics(fit)
print(diag)

cat("\nGoodness-of-fit statistics\n")
gof <- alea_gof(fit)
print(gof)

cat("\nDesign quantiles\n")
return_periods <- c(2, 5, 10, 25, 50, 100, 200)
quantiles <- alea_quantile(fit, return_period = return_periods)
print(quantiles)

cat("\nAI-assisted distribution-selection support\n")
selection <- alea_select(x)
print(selection)

cat("\nBasic plots\n")
print(plot(fit, type = "density"))
print(plot(fit, type = "cdf"))
print(plot(fit, type = "quantile"))

cat("\nSame quantile plot without observed plotting-position points\n")
print(plot(fit, type = "quantile", plot_observed = FALSE))

cat("\nExample 01 completed.\n")
