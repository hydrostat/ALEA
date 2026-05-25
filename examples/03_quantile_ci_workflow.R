# ALEA-R example 03: Quantiles and bootstrap confidence intervals
#
# This script focuses on design quantiles and percentile bootstrap confidence
# intervals for quantiles.

suppressPackageStartupMessages({
  library(ALEA)
})

cat("\n============================================================\n")
cat("Example 03: Quantiles and bootstrap confidence intervals\n")
cat("============================================================\n")

data_dir <- if (dir.exists("examples/data")) "examples/data" else "data"
flow_file <- file.path(data_dir, "paraopeba_annual_max_flow.csv")
flow_data <- read.csv(flow_file)
x <- flow_data$flow_m3s

return_periods <- c(2, 5, 10, 25, 50, 100, 200)
ci_return_periods <- c(10, 25, 50, 100)

cat("\nFit one model\n")
fit <- alea_fit(x, distribution = "gev", method = "lmom")
print(fit)

cat("\nCompute quantiles\n")
quantiles <- alea_quantile(fit, return_period = return_periods)
print(quantiles)

cat("\nPlot quantiles with and without observed plotting-position points\n")
print(plot(quantiles))
print(plot(quantiles, plot_observed = FALSE))

cat("\nBootstrap confidence intervals for quantiles\n")
cat("Use larger n_boot values for production analyses.\n")
ci <- confint(
  fit,
  parm = "quantile",
  return_period = ci_return_periods,
  method = "bootstrap",
  n_boot = 100,
  seed = 123
)
print(ci)

cat("\nFull confidence-interval table\n")
print(as.data.frame(ci))

cat("\nConfidence-interval plots\n")
print(plot(ci))
print(plot(ci, plot_observed = FALSE))

cat("\nExample 03 completed.\n")
