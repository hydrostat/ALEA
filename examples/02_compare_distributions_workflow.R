# ALEA-R example 02: Compare distributions and estimation methods
#
# This script demonstrates the multi-model single-site workflow:
# one hydrological series -> several candidate models -> one alea_compare object.
# The user does not need to manually align parameter columns before combining
# quantile, GOF, or confidence-interval outputs.

suppressPackageStartupMessages({
  library(ALEA)
})

cat("\n============================================================\n")
cat("Example 02: Multi-model single-site comparison\n")
cat("============================================================\n")

data_dir <- if (dir.exists("examples/data")) "examples/data" else "data"
flow_file <- file.path(data_dir, "paraopeba_annual_max_flow.csv")
flow_data <- read.csv(flow_file)
x <- flow_data$flow_m3s

candidate_distributions <- c("gev", "gum", "pe3", "ln2", "ln3")
candidate_methods <- c("lmom", "mle")
return_periods <- c(2, 5, 10, 25, 50, 100, 200)

cat("\nFit several models with alea_fit()\n")
cmp <- alea_fit(
  x,
  distribution = candidate_distributions,
  method = candidate_methods
)
print(cmp)

cat("\nThe multi-model object class\n")
print(class(cmp))

cat("\nModel summary\n")
print(summary(cmp))

cat("\nCompact attempted-model table\n")
print(as.data.frame(cmp))

cat("\nCoefficient table with standardized parameter columns\n")
print(coef(cmp))

cat("\nQuantiles for all successful models\n")
quantiles <- alea_quantile(cmp, return_period = return_periods)
print(quantiles)

cat("\nFull quantile table\n")
print(as.data.frame(quantiles))

cat("\nGoodness-of-fit for all successful models\n")
gof <- alea_gof(cmp)
print(gof)

cat("\nFull GOF table\n")
print(as.data.frame(gof))

cat("\nBootstrap quantile confidence intervals for all successful models\n")
cat("The small n_boot value is used only to keep this teaching example fast.\n")
ci <- confint(
  cmp,
  parm = "quantile",
  return_period = c(10, 25, 50, 100),
  method = "bootstrap",
  n_boot = 50,
  seed = 123
)
print(ci)

cat("\nComparison plots\n")
print(plot(cmp, type = "quantile"))
print(plot(quantiles))
print(plot(ci))
print(plot(gof, type = "rank"))

cat("\nExample 02 completed.\n")
