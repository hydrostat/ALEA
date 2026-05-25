# ALEA-R example 04: GOF, diagnostics, and AI-assisted selection
#
# This script demonstrates model diagnostics, goodness-of-fit summaries,
# and FADS_AI decision-support evidence.

suppressPackageStartupMessages({
  library(ALEA)
})

cat("\n============================================================\n")
cat("Example 04: GOF, diagnostics, and AI-assisted selection\n")
cat("============================================================\n")

data_dir <- if (dir.exists("examples/data")) "examples/data" else "data"
flow_file <- file.path(data_dir, "paraopeba_annual_max_flow.csv")
flow_data <- read.csv(flow_file)
x <- flow_data$flow_m3s

cat("\nSample diagnostics before fitting\n")
diag_x <- alea_diagnostics(x)
print(diag_x)
print(plot(diag_x, type = "status"))

cat("\nFit a single model\n")
fit <- alea_fit(x, distribution = "gev", method = "lmom")
print(fit)

cat("\nDiagnostics associated with the fitted object\n")
diag_fit <- alea_diagnostics(fit)
print(diag_fit)
print(plot(diag_fit, type = "p_value"))

cat("\nGoodness-of-fit statistics for the fitted model\n")
gof_fit <- alea_gof(fit)
print(gof_fit)
print(plot(gof_fit, type = "statistic"))

cat("\nCompare GOF across several candidate models\n")
cmp <- alea_fit(
  x,
  distribution = c("gev", "gum", "pe3", "ln2", "ln3"),
  method = c("lmom", "mle")
)
gof_cmp <- alea_gof(cmp)
print(gof_cmp)
print(plot(gof_cmp, type = "rank"))

cat("\nAI model metadata\n")
info <- alea_ai_model_info()
print(info)

cat("\nAI-assisted distribution-selection support\n")
selection <- alea_select(x)
print(selection)
print(as.data.frame(selection))
print(plot(selection))

cat("\nInterpretation note\n")
cat("FADS_AI probabilities are decision-support evidence. They should be reviewed\n")
cat("together with diagnostics, GOF, quantile uncertainty, and hydrological judgement.\n")

cat("\nExample 04 completed.\n")
