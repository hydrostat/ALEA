# ALEA-R example 04
# GOF, diagnostics, and AI-assisted selection workflow
#
# Purpose:
#   This teaching script shows how to inspect goodness-of-fit statistics,
#   sample diagnostics, and FADS_AI decision-support output for one series.
#
# Data:
#   Public annual maximum mean daily flow data for the Paraopeba River at
#   P. N. Paraopeba, Brazil.
#
# Notes:
#   - FADS_AI output is decision-support evidence.
#   - It is not proof of the true generating distribution.
#   - It should be interpreted with GOF, diagnostics, return-level behavior,
#     sample size, and hydrological judgement.
#   - Run this script from the ALEA package root directory:
#       source("examples/04_gof_diagnostics_selection_workflow.R")

cat("\n============================================================\n")
cat("ALEA-R example 04: GOF, diagnostics, and AI selection\n")
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

cat("--- 2. Fitting a reference model ---\n")

fit_gev <- alea_fit(
  x,
  distribution = "gev",
  method = "lmom"
)

print(fit_gev)
cat("\n")

cat("--- 3. Goodness-of-fit statistics ---\n")

gof_gev <- alea_gof(fit_gev, statistics = "all")
print(gof_gev)
cat("\n")

p_gof_stat <- plot(gof_gev, type = "statistic")
cat("Printing GOF statistic plot...\n")
print(p_gof_stat)

cat(
  "\nTeaching note: GOF statistics and information criteria are evidence for\n",
  "model comparison. In ALEA-R 0.1.0, EDF GOF p-values are not calibrated.\n\n",
  sep = ""
)

cat("--- 4. Sample diagnostics ---\n")

diagnostics_gev <- alea_diagnostics(fit_gev, diagnostics = "all")
print(diagnostics_gev)
cat("\n")

p_diag_status <- plot(diagnostics_gev, type = "status")
cat("Printing diagnostics status plot...\n")
print(p_diag_status)

# The p-value plot is meaningful when hypothesis-test diagnostics are available.
p_diag_pvalue <- plot(diagnostics_gev, type = "p_value")
cat("Printing diagnostics p-value plot...\n")
print(p_diag_pvalue)

cat(
  "\nTeaching note: diagnostic warnings flag issues for review. They do not\n",
  "automatically invalidate a fitted model.\n\n",
  sep = ""
)

cat("--- 5. AI-assisted distribution selection ---\n")

selection <- alea_select(x)

cat("AI-assisted selection object:\n")
print(selection)
cat("\n")

cat("Candidate ranking:\n")
print(as.data.frame(selection))
cat("\n")

p_selection <- plot(selection)
cat("Printing AI-selection probability ranking plot...\n")
print(p_selection)

cat(
  "\nTeaching note: FADS_AI probabilities are model-based decision-support\n",
  "evidence for candidate families. They should not be interpreted as proof of\n",
  "the true generating distribution or as an automatic replacement for\n",
  "hydrological and statistical judgement.\n\n",
  sep = ""
)

cat("--- 6. AI model metadata ---\n")

model_info <- alea_ai_model_info()
print(model_info)
cat("\n")

cat("============================================================\n")
cat("Example 04 completed successfully.\n")
cat("============================================================\n")
