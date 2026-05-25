# ALEA-R example runner
#
# Run all teaching examples in order. This script assumes it is called from
# the package root.

scripts <- c(
  "examples/01_single_site_basic_workflow.R",
  "examples/02_compare_distributions_workflow.R",
  "examples/03_quantile_ci_workflow.R",
  "examples/04_gof_diagnostics_selection_workflow.R",
  "examples/05_batch_analysis_workflow.R",
  "examples/06_plots_and_exports_workflow.R"
)

for (script in scripts) {
  cat("\n============================================================\n")
  cat("Running", script, "\n")
  cat("============================================================\n")
  source(script, local = new.env(parent = globalenv()))
}

cat("\nAll ALEA-R teaching examples completed.\n")
