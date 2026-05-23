# ALEA-R example 05
# Batch analysis workflow
#
# Purpose:
#   This teaching script demonstrates a small batch workflow with public
#   Paraopeba rainfall and flow annual-maximum series treated as two teaching
#   sites/variables.
#
# Data:
#   - Annual maximum mean daily flow at P. N. Paraopeba.
#   - Annual maximum daily rainfall at P. N. Paraopeba.
#
# Notes:
#   - This is a teaching batch example, not a regional analysis.
#   - The two series have different units and should not be compared as if they
#     were the same hydrological variable.
#   - Batch analysis is used here to demonstrate ALEA-R mechanics:
#     fits, return levels, GOF, diagnostics, AI selection, selected models, and
#     structured errors.
#   - Run this script from the ALEA package root directory:
#       source("examples/05_batch_analysis_workflow.R")

cat("\n============================================================\n")
cat("ALEA-R example 05: Batch analysis workflow\n")
cat("============================================================\n\n")

cat("--- 1. Loading package and data ---\n")
library(ALEA)

flow_file <- file.path("examples", "data", "paraopeba_annual_max_flow.csv")
rain_file <- file.path("examples", "data", "paraopeba_annual_max_rainfall.csv")

if (!file.exists(flow_file)) {
  stop("Data file not found: ", flow_file)
}
if (!file.exists(rain_file)) {
  stop("Data file not found: ", rain_file)
}

flow <- read.csv(flow_file, stringsAsFactors = FALSE)
rain <- read.csv(rain_file, stringsAsFactors = FALSE)

if (!"flow_m3s" %in% names(flow)) {
  stop("Expected column 'flow_m3s' was not found in the flow data file.")
}
if (!"rainfall_mm" %in% names(rain)) {
  stop("Expected column 'rainfall_mm' was not found in the rainfall data file.")
}

cat("Flow rows:", nrow(flow), "\n")
cat("Rainfall rows:", nrow(rain), "\n\n")

cat("--- 2. Building a small batch data frame ---\n")

batch_data <- rbind(
  data.frame(
    station = "paraopeba_flow",
    time = flow$water_year,
    value = flow$flow_m3s,
    variable = "annual maximum mean daily flow",
    unit = "m3/s",
    stringsAsFactors = FALSE
  ),
  data.frame(
    station = "paraopeba_rainfall",
    time = rain$water_year,
    value = rain$rainfall_mm,
    variable = "annual maximum daily rainfall",
    unit = "mm",
    stringsAsFactors = FALSE
  )
)

cat("Batch rows:", nrow(batch_data), "\n")
cat("Stations:", paste(unique(batch_data$station), collapse = ", "), "\n\n")

cat("First rows of batch data:\n")
print(utils::head(batch_data))
cat("\n")

cat(
  "Teaching note: the two series use different units. This example demonstrates\n",
  "batch workflow mechanics only. Interpret each station/variable separately.\n\n",
  sep = ""
)

cat("--- 3. Running batch frequency analysis ---\n")

batch <- alea_batch_fit(
  data = batch_data,
  station = "station",
  time = "time",
  value = "value",
  distributions = c("gev", "gum"),
  methods = "lmom",
  return_period = c(2, 5, 10, 25, 50, 100, 200),
  gof = TRUE,
  diagnostics = TRUE,
  select = "ai",
  quiet = FALSE
)

cat("Batch object:\n")
print(batch)
cat("\n")

cat("--- 4. Extracting batch result tables ---\n")

cat("\nStations table:\n")
stations_table <- alea_results(batch, "stations")
print(stations_table)

cat("\nFits table:\n")
fits_table <- alea_results(batch, "fits")
print(fits_table)

cat("\nSelected models table:\n")
selected_models <- alea_results(batch, "selected_models")
print(selected_models)

cat("\nReturn-level table:\n")
return_levels <- alea_results(batch, "return_levels")
print(return_levels)

cat("\nGOF table:\n")
gof <- alea_results(batch, "gof")
print(gof)

cat("\nDiagnostics table:\n")
diagnostics <- alea_results(batch, "diagnostics")
print(diagnostics)

cat("\nAI-selection table:\n")
selection <- alea_results(batch, "selection")
print(selection)

cat("\nErrors table:\n")
errors <- alea_results(batch, "errors")
print(errors)

cat(
  "\nTeaching note: the errors table is part of the expected batch structure.\n",
  "When no errors occur, it should still exist as an empty data frame with\n",
  "stable columns.\n\n",
  sep = ""
)

cat("--- 5. Plotting batch summaries ---\n")

p_selected <- plot(batch, type = "selected_models")
cat("Printing selected-model plot...\n")
print(p_selected)

p_return <- plot(
  batch,
  type = "return_levels",
  return_period_scale = "gumbel"
)
cat("Printing batch return-level plot...\n")
print(p_return)

p_gof <- plot(batch, type = "gof", statistic = "aic")
cat("Printing batch GOF plot for AIC...\n")
print(p_gof)

p_diag <- plot(batch, type = "diagnostics")
cat("Printing batch diagnostics plot...\n")
print(p_diag)

cat("\n============================================================\n")
cat("Example 05 completed successfully.\n")
cat("============================================================\n")
