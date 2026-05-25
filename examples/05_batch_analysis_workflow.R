# ALEA-R example 05: Batch analysis workflow
#
# This script demonstrates the multi-site workflow. For a compact teaching
# example, a second station is created as a scaled copy of the Paraopeba annual
# maximum flow series. This keeps the example reproducible and focused on the
# ALEA-R batch API.

suppressPackageStartupMessages({
  library(ALEA)
})

cat("\n============================================================\n")
cat("Example 05: Batch analysis workflow\n")
cat("============================================================\n")

data_dir <- if (dir.exists("examples/data")) "examples/data" else "data"
flow_file <- file.path(data_dir, "paraopeba_annual_max_flow.csv")
flow_data <- read.csv(flow_file)

batch_data <- rbind(
  data.frame(
    station = "Paraopeba",
    year = flow_data$water_year_end,
    value = flow_data$flow_m3s
  ),
  data.frame(
    station = "Paraopeba_scaled_teaching_copy",
    year = flow_data$water_year_end,
    value = 1.20 * flow_data$flow_m3s
  )
)

cat("\nBatch input preview\n")
print(head(batch_data))

cat("\nRun batch analysis through alea_fit()\n")
batch <- alea_fit(
  batch_data,
  station = "station",
  time = "year",
  value = "value",
  distribution = c("gev", "gum"),
  method = "lmom",
  return_period = c(10, 25, 50, 100, 200),
  gof = TRUE,
  diagnostics = TRUE,
  select = "ai"
)
print(batch)

cat("\nStation metadata\n")
print(alea_results(batch, "stations"))

cat("\nFit summary\n")
print(alea_results(batch, "fits"))

cat("\nQuantiles: compact console output\n")
print(alea_results(batch, "quantiles"))

cat("\nQuantiles: full table for analysis/export\n")
print(as.data.frame(alea_results(batch, "quantiles")))

cat("\nGOF summary\n")
print(alea_results(batch, "gof"))

cat("\nDiagnostics summary\n")
print(alea_results(batch, "diagnostics"))

cat("\nAI selection summary\n")
print(alea_results(batch, "selection"))

cat("\nResolved selected models\n")
print(alea_results(batch, "selected_models"))

cat("\nStructured errors, if any\n")
print(alea_results(batch, "errors"))

cat("\nBatch plots\n")
print(plot(batch, type = "quantiles"))
print(plot(batch, type = "quantiles", plot_observed = FALSE))
print(plot(batch, type = "gof"))
print(plot(batch, type = "diagnostics"))

cat("\nExample 05 completed.\n")
