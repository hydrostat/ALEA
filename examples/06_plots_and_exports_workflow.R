# ALEA-R example 06
# Plots and exports workflow
#
# Purpose:
#   This teaching script demonstrates how to create ALEA-R plots and export
#   plots and tables using the public export helpers.
#
# Data:
#   Public annual maximum mean daily flow data for the Paraopeba River at
#   P. N. Paraopeba, Brazil.
#
# Notes:
#   - The script writes files to examples/output/.
#   - Output files are teaching artifacts and can be deleted safely.
#   - This script creates output directories before exporting files.
#   - Run this script from the ALEA package root directory:
#       source("examples/06_plots_and_exports_workflow.R")

cat("\n============================================================\n")
cat("ALEA-R example 06: Plots and exports workflow\n")
cat("============================================================\n\n")

cat("--- 1. Loading package and data ---\n")
library(ALEA)

data_file <- file.path("examples", "data", "paraopeba_annual_max_flow.csv")
output_dir <- file.path("examples", "output")
multi_plot_dir <- file.path(output_dir, "example06_plots")
batch_export_dir <- file.path(output_dir, "example06_batch_tables")

if (!file.exists(data_file)) {
  stop(
    "Data file not found: ", data_file, "\n",
    "Please run this script from the ALEA package root directory."
  )
}

# Create all output directories before calling export helpers.
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(multi_plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(batch_export_dir, recursive = TRUE, showWarnings = FALSE)

paraopeba_flow <- read.csv(data_file, stringsAsFactors = FALSE)

if (!"flow_m3s" %in% names(paraopeba_flow)) {
  stop("Expected column 'flow_m3s' was not found in the data file.")
}

x <- paraopeba_flow$flow_m3s
x <- x[is.finite(x)]

cat("Finite observations:", length(x), "\n")
cat("Output directory:", output_dir, "\n")
cat("Multi-plot directory:", multi_plot_dir, "\n")
cat("Batch export directory:", batch_export_dir, "\n\n")

cat("--- 2. Fitting a model and computing results ---\n")

fit_gev <- alea_fit(
  x,
  distribution = "gev",
  method = "lmom"
)

return_periods <- c(2, 5, 10, 25, 50, 100, 200)

return_levels <- alea_return_level(
  fit_gev,
  return_period = return_periods
)

gof_gev <- alea_gof(fit_gev, statistics = "all")
diagnostics_gev <- alea_diagnostics(fit_gev, diagnostics = "all")
selection <- alea_select(x)

cat("Fitted model:\n")
print(fit_gev)
cat("\n")

cat("--- 3. Creating ggplot objects ---\n")

p_density <- plot(fit_gev, type = "density")
p_cdf <- plot(fit_gev, type = "cdf")
p_qq <- plot(fit_gev, type = "qq")
p_pp <- plot(fit_gev, type = "pp")
p_return <- plot(
  fit_gev,
  type = "return_level",
  return_period = return_periods
)
p_gof <- plot(gof_gev, type = "statistic")
p_diagnostics <- plot(diagnostics_gev, type = "status")
p_selection <- plot(selection)

cat("Printing selected plots...\n")
print(p_density)
print(p_return)
print(p_selection)

plots <- list(
  density = p_density,
  cdf = p_cdf,
  qq = p_qq,
  pp = p_pp,
  return_level = p_return,
  gof = p_gof,
  diagnostics = p_diagnostics,
  selection = p_selection
)

cat("\nCreated plot objects:\n")
print(names(plots))
cat("\n")

cat("--- 4. Exporting one plot ---\n")

single_plot_file <- file.path(output_dir, "example06_return_level.png")

# alea_save_plot() does not use an overwrite argument in the current API.
# If the file already exists, remove it explicitly for this teaching example.
if (file.exists(single_plot_file)) {
  file.remove(single_plot_file)
}

alea_save_plot(
  p_return,
  filename = single_plot_file,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

cat("Saved:", single_plot_file, "\n\n")

cat("--- 5. Exporting multiple plots ---\n")

# alea_save_plots() does not use an overwrite argument in the current API.
# Remove existing files with the same teaching prefix before exporting.
existing_plot_files <- list.files(
  multi_plot_dir,
  pattern = "^paraopeba_.*\\.png$",
  full.names = TRUE
)

if (length(existing_plot_files) > 0L) {
  file.remove(existing_plot_files)
}

alea_save_plots(
  plots,
  directory = multi_plot_dir,
  prefix = "paraopeba",
  extension = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300,
  overwrite = TRUE
)

cat("Saved multiple plots to:", multi_plot_dir, "\n")
cat("Files:\n")
print(list.files(multi_plot_dir, full.names = TRUE))
cat("\n")

cat("--- 6. Exporting data frames ---\n")

return_level_file <- file.path(output_dir, "example06_return_levels.csv")
gof_file <- file.path(output_dir, "example06_gof.csv")
diagnostics_file <- file.path(output_dir, "example06_diagnostics.csv")
selection_file <- file.path(output_dir, "example06_selection.csv")

alea_export(return_levels, path = return_level_file, overwrite = TRUE)
alea_export(gof_gev, path = gof_file, overwrite = TRUE)
alea_export(diagnostics_gev, path = diagnostics_file, overwrite = TRUE)
alea_export(as.data.frame(selection), path = selection_file, overwrite = TRUE)

cat("Saved table files:\n")
print(c(return_level_file, gof_file, diagnostics_file, selection_file))
cat("\n")

cat("--- 7. Exporting batch flat tables ---\n")

# A small batch object is created only to demonstrate batch export.
batch_data <- data.frame(
  station = "paraopeba_flow",
  time = paraopeba_flow$water_year,
  value = paraopeba_flow$flow_m3s,
  stringsAsFactors = FALSE
)

batch <- alea_batch_fit(
  data = batch_data,
  station = "station",
  time = "time",
  value = "value",
  distributions = c("gev", "gum"),
  methods = "lmom",
  return_period = c(2, 10, 50, 100, 200),
  gof = TRUE,
  diagnostics = TRUE,
  select = "ai",
  quiet = TRUE
)

# Remove previous CSV outputs from this teaching batch-export folder.
existing_batch_files <- list.files(
  batch_export_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

if (length(existing_batch_files) > 0L) {
  file.remove(existing_batch_files)
}

alea_export(
  batch,
  path = batch_export_dir,
  type = "all",
  overwrite = TRUE
)

cat("Saved batch tables to:", batch_export_dir, "\n")
cat("Files:\n")
print(list.files(batch_export_dir, full.names = TRUE))
cat("\n")

cat(
  "Teaching note: ALEA-R exports flat batch tables. Heavy object-list fields,\n",
  "such as fit_objects and selection_objects, are preserved in the alea_batch\n",
  "object but are not exported as CSV tables by default.\n\n",
  sep = ""
)

cat("============================================================\n")
cat("Example 06 completed successfully.\n")
cat("============================================================\n")
