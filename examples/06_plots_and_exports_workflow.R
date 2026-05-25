# ALEA-R example 06: Plots and exports
#
# This script demonstrates ggplot-returning plot methods and export helpers.
# Generated outputs are written to examples/output/ and are not intended to be
# versioned in the repository.

suppressPackageStartupMessages({
  library(ALEA)
})

cat("\n============================================================\n")
cat("Example 06: Plots and exports\n")
cat("============================================================\n")

data_dir <- if (dir.exists("examples/data")) "examples/data" else "data"
output_dir <- if (dir.exists("examples")) "examples/output" else "output"
plots_dir <- file.path(output_dir, "example06_plots")
batch_tables_dir <- file.path(output_dir, "example06_batch_tables")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(batch_tables_dir, recursive = TRUE, showWarnings = FALSE)

remove_if_exists <- function(path) {
  if (file.exists(path)) {
    unlink(path, recursive = TRUE, force = TRUE)
  }
}

flow_file <- file.path(data_dir, "paraopeba_annual_max_flow.csv")
flow_data <- read.csv(flow_file)
x <- flow_data$flow_m3s

fit <- alea_fit(x, distribution = "gev", method = "lmom")
quantiles <- alea_quantile(fit, return_period = c(2, 5, 10, 25, 50, 100, 200))
ci <- confint(
  fit,
  parm = "quantile",
  return_period = c(10, 25, 50, 100),
  method = "bootstrap",
  n_boot = 50,
  seed = 123
)
gof <- alea_gof(fit)
diag <- alea_diagnostics(fit)
selection <- alea_select(x)

cat("\nCreate plot objects\n")
plots <- list(
  density = plot(fit, type = "density"),
  cdf = plot(fit, type = "cdf"),
  qq = plot(fit, type = "qq"),
  pp = plot(fit, type = "pp"),
  quantile = plot(fit, type = "quantile"),
  quantile_no_observed = plot(fit, type = "quantile", plot_observed = FALSE),
  quantile_table_plot = plot(quantiles),
  quantile_ci = plot(ci),
  gof = plot(gof, type = "statistic"),
  diagnostics = plot(diag, type = "status"),
  selection = plot(selection)
)

print(plots$quantile)
print(plots$quantile_no_observed)
print(plots$quantile_ci)

cat("\nSave one plot\n")
single_plot_file <- file.path(output_dir, "example06_quantile.png")
remove_if_exists(single_plot_file)
alea_save_plot(
  plots$quantile,
  filename = single_plot_file,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)
print(single_plot_file)

cat("\nSave multiple plots\n")
remove_if_exists(plots_dir)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
alea_save_plots(
  plots,
  directory = plots_dir,
  prefix = "example06",
  extension = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)
print(list.files(plots_dir, full.names = TRUE))

cat("\nExport data frames\n")
quantile_csv <- file.path(output_dir, "example06_quantiles.csv")
gof_csv <- file.path(output_dir, "example06_gof.csv")
diag_csv <- file.path(output_dir, "example06_diagnostics.csv")
selection_csv <- file.path(output_dir, "example06_selection.csv")

alea_export(as.data.frame(quantiles), quantile_csv, overwrite = TRUE)
alea_export(as.data.frame(gof), gof_csv, overwrite = TRUE)
alea_export(as.data.frame(diag), diag_csv, overwrite = TRUE)
alea_export(as.data.frame(selection), selection_csv, overwrite = TRUE)

print(list.files(output_dir, pattern = "\\.csv$", full.names = TRUE))

cat("\nCreate and export batch tables\n")
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

batch <- alea_fit(
  batch_data,
  station = "station",
  time = "year",
  value = "value",
  distribution = c("gev", "gum"),
  method = "lmom",
  return_period = c(10, 25, 50, 100),
  gof = TRUE,
  diagnostics = TRUE,
  select = "ai"
)

print(plot(batch, type = "quantiles"))
print(plot(batch, type = "quantiles", plot_observed = FALSE))

remove_if_exists(batch_tables_dir)
dir.create(batch_tables_dir, recursive = TRUE, showWarnings = FALSE)
alea_export(
  batch,
  path = batch_tables_dir,
  type = "all",
  overwrite = TRUE
)
print(list.files(batch_tables_dir, full.names = TRUE))

cat("\nExample 06 completed.\n")
