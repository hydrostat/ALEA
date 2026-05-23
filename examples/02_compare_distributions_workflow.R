# ALEA-R example 02
# Compare distributions workflow
#
# Purpose:
#   This teaching script compares several ALEA-R candidate distributions for
#   one annual maximum flow series.
#
# Data:
#   Public annual maximum mean daily flow data for the Paraopeba River at
#   P. N. Paraopeba, Brazil.
#
# Notes:
#   - This script uses only the ALEA-R public API.
#   - It compares distributions supported by ALEA-R 0.1.0.
#   - It does not include LP3.
#   - Run this script from the ALEA package root directory:
#       source("examples/02_compare_distributions_workflow.R")

cat("\n============================================================\n")
cat("ALEA-R example 02: Compare distributions workflow\n")
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

cat("--- 2. Fitting candidate distributions ---\n")

# This set is restricted to the distributions implemented in ALEA-R.
candidate_distributions <- c("gev", "gum", "pe3", "ln2", "ln3")
method <- "lmom"

fits <- list()

for (dist in candidate_distributions) {
  cat("Fitting", dist, "using", method, "...\n")
  fits[[dist]] <- tryCatch(
    alea_fit(x, distribution = dist, method = method),
    error = function(e) {
      message("  Fit failed for ", dist, ": ", conditionMessage(e))
      NULL
    }
  )
}

fits <- fits[!vapply(fits, is.null, logical(1))]

if (length(fits) == 0L) {
  stop("No candidate distribution could be fitted.")
}

cat("\nSuccessful fits:", paste(names(fits), collapse = ", "), "\n\n")

cat("--- 3. Inspecting fitted parameters ---\n")

for (dist in names(fits)) {
  cat("\nDistribution:", dist, "\n")
  print(coef(fits[[dist]]))
}

cat("\n--- 4. Comparing return levels ---\n")

return_periods <- c(2, 5, 10, 25, 50, 100, 200)

return_level_tables <- lapply(fits, function(fit) {
  alea_return_level(fit, return_period = return_periods)
})

return_level_tables_df <- lapply(return_level_tables, as.data.frame)

all_return_level_columns <- unique(unlist(lapply(return_level_tables_df, names)))

return_level_tables_df <- lapply(return_level_tables_df, function(tab) {
  missing_columns <- setdiff(all_return_level_columns, names(tab))
  
  for (col in missing_columns) {
    tab[[col]] <- NA
  }
  
  tab <- tab[, all_return_level_columns, drop = FALSE]
  tab
})

return_levels_all <- do.call(rbind, return_level_tables_df)
row.names(return_levels_all) <- NULL

cat("Combined return-level table:\n")
print(return_levels_all)
cat("\n")

cat("--- 5. Comparing goodness-of-fit statistics ---\n")

gof_tables <- lapply(fits, function(fit) {
  alea_gof(fit, statistics = "all")
})

gof_all <- do.call(rbind, lapply(gof_tables, as.data.frame))
row.names(gof_all) <- NULL

cat("Combined GOF table:\n")
print(gof_all)
cat("\n")

cat("AIC ranking, where lower AIC is better:\n")
aic_rank <- gof_all[gof_all$statistic == "aic", c("distribution", "method", "estimate")]
aic_rank <- aic_rank[order(aic_rank$estimate), ]
row.names(aic_rank) <- NULL
print(aic_rank)
cat("\n")

cat("BIC ranking, where lower BIC is better:\n")
bic_rank <- gof_all[gof_all$statistic == "bic", c("distribution", "method", "estimate")]
bic_rank <- bic_rank[order(bic_rank$estimate), ]
row.names(bic_rank) <- NULL
print(bic_rank)
cat("\n")

cat(
  "Teaching note: model comparison should not rely on one number only.\n",
  "Compare GOF statistics, diagnostics, return-level behavior, uncertainty,\n",
  "sample size, and hydrological judgement.\n\n",
  sep = ""
)

cat("--- 6. Plotting selected comparisons ---\n")

# Plot the first fitted model as a single-model diagnostic view.
first_fit <- fits[[1]]
p_first_return <- plot(
  first_fit,
  type = "return_level",
  return_period = return_periods
)

cat("Printing return-level plot for the first successful fitted model...\n")
print(p_first_return)

# Plot GOF summaries using the combined GOF object. The object retains the
# alea_gof class when row-bound in many workflows; if not, users can still
# inspect the printed table above.
if (inherits(gof_tables[[1]], "alea_gof")) {
  gof_for_plot <- do.call(rbind, gof_tables)
  class(gof_for_plot) <- unique(c("alea_gof", class(gof_for_plot)))

  p_gof_stat <- plot(gof_for_plot, type = "statistic")
  p_gof_rank <- plot(gof_for_plot, type = "rank")

  cat("Printing GOF statistic plot...\n")
  print(p_gof_stat)

  cat("Printing GOF rank plot...\n")
  print(p_gof_rank)
}

cat("\n============================================================\n")
cat("Example 02 completed successfully.\n")
cat("============================================================\n")

