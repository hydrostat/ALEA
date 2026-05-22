#' ALEA-R: Hydrological Frequency Analysis in R
#'
#' ALEA-R provides tools for hydrological frequency analysis, including
#' exploratory data analysis, probability distribution fitting, return-level
#' estimation, bootstrap confidence intervals, goodness-of-fit assessment,
#' sample diagnostics, AI-assisted distribution-selection support, batch
#' analysis, and publication-ready plots and exports.
#'
#' The package is designed as a modern R implementation of the ALEA workflow
#' for reproducible statistical hydrology. User-facing objects use S3 classes,
#' and the public API is intentionally small and workflow-oriented.
#'
#' @section Supported distributions:
#'
#' The initial implementation supports the following distributions:
#'
#' \itemize{
#'   \item GEV: Generalized Extreme Value distribution;
#'   \item GPA: Generalized Pareto distribution;
#'   \item PE3: Pearson type III distribution;
#'   \item LN2: two-parameter lognormal distribution;
#'   \item LN3: three-parameter lognormal distribution;
#'   \item GUM: Gumbel distribution.
#' }
#'
#' LP3 is not included in the initial implementation.
#'
#' @section Main workflows:
#'
#' The main user-facing workflows are:
#'
#' \itemize{
#'   \item data preparation and exploratory summaries with `alea_data()`,
#'     `alea_stats()`, and `alea_lmoments()`;
#'   \item distribution fitting with `alea_fit()` and `alea_compare()`;
#'   \item return-level estimation with `alea_return_level()`;
#'   \item bootstrap confidence intervals for return levels with `confint()`;
#'   \item goodness-of-fit assessment with `alea_gof()`;
#'   \item sample diagnostics with `alea_diagnostics()`;
#'   \item AI-assisted distribution-selection support with `alea_select()`;
#'   \item batch analysis with `alea_batch_fit()` and `alea_results()`;
#'   \item plotting and export with `plot()`, `alea_save_plot()`,
#'     `alea_save_plots()`, and `alea_export()`.
#' }
#'
#' @section Confidence intervals:
#'
#' The initial implementation provides percentile bootstrap confidence
#' intervals for return levels. Parameter confidence intervals, asymptotic
#' return-level intervals, and generic delta-method intervals are not
#' implemented in the initial release.
#'
#' @section Goodness-of-fit and diagnostics:
#'
#' Goodness-of-fit results report empirical distribution function statistics
#' and information criteria. Calibrated goodness-of-fit p-values and
#' chi-square goodness-of-fit tests are deferred.
#'
#' Diagnostics are sample-level checks intended to flag possible data-quality
#' issues or assumption concerns. Diagnostic warnings do not automatically
#' invalidate a fitted model.
#'
#' @section AI-assisted distribution selection:
#'
#' ALEA-R includes AI-assisted distribution-selection support through the
#' bundled FADS_AI lightweight operational application model.
#'
#' FADS_AI output should be interpreted as model-based decision-support
#' evidence for candidate distribution families. It is not proof of the true
#' generating distribution and should not replace goodness-of-fit assessment,
#' diagnostics, return-level uncertainty evaluation, or hydrological judgement.
#'
#' @section Plotting and export:
#'
#' Plot methods return `ggplot` objects. Export helpers can save plots and
#' flat result tables for reporting and publication workflows.
#'
#' @keywords internal
"_PACKAGE"