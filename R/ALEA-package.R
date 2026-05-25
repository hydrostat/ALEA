#' ALEA-R: Hydrological Frequency Analysis in R
#'
#' ALEA-R provides tools for hydrological frequency analysis, including
#' probability distribution fitting, quantile estimation, bootstrap confidence
#' intervals, goodness-of-fit assessment, sample diagnostics,
#' AI-assisted distribution-selection support, batch analysis, and
#' publication-ready plots and exports.
#'
#' The package implements a reproducible ALEA workflow for statistical
#' hydrology. User-facing objects use S3 classes, and the public API is
#' intentionally small and workflow-oriented.
#'
#' @section Supported distributions:
#'
#' ALEA-R supports the following distribution codes:
#'
#' \itemize{
#'   \item `gev`: Generalized Extreme Value distribution;
#'   \item `gpa`: Generalized Pareto distribution;
#'   \item `pe3`: Pearson type III distribution;
#'   \item `ln2`: two-parameter lognormal distribution;
#'   \item `ln3`: three-parameter lognormal distribution;
#'   \item `gum`: Gumbel distribution.
#' }
#'
#' @section Main workflows:
#'
#' The main user-facing workflows are:
#'
#' \itemize{
#'   \item model fitting with `alea_fit()` and `alea_compare()`;
#'   \item quantile estimation with `alea_quantile()`;
#'   \item bootstrap confidence intervals for quantiles with `confint()`;
#'   \item goodness-of-fit assessment with `alea_gof()`;
#'   \item sample diagnostics with `alea_diagnostics()`;
#'   \item AI-assisted distribution-selection support with `alea_select()`;
#'   \item batch analysis with `alea_batch_fit()` and `alea_results()`;
#'   \item plotting and export with `plot()`, `alea_save_plot()`,
#'     `alea_save_plots()`, and `alea_export()`;
#'   \item distribution and parameter-role lookup with `alea_dist()`.
#' }
#'
#' @section Confidence intervals:
#'
#' ALEA-R provides percentile bootstrap confidence intervals for quantiles.
#'
#' @section Goodness-of-fit and diagnostics:
#'
#' Goodness-of-fit results report empirical distribution function statistics
#' and information criteria.
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
#' diagnostics, quantile uncertainty evaluation, or hydrological judgement.
#'
#' @section Plotting and export:
#'
#' Plot methods return `ggplot` objects. Export helpers can save plots and
#' flat result tables for reporting and publication workflows.
#'
#' @examples
#' x <- c(42.1, 38.5, 51.3, 47.0, 62.4, 55.2, 49.8, 58.1,
#'        60.3, 45.9)
#' fit <- alea_fit(x, distribution = "gev", method = "lmom")
#' q <- alea_quantile(fit, return_period = c(10, 25, 50))
#' q
"_PACKAGE"