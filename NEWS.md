# ALEA 0.2.1

## Installation fix

- Removed generated vignette build metadata accidentally committed under `build/`.
- Removed generated `README.html` from the source repository.
- Fixed GitHub installation with `remotes::install_github("hydrostat/ALEA")`.

No statistical methods, public API functions, supported distributions, examples,
or workflows were changed from ALEA 0.2.0.

# ALEA 0.2.0

## Phase 17 — Multi-model single-site workflow

- Added `alea_compare()` as the user-facing workflow for fitting several supported distribution-method combinations to one hydrological series.
- Updated `alea_fit()` so one distribution-method combination still returns `alea_fit`, while vector-valued `distribution` or `method` inputs return `alea_compare`.
- Added S3 methods for `alea_compare`: `print()`, `summary()`, `as.data.frame()`, `coef()`, `alea_quantile()`, `alea_gof()`, `confint()`, and `plot()`.
- Added bootstrap quantile confidence intervals across all successfully fitted models in an `alea_compare` object.
- Added automatic alignment of distribution-specific parameter columns in combined quantile, confidence-interval, and coefficient outputs.
- Added structured error capture so one failed distribution-method fit does not stop the single-site multi-model workflow.
- Preserved the existing one-model `alea_fit()` behavior for scalar `distribution` and `method` inputs.
- Kept the statistical scope unchanged: supported distributions remain `gev`, `gpa`, `pe3`, `ln2`, `ln3`, and `gum`; LP3, Portuguese aliases, calibrated GOF p-values, chi-square GOF tests, new confidence-interval methods, and HidroWeb access remain outside this release.

## Highlights

- Added external validation cases for theoretical, numerical, FADS_AI,
  goodness-of-fit, and batch-analysis workflows.
- Added user-facing teaching examples using public Paraopeba hydrological data.
- Added README guidance for sourceable learning examples.
- Added explicit `R (>= 3.5.0)` package metadata.
- Improved CRAN-oriented source-package cleanup through `.Rbuildignore`.
- Added `cran-comments.md` and a Phase 15 CRAN-readiness report.
- Confirmed that `xgboost` remains a justified hard dependency for the default
  offline `alea_select(x)` workflow.
- Confirmed local CRAN-oriented check status: 0 errors, 0 warnings, and 3
  explainable notes.
- Confirmed GitHub Actions R CMD check passes.

## Scope

This release does not change the public API or package scope.

Supported distributions remain:

- `gev`
- `gpa`
- `pe3`
- `ln2`
- `ln3`
- `gum`

LP3 remains outside the current implementation scope.

FADS_AI output remains decision-support evidence. It should be interpreted
together with goodness-of-fit results, diagnostics, L-moment behavior, quantile
uncertainty, and hydrological judgement. It should not be interpreted as proof
of the true generating distribution.

## CRAN status

ALEA-R is technically close to CRAN-ready, but CRAN submission is intentionally
deferred while the package is used more broadly through GitHub.

# ALEA 0.1.0

Planned first public development release of ALEA-R, an R package for
hydrological frequency analysis.

## Main features

- Added a compact, workflow-oriented public API for hydrological frequency
  analysis.
- Added S3 user-facing objects for fitted models, quantiles,
  confidence intervals, goodness-of-fit results, diagnostics, AI-assisted
  selection, and batch analysis.
- Added support for the initial six candidate distributions:
  - GEV (`gev`);
  - GPA (`gpa`);
  - PE3 (`pe3`);
  - two-parameter lognormal (`ln2`);
  - three-parameter lognormal (`ln3`);
  - Gumbel (`gum`).
- Adopted Hosking's `lmom` parameterization and parameter names for supported
  distributions.
- Excluded LP3 from the initial implementation.
- Kept all package code, function names, arguments, documentation, warnings,
  errors, examples, and vignettes in English.

## Distribution fitting

- Added `alea_fit()` for fitting one supported probability distribution to a
  hydrological sample.
- Added support for L-moment, method-of-moments, and maximum-likelihood fitting
  where implemented for the supported distributions.
- Added internal density, distribution, quantile, random-generation,
  parameter-validation, fitting, and quantile helpers for all six
  supported distributions.
- Added `print.alea_fit()` and `coef.alea_fit()` methods.

## Quantiles and confidence intervals

- Added `alea_quantile()` and `alea_quantile.alea_fit()`.
- Added the `alea_quantile` S3 data-frame output.
- Added `print.alea_quantile()` and `as.data.frame.alea_quantile()`.
- Added `confint.alea_fit()` for quantile confidence intervals.
- Added percentile bootstrap confidence intervals for quantiles.
- Added the `alea_quantile_ci` S3 data-frame output.
- Added `print.alea_quantile_ci()` and
  `as.data.frame.alea_quantile_ci()`.

## Goodness-of-fit and diagnostics

- Added `alea_gof()` and `alea_gof.alea_fit()`.
- Added goodness-of-fit statistics:
  - Kolmogorov-Smirnov statistic (`ks`);
  - Cramer-von Mises statistic (`cvm`);
  - Anderson-Darling statistic (`ad`);
  - log-likelihood (`loglik`);
  - Akaike information criterion (`aic`);
  - Bayesian information criterion (`bic`).
- Added the `alea_gof` S3 data-frame output.
- Added `print.alea_gof()` and `as.data.frame.alea_gof()`.
- Added `alea_diagnostics()` methods for numeric vectors and `alea_fit`
  objects.
- Added simple diagnostics for sample size, missing or non-finite values, tied
  values, range adequacy, and sample skewness.
- Added optional hypothesis-based diagnostics through the suggested package
  `trend`:
  - randomness through `trend::bartels.test()`;
  - independence through `trend::ww.test()`;
  - homogeneity through `trend::pettitt.test()`;
  - stationarity through `trend::mk.test()`.
- Added structured warning rows when optional diagnostic tests are unavailable
  or fail internally.

## AI-assisted distribution selection

- Added `alea_select()` for AI-assisted distribution-selection support.
- Added `alea_select.numeric()` and `alea_select.alea_fit()`.
- Added `alea_ai_model_info()` and `print.alea_ai_model_info()`.
- Added the `alea_selection` S3 output.
- Added `print.alea_selection()` and `as.data.frame.alea_selection()`.
- Added internal FADS_AI feature extraction using sample L-moment descriptors:
  - `lmom_l1`;
  - `lmom_l2`;
  - `lmom_l3`;
  - `lmom_l4`;
  - `lmom_t3`;
  - `lmom_t4`.
- Bundled the FADS_AI lightweight operational application model under
  `inst/extdata/fads_ai/fads_ai_application_model_light.rds`.
- Bundled the FADS_AI light-model validation file under
  `inst/extdata/fads_ai/fads_ai_application_model_light_validation.csv`.
- Replaced ordinary operational dependence on the large full tidymodels
  workflow model with the lightweight model.
- Added expanded decision-support output with candidate ranking, predicted
  probabilities, top1-top2 margin, decision-strength label, interpretation
  text, application features, model metadata, warnings, and the original call.
- Normalized FADS_AI distribution labels to ALEA-R lowercase names in
  user-facing outputs.
- Documented FADS_AI output as model-based decision-support evidence, not proof
  of the true generating distribution.

## Batch analysis

- Added `alea_batch_fit()` for multi-station or multi-site workflows.
- Added `alea_results()` as the official extractor for `alea_batch` objects.
- Added the `alea_batch` S3 output.
- Added `print.alea_batch()` and `as.data.frame.alea_batch()`.
- Added station-level metadata, fit-summary tables, optional quantile
  tables, optional goodness-of-fit tables, optional diagnostics tables,
  station-level AI-selection summaries, and selected-model summaries.
- Stored successful fitted models in `fit_objects`.
- Stored successful AI-selection objects in `selection_objects`.
- Added structured error capture so failures for one station, distribution,
  method, quantile calculation, goodness-of-fit calculation, diagnostic
  calculation, or selection step do not stop the full batch workflow.
- Ensured empty batch error outputs retain stable columns:
  `station`, `step`, `distribution`, `method`, `message`, and `class`.
- Added batch AI selection using the bundled FADS_AI light model by default,
  with optional support for a pre-loaded model object or a model path.

## Plots and exports

- Added ggplot-based S3 plot methods:
  - `plot.alea_fit()`;
  - `plot.alea_quantile()`;
  - `plot.alea_quantile_ci()`;
  - `plot.alea_gof()`;
  - `plot.alea_diagnostics()`;
  - `plot.alea_selection()`;
  - `plot.alea_batch()`.
- Added fitted density, fitted CDF, Q-Q, P-P, and quantile plots for
  `alea_fit` objects.
- Added quantile plots for point estimates and bootstrap confidence
  intervals.
- Added goodness-of-fit statistic and rank plots.
- Added diagnostics status and p-value plots.
- Added AI-selection probability-ranking plots.
- Added batch plots for selected models, quantiles, goodness-of-fit
  summaries, and diagnostics summaries.
- Added the shared ALEA plotting theme through `alea_plot_theme()`.
- Finalized publication-oriented visual defaults after manual visual review.
- Added `alea_save_plot()` for saving one ggplot object.
- Added `alea_save_plots()` for saving a list of ggplot objects.
- Added `alea_export()` for exporting ggplot objects, lists of ggplot objects,
  data frames, and flat `alea_batch` tables.
- Added support for PDF, PNG, TIFF/TIF, and SVG plot export. SVG export remains
  conditional on optional graphics-device support.

## Documentation and release preparation

- Added package-level documentation.
- Added an initial `README.Rmd` and generated `README.md`.
- Updated package metadata in `DESCRIPTION`.
- Cleaned documentation issues detected by `R CMD check`.
- Reached the current release-preparation checkpoint:
  - `devtools::test()`: 1637 passed tests, 0 failures, 0 warnings, 2 skipped
    tests;
  - `devtools::check()`: 0 errors, 0 warnings, 4 notes.
- The skipped tests are conditional SVG export tests requiring optional
  `svglite` support.

## Deferred or excluded from this release

- LP3 distribution support.
- Portuguese API aliases.
- Parameter confidence intervals.
- Asymptotic quantile confidence intervals.
- Generic delta-method quantile confidence intervals.
- Calibrated goodness-of-fit p-values.
- Chi-square goodness-of-fit tests.
- HidroWeb access in the core package.
