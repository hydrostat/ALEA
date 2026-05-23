# Phase 14 — User-Facing Examples and Teaching Material Inventory

## Status

Phase 14 status: in progress / examples layer completed locally

This inventory records the user-facing example material created for ALEA-R Phase 14.

Phase 14 focuses on practical, teaching-oriented examples for ALEA-R after the validated `0.1.0` GitHub pre-release. The phase is intentionally limited to user-facing examples, teaching notes, example data organization, and documentation usability. It does not change the public API, expand statistical scope, add new distributions, or address CRAN submission mechanics.

## Documentation usability checkpoint

Before creating the Phase 14 examples, the installed documentation layer was checked.

### Checks performed

```r
library(ALEA)
help(package = "ALEA")
?ALEA
utils::browseVignettes("ALEA")
system.file("html", "00Index.html", package = "ALEA")
```

### Outcome

```text
help(package = "ALEA"): OK
function help pages: OK
?ALEA: OK
utils::browseVignettes("ALEA"): OK after installation with build_vignettes = TRUE
installed vignettes visible in RStudio: OK
```

### Installation note

The local command that made installed vignettes visible was:

```r
devtools::install(
  build_vignettes = TRUE,
  dependencies = TRUE,
  upgrade = "never"
)
```

For GitHub installation, users who want installed vignettes should use:

```r
remotes::install_github(
  "hydrostat/ALEA",
  dependencies = TRUE,
  upgrade = "never",
  build_vignettes = TRUE
)
```

Standard GitHub installation without `build_vignettes = TRUE` is expected to install the package and function help pages, but may not install vignettes.

### Help index note

Opening `system.file("html", "00Index.html", package = "ALEA")` directly through a `file://` browser path is not the recommended test of installed help links. Function help pages should be accessed through R/RStudio help, for example:

```r
help(package = "ALEA")
help("alea_gof", package = "ALEA")
?alea_gof
```

## Objectives

The objectives of the Phase 14 examples layer are to:

1. Provide practical example scripts for common ALEA-R workflows.
2. Help users learn single-site and batch hydrological frequency analysis with ALEA-R.
3. Demonstrate the existing public API through clear, lightweight examples.
4. Support README-compatible educational material.
5. Provide scripts that can be copied, adapted, and used in teaching.
6. Use public hydrological teaching data from the Paraopeba case.
7. Keep examples reproducible without internet access.
8. Avoid long-running examples, especially expensive bootstrap examples.
9. Preserve the current package scope and public API.

## Target audience

The examples are intended for:

- hydrology students;
- applied hydrologists;
- researchers learning ALEA-R;
- users installing ALEA-R from GitHub;
- future vignette and README readers;
- instructors using ALEA-R in frequency-analysis classes;
- developers checking expected workflows manually.

The examples assume basic R knowledge but do not assume advanced package-development knowledge.

## Folder structure

The Phase 14 example files should be placed under the package root:

```text
package/ALEA/examples/
```

Recommended structure:

```text
examples/
  phase14_examples_inventory.md
  01_single_site_basic_workflow.R
  02_compare_distributions_workflow.R
  03_return_level_ci_workflow.R
  04_gof_diagnostics_selection_workflow.R
  05_batch_analysis_workflow.R
  06_plots_and_exports_workflow.R
  data/
    paraopeba_data_inventory.md
    paraopeba_annual_max_flow.csv
    paraopeba_annual_max_rainfall.csv
    paraopeba_annual_mean_flow.csv
  output/
  teaching/
```

The `examples/output/` directory is used for generated teaching artifacts and can be cleaned safely.

## Data policy

Phase 14 uses public hydrological data from the Paraopeba case, as discussed in *Fundamentals of Statistical Hydrology*.

The book is used as the teaching reference, but the examples do not copy protected book text, figures, formatted tables, or editorial conclusions.

The adopted policy is:

```text
Phase 14 may use complete public PLU/FLU hydrological series also presented
in Naghettini (2017), provided that ALEA-R documents public data provenance,
cites the book as a teaching reference, and does not reproduce protected book
text, figures, formatted tables, or editorial conclusions.
```

The examples should refer to the data as public hydrological records and to Naghettini (2017) as the teaching reference.

## Teaching reference

```text
Naghettini, M. (ed.) (2017). Fundamentals of Statistical Hydrology.
Springer International Publishing.
```

## Paraopeba teaching data files

### 1. Annual maximum mean daily flow

File:

```text
examples/data/paraopeba_annual_max_flow.csv
```

Expected columns:

```text
water_year
flow_m3s
station_code
station_name
```

Use:

- single-site frequency analysis;
- return-level estimation;
- GOF statistics;
- diagnostics;
- AI-assisted selection;
- plots and exports;
- batch demonstration.

Primary examples:

```text
01_single_site_basic_workflow.R
02_compare_distributions_workflow.R
03_return_level_ci_workflow.R
04_gof_diagnostics_selection_workflow.R
06_plots_and_exports_workflow.R
```

### 2. Annual maximum daily rainfall

File:

```text
examples/data/paraopeba_annual_max_rainfall.csv
```

Expected columns:

```text
water_year
rainfall_mm
station_code
station_name
```

Use:

- rainfall teaching example;
- missing-data handling;
- batch demonstration;
- exploratory comparison with flow series, when appropriate.

Primary example:

```text
05_batch_analysis_workflow.R
```

### 3. Annual mean daily flow

File:

```text
examples/data/paraopeba_annual_mean_flow.csv
```

Expected columns:

```text
year
flow_m3s
station_name
```

Use:

- optional exploratory data analysis;
- descriptive-statistics teaching;
- future teaching notes.

Primary examples:

```text
future optional EDA script
future teaching notes
```

## General script policy

All Phase 14 scripts should:

- use English comments, section headers, object names, messages, warnings, and explanatory text;
- use only the current public ALEA-R API;
- avoid Portuguese aliases;
- avoid unsupported distributions;
- avoid LP3;
- avoid internet access;
- avoid large datasets;
- run from the ALEA package root directory;
- use `cat()` for visible section headers;
- use `print()` for results that should appear when run with `source()`;
- use `print()` for ggplot objects so plots appear when run with `source()`;
- be suitable for RStudio users who run the full script or run it line by line.

Recommended execution pattern:

```r
setwd("C:/Users/wilso/OneDrive/Projects/R_ALEA/package/ALEA")
source("examples/01_single_site_basic_workflow.R")
```

## Public API used in examples

The examples use only the current public API:

```r
alea_fit()
alea_return_level()
confint()
alea_gof()
alea_diagnostics()
alea_select()
alea_ai_model_info()
alea_batch_fit()
alea_results()
plot()
alea_save_plot()
alea_save_plots()
alea_export()
coef()
as.data.frame()
```

No unexported internal functions are used.

## Distribution policy

Examples are restricted to the distributions implemented in ALEA-R:

```r
c("gev", "gpa", "pe3", "ln2", "ln3", "gum")
```

LP3 is not used or presented as supported.

## AI-selection interpretation policy

Examples using `alea_select()` state that FADS_AI probabilities are model-based decision-support evidence.

They must not describe the AI-selected family as proof of the true generating distribution.

Recommended wording:

```text
FADS_AI probabilities are model-based decision-support evidence for candidate
families. They should not be interpreted as proof of the true generating
distribution or as an automatic replacement for hydrological and statistical
judgement.
```

## Bootstrap uncertainty policy

Examples using `confint()` should explain that:

- ALEA-R currently implements percentile bootstrap intervals for return levels;
- small `n_boot` is used only to keep teaching scripts fast;
- production analysis should use larger `n_boot`;
- `n_success` and `n_failed` should be inspected;
- long return-period estimates from short samples can be highly uncertain.

For Phase 14 teaching scripts, the adopted example setting is:

```r
n_boot = 50
```

## GOF and diagnostics interpretation policy

Examples using `alea_gof()` and `alea_diagnostics()` should explain that:

- GOF statistics and information criteria are evidence for model comparison;
- EDF GOF p-values are not calibrated in ALEA-R 0.1.0;
- diagnostic warnings do not automatically invalidate a fitted model;
- hydrological judgement remains necessary.

Recommended wording:

```text
GOF statistics and diagnostics should be interpreted as evidence, not as
automatic accept/reject rules.
```

## Return-period plotting policy

For examples that use fitted return-level plots, the adopted return periods are:

```r
return_periods <- c(2, 5, 10, 25, 50, 100, 200)
```

Reason:

- this avoids truncating observed plotting positions in return-level plots;
- it avoids a warning about observed plotting positions larger than the largest requested return period;
- it gives a more complete teaching plot.

Teaching caution:

```text
The 100-year and 200-year return levels are extrapolations beyond the central
part of the observed record. They should be interpreted together with
uncertainty, diagnostics, and hydrological judgement.
```

## Data-frame binding policy

When combining `alea_return_level()` outputs from different distributions, scripts must not use direct `rbind()` unless columns are known to match.

Reason:

- different distributions append different fitted-parameter columns;
- for example, GEV/GPA/GUM, LN2/LN3, and PE3 may produce different parameter columns.

Recommended helper pattern:

```r
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
```

## Export policy

Example 06 validated the following export behavior:

- `alea_save_plot()` does not use an `overwrite` argument in the current API;
- `alea_save_plots()` does not use an `overwrite` argument in the current API;
- existing plot files should be removed explicitly in teaching scripts before re-exporting;
- output directories should be created before calling export helpers;
- `alea_export()` supports `overwrite = TRUE` for data-frame and batch table exports.

Required directory creation pattern:

```r
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(multi_plot_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(batch_export_dir, recursive = TRUE, showWarnings = FALSE)
```

## Completed scripts

### 01_single_site_basic_workflow.R

Status:

```text
PASS after local manual execution
```

Purpose:

Demonstrates a minimal single-site ALEA-R frequency-analysis workflow.

Data:

```text
examples/data/paraopeba_annual_max_flow.csv
```

Main workflow:

```r
alea_fit()
coef()
alea_return_level()
alea_gof()
alea_diagnostics()
plot()
```

Main choices:

```r
distribution = "gev"
method = "lmom"
return_periods = c(2, 5, 10, 25, 50, 100, 200)
```

Manual validation notes:

- results print correctly with `source()`;
- plots appear with `source()` because ggplot objects are explicitly printed;
- return periods extended to 200 years to avoid observed plotting-position truncation warning.

### 02_compare_distributions_workflow.R

Status:

```text
PASS after local manual execution and column-alignment fix
```

Purpose:

Compares several candidate distributions for one annual maximum flow series.

Data:

```text
examples/data/paraopeba_annual_max_flow.csv
```

Main workflow:

```r
alea_fit()
alea_return_level()
alea_gof()
plot()
```

Candidate distributions used:

```r
candidate_distributions <- c("gev", "gum", "pe3", "ln2", "ln3")
```

Main correction applied:

- direct `rbind()` of return-level tables failed because different distributions have different parameter columns;
- the script now aligns columns and fills missing parameter columns with `NA` before binding.

Manual validation notes:

- distribution fitting works;
- return-level comparison works;
- GOF comparison works;
- AIC/BIC rankings print correctly;
- plots print correctly.

### 03_return_level_ci_workflow.R

Status:

```text
PASS after local manual execution
```

Purpose:

Teaches return-level estimation with percentile bootstrap confidence intervals.

Data:

```text
examples/data/paraopeba_annual_max_flow.csv
```

Main workflow:

```r
alea_fit()
alea_return_level()
confint()
plot()
```

Main choices:

```r
distribution = "gev"
method = "lmom"
n_boot = 50
seed = 123
```

Teaching note:

- `n_boot = 50` is used only to keep the example fast;
- production analyses should use larger `n_boot` and inspect `n_success` and `n_failed`.

### 04_gof_diagnostics_selection_workflow.R

Status:

```text
PASS after local manual execution
```

Purpose:

Teaches combined interpretation of GOF, diagnostics, and AI-assisted selection.

Data:

```text
examples/data/paraopeba_annual_max_flow.csv
```

Main workflow:

```r
alea_fit()
alea_gof()
alea_diagnostics()
alea_select()
alea_ai_model_info()
as.data.frame()
plot()
```

Teaching notes:

- GOF statistics and diagnostics are evidence, not automatic accept/reject rules;
- FADS_AI output is decision-support evidence;
- FADS_AI output is not proof of the true generating distribution.

### 05_batch_analysis_workflow.R

Status:

```text
PASS after local manual execution
```

Purpose:

Demonstrates a small batch workflow using Paraopeba flow and rainfall annual maxima as two teaching series.

Data:

```text
examples/data/paraopeba_annual_max_flow.csv
examples/data/paraopeba_annual_max_rainfall.csv
```

Main workflow:

```r
alea_batch_fit()
alea_results()
plot()
```

Main choices:

```r
distributions = c("gev", "gum")
methods = "lmom"
return_period = c(2, 5, 10, 25, 50, 100, 200)
gof = TRUE
diagnostics = TRUE
select = "ai"
```

Teaching caution:

- the two series have different units;
- the script demonstrates batch mechanics, not regional analysis;
- each station/variable should be interpreted separately.

Manual validation notes:

- batch object prints correctly;
- stations, fits, selected models, return levels, GOF, diagnostics, selection, and errors tables extract correctly;
- errors table exists even when empty;
- batch plots print correctly.

### 06_plots_and_exports_workflow.R

Status:

```text
PASS after local manual execution and export fixes
```

Purpose:

Demonstrates plot creation and export of plots, data frames, and batch flat tables.

Data:

```text
examples/data/paraopeba_annual_max_flow.csv
```

Main workflow:

```r
plot()
alea_save_plot()
alea_save_plots()
alea_export()
alea_batch_fit()
```

Generated output location:

```text
examples/output/
```

Subdirectories:

```text
examples/output/example06_plots/
examples/output/example06_batch_tables/
```

Main corrections applied:

- removed `overwrite = TRUE` from `alea_save_plot()`;
- removed `overwrite = TRUE` from `alea_save_plots()`;
- output directories are created before export;
- existing plot files are removed explicitly before plot export;
- batch export directory is created before calling `alea_export()`.

Manual validation notes:

- selected plots print correctly;
- one plot exports correctly;
- multiple plots export correctly;
- return-level, GOF, diagnostics, and AI-selection tables export correctly;
- batch flat tables export correctly.

## Manual validation summary

The user manually executed the Phase 14 scripts one by one.

Final status:

```text
01_single_site_basic_workflow.R: OK
02_compare_distributions_workflow.R: OK after column-alignment fix
03_return_level_ci_workflow.R: OK
04_gof_diagnostics_selection_workflow.R: OK
05_batch_analysis_workflow.R: OK
06_plots_and_exports_workflow.R: OK after export-API and directory fixes
```

Overall examples status:

```text
Phase 14 examples scripts: PASS after local manual execution
```

## Automated example-smoke test policy

No automated Phase 14 example-smoke tests have been added yet.

Recommended decision:

```text
Do not immediately automate all teaching scripts.
```

Reason:

- the scripts are user-facing and intentionally verbose;
- some scripts create plots and output files;
- some scripts run bootstrap or AI selection;
- manual execution is more appropriate for teaching examples.

Possible future lightweight smoke test:

```text
tests/testthat/test-phase14-example-smoke.R
```

Candidate checks:

- verify example data files exist;
- source a reduced subset of scripts in a temporary output directory;
- check scripts complete without error;
- avoid testing visual appearance;
- avoid long bootstrap runs;
- avoid optional SVG/device dependencies.

For now, Phase 14 manual execution is sufficient.

## README update recommendation

Add a short README section pointing users to the examples folder.

Suggested text:

```markdown
## Learning examples

Teaching-oriented ALEA-R workflow scripts are available in the `examples/`
folder.

The examples use public Paraopeba hydrological data and demonstrate:

- single-site frequency analysis;
- comparison of candidate distributions;
- return levels and bootstrap confidence intervals;
- goodness-of-fit, diagnostics, and AI-assisted selection;
- small batch analysis;
- plots and exports.

Run them from the package root directory, for example:

```r
source("examples/01_single_site_basic_workflow.R")
```

Users who want installed vignettes from GitHub should install with:

```r
remotes::install_github(
  "hydrostat/ALEA",
  dependencies = TRUE,
  upgrade = "never",
  build_vignettes = TRUE
)
```
```

## Completion criteria

### Completed

- [x] Installed help pages checked.
- [x] Installed vignettes checked.
- [x] Vignette installation behavior clarified.
- [x] `examples/` folder structure defined.
- [x] Paraopeba data policy defined.
- [x] `examples/data/paraopeba_data_inventory.md` created.
- [x] `examples/data/paraopeba_annual_max_flow.csv` created.
- [x] `examples/data/paraopeba_annual_max_rainfall.csv` created.
- [x] `examples/data/paraopeba_annual_mean_flow.csv` created.
- [x] `examples/01_single_site_basic_workflow.R` created and manually validated.
- [x] `examples/02_compare_distributions_workflow.R` created and manually validated.
- [x] `examples/03_return_level_ci_workflow.R` created and manually validated.
- [x] `examples/04_gof_diagnostics_selection_workflow.R` created and manually validated.
- [x] `examples/05_batch_analysis_workflow.R` created and manually validated.
- [x] `examples/06_plots_and_exports_workflow.R` created and manually validated.
- [x] Scripts use only current public API.
- [x] Scripts use only supported distributions.
- [x] Scripts contain English-only comments and teaching text.
- [x] Scripts avoid internet access.
- [x] Bootstrap example uses lightweight teaching settings.
- [x] Bootstrap example warns that production `n_boot` should be larger.
- [x] FADS_AI examples describe AI output as decision-support evidence.
- [x] GOF and diagnostics examples avoid automatic accept/reject language.
- [x] Batch example inspects the structured `errors` table.
- [x] Plot/export example writes to safe example output locations.
- [x] No public API changes introduced.
- [x] No package-scope expansion introduced.
- [x] CRAN-readiness work remains deferred to Phase 15.

### Pending

- [ ] Add README section pointing to `examples/`.
- [ ] Decide whether to add lightweight automated example-smoke tests.
- [ ] Optionally add `examples/teaching/phase14_teaching_notes.md`.
- [ ] Run full package tests after all example files are committed, if any tests are added.
- [ ] Update planning files after Phase 14 completion:
  - `PROJECT_BRIEF.md`
  - `DECISIONS.md`
  - `API_SPEC.md`
  - `DISTRIBUTIONS.md`
  - `ARCHITECTURE.md`
  - `VALIDATION_PLAN.md`
  - `TASKS.md`
  - `CHANGELOG_DRAFT.md`

## Final Phase 14 examples checkpoint

```text
Documentation usability: PASS
Paraopeba teaching data: prepared
Example scripts 01-06: PASS after manual execution
Public API changes: none
Package scope changes: none
CRAN-readiness work: deferred to Phase 15
```
