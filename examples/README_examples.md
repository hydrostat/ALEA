# ALEA-R teaching examples

This folder contains sourceable teaching scripts for the current ALEA-R API.

Run scripts from the package root, for example:

```r
source("examples/01_single_site_basic_workflow.R")
```

The examples are intentionally explicit: they use `cat()` for section headings and `print()` for objects and plots so that results appear when scripts are run with `source()`.

## Scripts

| Script | Purpose |
|---|---|
| `00_run_all_examples.R` | Runs all teaching scripts in order. |
| `01_single_site_basic_workflow.R` | One model, one hydrological series. |
| `02_compare_distributions_workflow.R` | Several models for one series with `alea_compare`. |
| `03_quantile_ci_workflow.R` | Quantiles and bootstrap quantile confidence intervals. |
| `04_gof_diagnostics_selection_workflow.R` | GOF, diagnostics, and AI-assisted selection. |
| `05_batch_analysis_workflow.R` | Several stations/sites through the batch workflow. |
| `06_plots_and_exports_workflow.R` | Plot review and export helpers. |

Generated outputs are written under `examples/output/` and should not be committed to the repository.
