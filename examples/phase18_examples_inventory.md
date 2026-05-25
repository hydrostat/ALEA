# ALEA-R examples inventory

## Purpose

The `examples/` folder provides sourceable teaching scripts for GitHub users, students, instructors, and applied hydrologists.

The examples complement the installed help pages and vignettes:

- help pages: concise API references;
- vignettes: narrative installed tutorials;
- examples: complete, copyable teaching workflows.

## Current scripts

| Script | Main workflow |
|---|---|
| `00_run_all_examples.R` | Runs all examples in order. |
| `01_single_site_basic_workflow.R` | Single model for one series with `alea_fit`. |
| `02_compare_distributions_workflow.R` | Several models for one series with `alea_compare`. |
| `03_quantile_ci_workflow.R` | Quantiles and bootstrap quantile confidence intervals. |
| `04_gof_diagnostics_selection_workflow.R` | GOF, diagnostics, AI-assisted selection, and model metadata. |
| `05_batch_analysis_workflow.R` | Batch analysis for several sites/stations. |
| `06_plots_and_exports_workflow.R` | Plot objects and export helpers. |

## API coverage

The examples cover the main public ALEA-R workflows:

- `alea_fit()` for one model;
- `alea_fit()` dispatching to `alea_compare` for several models;
- `alea_compare()` behavior through downstream methods;
- `alea_quantile()`;
- `confint(..., parm = "quantile")`;
- `alea_gof()`;
- `alea_diagnostics()`;
- `alea_select()`;
- `alea_ai_model_info()`;
- `alea_dist()`;
- `alea_fit()` dispatching to batch analysis for data frames;
- `alea_results()` compact batch output and full-table extraction through `as.data.frame()`;
- S3 `plot()` methods;
- `alea_save_plot()`;
- `alea_save_plots()`;
- `alea_export()`.

## Output policy

Generated files are written to `examples/output/`. This directory contains a `.gitignore` file so generated outputs are not versioned.

## Package constraints

The examples use only supported distributions:

```text
gev, gpa, pe3, ln2, ln3, gum
```

The examples do not use LP3, Portuguese aliases, HidroWeb access, calibrated GOF p-values, chi-square GOF tests, or unimplemented confidence-interval methods.
