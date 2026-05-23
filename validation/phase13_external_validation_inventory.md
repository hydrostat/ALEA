# Phase 13 External Validation Inventory

## Purpose

This inventory records external and semi-external validation cases used to strengthen ALEA-R after the `0.1.0` GitHub pre-release.

Phase 13 does not change the public API, package scope, supported distributions, confidence-interval methods, or goodness-of-fit methods.

ALEA-R remains limited to:

- `gev`
- `gpa`
- `pe3`
- `ln2`
- `ln3`
- `gum`

LP3 remains excluded. Portuguese aliases remain excluded. FADS_AI output remains decision-support evidence, not proof of the true generating distribution.

## Validation objectives

Phase 13 validates existing ALEA-R workflows against external or semi-external references, including:

- hand-checked theoretical cases;
- independent R/reference calculations;
- published hydrological frequency-analysis examples, when suitable;
- legacy ALEA outputs, when available;
- small representative hydrological series;
- bundled FADS_AI light-model validation artifacts;
- batch-analysis reference workflows.

The goal is validation hardening, not feature expansion.

## Validation case status values

- `candidate`: identified but not yet reviewed.
- `accepted_manual`: suitable for manual/reference validation only.
- `accepted_automated`: suitable for automated `testthat` regression tests.
- `deferred`: useful but postponed.
- `rejected`: reviewed and not suitable.

## Validation source categories

- `published_reference`
- `legacy_alea`
- `theoretical_handcheck`
- `independent_r_reference`
- `representative_hydrological_series`
- `fads_ai_reference`
- `batch_workflow_reference`

## Automated versus manual validation policy

A validation case may become an automated test when:

- input data are small;
- data are synthetic, bundled, or clearly redistributable;
- expected values are deterministic or reproducible with fixed seeds;
- comparisons are stable across platforms;
- computation is lightweight enough for ordinary `devtools::test()` and R CMD check;
- no internet access is required.

A validation case should remain manual/reference-only when:

- the source is a screenshot, PDF table, or legacy report with limited precision;
- the result depends strongly on optimization details;
- licensing or redistribution is unclear;
- the computation is expensive;
- the comparison is interpretive rather than numerical.

## Numerical tolerance policy

| Validation type | Policy |
|---|---|
| S3 classes, names, columns, statuses | Exact checks |
| Probability conventions | Exact or near machine precision |
| Closed-form deterministic formulas | Tight tolerance, usually `1e-10` to `1e-8` |
| Direct `lmom` reference comparisons | Tight to moderate tolerance, usually `1e-8` to `1e-6` |
| Independent R/package comparisons | Moderate tolerance, usually `1e-6` to `1e-4` |
| Published rounded examples | Match reported precision |
| Optimization-sensitive MLE examples | Wider or qualitative tolerance |
| Bootstrap CI checks | Fixed seed; check structure, finite values, ordering, and reproducibility |
| AI-selection checks | Exact only for bundled validation metadata; otherwise qualitative/ranking checks |
| AI operational probability sums | Tolerance `1e-6` to allow floating-point prediction differences |
| Batch workflow structure | Exact structural checks |

## Data licensing and storage policy

External raw data should not be placed in the installed package unless the data are small, useful to users, redistributable, and clearly licensed.

Validation-only data should remain under `validation/`, not `inst/extdata/`, unless there is a strong reason to include them in the installed package.

If redistribution rights are unclear, store only:

- citation;
- source notes;
- instructions for obtaining the data;
- derived summary/reference outputs when allowed;
- scripts that can be run by users who have obtained the data independently.

Phase 13 did not add external raw datasets to the installed package. Completed cases used synthetic data, bundled package validation files, direct formulas, and direct `lmom` reference calculations.

## Smoke-test versus reference-regression distinction

Smoke tests verify that workflows run and return valid objects.

Reference-regression tests verify that known numerical outputs remain stable.

Phase 13 includes both structural smoke-style checks and reference-regression checks. Only deterministic, lightweight, license-safe cases were promoted to `tests/testthat/`.

## Final folder structure used

```text
validation/
  phase13_external_validation_inventory.md

  scripts/
    phase13_01_theoretical_gumbel_reference.R
    phase13_02_lmom_reference_comparisons.R
    phase13_03_ln2_ln3_zero_threshold_reference.R
    phase13_04_gof_reference_recalculation.R
    phase13_05_ai_light_reference_check.R
    phase13_06_batch_reference_workflow.R

  reference_outputs/
    phase13_gumbel_reference.csv
    phase13_lmom_reference_comparisons.csv
    phase13_ln2_ln3_zero_threshold_reference.csv
    phase13_gof_reference_recalculation.csv
    phase13_ai_light_reference_check.csv
    phase13_batch_reference_summary.csv

  reports/
    phase13_validation_report.md
```

## Automated tests added

```text
tests/testthat/test-phase13-gumbel-reference.R
tests/testthat/test-phase13-lmom-reference-comparisons.R
tests/testthat/test-phase13-ln2-ln3-zero-threshold.R
tests/testthat/test-phase13-gof-reference-recalculation.R
tests/testthat/test-phase13-ai-light-reference.R
tests/testthat/test-phase13-batch-reference-workflow.R
```

## Inventory table

| Case ID | Category | Source | Workflow area | Data status | Expected comparison | Automation status | Result | Notes |
|---|---|---|---|---|---|---|---|---|
| P13-GUM-THEORY-001 | theoretical_handcheck | Closed-form Gumbel formula | Return levels | Synthetic | Tight deterministic comparison | accepted_automated | PASS | Validated `p = 1 - 1 / T` and `z_T = xi - alpha * log(-log(p))`; automated in `test-phase13-gumbel-reference.R` |
| P13-LMOM-REF-001 | independent_r_reference | `lmom` reference calculations | Distribution wrappers and L-moment fitted return levels | Synthetic | Direct `lmom` comparison | accepted_automated | PASS | Validated `gev`, `gpa`, `gum`, and `pe3` against direct `lmom` functions; automated in `test-phase13-lmom-reference-comparisons.R` |
| P13-LN2-LN3-001 | theoretical_handcheck | ALEA-R LN2/LN3 convention | Distribution wrappers and return levels | Synthetic | Tight deterministic comparison | accepted_automated | PASS | Validated `ln2` as `ln3` with `zeta = 0`; automated in `test-phase13-ln2-ln3-zero-threshold.R` |
| P13-GOF-REF-001 | theoretical_handcheck | Direct EDF/log-likelihood formulas | GOF statistics and information criteria | Synthetic | Direct recalculation | accepted_automated | PASS | Recalculated `ks`, `cvm`, `ad`, `loglik`, `aic`, and `bic`; automated in `test-phase13-gof-reference-recalculation.R` |
| P13-AI-LIGHT-001 | fads_ai_reference | Bundled FADS_AI light model and validation file | AI selection | Bundled package file + synthetic sample | Exact metadata and structural checks; probability sum tolerance `1e-6` | accepted_automated | PASS | Validated model availability, metadata, validation metadata, and ordinary `alea_select()` output; automated in `test-phase13-ai-light-reference.R` |
| P13-BATCH-001 | batch_workflow_reference | Synthetic multi-station data | Batch analysis | Synthetic | Structural checks plus selected numeric checks | accepted_automated | PASS | Validated integrated `alea_batch_fit()` / `alea_results()` workflow; automated in `test-phase13-batch-reference-workflow.R` |
| P13-PUB-001 | published_reference | To be identified | Fitting and return levels | To be reviewed | Match reported precision | deferred | Not run | Deferred because source selection and reproducible numerical tables require separate review |
| P13-LEGACY-001 | legacy_alea | To be identified | Fitting and return levels | To be reviewed | Match reported precision | deferred | Not run | Deferred until legacy ALEA outputs, screenshots, reports, or exported tables are available |

## Completed reference outputs

| File | Case ID | Status |
|---|---|---|
| `validation/reference_outputs/phase13_gumbel_reference.csv` | P13-GUM-THEORY-001 | PASS |
| `validation/reference_outputs/phase13_lmom_reference_comparisons.csv` | P13-LMOM-REF-001 | PASS |
| `validation/reference_outputs/phase13_ln2_ln3_zero_threshold_reference.csv` | P13-LN2-LN3-001 | PASS |
| `validation/reference_outputs/phase13_gof_reference_recalculation.csv` | P13-GOF-REF-001 | PASS |
| `validation/reference_outputs/phase13_ai_light_reference_check.csv` | P13-AI-LIGHT-001 | PASS |
| `validation/reference_outputs/phase13_batch_reference_summary.csv` | P13-BATCH-001 | PASS |

## Metadata note

The possible `Depends: R (>= 3.5.0)` follow-up remains a Phase 15 CRAN-readiness candidate unless validation reveals an installation or runtime problem related to R serialization.

Phase 13 did not reveal an installation or runtime failure related to R serialization.

## Phase 13 completion criteria

Phase 13 can be considered complete because:

- this inventory was reviewed and updated;
- each identified case has a status;
- at least one theoretical hand-check case was completed;
- at least one independent R/reference calculation was completed;
- at least one AI-selection validation check was completed;
- at least one representative workflow or batch validation case was completed;
- all accepted automated validation cases were promoted to `testthat`;
- promoted automated tests passed in isolation;
- the full test suite passed after Phase 13 additions;
- no public API expansion occurred;
- no package-scope expansion occurred.

## Final Phase 13 status

```text
Phase 13 External Validation Cases: COMPLETE
Status: PASS
Public API changes: none
Package scope changes: none
```
