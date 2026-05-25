# Phase 13 External Validation Report

## Purpose

Phase 13 strengthened ALEA-R validation using external and semi-external validation cases after the `0.1.0` GitHub pre-release.

The phase did not change the public API, package scope, supported distributions, confidence-interval methods, goodness-of-fit methods, or AI-selection interpretation policy.

ALEA-R remains limited to:

- `gev`
- `gpa`
- `pe3`
- `ln2`
- `ln3`
- `gum`

LP3 remains excluded. Portuguese aliases remain excluded. FADS_AI output remains decision-support evidence, not proof of the true generating distribution.

## Validation strategy

Phase 13 used lightweight, reproducible validation cases suitable for ordinary package development workflows.

The cases covered:

- hand-checked theoretical formulas;
- direct independent `lmom` reference calculations;
- ALEA-R LN2/LN3 convention checks;
- direct goodness-of-fit statistic recalculation;
- bundled FADS_AI light-model metadata and prediction checks;
- synthetic multi-station batch workflow validation.

Only deterministic, synthetic, bundled, or license-safe cases were promoted to automated tests.

## Completed validation cases

| Case ID | Category | Workflow area | Status | Automated test |
|---|---|---|---|---|
| P13-GUM-THEORY-001 | theoretical_handcheck | Gumbel quantiles | PASS | `tests/testthat/test-phase13-gumbel-reference.R` |
| P13-LMOM-REF-001 | independent_r_reference | L-moment fitted quantiles | PASS | `tests/testthat/test-phase13-lmom-reference-comparisons.R` |
| P13-LN2-LN3-001 | theoretical_handcheck | LN2/LN3 zero-threshold convention | PASS | `tests/testthat/test-phase13-ln2-ln3-zero-threshold.R` |
| P13-GOF-REF-001 | theoretical_handcheck | GOF statistics and information criteria | PASS | `tests/testthat/test-phase13-gof-reference-recalculation.R` |
| P13-AI-LIGHT-001 | fads_ai_reference | FADS_AI light-model metadata and prediction | PASS | `tests/testthat/test-phase13-ai-light-reference.R` |
| P13-BATCH-001 | batch_workflow_reference | Integrated batch workflow | PASS | `tests/testthat/test-phase13-batch-reference-workflow.R` |

## Reference output files

The Phase 13 scripts generated the following reference outputs:

```text
validation/reference_outputs/phase13_gumbel_reference.csv
validation/reference_outputs/phase13_lmom_reference_comparisons.csv
validation/reference_outputs/phase13_ln2_ln3_zero_threshold_reference.csv
validation/reference_outputs/phase13_gof_reference_recalculation.csv
validation/reference_outputs/phase13_ai_light_reference_check.csv
validation/reference_outputs/phase13_batch_reference_summary.csv
```

## Validation scripts

The completed Phase 13 validation scripts are:

```text
validation/scripts/phase13_01_theoretical_gumbel_reference.R
validation/scripts/phase13_02_lmom_reference_comparisons.R
validation/scripts/phase13_03_ln2_ln3_zero_threshold_reference.R
validation/scripts/phase13_04_gof_reference_recalculation.R
validation/scripts/phase13_05_ai_light_reference_check.R
validation/scripts/phase13_06_batch_reference_workflow.R
```

## Automated tests added

The completed automated tests are:

```text
tests/testthat/test-phase13-gumbel-reference.R
tests/testthat/test-phase13-lmom-reference-comparisons.R
tests/testthat/test-phase13-ln2-ln3-zero-threshold.R
tests/testthat/test-phase13-gof-reference-recalculation.R
tests/testthat/test-phase13-ai-light-reference.R
tests/testthat/test-phase13-batch-reference-workflow.R
```

## Case summaries

### P13-GUM-THEORY-001

Validated Gumbel quantiles against the closed-form Gumbel quantile formula:

```text
p = 1 - 1 / T
z_T = xi - alpha * log(-log(p))
```

The comparison used a deterministic synthetic annual-maximum-like sample and return periods:

```text
2, 5, 10, 25, 50, 100
```

Result: PASS.

### P13-LMOM-REF-001

Validated ALEA-R L-moment fitted quantiles against direct `lmom` reference calculations for:

```text
gev, gpa, gum, pe3
```

The comparison used:

```r
lmom::samlmu()
lmom::pelgev()
lmom::quagev()
lmom::pelgpa()
lmom::quagpa()
lmom::pelgum()
lmom::quagum()
lmom::pelpe3()
lmom::quape3()
```

Result: PASS.

### P13-LN2-LN3-001

Validated the ALEA-R convention that `ln2` behaves as `ln3` with fixed lower bound:

```text
zeta = 0
```

The checked formula was:

```text
Q(p) = exp(mu + sigma * qnorm(p))
```

Result: PASS.

### P13-GOF-REF-001

Validated `alea_gof()` by recalculating the following statistics directly for a Gumbel L-moment fit:

```text
ks
cvm
ad
loglik
aic
bic
```

The direct recalculation used fitted Gumbel CDF and density formulas.

Result: PASS.

### P13-AI-LIGHT-001

Validated bundled FADS_AI light-model availability, metadata, validation-file metadata, and ordinary `alea_select()` behavior.

Confirmed:

```text
model_version: 1.0.0-light
parent_model_version: 1.0.0
scenario: classical
algorithm: xgb
candidate distributions: gev, gpa, pe3, ln2, ln3, gum
feature columns: lmom_l1, lmom_l2, lmom_l3, lmom_l4, lmom_t3, lmom_t4
n_validation_rows: 1000
max_abs_probability_difference: 0
class_agreement: 1
```

A tolerance of `1e-6` was used for the operational predicted-probability sum because the XGBoost prediction output may show small floating-point differences from exactly 1.

Result: PASS.

### P13-BATCH-001

Validated a small integrated batch workflow with three synthetic stations and two fitted distributions:

```text
stations: A, B, C
distributions: gev, gum
method: lmom
return periods: 10, 50, 100
```

The workflow validated:

```text
alea_batch_fit()
alea_results()
stations
fits
quantiles
gof
diagnostics
selection
selected_models
errors
```

Result: PASS.

## Tolerance policy used

Phase 13 used the following tolerance policy:

| Validation type | Tolerance or policy |
|---|---|
| S3 classes, output columns, status values | Exact |
| Return-period probability convention | Tight numerical check |
| Closed-form formulas | `1e-10` where feasible |
| Direct `lmom` comparisons | `1e-8` |
| GOF recalculation | `1e-10` |
| AI validation metadata | Exact |
| AI operational probability sum | `1e-6` |
| Batch workflow structure | Exact structural checks |

## Deferred validation cases

The following case categories remain useful but were not required to complete Phase 13:

| Category | Reason for deferral |
|---|---|
| Published hydrological examples | Requires source selection and review of reproducible numerical tables |
| Legacy ALEA outputs | Requires available legacy ALEA output files, screenshots, reports, or exported tables |
| Real hydrological datasets | Requires licensing review before storing raw data in the repository |

These can be added later without changing the public API.

## Data and licensing outcome

Phase 13 did not add external raw datasets to the installed package.

All completed cases used:

- synthetic data;
- bundled package validation files;
- direct formulas;
- direct `lmom` reference calculations.

No new files were added to `inst/extdata`.

## Metadata note

The possible `Depends: R (>= 3.5.0)` follow-up remains a Phase 15 CRAN-readiness candidate.

Phase 13 did not reveal an installation or runtime failure related to R serialization.

## Completion status

Phase 13 completion criteria were met:

- external validation inventory created;
- theoretical hand-check validation completed;
- independent R/reference validation completed;
- LN2/LN3 convention validation completed;
- GOF direct recalculation completed;
- FADS_AI light-model validation completed;
- representative batch workflow validation completed;
- all accepted automated validation cases promoted to `testthat`;
- individual Phase 13 tests passed;
- full test suite passed after Phase 13 additions;
- no public API expansion occurred;
- no package-scope expansion occurred.

## Final Phase 13 status

```text
Phase 13 External Validation Cases: COMPLETE
Status: PASS
Public API changes: none
Package scope changes: none
```
