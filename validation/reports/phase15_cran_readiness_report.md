# Phase 15 CRAN-readiness report

## Package

- Package: ALEA
- Version checked: 0.1.0
- Repository: https://github.com/hydrostat/ALEA
- Phase: 15 — CRAN-readiness assessment

## Objective

Assess whether ALEA-R is ready for eventual CRAN submission, identify CRAN-blocking issues, and prepare a practical pre-CRAN remediation plan without changing the package scope, public API, or statistical methods.

Phase 15 remained a readiness and risk-reduction phase. It did not add distributions, public functions, confidence interval methods, GOF methods, diagnostics methods, or new data-access functionality.

## Source-of-truth constraints

The following constraints remained unchanged during Phase 15:

- all package code, function names, arguments, documentation, warnings, errors, examples, and vignettes remain in English;
- no Portuguese aliases are included;
- supported distributions remain `gev`, `gpa`, `pe3`, `ln2`, `ln3`, and `gum`;
- LP3 remains excluded;
- user-facing objects use S3 classes;
- bootstrap remains the only implemented quantile confidence interval method;
- calibrated GOF p-values and chi-square GOF tests remain deferred;
- FADS_AI output is decision-support evidence, not proof of the true generating distribution;
- HidroWeb access remains future optional scope and is not part of the core release.

## Phase 15 recommendations completed

### 1. Add `R (>= 3.5.0)` to `DESCRIPTION`

Completed.

`DESCRIPTION` was updated to include:

```text
Depends:
    R (>= 3.5.0)
```

This records the R version requirement associated with the bundled `.rds` serialization used by the FADS_AI light model.

### 2. Exclude `README.Rmd` from the CRAN source package

Completed.

`README.Rmd` is retained for GitHub/source maintenance, but it is excluded from the CRAN source package through `.Rbuildignore`.

The rendered `README.md` remains available for repository users.

### 3. Ensure `examples/output/` is excluded from the CRAN source package

Completed.

Generated teaching outputs under `examples/output/` are excluded from the CRAN source package. The teaching examples remain GitHub-oriented materials and generated output files are not shipped.

### 4. Prepare `cran-comments.md` explanation for the bundled FADS_AI light model

Completed.

`cran-comments.md` now explains:

- the final local R CMD check result;
- the new-submission note;
- the installed-size note caused by `inst/extdata/fads_ai/`;
- the purpose of the bundled FADS_AI light model;
- the offline and reproducible nature of `alea_select(x)`;
- the validation metadata for the light model;
- the environment-related future timestamp note;
- why `xgboost` remains in `Imports`.

### 5. Run clean CRAN-oriented local check with vignettes and `run_dont_test = TRUE`

Completed.

A clean CRAN-oriented local check was run after fixing local LaTeX tooling. Earlier failures in the PDF manual step were traced to missing local LaTeX executables (`pdflatex`, then `makeindex`), not to package Rd errors.

Final local check result:

```text
0 errors | 0 warnings | 3 notes
```

Remaining notes:

```text
checking CRAN incoming feasibility ... NOTE
  New submission

checking installed package size ... NOTE
  installed size is 12.4Mb
  sub-directories of 1Mb or more:
    extdata  11.3Mb

checking for future file timestamps ... NOTE
  unable to verify current time
```

### 6. Confirm `xgboost` remains acceptable as a hard dependency

Completed.

`xgboost` remains acceptable in `Imports` because it is required by the default public AI-assisted distribution-selection workflow:

```r
alea_select(x)
```

Moving `xgboost` to `Suggests` would make the default AI workflow unavailable after ordinary installation. The previous heavier tidymodels prediction stack is not used in the default ALEA-R AI workflow.

## R CMD check note classification

| Note | Classification | CRAN risk | Action |
|---|---|---:|---|
| New submission | Expected for first CRAN submission | Low | Explain in `cran-comments.md` |
| Installed package size | Acceptable with explanation | Medium | Explain bundled FADS_AI light model and offline workflow |
| Unable to verify current time | Local environment note | Low | Explain as environment-related; recheck on external services if needed |

## CRAN-blocking issues

No package-level CRAN-blocking issues remain based on the final local check.

The final CRAN-oriented local check has:

```text
0 errors | 0 warnings | 3 notes
```

The remaining notes are explainable.

## Package-size assessment

The installed package size note is caused by the bundled FADS_AI light model and validation metadata under:

```text
inst/extdata/fads_ai/
```

The model is intentionally bundled because the default AI-assisted workflow is part of the public ALEA-R API and should work offline:

```r
alea_select(x)
```

Removing the model would either break the default workflow or require runtime external downloads, which would be less appropriate for CRAN and less reproducible for users.

Current decision:

```text
Keep the bundled FADS_AI light model and explain the installed-size note in cran-comments.md.
```

## Dependency assessment

The key Phase 15 dependency decision concerns `xgboost`.

Decision:

```text
Keep xgboost in Imports.
```

Rationale:

- `alea_select(x)` is a public core workflow;
- the bundled FADS_AI light model requires `xgboost` for local prediction;
- making `xgboost` optional would weaken the default user experience;
- the package no longer depends on the heavier tidymodels prediction stack for the default AI workflow.

## `.Rbuildignore` and source-package cleanup

Phase 15 source-package cleanup removed or excluded CRAN-irrelevant files and directories from the built source package, including:

- `.github/`;
- `README.Rmd`;
- `README.html`;
- `Rplot.png`;
- `examples/`;
- `examples/output/`;
- `doc/`;
- `Meta/`;
- `LICENSE.md`;
- development-only summaries and prompts;
- validation/planning/chat-summary materials as applicable.

The final check no longer reports hidden-file, GitHub-directory, README source, examples, or top-level file notes.

## Documentation and manual PDF assessment

The package documentation passed after the local LaTeX toolchain was completed.

Earlier `R CMD check` failures in the PDF manual step were environmental:

- first, `pdflatex` was unavailable to the check subprocess;
- then, `makeindex` was unavailable.

After resolving the local toolchain, the manual PDF step no longer produced an error or warning in the final reported result.

## Examples, tests, and vignettes

The final CRAN-oriented local check reported no errors or warnings in:

- package installation;
- namespace checks;
- dependency checks;
- R code checks;
- Rd checks;
- documentation consistency;
- examples;
- tests;
- vignettes;
- HTML manual.

The Phase 14 teaching examples remain repository-level teaching material and are excluded from the CRAN source package.

## Current CRAN-readiness decision

Current decision:

```text
Technically close to CRAN-ready, with submission still intentionally deferred until an explicit CRAN submission decision is made.
```

ALEA-R is now in a strong pre-CRAN state:

- no errors;
- no warnings;
- only three explainable notes;
- cleaned CRAN source package structure;
- bundled model rationale documented;
- `xgboost` dependency rationale documented;
- `cran-comments.md` prepared.

## Recommended next actions before actual CRAN submission

Before submitting to CRAN, run or confirm:

1. One more clean local `--as-cran` check from a fresh R session.
2. GitHub Actions passing on Linux, macOS, and Windows.
3. Optional external checks through rhub or win-builder if desired.
4. Review `cran-comments.md` immediately before submission and update check dates/platforms.
5. Consider whether the first CRAN submission should use version `0.1.0` or a new patch version if the repository has already published `0.1.0` as a GitHub pre-release.

## Final Phase 15 checkpoint

```text
DESCRIPTION R version metadata: updated
CRAN source package cleanup: completed
examples/output exclusion: completed
cran-comments.md: prepared
xgboost dependency decision: recorded
CRAN-oriented local check: 0 errors | 0 warnings | 3 notes
CRAN-blocking issues identified: none
Submission status: deferred until explicit decision
```
