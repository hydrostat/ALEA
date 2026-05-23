## R CMD check results

0 errors | 0 warnings | 3 notes

## Check command

The final local CRAN-oriented check was run from the package root with a source tarball built by `pkgbuild::build()` and checked with:

```r
rcmdcheck::rcmdcheck(
  path = tarball,
  args = c("--as-cran"),
  error_on = "warning"
)
```

The local LaTeX toolchain was completed before the final successful check. The earlier local failures in the PDF manual check were caused by missing local LaTeX executables (`pdflatex` and then `makeindex`), not by package Rd errors.

## Test environments

- Local check: Windows 11 x64, R 4.3.1, UTF-8 session charset
- GitHub Actions: Linux, macOS, and Windows
- Additional GitHub Actions job with optional `svglite` installed

## Notes

### CRAN incoming feasibility

R CMD check reports:

```text
checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Wilson Fernandes <wilson@ehr.ufmg.br>'

  New submission
```

This is expected because ALEA-R is a new CRAN submission.

### Installed package size

R CMD check reports:

```text
checking installed package size ... NOTE
  installed size is 12.4Mb
  sub-directories of 1Mb or more:
    extdata  11.3Mb
```

The installed-size note is caused by the bundled FADS_AI lightweight operational model and its validation metadata stored under:

```text
inst/extdata/fads_ai/
```

These files are intentionally included in the package because they support ALEA-R's default AI-assisted distribution-selection workflow through:

```r
alea_select(x)
alea_ai_model_info()
```

The bundled model allows the default AI-assisted workflow to run offline, reproducibly, and without requiring users to download external model files at runtime.

The bundled model is not a newly trained model inside the package. It is a compact operational representation of the archived FADS_AI XGBoost application model used for hydrological distribution-selection support.

The corresponding validation metadata are included to document equivalence of the lightweight operational representation against the archived full workflow on the validation check. The validation records:

- 1000 validation rows;
- maximum absolute probability difference equal to 0;
- class agreement equal to 1.

The FADS_AI output is presented in ALEA-R as decision-support evidence for candidate distribution families, not as proof of the true generating distribution and not as a replacement for hydrological and statistical judgement.

Because `alea_select()` is part of the core public API of ALEA-R, removing the bundled model would make the default AI-assisted workflow unavailable offline and would require runtime external file access. For this reason, the model is bundled with the package despite the installed-size note.

### Future file timestamps

R CMD check reports:

```text
checking for future file timestamps ... NOTE
  unable to verify current time
```

This appears to be an environment-related timestamp verification note from the local Windows check environment. No future-dated package files are intentionally included.

## xgboost dependency

ALEA-R imports `xgboost` because it is required by the default AI-assisted distribution-selection workflow.

The public function:

```r
alea_select(x)
```

uses the bundled FADS_AI lightweight operational model stored under:

```text
inst/extdata/fads_ai/
```

The model is applied locally and offline. `xgboost` is required to reconstruct and use the serialized booster representation used by this bundled model.

We considered making `xgboost` optional, but doing so would make the default `alea_select(x)` workflow unavailable after a standard package installation. Because AI-assisted distribution-selection is part of ALEA-R's core public API, `xgboost` is kept in `Imports`.

The package does not use the previous heavier tidymodels prediction stack (`recipes`, `parsnip`, or `workflows`) for the default AI workflow.

## Downstream dependencies

There are currently no known downstream CRAN dependencies because this is the first planned CRAN submission of ALEA-R.
