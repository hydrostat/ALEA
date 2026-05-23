# Paraopeba Data Inventory

## Status

Phase 14 status: planned / in progress

This inventory defines the public Paraopeba hydrological series selected for ALEA-R user-facing examples and teaching material.

The Paraopeba case will be used as the main real-data teaching case for Phase 14, complementing any synthetic examples. The series are public hydrological records discussed in *Fundamentals of Statistical Hydrology* and are adapted here to the current ALEA-R public API.

## Purpose

The purpose of this inventory is to document:

- which Paraopeba series will be used in Phase 14 examples;
- the intended CSV file names;
- station identifiers and names;
- variable definitions;
- units;
- periods of record;
- water-year convention;
- missing-data handling;
- intended use in ALEA-R teaching scripts;
- provenance and redistribution policy.

## General policy

The Phase 14 examples may use complete public PLU/FLU hydrological series that are also presented in Naghettini (2017), provided that ALEA-R:

- documents the public data provenance;
- cites *Fundamentals of Statistical Hydrology* as the teaching reference;
- does not reproduce protected book text, figures, formatted tables, or editorial conclusions;
- uses the data in plain CSV form for reproducible teaching workflows;
- keeps all comments, column names, examples, and teaching text in English;
- uses only the current ALEA-R public API;
- uses only the supported ALEA-R distributions: `gev`, `gpa`, `pe3`, `ln2`, `ln3`, and `gum`;
- does not introduce LP3 or any other unsupported distribution.

## Teaching reference

Primary teaching reference:

Naghettini, M. (ed.) (2017). *Fundamentals of Statistical Hydrology*. Springer.

The book is used as the teaching reference for the hydrological frequency-analysis workflow, not as a source of copied package text, copied figures, or copied editorial conclusions.

## Data provenance note

The selected series are public Brazilian hydrological records associated with PLU/FLU gauging-station data. The book presents and discusses these records in worked examples and exercises.

ALEA-R examples should describe the data as public hydrological station records discussed in Naghettini (2017), not as proprietary package data created by ALEA-R.

Recommended note for scripts:

```r
# This example uses public Brazilian hydrological records discussed in
# Naghettini (2017), Fundamentals of Statistical Hydrology.
# The workflow is adapted to the ALEA-R public API.
```

## Selected series

### 1. Annual maximum daily rainfall at P. N. Paraopeba

Recommended CSV file:

```text
examples/data/paraopeba_annual_max_rainfall.csv
```

Source type:

```text
Public PLU hydrological station record
```

Teaching reference:

```text
Naghettini (2017), Chapter 1, Table 1.1
```

Station:

```text
P. N. Paraopeba
```

Station code:

```text
19440004
```

Variable:

```text
Annual maximum daily rainfall depth
```

Unit:

```text
mm
```

Time basis:

```text
Water year
```

Water-year convention:

```text
October 1 to September 30
```

Period represented in the book table:

```text
1941/42 to 1999/00
```

Missing values:

```text
Use NA for missing records.
```

Recommended CSV columns:

```text
water_year,rainfall_mm,station_code,station_name,source_note
```

Recommended use in Phase 14:

- exploratory data analysis;
- handling missing values;
- comparison with annual maximum flow data;
- optional rainfall-frequency teaching example;
- possible scatterplot or paired-data teaching example with annual maximum flow where concurrent years are available.

Primary ALEA-R workflows:

```r
alea_diagnostics()
alea_fit()
alea_return_level()
alea_gof()
plot()
```

Notes:

This series is useful for introducing annual maxima, missing values, and rainfall-frequency examples. For direct frequency-analysis workflows, examples should remove or omit missing values before fitting.

### 2. Annual maximum mean daily flow at P. N. Paraopeba

Recommended CSV file:

```text
examples/data/paraopeba_annual_max_flow.csv
```

Source type:

```text
Public FLU hydrological station record
```

Teaching reference:

```text
Naghettini (2017), Chapter 1, Table 1.2
```

Station:

```text
P. N. Paraopeba
```

Station code:

```text
40800001
```

Variable:

```text
Annual maximum mean daily discharge
```

Unit:

```text
m3/s
```

Catchment information:

```text
Paraopeba River catchment at Ponte Nova do Paraopeba
Drainage area: 5680 km2
```

Time basis:

```text
Water year
```

Water-year convention:

```text
October 1 to September 30
```

Period represented in the book table:

```text
1938/39 to 1998/99, with some years absent from the table
```

Missing values:

```text
Represent absent or unavailable records as NA only when an explicit missing record is part of the series structure.
Do not invent records for years that are not listed.
```

Recommended CSV columns:

```text
water_year,flow_m3s,station_code,station_name,source_note
```

Recommended use in Phase 14:

- main single-site flood-frequency example;
- distribution fitting;
- return-level estimation;
- bootstrap return-level confidence intervals;
- GOF statistics;
- diagnostics;
- AI-assisted model-selection support;
- fitted-model and return-level plots.

Primary ALEA-R workflows:

```r
alea_fit()
coef()
alea_return_level()
confint()
alea_gof()
alea_diagnostics()
alea_select()
plot()
alea_save_plot()
alea_export()
```

Recommended candidate distributions for teaching:

```r
c("gev", "gum", "pe3", "ln2", "ln3")
```

Optional candidate distribution:

```r
"gpa"
```

Note:

For annual maximum block-maxima examples, `gev`, `gum`, `pe3`, `ln2`, and `ln3` are likely the most natural teaching candidates. `gpa` may still be used because it is supported by ALEA-R, but the script should avoid implying that GPA is the standard block-maxima model.

### 3. Annual mean daily flow at Ponte Nova do Paraopeba

Recommended CSV file:

```text
examples/data/paraopeba_annual_mean_flow.csv
```

Source type:

```text
Public FLU hydrological station record
```

Teaching reference:

```text
Naghettini (2017), Chapter 2, Table 2.2
```

Station:

```text
Ponte Nova do Paraopeba
```

Variable:

```text
Annual mean daily discharge
```

Unit:

```text
m3/s
```

Time basis:

```text
Calendar year
```

Period represented in the book table:

```text
1938 to 1999
```

Missing values:

```text
No missing values are listed in the table.
```

Recommended CSV columns:

```text
year,flow_m3s,station_name,source_note
```

Recommended use in Phase 14:

- exploratory data analysis;
- descriptive statistics;
- histogram and empirical distribution examples;
- diagnostic examples;
- comparison between exploratory data analysis and frequency-analysis workflows.

Primary ALEA-R workflows:

```r
alea_diagnostics()
plot()
```

Optional ALEA-R workflows:

```r
alea_fit()
alea_gof()
alea_return_level()
```

Note:

This series is not the primary flood-frequency example because it represents annual mean flow rather than annual maxima. It is better suited to preliminary analysis and teaching of descriptive behavior.

## Initial Phase 14 data files to create

Priority 1:

```text
examples/data/paraopeba_annual_max_flow.csv
examples/data/paraopeba_annual_max_rainfall.csv
```

Priority 2:

```text
examples/data/paraopeba_annual_mean_flow.csv
```

## Recommended example mapping

### 01_single_site_basic_workflow.R

Primary data:

```text
examples/data/paraopeba_annual_max_flow.csv
```

Purpose:

Basic flood-frequency workflow for one station.

Main steps:

```r
x <- flow$flow_m3s
fit <- alea_fit(x, distribution = "gev", method = "lmom")
coef(fit)
alea_return_level(fit, return_period = c(10, 25, 50, 100))
alea_gof(fit)
alea_diagnostics(fit)
plot(fit)
```

### 02_compare_distributions_workflow.R

Primary data:

```text
examples/data/paraopeba_annual_max_flow.csv
```

Purpose:

Compare several supported ALEA-R distributions for the same annual maximum flow series.

Candidate distributions:

```r
c("gev", "gum", "pe3", "ln2", "ln3")
```

Optional:

```r
"gpa"
```

Main steps:

```r
fits <- lapply(distributions, function(d) alea_fit(x, distribution = d, method = "lmom"))
gof <- do.call(rbind, lapply(fits, alea_gof))
return_levels <- do.call(rbind, lapply(fits, alea_return_level, return_period = c(10, 50, 100)))
```

### 03_return_level_ci_workflow.R

Primary data:

```text
examples/data/paraopeba_annual_max_flow.csv
```

Purpose:

Teach return levels and percentile bootstrap uncertainty.

Main steps:

```r
fit <- alea_fit(x, distribution = "gev", method = "lmom")
rl <- alea_return_level(fit, return_period = c(10, 25, 50, 100))
ci <- confint(fit, parm = "return_level", return_period = c(10, 25, 50, 100), n_boot = 50, seed = 123)
plot(ci)
```

Teaching note:

Use small `n_boot` only to keep the example fast. Production analysis should use larger `n_boot` and should inspect `n_success` and `n_failed`.

### 04_gof_diagnostics_selection_workflow.R

Primary data:

```text
examples/data/paraopeba_annual_max_flow.csv
```

Purpose:

Teach combined interpretation of GOF, diagnostics, and FADS_AI selection support.

Main steps:

```r
fit <- alea_fit(x, distribution = "gev", method = "lmom")
gof <- alea_gof(fit)
diagnostics <- alea_diagnostics(fit)
selection <- alea_select(x)
as.data.frame(selection)
plot(selection)
```

Teaching note:

FADS_AI probabilities are model-based decision-support evidence and must be interpreted together with GOF statistics, diagnostics, uncertainty, sample size, and hydrological judgement.

### 05_batch_analysis_workflow.R

Primary data:

```text
examples/data/paraopeba_annual_max_flow.csv
examples/data/paraopeba_annual_max_rainfall.csv
```

Purpose:

Teach the batch interface using two small station-like series or two variable-specific teaching sites.

Possible approach:

Create a compact data frame with one `station` column and one `value` column, using:

- `paraopeba_flow_max`;
- `paraopeba_rainfall_max`.

Main steps:

```r
batch <- alea_batch_fit(
  data = batch_data,
  station = "station",
  value = "value",
  time = "water_year",
  distributions = c("gev", "gum"),
  methods = "lmom",
  return_period = c(10, 50, 100),
  gof = TRUE,
  diagnostics = TRUE,
  select = "ai"
)

alea_results(batch, "stations")
alea_results(batch, "fits")
alea_results(batch, "return_levels")
alea_results(batch, "gof")
alea_results(batch, "diagnostics")
alea_results(batch, "selection")
alea_results(batch, "selected_models")
alea_results(batch, "errors")
```

Caution:

When combining rainfall and flow as separate batch series, examples must not imply that they are physically equivalent variables. This is only a compact teaching use of the batch interface.

### 06_plots_and_exports_workflow.R

Primary data:

```text
examples/data/paraopeba_annual_max_flow.csv
```

Purpose:

Teach plot generation and export.

Main steps:

```r
fit <- alea_fit(x, distribution = "gev", method = "lmom")
rl <- alea_return_level(fit, return_period = c(10, 25, 50, 100))
gof <- alea_gof(fit)

p1 <- plot(fit, type = "density")
p2 <- plot(fit, type = "return_level")
p3 <- plot(gof)

alea_save_plot(p1, filename = "examples/output/paraopeba_density.png", overwrite = TRUE)
alea_save_plots(list(density = p1, return_level = p2), directory = "examples/output", overwrite = TRUE)
alea_export(rl, path = "examples/output/paraopeba_return_levels.csv", overwrite = TRUE)
```

## Column naming policy

Use simple English names in all CSV files.

Recommended names:

```text
water_year
year
flow_m3s
rainfall_mm
station_code
station_name
source_note
```

Avoid:

```text
Portuguese column names
special symbols in column names
spaces in column names
accented characters in column names
```

## Missing-data policy

Use `NA` for missing numeric values.

Examples should demonstrate explicit filtering before fitting:

```r
x <- flow$flow_m3s
x <- x[is.finite(x)]
```

For paired rainfall-flow examples, use complete cases only:

```r
paired <- paired[stats::complete.cases(paired[, c("rainfall_mm", "flow_m3s")]), ]
```

Do not impute missing values in the Phase 14 teaching examples unless the example is explicitly about missing-data handling.

## Scope limitations

The Paraopeba examples must not:

- add new package functions;
- add new supported distributions;
- use LP3;
- present FADS_AI as proof of the true distribution;
- imply that GOF statistics provide calibrated p-values;
- introduce chi-square GOF tests;
- introduce HidroWeb access as part of the core workflow;
- require internet access;
- require long bootstrap runs;
- reproduce book figures;
- copy book conclusions or paragraphs;
- convert Phase 14 into CRAN-readiness work.

## Completion checklist for Paraopeba data preparation

- [ ] Create `examples/data/`.
- [ ] Create `examples/data/paraopeba_data_inventory.md`.
- [ ] Create `examples/data/paraopeba_annual_max_flow.csv`.
- [ ] Create `examples/data/paraopeba_annual_max_rainfall.csv`.
- [ ] Optionally create `examples/data/paraopeba_annual_mean_flow.csv`.
- [ ] Check that numeric columns read correctly with `read.csv()`.
- [ ] Encode missing values as `NA`.
- [ ] Use English column names.
- [ ] Include source notes in the inventory, not as long repeated text in every data row.
- [ ] Confirm that the data files do not contain copied book text or formatted tables.
- [ ] Use the data in Phase 14 example scripts.
- [ ] Add a README or example note pointing users to Naghettini (2017) as the teaching reference.

## Phase 14 decision record candidate

Suggested decision for `DECISIONS.md` after Phase 14 completion:

```text
Phase 14 may use complete public PLU/FLU hydrological series from the Paraopeba case discussed in Naghettini (2017) as teaching data, provided that ALEA-R documents public data provenance, cites the book as a teaching reference, and does not reproduce protected book text, figures, formatted tables, or editorial conclusions. These examples remain user-facing teaching material and do not change the package API or distribution scope.
```
