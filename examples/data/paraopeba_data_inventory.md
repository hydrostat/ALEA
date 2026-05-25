# Paraopeba data inventory for ALEA-R examples

## Purpose

This folder provides small, static, local datasets for the ALEA-R teaching examples. The examples do not require internet access.

## Files

| File | Variable | Main column | Intended use |
|---|---|---:|---|
| `paraopeba_annual_max_flow.csv` | Annual maximum flow | `flow_m3s` | Main single-site frequency-analysis example |
| `paraopeba_annual_max_rainfall.csv` | Annual maximum rainfall | `rainfall_mm` | Secondary hydrological frequency-analysis example |
| `paraopeba_annual_mean_flow.csv` | Annual mean flow | `flow_m3s` | Supplementary exploratory example |

## General policy

The examples are designed for teaching ALEA-R workflows, not for making final engineering decisions. Users should review data quality, station history, hydrological consistency, and local design standards before applying frequency-analysis results in practice.

## Package-scope constraints

The examples use only the supported ALEA-R distributions:

```text
gev, gpa, pe3, ln2, ln3, gum
```

The examples do not use LP3, Portuguese aliases, HidroWeb access, calibrated GOF p-values, chi-square GOF tests, or confidence-interval methods beyond the implemented bootstrap quantile intervals.

## Output policy

Generated figures and CSV files are written under `examples/output/` when users run the scripts. Generated output files are not intended to be versioned in the repository.
