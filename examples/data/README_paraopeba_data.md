# Paraopeba teaching data

These CSV files were prepared for ALEA-R Phase 14 educational examples.

Files:
- `paraopeba_annual_max_flow.csv`
- `paraopeba_annual_max_rainfall.csv`
- `paraopeba_annual_mean_flow.csv`

The series are public PLU/FLU hydrological records used as teaching data in
Naghettini (2017), *Fundamentals of Statistical Hydrology*. The book is cited as
the teaching reference; book text, figures, formatted tables, and editorial
conclusions are not redistributed here.

Missing values are encoded as blank cells in the CSV files so they are read as
`NA` by R's `read.csv()` with default settings.
