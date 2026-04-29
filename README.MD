# Plots and Tables for EJ Revision

This repository creates plots and tables for an EJ revision on displaced workers.

## Folder Structure

- `data/external`: Data from outside the project scope.
- `data/interim`: Transformed files that may be reused during the project.
- `results`: Project outputs, including plots and tables.
- `src`: Project scripts.

## Data Context

- Input data comes from the secured CASD enclave.
- The data contains either descriptive statistics or regression results.
- All data was exported as CSV files.
- The data comes from two separate exports: some outcomes are taken from export 1, and others from export 2.
- An already joined dataset was created for the event-study results `plotting_data.parquet`.

## Regression Results

1. A single regression can usually be identified by its `model_id`.
   - Exception: for `tbl_a6`, the export failed and IDs must be reconstructed using formulas.

2. Coefficient identifiers differ by specification:
   - For event-study regressions, the main coefficient descriptor is `distance`, which gives the relative distance to treatment.
   - For non-event-study regressions, regression results are stored under `variable`.

3. Common regression columns:
   - `beta`: estimated coefficient.
   - `std_error`: standard error.
   - `tval`: t-statistic.
   - `pval`: p-value.
   - `.*r^2.*`: R-squared-related statistics.

4. `dep_var` is the model's dependent variable.

5. `sample` describes the sample on which the estimation was completed:
   - `le5_panelsize0`: Sample licenciements économiques, no size restriction.
   - `le5_panelsize10`: Sample licenciements économiques, requires 10 workers at displacement time.
   - `le5_panelsize20`: Sample licenciements économiques, does not match on firm characteristics. For export 2 outcomes, this sample also does not include a fixed effect.
   - `le5_panelsize40`: Sample licenciements économiques, requires that the firm have no employment growth between t-1 and t.
   - `panelpanSIZE1`: Sample mass layoff 30%, requires no displacement over observation period.
   - `panelpanSIZE1`: Sample mass layoff 30%, requires L > 50.
   - `panelpanSIZE1`: Sample mass layoff 90%, requires L > 8.

6. `description` contains any meaningful additional information, such as added controls or whether the estimation was run on a subsample.

## Notes for Agents

Avoid excessive abstraction layering if it reduces legibility.