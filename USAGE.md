# User Guide

## Data

Upload a CSV or use the built-in synthetic data. The app lists numeric columns only. X, M, Y, and Z must be assigned to different columns.

Missing observations are handled by `lavaan` using its configured defaults. Inspect the effective sample size and model diagnostics before interpreting results.

## Models

### B1: parallel mediation

The app estimates X-to-M paths, M-to-Y paths, the direct X-to-Y path, each indirect effect, and the total effect. Selecting no M variable produces a direct-effect model.

### C2: parallel mediation with covariates

C2 adds each selected Z variable to every mediator equation and to the Y equation. At least one Z is required.

## Bootstrap

Bootstrap repetitions control the confidence interval for indirect effects. A fixed seed makes resampling reproducible. Failed resamples are skipped and the successful count is reported.

For exploratory runs, 100 repetitions are faster. For reported research results, choose the repetition count according to the study protocol and justify it explicitly.

## Outputs

### Data Preview

Shows the first N rows of the active dataset.

### Model Specification

Shows the exact `lavaan` syntax used for the full model.

### Model Fit

Reports model information and χ², df, CFI, TLI, AIC, BIC, adjusted BIC, RMSEA with 90% CI, and SRMR.

### Parameter Estimates

Contains:

- main-effect model paths;
- full mediation-model paths;
- indirect and total effects;
- R² values used by the application.

Bootstrap confidence intervals replace the corresponding indirect-effect confidence intervals when available.

### Path Diagram

Displays the main-effect model and full mediation model vertically. Coefficient labels are standardized estimates.

### Download

- `model_fit_YYYY-MM-DD.csv`: model information and fit indices.
- `parameter_estimates_YYYY-MM-DD.csv`: main-effect paths, mediation-model estimates, and R² rows.

## Troubleshooting

### C2 cannot run

Select at least one numeric Z column that is different from X, M, and Y.

### Model fitting fails

Check for insufficient sample size, constant columns, severe collinearity, missingness, or a model that is not identified.

### Some Bootstrap samples fail

The app continues and reports the successful count. If many samples fail, inspect data quality and model stability rather than relying on the resulting interval.

### Path diagram fails

Confirm that the model converged and that `semPlot` is installed. The result tables remain available even when a diagram cannot be drawn.
