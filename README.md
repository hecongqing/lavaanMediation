# lavaanMediation

English | [简体中文](README.zh-CN.md)

[![R-CMD-check](https://github.com/hecongqing/lavaanMediation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hecongqing/lavaanMediation/actions/workflows/R-CMD-check.yaml)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE.md)
[![R >= 4.1](https://img.shields.io/badge/R-%3E%3D%204.1-276DC3.svg)](https://www.r-project.org/)

`lavaanMediation` is an interactive parallel mediation analysis tool built with
[Shiny](https://shiny.posit.co/) and [`lavaan`](https://lavaan.ugent.be/). It
helps researchers configure, inspect, and export mediation-model results while
retaining the complete `lavaan` syntax for review and reproducibility.

> The current version is `0.2.0`. The application assists with model
> specification and result organization; it does not replace research design,
> model-identification checks, causal assumptions, or statistical diagnostics.

## Features

- B1: `X → M → Y`, including the direct `X → Y` effect.
- C2: B1 plus one or more covariates `Z`, with `Z → M` and `Z → Y` paths.
- Zero, one, or multiple parallel mediators.
- Manual Bootstrap confidence intervals for indirect effects, with the number
  of successful resamples reported.
- Model syntax, fit indices, parameter estimates, R² values, and path diagrams.
- CSV exports for Model Fit and Parameter Estimates, including main-effect,
  mediation-model, and R² results.
- CSV upload and built-in synthetic demonstration data.

## Installation

```r
install.packages("remotes")
remotes::install_github("hecongqing/lavaanMediation")
```

## Running the app

After installation, run:

```r
lavaanMediation::run_app()
```

To run from source:

```bash
git clone https://github.com/hecongqing/lavaanMediation.git
cd lavaanMediation
Rscript start_app.R
```

The startup script does not modify your R environment automatically. If a
dependency is missing, it reports the packages that need to be installed.

## Data requirements

The first row of the CSV file must contain variable names. Variables selected
as X, M, Y, or Z must:

- be numeric columns;
- be assigned to only one role within a model;
- contain enough usable observations for `lavaan` estimation; and
- have clear, unique column names.

Two synthetic datasets are available under [`inst/extdata`](inst/extdata):

- `mediation_demo.csv`: a single-mediator example;
- `parallel_mediation_demo.csv`: a multiple-mediator, multiple-covariate example.

## Model syntax

Single-mediator B1 model:

```text
M ~ a1*X
Y ~ cprime*X
Y ~ b1*M
indirect := a1*b1
total := cprime + (a1*b1)
```

C2 model with a covariate:

```text
M ~ a1*X
M ~ zm1_1*Z
Y ~ cprime*X
Y ~ b1*M
Y ~ zy1*Z
indirect := a1*b1
total := cprime + (a1*b1)
```

For multiple mediators, the app creates distinct `a`, `b`, and `indirect`
labels for each mediator and calculates the total indirect effect.

## Outputs

- **Model Fit**: estimator, number of parameters and observations, Bootstrap
  count, χ², df, CFI, TLI, AIC, BIC, adjusted BIC, RMSEA with 90% CI, and SRMR.
- **Parameter Estimates**: unstandardized and standardized coefficients,
  standard errors, z statistics, p values, and 95% CIs for the main-effect and
  mediation models.
- **R²**: R² from the full mediation model and the main-effect-model R² for Y
  used in the effect-conservation comparison.
- **Path Diagram**: main-effect and mediation models with standardized
  coefficients.
- **Download**: CSV files generated from the fitted analysis configuration.

## Project structure

```text
lavaanMediation/
├── R/                         # Public package interface (run_app)
├── inst/
│   ├── app/
│   │   ├── app.R             # Minimal Shiny entry point
│   │   ├── R/                # Modeling, results, plotting, UI, and server
│   │   └── www/              # Concept-model images
│   └── extdata/              # Synthetic example data
├── tests/testthat/            # Model, download, and UI tests
├── .github/workflows/         # R CMD check workflow
├── start_app.R                # Source-tree launcher
└── deploy_app.R               # shinyapps.io deployment entry point
```

The application follows the package structure described in
[R Packages](https://r-pkgs.org/structure.html), Posit's guidance for
[Shiny applications in R packages](https://shiny.posit.co/r/articles/share/deployment-local),
the separation-of-responsibilities approach used by
[`golem`](https://github.com/ThinkR-open/golem), and the R package CI practices
provided by [`r-lib/actions`](https://github.com/r-lib/actions).

## Development and checks

```r
install.packages(c("devtools", "testthat"))
devtools::test()
devtools::check()
```

See [QUICKSTART.md](QUICKSTART.md) for a short walkthrough,
[USAGE.md](USAGE.md) for detailed usage, and
[CONTRIBUTING.md](CONTRIBUTING.md) before submitting an issue or pull request.

## License

[MIT License](LICENSE.md) © 2026 Congqing He
