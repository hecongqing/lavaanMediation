# Contributing

Issues and pull requests are welcome.

## Before opening an issue

- Search existing issues.
- Use synthetic or de-identified data only.
- Include `sessionInfo()` and a minimal reproducible example for bugs.
- Distinguish software defects from general questions about mediation analysis or `lavaan`.

## Development workflow

1. Fork and clone the repository.
2. Create a focused branch.
3. Install development dependencies with `devtools::install_deps(dependencies = TRUE)`.
4. Add or update tests under `tests/testthat/`.
5. Run `devtools::test()` and `devtools::check()`.
6. Start the app with `Rscript start_app.R` and exercise the affected workflow.

Keep pull requests small enough to review. Any change to generated model syntax, estimators, confidence intervals, fit indices, or R² calculations must explain the statistical rationale and include regression tests.

## Code organization

- `R/` contains the public package entry point.
- `inst/app/R/10_modeling.R` contains model generation and validation.
- `inst/app/R/20_results.R` contains result and download builders.
- `inst/app/R/30_plotting.R` contains path-diagram helpers.
- `inst/app/R/40_ui.R` and `50_server.R` contain the Shiny interface.

Do not commit `.Rproj.user`, `.Rhistory`, `.RData`, deployment account metadata, generated package archives, or private datasets.
