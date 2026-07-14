# lavaanMediation 0.2.0

- Reorganized the Shiny application into focused model, result, plotting, asset, UI, and server files.
- Added CSV downloads for model-fit statistics and parameter estimates, including R² rows.
- Removed the internal R2 Debug tab and its server output.
- Added validation for numeric variables, duplicate role assignments, and C2 covariates.
- Made Bootstrap resampling resilient to individual failed samples and exposed successful-sample counts.
- Replaced placeholder package metadata with publishable authorship, licensing, URLs, and dependency declarations.
- Added unit tests, GitHub Actions checks, contribution guidance, and cleaned repository-only artifacts.
- Aligned the README and user guide with the currently supported B1 and C2 models.
- Made English the default README and added a complete Simplified Chinese version.
- Invalidated stale fitted results when analysis inputs change and bound every output to the fitted configuration.
- Reused a single main-effect fit across tables, downloads, and diagrams.
- Removed unused function arguments, duplicate example data, and unreachable PDF fallback assets.

# lavaanMediation 0.1.0

- Initial R package and Shiny application for mediation analysis.
