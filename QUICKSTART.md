# Quick Start

## 1. Install from GitHub

```r
install.packages("remotes")
remotes::install_github("hecongqing/lavaanMediation")
lavaanMediation::run_app()
```

## 2. Run from source

```bash
git clone https://github.com/hecongqing/lavaanMediation.git
cd lavaanMediation
Rscript start_app.R
```

For a headless session or a fixed port:

```bash
Rscript start_app.R --no-browser --port=3838
```

If dependencies are missing:

```r
install.packages(c("shiny", "lavaan", "semPlot", "htmltools"))
```

## 3. First analysis

1. Keep **Use built-in demo data** selected.
2. Choose **B1: X → M → Y**.
3. Select `X`, `M1`, and `Y`.
4. Choose the Bootstrap repetitions and optional seed.
5. Click **Run Analysis**.
6. Review Model Fit, Parameter Estimates, and Path Diagram.
7. Open Download to export the two CSV result files.

For a C2 model, choose at least one numeric Z covariate distinct from X, M, and Y.

See [USAGE.md](USAGE.md) for model and output details.
