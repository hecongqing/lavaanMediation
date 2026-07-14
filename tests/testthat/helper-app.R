suppressPackageStartupMessages({
  library(shiny)
  library(lavaan)
  library(htmltools)
})

app_test_env <- new.env(parent = globalenv())
app_test_dir <- system.file("app", package = "lavaanMediation")
app_r_files <- sort(list.files(file.path(app_test_dir, "R"), pattern = "[.]R$", full.names = TRUE))

for (app_r_file in app_r_files) {
  sys.source(app_r_file, envir = app_test_env)
}
