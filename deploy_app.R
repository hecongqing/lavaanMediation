#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
account <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else "congqing"
app_name <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else "mediation-app"

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("deploy_app.R", mustWork = TRUE)
}
app_dir <- file.path(dirname(script_path), "inst", "app")

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop(
    "Package 'rsconnect' is required. Install it with install.packages('rsconnect').",
    call. = FALSE
  )
}

message("Deploying ", app_dir)
message("  account: ", account)
message("  app name: ", app_name)

rsconnect::deployApp(
  appDir = app_dir,
  appPrimaryDoc = "app.R",
  appName = app_name,
  account = account,
  server = "shinyapps.io",
  forceUpdate = TRUE
)
