#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
} else {
  normalizePath("start_app.R", mustWork = TRUE)
}
project_dir <- dirname(script_path)
app_dir <- file.path(project_dir, "inst", "app")

required_packages <- c("shiny", "lavaan", "semPlot", "htmltools")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required packages: ", paste(missing_packages, collapse = ", "),
      ". Install them with install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))."
    ),
    call. = FALSE
  )
}

message("Starting lavaanMediation from ", app_dir)
runtime_args <- commandArgs(trailingOnly = TRUE)
launch_browser <- !"--no-browser" %in% runtime_args
port_arg <- grep("^--port=", runtime_args, value = TRUE)
port <- if (length(port_arg) > 0) {
  as.integer(sub("^--port=", "", port_arg[[1]]))
} else {
  NULL
}

if (length(port_arg) > 0 && (is.na(port) || port < 1 || port > 65535)) {
  stop("--port must be an integer between 1 and 65535.", call. = FALSE)
}

shiny::runApp(
  app_dir,
  launch.browser = launch_browser,
  port = port
)
