#' Launch the Shiny mediation app
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "lavaanMediation")
  if (identical(app_dir, "")) stop("App directory not found. Please ensure the package is correctly installed.", call. = FALSE)
  shiny::runApp(app_dir, display.mode = "normal")
}

