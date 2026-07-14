#' Run the Interactive Mediation Analysis App
#'
#' Launches the Shiny application bundled with the package.
#'
#' @param launch.browser Whether to open the application in a web browser.
#'   Defaults to [interactive()].
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return The value returned by [shiny::runApp()]. This function normally
#'   blocks until the application is stopped.
#' @export
run_app <- function(launch.browser = interactive(), ...) {
  app_dir <- system.file("app", package = "lavaanMediation")
  if (identical(app_dir, "")) {
    stop(
      "The bundled application could not be found. Reinstall lavaanMediation and try again.",
      call. = FALSE
    )
  }

  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    display.mode = "normal",
    ...
  )
}
