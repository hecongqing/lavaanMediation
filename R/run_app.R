#' 启动 Shiny 中介分析 App
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "lavaanMediation")
  if (app_dir == "") stop("App 目录未找到，请确认已正确安装该包。", call. = FALSE)
  shiny::runApp(app_dir, display.mode = "normal")
}
