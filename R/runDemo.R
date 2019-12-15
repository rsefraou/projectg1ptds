#' @title Run demonstration of Reddit Analyzer App
#'
#' @describe runDemo opens the app
#' @export
runDemo <- function() {
  appDir <- system.file("shiny-examples", "RedditAnalyzer",  package = "projectg1ptds")
  if (appDir == "") {
    stop(
      "Could not find example directory. Try re-installing projectg1ptds",
      call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal")

}
