#' @export
runApp <- function() {
  appDir <- system.file("app", package = "Ganaderia Sostenible")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
