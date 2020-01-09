#' Ganaderia Sostenible Web App
#'
#' @return None
#' @examples
#' \dontrun{
#' runGanaderiaSostenible()
#' }
#' @export
runGanaderiaSostenible <- function() {
  appDir <- system.file("app", package = "GanaderiaSostenible")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `GanaderiaSostenible`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
