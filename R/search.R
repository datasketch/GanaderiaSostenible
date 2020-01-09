#' searchInput
#' @importFrom shiny restoreInput addResourcePath tagList singleton
#' @importFrom htmltools tags HTML validateCssUnit
#' @param inputId inputId
#' @param data data to be searched
#' @param placeholder text placeholder for input
#'@export

searchInput <- function(inputId, data, placeholder) {

  addResourcePath(
    prefix = 'wwwAutoSuggest',
    directoryPath = system.file('www', package='GanaderiaSostenible')
  )


  l <- tagList(
    singleton(tags$head(
      tags$link(rel = 'stylesheet',
                type = 'text/css',
                href = 'wwwAutoSuggest/searchBinding.css'),
      tags$script(src = 'wwwAutoSuggest/search.js'),
      tags$script(src = 'wwwAutoSuggest/searchBinding.js')
    )),
    tags$div(
      id = inputId,
      class = 'input-autosuggest',
      "data-top"= jsonlite::toJSON(data),
      "data-placeholder" = placeholder
    )
  )

  shiny::div(
    `data-shiny-input-type` = "searchInput",
    l
  )


}
