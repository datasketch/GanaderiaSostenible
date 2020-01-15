availableRegiones <- function() c("Eje Cafetero", "Piedemonte del Meta", "Valle del Rio Cesar", "Bajo Magdalena", "Boyac\u00e1 y Santander", "Otras \u00c1reas")
availableTipoCobertura <- function() c( "bosque_primario",  "bosque_secundario", "arboles_dispersos", "cercas_vivas", "silvopastoriles")

utils::globalVariables(c("region_colombia", "tipo", "DEPARTAMEN", "NOMBRE_ENT", ".", "co2",
                         "Municipio", "Departamento", "carbono", "Suelo", "Ano", "total"))



#' Match geographic regions to municipalities
#'
#' @param departamento  Department
#' @param municipio Municipality
#' @return None
#' @examples
#' regiones_match(departamento = NULL, municipio = 'usiacuri')
#' @export
regiones_match <- function(departamento = NULL, municipio) {

  if (is.null(municipio)) {
    stop("you must type at least one municipality")
  }

  path <- system.file("aux/regiones.csv", package = "GanaderiaSostenible")
  info_regiones <- suppressMessages(readr::read_csv(path))
  info_regiones$Municipio <- iconv(tolower(info_regiones$Municipio), to="ASCII//TRANSLIT")
  info_regiones$Departamento <- iconv(tolower(info_regiones$Departamento), to="ASCII//TRANSLIT")
  municipio <- iconv(tolower(municipio), to="ASCII//TRANSLIT")

  if (is.null(departamento)) {
    data_mun <- info_regiones %>% dplyr::filter(Municipio %in% municipio)
  } else {
    departamento <- iconv(tolower(departamento), to="ASCII//TRANSLIT")
    data_depto <- info_regiones %>% dplyr::filter(Departamento %in% departamento)
    data_mun <- data_depto %>% dplyr::filter(Municipio %in% municipio)
  }

  if (nrow(data_mun) == 0) {
    region <- 'Otras \u00c1reas'
  } else {
    region <- unique(data_mun$Region)
  }
  region
}

#' Captured carbon
#'
#' @param carbono_capturado  Captured carbon in CO2 tons
#' @return None
#' @examples
#' co2_carros(10000)
#' @export
co2_carros <- function(carbono_capturado) {

  if (is.null(carbono_capturado)) return()

  ce <- carbono_capturado / 2.26
  ce
}






