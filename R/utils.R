#' Available regions
#'
#' @return None
#' @examples
#' availableRegiones()
#' @export
availableRegiones <- function() c("Eje Cafetero", "Piedemonte del Meta", "Valle del Rio Cesar", "Bajo Magdalena", "Boyac\u00e1 y Santander", "Otras \u00c1reas")

#' Available Tipo de Cobertura
#'
#' @return None
#' @examples
#' availableRegiones()
#' @export
availableTipoCobertura <- function() c( "bosque_primario",  "bosque_secundario", "arboles_dispersos", "cercas_vivas", "silvopastoriles")



utils::globalVariables(c("region_colombia", "tipo", "DEPARTAMEN", "NOMBRE_ENT", ".", "co2",
                         "Municipio", "Departamento", "carbono", "Suelo", "Ano", "total",
                         "year", "tipo_cobertura", "cambio", "cambio_acumulado", ".debug", ".preset",
                         "drop_na", "value", "Tiempo", "Estimacion", "MeanCO2e",
                         "Bajo_Magdalena_bosque_secundario",
                         "Bajo_Magdalena_silvopastoriles",
                         "Boyaca_y_Santander_bosque_secundario",
                         "Boyaca_y_Santander_silvopastoriles",
                         "Eje_Cafetero_bosque_secundario",
                         "Eje_Cafetero_silvopastoriles",
                         "Piedemonte_del_Meta_bosque_secundario",
                         "Piedemonte_del_Meta_silvopastoriles",
                         "Valle_del_Rio_Cesar_bosque_secundario",
                         "Valle_del_Rio_Cesar_silvopastoriles"
                         ))



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

  path <- system.file("helpers", "regiones.csv", package = "GanaderiaSostenible")
  info_regiones <- suppressMessages(readr::read_csv(path))

  stopifnot(all(unique(info_regiones$Region) %in% availableRegiones()))

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





remove_accents <- function(string){
  # accents <- "àèìòùÀÈÌÒÙáéíóúýÁÉÍÓÚÝäëïöüÄËÏÖÜâêîôûÂÊÎÔÛñÑç"
  accents <- "\u00e0\u00e8\u00ec\u00f2\u00f9\u00c0\u00c8\u00cc\u00d2\u00d9\u00e1\u00e9\u00ed\u00f3\u00fa\u00fd\u00c1\u00c9\u00cd\u00d3\u00da\u00dd\u00e4\u00eb\u00ef\u00f6\u00fc\u00c4\u00cb\u00cf\u00d6\u00dc\u00e2\u00ea\u00ee\u00f4\u00fb\u00c2\u00ca\u00ce\u00d4\u00db\u00f1\u00d1\u00e7"
  translation <- "aeiouAEIOUaeiouyAEIOUYaeiouAEIOUaeiouAEIOUnNc"
  chartr(accents, translation, string)
}


