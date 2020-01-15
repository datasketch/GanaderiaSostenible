
#' Bird biodiversity for a given area
#'
#' @param area  Area in hectares
#' @param region Region
#' @param tipo_cobertura  Land coverage type
#' @param t time
#' @return None
#' @examples
#' biodiv_area(10, "Eje Cafetero", "bosque_secundario")
#' @import dplyr
#' @export
biodiv_area <- function(area, region, tipo_cobertura, t = 0) {
  if(!region %in% availableRegiones()){
    stop("regions must be one of: ", availableRegiones())
  }
  if(!tipo_cobertura %in% availableTipoCobertura()){
    stop("tipo_cobertura must be one of", availableTipoCobertura())
  }
  if(tipo_cobertura == "cercas_vivas") return(0)
  path <- system.file("aux/biodiversidad_region_tipo.csv", package = "GanaderiaSostenible")
  biodiversidad_region_tipo <- suppressMessages(readr::read_csv(path))
  especies <- biodiversidad_region_tipo %>% dplyr::filter(region_colombia == region, tipo == tipo_cobertura)
  d <- especies$d
  c <- especies$c
  z <- especies$z
  A <- area * 10000
  f <- especies$formula
  especies <- eval(parse(text=f))
  especies
}
