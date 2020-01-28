
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
  path <- system.file("helpers", "biodiv_region_tipo.csv", package = "GanaderiaSostenible")
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

#' Bird biodiversity for a given area
#'
#' @param area  Area in hectares
#' @param region Region
#' @param tipo_cobertura  Land coverage type
#' @param t time
#' @return None
#' @examples
#' biodiv_area2(10, "Eje Cafetero", "bosque_secundario")
#' @import dplyr
#' @export
biodiv_area2 <- function(area, region, tipo_cobertura, t = 0){
  if(!region %in% availableRegiones()){
    stop("regions must be one of: ", availableRegiones())
  }
  if(!tipo_cobertura %in% availableTipoCobertura()){
    stop("tipo_cobertura must be one of", availableTipoCobertura())
  }
  # if(tipo_cobertura %in% c("cercas_vivas")) return(0)
  area <- area * 10000
  load(system.file("biodiversity", "funciones_area_especies.RData", package = "GanaderiaSostenible"))
  funs <- list(
     "Bajo Magdalena_bosque_secundario" = Bajo_Magdalena_bosque_secundario,
     "Bajo Magdalena_bosque_primario" = Bajo_Magdalena_bosque_secundario,
     "Bajo Magdalena_silvopastoriles" = Bajo_Magdalena_silvopastoriles,
     "Bajo Magdalena_arboles_dispersos" = Bajo_Magdalena_silvopastoriles,
     "Bajo Magdalena_cercas_vivas" = Bajo_Magdalena_silvopastoriles,
     "Boyac\u00e1 y Santander_bosque_secundario" = Boyaca_y_Santander_bosque_secundario,
     "Boyac\u00e1 y Santander_bosque_primario" = Boyaca_y_Santander_bosque_secundario,
     "Boyac\u00e1 y Santander_silvopastoriles" = Boyaca_y_Santander_silvopastoriles,
     "Boyac\u00e1 y Santander_arboles_dispersos" = Boyaca_y_Santander_silvopastoriles,
     "Boyac\u00e1 y Santander_cercas_vivas" = Boyaca_y_Santander_silvopastoriles,
     "Eje Cafetero_bosque_secundario" = Eje_Cafetero_bosque_secundario,
     "Eje Cafetero_bosque_primario" = Eje_Cafetero_bosque_secundario,
     "Eje Cafetero_silvopastoriles" = Eje_Cafetero_silvopastoriles,
     "Eje Cafetero_arboles_dispersos" = Eje_Cafetero_silvopastoriles,
     "Eje Cafetero_cercas_vivas" = Eje_Cafetero_silvopastoriles,
     "Piedemonte del Meta_bosque_secundario" = Piedemonte_del_Meta_bosque_secundario,
     "Piedemonte del Meta_bosque_primario" = Piedemonte_del_Meta_bosque_secundario,
     "Piedemonte del Meta_silvopastoriles" = Piedemonte_del_Meta_silvopastoriles,
     "Piedemonte del Meta_arboles_dispersos" = Piedemonte_del_Meta_silvopastoriles,
     "Piedemonte del Meta_cercas_vivas" = Piedemonte_del_Meta_silvopastoriles,
     "Valle del Rio Cesar_bosque_secundario" = Valle_del_Rio_Cesar_bosque_secundario,
     "Valle del Rio Cesar_bosque_primario" = Valle_del_Rio_Cesar_bosque_secundario,
     "Valle del Rio Cesar_silvopastoriles" = Valle_del_Rio_Cesar_silvopastoriles,
     "Valle del Rio Cesar_arboles_dispersos" = Valle_del_Rio_Cesar_silvopastoriles,
     "Valle del Rio Cesar_cercas_vivas" = Valle_del_Rio_Cesar_silvopastoriles
     )
  x <- sars::sar_pred(funs[[paste(region, tipo_cobertura, sep = "_")]], area)
  if(is.infinite(x$Prediction)){
    x$Prediction <- 0
  }
  x$Prediction
}


