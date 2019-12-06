

library(tidyverse)


availableRegiones <- function() c("Eje Cafetero", "Piedemonte del Meta", "Valle del Rio Cesar", "Otras Áreas")
availableTipoCobertura <- function() c("arboles_dispersos", "cercas_vivas", "silvopastoriles", "bosque_secundario")

#' @export
captura_carbono <- function(region, tipo_cobertura, t = 0) {
  if(!region %in% availableRegiones()){
    stop("regions must be one of: ", availableRegiones())
  }
  if(!tipo_cobertura %in% availableTipoCobertura()){
    stop("tipo_cobertura must be one of", availableTipoCobertura())
  }
  path <- system.file("captura_region_tipo.csv", package = "GanaderiaSostenible")
  captura_region_tipo <- suppressMessages(read_csv(path))
  if(tipo_cobertura == "bosque_secundario"){
    captura <- ((((1-exp((t*0.064)*(-1)))^1.964)*111.51)*0.5)*(44/12)
  } else{
    captura <- captura_region_tipo %>% filter(region_colombia == region, tipo == tipo_cobertura)
    captura <- captura$b + captura$m * t
  }
  captura
}


#' @export
captura_areas <- function(areas, region, t = 0){
  if(!region %in% availableRegiones()){
    stop("regions must be one of: ", availableRegiones())
  }
  tipos_coberturas <- names(areas)
  if(!all(tipos_coberturas %in% availableTipoCobertura())){
    stop("tipo_cobertura (names of areas) must be one of", availableTipoCobertura())
  }
  capturas <- lapply(tipos_coberturas, function(tipo_cob){
    captura_carbono(region, tipo_cob, t = t)
  })
  capturas <- setNames(capturas, tipos_coberturas) %>% as_tibble()
  capturas$t <- t
  capturas$bosque_secundario <- capturas$bosque_secundario *areas["bosque_secundario"]
  capturas$cercas_vivas <- capturas$cercas_vivas *areas["cercas_vivas"]
  capturas$silvopastoriles <- capturas$silvopastoriles *areas["silvopastoriles"]
  capturas$arboles_dispersos <- capturas$arboles_dispersos *areas["arboles_dispersos"]
  capturas
}




#' @export
biodiv_area <- function(area, region, tipo_cobertura, t = 0){
  if(!region %in% availableRegiones()){
    stop("regions must be one of: ", availableRegiones())
  }
  if(!tipo_cobertura %in% availableTipoCobertura()){
    stop("tipo_cobertura must be one of", availableTipoCobertura())
  }
  if(tipo_cobertura == "cercas_vivas") return(0)
  path <- system.file("biodiversidad_region_tipo.csv", package = "GanaderiaSostenible")
  biodiversidad_region_tipo <- suppressMessages(read_csv(path))
  especies <- biodiversidad_region_tipo %>% filter(region_colombia == region, tipo == tipo_cobertura)
  d <- especies$d
  c <- especies$c
  z <- especies$z
  A <- area
  f <- especies$formula
  especies <- eval(parse(text=f))
  especies
}

#' @export
biodiv_areas <- function(areas, region, t = 0){
  if(!region %in% availableRegiones()){
    stop("regions must be one of: ", availableRegiones())
  }
  tipos_coberturas <- names(areas)
  if(!all(tipos_coberturas %in% availableTipoCobertura())){
    stop("tipo_cobertura (names of areas) must be one of", availableTipoCobertura())
  }
  biodivs <- lapply(tipos_coberturas, function(tipo_cob){
    area <- areas[tipo_cob]
    biodiv_area(area, region, tipo_cob)
  })
  capturas <- setNames(biodivs, tipos_coberturas) %>% as_tibble()
  capturas$t <- t

}



