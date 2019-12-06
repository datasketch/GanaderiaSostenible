

library(tidyverse)


availableRegiones <- function() c("Eje Cafetero", "Piedemonte del Meta", "Valle del Rio Cesar", "Otras Áreas")
availableTipoCobertura <- function() c("arboles_dispersos", "cercas_vivas", "silvopastoril", "bosque_secundario")

#' @export
captura_carbono <- function(region, tipo_cobertura, t = 0){
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
  tipo_cobertura <- names(areas)
  if(!all(tipo_cobertura %in% availableTipoCobertura())){
    stop("tipo_cobertura (names of areas) must be one of", availableTipoCobertura())
  }
  captura <- unlist(lapply(tipo_cobertura, function(tipo_cobertura){
    captura_carbono(region, tipo_cobertura, t = t)
  }))
  capturas_default <- tibble(tipo_cobertura = availableTipoCobertura(), captura = 0)
  #capturas <- tibble(tipo_cobertura = "cercas_vivas", captura = 10)
  capturas <- tibble(tipo_cobertura = tipo_cobertura, captura = captura)
  capturas %>% full_join(capturas_default) %>% distinct(tipo_cobertura, .keep_all = TRUE)
}



