availableRegiones <- function() c("Eje Cafetero", "Piedemonte del Meta", "Valle del Rio Cesar", "Bajo Magdalena", "Boyacá y Santander", "Otras Áreas")
availableTipoCobertura <- function() c("arboles_dispersos", "cercas_vivas", "silvopastoriles", "bosque_secundario")


#'
#' Estimación del cambio en las reservas de carbono según categorías de uso del suelo
#'
#' @param region Nombre de la zona en la cual estan ubicados los terrenos.
#' @param tipo_cobertura Categoría de uso del suelo en la cuál tiene ganado.
#' @param t_f Tiempo del último año en el que aumento el número de hectareas del terreno
#'
#' @return None
#'
#' @examples
#' cambio_carbono('Eje Cafetero', 'bosque_secundario', 5)
#'
#'@export
cambio_carbono <- function(region, tipo_cobertura, t_f = 0) {
  if(!region %in% availableRegiones()){
    stop("regions must be one of: ", availableRegiones())
  }
  if(!tipo_cobertura %in% availableTipoCobertura()){
    stop("tipo_cobertura must be one of", availableTipoCobertura())
  }

  t <- 0:t_f
  if(tipo_cobertura == "bosque_secundario"){
    captura <- ((((1-exp((t*0.064)*(-1)))^1.964)*111.51)*0.5)*(44/12)
  }  else {
    path <- system.file("dataR/captura_region_tipo.csv", package = "GanaderiaSostenible")
    captura_region_tipo <- suppressMessages(read_csv(path))
    captura <- captura_region_tipo %>% filter(region_colombia == region, tipo == tipo_cobertura)
    captura <- captura$b + captura$m * t
  }
  captura
}

#'
#' Estimación de factores de emisión según categorías de uso del suelo
#'
#' @param cambio_carbono Valor o vector de valores del cambio de carbono según tipo de terreno
#' @param region Nombre de la zona en la cual estan ubicados los terrenos.
#'
#' @return None
#'
#' @examples
#' factor_emision(c(1.3, 3, 4), 'Eje Cafetero')
#'
#' @export
factor_emision <- function(cb_carbono,  region) {

  if(!region %in% availableRegiones()){
    stop("regions must be one of: ", availableRegiones())
  }


  if (is.null(cb_carbono)) {
    stop("You must type the carbono change")
  }

  path <- system.file("dataR/captura_pasturas.csv", package = "GanaderiaSostenible")
  captura_region_pasturas <- suppressMessages(read_csv(path))
  captura_pastura <- captura_region_pasturas %>% filter(region_colombia == region) %>% .$captura

  cb_carbono[1] <- captura_pastura
  if (length(cb_carbono) == 1) cb_carbono <- c(cb_carbono, 0)
  emision <- map(1:length(cb_carbono), function(i) {
    f_i <- c()
    f_i[i] <- cb_carbono[i + 1] - cb_carbono[i]
  }) %>% unlist()
  emision <- emision[!is.na(emision)]
  if (length(emision) == 1) emision <- c(emision, 0)
  emision
}


#'
#' Estimación de carbono capturado
#'
#' @param area  área (en hectáreas) implementada por año en cada una de las categorías de uso del suelo
#' @param t_e tiempo de estimación
#'
#' @return None
#'
#' @examples
#' carbono_capturado_estimacion(area = c(6418.8, 43, 0, 69.5), t_e = 20, region = 'Eje Cafetero', tipo_cobertura = 'cercas_vivas')
#'
#' @export
carbono_capturado_estimacion <- function(area, años, t_e, region, tipo_cobertura) {

  area <- c(123, 21, 3)
  años <- c(2000, 2007, 2013)

  if(!region %in% availableRegiones()){
    stop("regions must be one of: ", availableRegiones())
  }
  if(!tipo_cobertura %in% availableTipoCobertura()){
    stop("tipo_cobertura must be one of", availableTipoCobertura())
  }

  if (is.null(area)) return()
  if (length(area) == 0) area <- 0
  area <- data.frame(area, años)
  annios <- max(area$año) - min(area$año)
  todos_anios <- data.frame(años = min(area$año) + 0:annios)
  area <- left_join(todos_anios, area)

  area$area[is.na(area$area)] <- 0
  area_i <- area$area

  cambioCarbono <- cambio_carbono(region = region, tipo_cobertura = tipo_cobertura, t_f = t_e)
  factorEmision <- factor_emision(cambioCarbono, region = region)
 l <- map(1:length(area_i) , function(z) {
  area <- area[z]
  area <- c(area, rep(area, t_e-length(area)))
  captura  <- area * factorEmision
  captura <- captura[!is.na(captura)]
  captura
  })

 l_e <- map(seq_along(l), function(i) {
         l[[i]] <- c(rep(0, i-1), l[[i]])
         l[[i]] <- l[[i]][1:t_e]
         }) %>% bind_cols()
 dt_estimacion <- data.frame(Tiempo = 1:t_e, co2 = rowSums(l_e))
 dt_estimacion
}

#' @export
regiones_match <- function(departamento = NULL, municipio, ...) {

  if (is.null(municipio)) {
    stop("you must type at least one municipality")
  }

  path <- system.file("dataR/regiones.csv", package = "GanaderiaSostenible")
  info_regiones <- suppressMessages(read_csv(path))
  info_regiones$Municipio <- iconv(tolower(info_regiones$Municipio), to="ASCII//TRANSLIT")
  info_regiones$Departamento <- iconv(tolower(info_regiones$Departamento), to="ASCII//TRANSLIT")
  municipio <- iconv(tolower(municipio), to="ASCII//TRANSLIT")

  if (is.null(departamento)) {
    data_mun <- info_regiones %>% filter(Municipio %in% municipio)
  } else {
    departamento <- iconv(tolower(departamento), to="ASCII//TRANSLIT")
    data_depto <- info_regiones %>% filter(Departamento %in% departamento)
    data_mun <- data_depto %>% filter(Municipio %in% municipio)
  }

  if (nrow(data_mun) == 0) {
    region <- 'Otras Áreas'
  } else {
    region <- unique(data_mun$Región)
  }
  region
}

#' @export
co2_carros <- function(carbono_capturado) {

  if (is.null(carbono_capturado)) return()

  ce <- carbono_capturado / 2.26
  ce
}

#' @export
captura_carbono_bosques <- function(departamento = NULL, municipio, area_bosque = NULL, ...) {

  if (is.null(municipio)) {
    stop("you must type at least one municipality")
  }

  if (is.null(area_bosque)) area_bosque <- 0
  area_bosque[is.na(area_bosque)] <- 0

  path <- system.file("dataR/co2_municipios.csv", package = "GanaderiaSostenible")
  data_mun <-  suppressMessages(read_csv(path))
  mun_data <- unique(data_mun$NOMBRE_ENT)
  data_mun$NOMBRE_ENT <- iconv(tolower(data_mun$NOMBRE_ENT), to="ASCII//TRANSLIT")
  municipio <- iconv(tolower(municipio), to="ASCII//TRANSLIT")

  if (!is.null(departamento)) {
    data_mun$DEPARTAMEN <- iconv(tolower(data_mun$DEPARTAMEN), to="ASCII//TRANSLIT")
    departamento <- iconv(tolower(departamento), to="ASCII//TRANSLIT")
    data_mun <- data_mun %>% filter(DEPARTAMEN %in% departamento)
  }

  if(!municipio %in% unique(data_mun$NOMBRE_ENT)){
    stop("municipio must be one of: ", mun_data)
  }

  data_mun <- data_mun %>% filter(NOMBRE_ENT %in% municipio)
  co2 <- data_mun$MeanCO2e * area_bosque
  co2
}


#' @export
biodiv_area <- function(area, region, tipo_cobertura, t = 0) {
  if(!region %in% availableRegiones()){
    stop("regions must be one of: ", availableRegiones())
  }
  if(!tipo_cobertura %in% availableTipoCobertura()){
    stop("tipo_cobertura must be one of", availableTipoCobertura())
  }
  if(tipo_cobertura == "cercas_vivas") return(0)
  path <- system.file("dataR/biodiversidad_region_tipo.csv", package = "GanaderiaSostenible")
  biodiversidad_region_tipo <- suppressMessages(read_csv(path))
  especies <- biodiversidad_region_tipo %>% filter(region_colombia == region, tipo == tipo_cobertura)
  d <- especies$d
  c <- especies$c
  z <- especies$z
  A <- area * 10000
  f <- especies$formula
  especies <- eval(parse(text=f))
  especies
}
