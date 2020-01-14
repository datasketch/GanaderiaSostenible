availableRegiones <- function() c("Eje Cafetero", "Piedemonte del Meta", "Valle del Rio Cesar", "Bajo Magdalena", "Boyac\u00e1 y Santander", "Otras \u00c1reas")
availableTipoCobertura <- function() c("arboles_dispersos", "cercas_vivas", "silvopastoriles", "bosque_secundario")

utils::globalVariables(c("region_colombia", "tipo", "DEPARTAMEN", "NOMBRE_ENT", ".", "co2",
                         "Municipio", "Departamento", "carbono", "Suelo", "Ano", "total"))

#' Carbon capture estimation according to land cover type
#'
#' @param region geographic region name
#' @param tipo_cobertura Coverage type for the land
#' @param t_f Time of intervention
#'
#' @return None
#'
#' @examples
#' cambio_carbono('Eje Cafetero', 'bosque_secundario', 5)
#' @export
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
    captura_region_tipo <- suppressMessages(readr::read_csv(path))
    captura <- captura_region_tipo %>% dplyr::filter(region_colombia == region, tipo == tipo_cobertura)
    captura <- captura$b + captura$m * t
  }

  if (tipo_cobertura == 'cercas_vivas') {
    captura <- captura * 3.5
  } else {
    captura <- captura
  }

  captura
}

#' Carbon Emision Factor
#'
#' @param cb_carbono Vector of carbon emissions changes for each land coverage type
#' @param region geographic region name
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
  captura_region_pasturas <- suppressMessages(readr::read_csv(path))
  captura_pastura <- captura_region_pasturas %>% dplyr::filter(region_colombia == region) %>% .$captura

  cb_carbono[1] <- captura_pastura
  if (length(cb_carbono) == 1) cb_carbono <- c(cb_carbono, 0)
  emision <- purrr::map(1:length(cb_carbono), function(i) {
    f_i <- c()
    f_i[i] <- cb_carbono[i + 1] - cb_carbono[i]
  }) %>% unlist()
  emision <- emision[!is.na(emision)]
  if (length(emision) == 1) emision <- c(emision, 0)
  emision
}


#' Carbon capture estimation
#'
#' @param area  Area (hectares) of coverage implementation by year for each land coverage type
#' @param anos  Years
#' @param t_e time of estimation
#' @param region region
#' @param tipo_cobertura Land coverage type
#' @return None
#' @examples
#' pastoriles_area <- c(10.6,	142.5, 0.0,	131.0, 170.0, 222.7, 95.4 )
#' carbono_capturado_estimacion(pastoriles_area, anos = c(2013:2019), t_e = 7,
#'   region = 'Bajo Magdalena', tipo_cobertura = 'silvopastoriles')
#' @export
carbono_capturado_estimacion <- function(area, anos, t_e, region, tipo_cobertura) {


  if(!region %in% availableRegiones()){
    stop("regions must be one of: ", availableRegiones())
  }
  if(!tipo_cobertura %in% availableTipoCobertura()){
    stop("tipo_cobertura must be one of", availableTipoCobertura())
  }

  if (is.null(area)) return()
  if (length(area) == 0) area <- 0
  if (is.null(anos)) anos <- rep(0, length(area))
  if (length(anos) == 0) anos <- 0
  area <- data.frame(area, anos)
  annios <- max(area$anos) - min(area$anos)
  todos_anios <- data.frame(anos = min(area$anos) + 0:annios)
  area <- dplyr::left_join(todos_anios, area)

  area$area[is.na(area$area)] <- 0
  area_end <- area$area
  area_i <- area$area

  cambioCarbono <- cambio_carbono(region = region, tipo_cobertura = tipo_cobertura, t_f = t_e)
  factorEmision <- factor_emision(cambioCarbono, region = region)
  l <- purrr::map(1:length(area_i) , function(z) {
    area_end <- area_end[z]
    area_end <- c(area_end, rep(area_end, t_e-length(area_end)))
    captura  <- area_end * factorEmision
    captura <- captura[!is.na(captura)]
    captura
  })

  l_e <- purrr::map(seq_along(l), function(i) {
    l[[i]] <- c(rep(0, i-1), l[[i]])
    l[[i]] <- l[[i]][1:t_e]
  }) %>% dplyr::bind_cols()
  dt_estimacion <- data.frame(Tiempo = min(area$anos)+(0:(t_e - 1)), co2 = rowSums(l_e))
  dt_estimacion
}

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

  path <- system.file("dataR/regiones.csv", package = "GanaderiaSostenible")
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

#' Captured carbon for forests
#'
#' @param departamento  Department
#' @param municipio Municipality
#' @param area_bosque  Hectares of forest
#' @param anos  Years
#' @param t_e Estimated times
#' @return None
#' @examples
#' captura_carbono_bosques(departamento = 'Nariño', municipio = 'Córdoba',
#'   area_bosque = 500, t_e = 1, anos = 2001)
#' @export
captura_carbono_bosques <- function(departamento, municipio, area_bosque, anos, t_e) {

  if (is.null(municipio)) {
    stop("you must type at least one municipality")
  }

  if (is.null(area_bosque)) return()
  if (length(area_bosque) == 0) area_bosque <- 0
  if (is.null(anos)) anos <- rep(0, length(area_bosque))
  if (length(anos) == 0) anos <- 0


  area_bosque <- data.frame(area_bosque, anos)
  annios <- max(area_bosque$anos, na.rm = TRUE) - min(area_bosque$anos, na.rm = TRUE)
  annios_t <-  min(area_bosque$anos, na.rm = TRUE) + 0:annios

  if (is.null(t_e)) t_e <- as.numeric(format(Sys.Date(), '%Y')) - max(annios_t,  na.rm = TRUE)
  t_e <- t_e - length(annios_t)

  if (t_e > 0 ) {
    annios_t <- c( annios_t, max(annios_t, na.rm = TRUE) + 1:t_e)
  }

  todos_anios <- data.frame(anos = annios_t)
  area_bosque <- dplyr::left_join(todos_anios, area_bosque)
  area_bosque$area_bosque[is.na(area_bosque$area_bosque)] <- 0

  path <- system.file("dataR/co2_municipios.csv", package = "GanaderiaSostenible")
  data_mun <-  suppressMessages(readr::read_csv(path))
  mun_data <- unique(data_mun$NOMBRE_ENT)
  data_mun$NOMBRE_ENT <- iconv(tolower(data_mun$NOMBRE_ENT), to="ASCII//TRANSLIT")
  municipio <- iconv(tolower(municipio), to="ASCII//TRANSLIT")

  if (!is.null(departamento)) {
    data_mun$DEPARTAMEN <- iconv(tolower(data_mun$DEPARTAMEN), to="ASCII//TRANSLIT")
    departamento <- iconv(tolower(departamento), to="ASCII//TRANSLIT")
    data_mun <- data_mun %>% dplyr::filter(DEPARTAMEN %in% departamento)
  }

  if(!municipio %in% unique(data_mun$NOMBRE_ENT)){
    stop("municipio must be one of: ", mun_data)
  }

  data_mun <- data_mun %>% dplyr::filter(NOMBRE_ENT %in% municipio)
  area_bosque$co2 <- cumsum(data_mun$MeanCO2e * area_bosque$area_bosque)
  area_bosque <- area_bosque %>% dplyr::select(Tiempo = anos, co2)
  area_bosque
}

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
  path <- system.file("dataR/biodiversidad_region_tipo.csv", package = "GanaderiaSostenible")
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
