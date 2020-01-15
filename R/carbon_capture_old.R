

results_old <- function(inputs, departamento, municipio){
  l <- inputs

  lugar <- c(departamento, municipio)
  region <- regiones_match(departamento = departamento, municipio = municipio)
  fecha_hoy <- as.numeric(format(Sys.Date(), "%Y"))

  bosque_primario <- l$bosque_primario %>% bind_rows() %>% drop_na() %>% filter(value > 0)
  annio_0_pr <- bosque_primario$year[1]
  if (is.na(annio_0_pr)){annio_0_pr <- 0}
  captura_primario <- captura_carbono_bosques(departamento = lugar[1], municipio = lugar[2], area_bosque = bosque_primario$value,
                                              anos = bosque_primario$year, t_e = (fecha_hoy -  annio_0_pr) + 10)
  captura_primario$Suelo <- 'Bosque primario'
  # captura_primario$Estimacion  <- cumsum(captura_primario$co2) ## REMOVER CUMSUM
  captura_primario$Estimacion  <- captura_primario$co2
  #if (sum(captura_primario$co2) == 0)  return() ## REMOVER
  if (region != 'Otras Áreas') {
    pajaros_bosque_primario <- biodiv_area(area = sum(bosque_primario$value, na.rm = T), region = region, tipo_cobertura = 'bosque_secundario')
    if (pajaros_bosque_primario != 0) {
      pajaros_bosque_primario <- HTML(paste0('Bosque primario: ', round(pajaros_bosque_primario), ' aves'))
    } else {
      pajaros_bosque_primario <- NULL
    }
  } else {
    pajaros_bosque_primario <- NULL
  }

  bosque_secundario <- l$bosque_secundario %>% bind_rows() %>% drop_na()
  annio_0_s <- bosque_secundario$year[1]
  if (is.na(annio_0_s)) annio_0_s <- 0
  captura_secundario <- carbono_capturado_estimacion(area = bosque_secundario$value,anos = bosque_secundario$year, region = region, tipo_cobertura = 'bosque_secundario', t_e = (fecha_hoy -annio_0_s) + 10)
  captura_secundario$Suelo <- 'Bosque secundario'
  captura_secundario$Estimacion  <- cumsum(captura_secundario$co2)
  if (region != 'Otras Áreas') {
    pajaros_bosque_secundario <- biodiv_area(area = sum(bosque_secundario$value, na.rm = T), region = region, tipo_cobertura = 'bosque_secundario')
    if (pajaros_bosque_secundario != 0) {
      pajaros_bosque_secundario <- HTML(paste0('Bosque secundario: ', round(pajaros_bosque_secundario), ' aves'))
    } else {
      pajaros_bosque_secundario <- NULL
    }
  } else {
    pajaros_bosque_secundario <- NULL
  }

  potreros <- l$arboles_dispersos %>% bind_rows() %>% drop_na()
  annio_0_p <- potreros$year[1]
  if (is.na(annio_0_p)) annio_0_p <- 0
  captura_potreros<- carbono_capturado_estimacion(area = potreros$value, anos = potreros$year, region = region, tipo_cobertura = 'arboles_dispersos', t_e = (fecha_hoy - annio_0_p) + 10)
  captura_potreros$Suelo <- "Árboles dispersos"
  captura_potreros$Estimacion  <- cumsum(captura_potreros$co2)
  if (region != 'Otras Áreas') {
    pajaros_potreros <- biodiv_area(area = sum(potreros$value, na.rm = T), region = region, tipo_cobertura = 'silvopastoriles')
    if (pajaros_potreros != 0) {
      pajaros_potreros <- HTML(paste0('Árboles dispersos en potreros: ', round(pajaros_potreros), ' aves'))
    } else {
      pajaros_potreros <- NULL
    }
  } else {
    pajaros_potreros <- NULL
  }


  cercas <- l$cercas_vivas %>% bind_rows() %>% drop_na()
  annio_0_c <- cercas$year[1]
  if (is.na(annio_0_c)) annio_0_c <- 0
  captura_cercas <- carbono_capturado_estimacion(area = cercas$value, anos = cercas$year, region = region, tipo_cobertura = 'cercas_vivas', t_e = (fecha_hoy - annio_0_c) + 10)
  captura_cercas$Suelo <-  'Cercas vivas'
  captura_cercas$Estimacion  <- cumsum(captura_cercas$co2)
  if (region != 'Otras Áreas') {
    pajaros_cercas <- biodiv_area(area = sum(cercas$value, na.rm = T), region = region, tipo_cobertura = 'silvopastoriles')
    if (pajaros_cercas != 0) {
      pajaros_cercas <- HTML(paste0('Cercas vivas: ', round(pajaros_cercas), ' aves'))
    } else {
      pajaros_cercas <- NULL
    }
  } else {
    pajaros_cercas <- NULL
  }

  pastoriles <- l$silvopastoriles %>% bind_rows() %>% drop_na()
  annio_0_sv <- pastoriles$year[1]
  if (is.na(annio_0_sv)) annio_0_sv <- 0
  captura_pastoriles <- carbono_capturado_estimacion(area = pastoriles$value,anos = pastoriles$year, region = region, tipo_cobertura = 'silvopastoriles', (fecha_hoy - annio_0_sv) + 10)
  captura_pastoriles$Suelo <- 'Silvopastoriles'
  captura_pastoriles$Estimacion  <- cumsum(captura_pastoriles$co2)
  if (region != 'Otras Áreas') {
    pajaros_pastoriles <- biodiv_area(area = sum(pastoriles$value, na.rm = T), region = region, tipo_cobertura = 'silvopastoriles')
    if (pajaros_pastoriles != 0) {
      pajaros_pastoriles <- HTML(paste0('Sistemas silvopastoriles: ', round(pajaros_pastoriles), ' aves'))
    } else {
      pajaros_pastoriles <- NULL
    }
  } else {
    pajaros_pastoriles <- NULL
  }

  captura_general <- bind_rows(captura_primario, captura_secundario, captura_potreros, captura_cercas, captura_pastoriles)
  captura_general <- captura_general %>% select(Tiempo, Suelo, carbono = co2, Estimacion) %>% filter(carbono != 0)
  captura_total <- captura_general %>% group_by(Tiempo) %>% summarise(Estimacion = sum(Estimacion, na.rm = T))
  if(nrow(captura_total) == 0) return()
  captura_total$Suelo <- 'Todas las coberturas'
  captura_total <- captura_total %>% select(Tiempo, Suelo, Estimacion)
  estimacion_pajaros <- list(pajaros_bosque_primario, pajaros_bosque_secundario, pajaros_potreros, pajaros_cercas, pajaros_pastoriles)
  result_old <- list("region" = region,"captura_general" = captura_general, "captura_total" = captura_total,  "pajaros" = estimacion_pajaros)
  result_old
}



cambio_carbono_old <- function(region, tipo_cobertura, t_f = 0) {
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
    path <- system.file("aux/captura_region_tipo.csv", package = "GanaderiaSostenible")
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

  captura_pastura <- captura_pasturas(region)

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

  cambioCarbono <- cambio_carbono_old(region = region, tipo_cobertura = tipo_cobertura, t_f = t_e)
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

  path <- system.file("aux/co2_municipios.csv", package = "GanaderiaSostenible")
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
