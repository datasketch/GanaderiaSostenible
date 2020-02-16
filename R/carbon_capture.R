


#' Carbon capture estimation according to land cover type
#'
#' @param region geographic region name
#' @param tipo_cobertura Coverage type for the land
#' @param t Time of intervention
#'
#' @return None
#'
#' @examples
#' captura_carbono('Eje Cafetero', 'bosque_secundario')
#' @export
captura_carbono <- function(region, tipo_cobertura, t = 0){
  if(!region %in% availableRegiones()){
    stop("Current region: ", region,". Regions must be one of: ", availableRegiones())
  }
  if(!tipo_cobertura %in% availableTipoCobertura()){
    stop("tipo_cobertura must be one of", availableTipoCobertura())
  }
  path <- system.file("helpers", "captura_region_tipo.csv", package = "GanaderiaSostenible")
  captura_region_tipo <- suppressMessages(readr::read_csv(path))
  if(tipo_cobertura == "bosque_secundario"){
    captura <- ((((1-exp((t*0.064)*(-1)))^1.964)*111.51)*0.5)*(44/12)
  } else{
    captura <- captura_region_tipo %>% filter(region_colombia == region, tipo == tipo_cobertura)
    captura <- captura$b + captura$m * t
  }
  captura
}



#' Carbon capture change
#'
#' @param captura Vector of carbon emissions changes for each land coverage type
#' @param region geographic region name
#' @param area area in hectares
#'
#' @return None
#'
#' @examples
#' cambio_carbono(c(1.3, 3, 4), 'Eje Cafetero')
#'
#' @export
cambio_carbono <- function(captura,  region, area = 1){
  if(is.na(region)){
    #message("here")
    #return(rep(0, length(captura)-1))
    return(captura)
  }
  captura_pastura <- captura_pasturas(region)
  captura[1]<- area * captura_pastura
  diff(captura)
}



#' Carbon capture estimation
#'
#' @param inputs  List with implementations by year and value of hectares
#' @param departamento  Departamento
#' @param municipio Municipio
#' @param t_max Maximum number of years for estimation
#' @return None
#' @examples
#' inputs <- list(
#'   bosque_primario = list(year = 2000, value = 5),
#'   bosque_secundario = list(year = 2000, value = 10),
#'   arboles_dispersos = list(year = 2000, value = 2),
#'   cercas_vivas = list(year = 2000, value = 2),
#'   silvopastoriles = list(year = 2000, value = 5)
#' )
#' estimacion_co2_tidy(inputs, departamento = "Quindio", municipio = "Montenegro")
#' @export
estimacion_co2_tidy <- function(inputs, departamento, municipio, t_max = 20) {

  region <- regiones_match(departamento = departamento, municipio = municipio)
  inputs_tipos_cobertura <- names(inputs)

  inputs <- bind_rows(inputs, .id = "tipo_cobertura") %>% purrr::transpose()

  captura_df <- purrr::map(inputs, function(input){
    if(input$tipo_cobertura == "bosque_primario"){
      captura <- captura_carbono_bosque_primario(departamento = departamento,
                                                 municipio = municipio, area = input$value, t = 0:(t_max-0))
      cambio <- cambio_carbono(captura, NA)
      #cambio <- cambio_carbono(captura, NA)
    }else{
      if(input$tipo_cobertura == "cercas_vivas"){
        input$value <- input$value * 3.5
      }
      captura <- captura_carbono(region, input$tipo_cobertura, t = 0:(t_max + 1)) * input$value
      cambio <- cambio_carbono(captura, region, area = input$value)
    }
    tibble(tipo_cobertura = input$tipo_cobertura, captura = captura[1:t_max], cambio = cambio[1:t_max], cambio_acumulado = cumsum(cambio[1:t_max]),
           year = seq_along(captura) + input$year - 1)
  }) %>% dplyr::bind_rows()
  captura_df <- captura_df %>% tidyr::drop_na()
}


#' Carbon capture estimation in tidy format
#'
#' @param inputs  List with implementations by year and value of hectares
#' @param departamento  Departamento
#' @param municipio Municipio
#' @param this_year Current year, defaults to current year
#' @param t_max Maximum number of years for estimation
#' @return None
#' @examples
#' inputs <- list(
#'   bosque_primario = list(year = 2000, value = 5),
#'   bosque_secundario = list(year = 2000, value = 10),
#'   arboles_dispersos = list(year = 2000, value = 2),
#'   cercas_vivas = list(year = 2000, value = 2),
#'   silvopastoriles = list(year = 2000, value = 5)
#' )
#' estimacion_co2(inputs, departamento = "Quindio", municipio = "Montenegro")
#' @export
estimacion_co2 <- function(inputs, departamento, municipio, this_year = NULL, t_max = 20) {
  if(is.null(this_year))
    this_year <- as.numeric(format(Sys.Date(), "%Y"))
  captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio, t_max = t_max)

  if(nrow(captura_df) == 0) return()

  min_year <- min(captura_df$year)
  captura_df <- captura_df %>% filter(year <= (min_year + t_max - 1))

  captura_cumsum <- captura_df %>% group_by(year) %>% summarise(cambio = sum(cambio)) %>%
    mutate(cambio_acumulado = cumsum(cambio))

  list(
    carbono_capturado_total = captura_df %>% pull(cambio) %>% sum(),
    carbono_capturado_presente = captura_df %>% filter(year <= this_year) %>% pull(cambio) %>% sum(),
    carbono_capturado_futuro = captura_df %>% filter(year > this_year) %>% pull(cambio) %>% sum(),
    carbono_capturado_cumsum = captura_cumsum
  )
}

#' Captured carbon for forests
#'
#' @param departamento  Department
#' @param municipio Municipality
#' @param area  Hectares of forest
#' @param t Time
#' @return None
#' @examples
#' captura_carbono_bosque_primario(departamento = 'Caldas', municipio = 'Manizales',
#'   area = 500)
#' @export
captura_carbono_bosque_primario <- function(departamento, municipio, area = 1, t = NULL){
  path <- system.file("helpers", "co2_municipios.csv", package = "GanaderiaSostenible")
  data_mun <-  suppressMessages(readr::read_csv(path))

  mun_data <- unique(data_mun$NOMBRE_ENT)
  # data_mun$NOMBRE_ENT <- iconv(tolower(data_mun$NOMBRE_ENT), to="ASCII//TRANSLIT")
  municipio <- iconv(tolower(municipio), to="ASCII//TRANSLIT")

  if (!is.null(departamento)) {
    data_mun$DEPARTAMEN <- iconv(tolower(data_mun$DEPARTAMEN), to="ASCII//TRANSLIT")
    departamento <- iconv(tolower(departamento), to="ASCII//TRANSLIT")
    data_mun <- data_mun %>% dplyr::filter(DEPARTAMEN %in% departamento)
  }

  if(!municipio %in% unique(data_mun$NOMBRE_ENT)){
    #return(0)
    stop(municipio, " municipio must be one of: ", mun_data)
  }

  data_mun <- data_mun %>% dplyr::filter(NOMBRE_ENT %in% municipio) %>% pull(MeanCO2e)
  captura <- data_mun * area
  if(!is.null(t)) captura <- c(captura, rep(0, length(t)-1))
  captura
}

#' Captured carbon for pastures
#'
#' @param region  Region
#' @return None
#' @examples
#' captura_pasturas(region = 'Eje Cafetero')
#' @export
captura_pasturas <- function(region){
  path <- system.file("helpers","captura_pasturas.csv", package = "GanaderiaSostenible")
  captura_region_pasturas <- suppressMessages(readr::read_csv(path))
  captura_region_pasturas %>% dplyr::filter(region_colombia == region) %>% .$captura
}
