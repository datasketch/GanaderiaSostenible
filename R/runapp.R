#' Ganaderia Sostenible Web App
#'
#' @return None
#' @examples
#' \dontrun{
#' runGanaderiaSostenible()
#' }
#' @export
runGanaderiaSostenible <- function(debug = FALSE, preset = NULL) {
  appDir <- system.file("app", package = "GanaderiaSostenible")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `GanaderiaSostenible`.", call. = FALSE)
  }
  .GlobalEnv$.debug <- debug
  .GlobalEnv$.preset <- preset
  on.exit(rm(list=c(.debug, .preset), envir=.GlobalEnv))
  shiny::runApp(appDir, display.mode = "normal")
}


#' @export
app_results <- function(inputs, departamento, municipio){

  # CO2 CAPTURE
  ## Results from functions
  region <- regiones_match(departamento = departamento, municipio = municipio)
  captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio)
  if(nrow(captura_df) == 0) return()
  est_co2 <- estimacion_co2(inputs, departamento = departamento, municipio = municipio)
  captura_df_tot <- est_co2$carbono_capturado_cumsum

  ## Adjust results to app
  captura_general <- captura_df %>%
    select(Tiempo = year, Suelo = tipo_cobertura,
           carbono = cambio, Estimacion = cambio_acumulado)
  captura_total <- captura_df_tot
  if(nrow(captura_total) > 0){
    captura_total$Suelo <- "Todas las coberturas"
  }
  captura_total <- captura_total %>% select(Tiempo = year, Suelo, Estimacion = cambio_acumulado)
  res <- list(
    region = region,
    captura_general = captura_general,
    captura_total = captura_total,
    carbono_capturado_presente = est_co2$carbono_capturado_presente,
    carbono_capturado_futuro = est_co2$carbono_capturado_futuro,
    carbono_capturado_total = est_co2$carbono_capturado_presente + est_co2$carbono_capturado_futuro
  )

  # BIODIVERSIDAD
  l <- inputs
  bosque_primario <- l$bosque_primario %>% bind_rows() %>% drop_na() %>% filter(value > 0)
  if (region != 'Otras Áreas') {
    pajaros_bosque_primario <- biodiv_area2(area = sum(bosque_primario$value, na.rm = T), region = region, tipo_cobertura = 'bosque_secundario')
    if (pajaros_bosque_primario != 0) {
      pajaros_bosque_primario <- HTML(paste0('Bosque primario: ', round(pajaros_bosque_primario), ' aves'))
    } else {
      pajaros_bosque_primario <- NULL
    }
  } else {
    pajaros_bosque_primario <- NULL
  }
  bosque_secundario <- l$bosque_secundario %>% bind_rows() %>% drop_na()
  if (region != 'Otras Áreas') {
    pajaros_bosque_secundario <- biodiv_area2(area = sum(bosque_secundario$value, na.rm = T), region = region, tipo_cobertura = 'bosque_secundario')
    if (pajaros_bosque_secundario != 0) {
      pajaros_bosque_secundario <- HTML(paste0('Bosque secundario: ', round(pajaros_bosque_secundario), ' aves'))
    } else {
      pajaros_bosque_secundario <- NULL
    }
  } else {
    pajaros_bosque_secundario <- NULL
  }
  potreros <- l$arboles_dispersos %>% bind_rows() %>% drop_na()
  if (region != 'Otras Áreas') {
    pajaros_potreros <- biodiv_area2(area = sum(potreros$value, na.rm = T), region = region, tipo_cobertura = 'silvopastoriles')
    if (pajaros_potreros != 0) {
      pajaros_potreros <- HTML(paste0('Árboles dispersos en potreros: ', round(pajaros_potreros), ' aves'))
    } else {
      pajaros_potreros <- NULL
    }
  } else {
    pajaros_potreros <- NULL
  }
  cercas <- l$cercas_vivas %>% bind_rows() %>% drop_na()
  if (region != 'Otras Áreas') {
    pajaros_cercas <- biodiv_area2(area = sum(cercas$value, na.rm = T), region = region, tipo_cobertura = 'silvopastoriles')
    if (pajaros_cercas != 0) {
      pajaros_cercas <- HTML(paste0('Cercas vivas: ', round(pajaros_cercas), ' aves'))
    } else {
      pajaros_cercas <- NULL
    }
  } else {
    pajaros_cercas <- NULL
  }
  pastoriles <- l$silvopastoriles %>% bind_rows() %>% drop_na()
  if (region != 'Otras Áreas') {
    pajaros_pastoriles <- biodiv_area2(area = sum(pastoriles$value, na.rm = T), region = region, tipo_cobertura = 'silvopastoriles')
    if (pajaros_pastoriles != 0) {
      pajaros_pastoriles <- HTML(paste0('Sistemas silvopastoriles: ', round(pajaros_pastoriles), ' aves'))
    } else {
      pajaros_pastoriles <- NULL
    }
  } else {
    pajaros_pastoriles <- NULL
  }

  estimacion_pajaros <- list(pajaros_bosque_primario, pajaros_bosque_secundario, pajaros_potreros, pajaros_cercas, pajaros_pastoriles)
  res$pajaros <- estimacion_pajaros
  #res$pajaros <- NA

  res

}


