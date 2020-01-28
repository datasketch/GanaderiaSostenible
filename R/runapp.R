#' Ganaderia Sostenible Web App
#'
#' @param debug show debug mode
#' @param preset Load input presets
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

#' Ganaderia Sostenible Web App
#'
#' @param inputs List of inputs: a list with year and value for each tipo_cobertura
#' @param departamento Department
#' @param municipio Municipality
#' @return None
#' @examples
#' \dontrun{
#' runGanaderiaSostenible()
#' }
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

  total_areas_bosques <- sum(inputs$bosque_primario$value, na.rm = TRUE) + sum(inputs$bosque_secundario$value, na.rm = TRUE)
  total_areas_silvopastoriles <- sum(inputs$arboles_dispersos$value, na.rm = TRUE) +
    sum(inputs$cercas_vivas$value, na.rm = TRUE) + sum(inputs$silvopastoriles$value, na.rm = TRUE)

  #estimacion_pajaros <- list(pajaros_bosque_primario, pajaros_bosque_secundario, pajaros_potreros, pajaros_cercas, pajaros_pastoriles)
  estimacion_pajaros <- list()
  estimacion_pajaros$bosque <- biodiv_area2(area = total_areas_bosques, region = region, tipo_cobertura = 'bosque_secundario')
  if(!is.null(estimacion_pajaros$bosque)){
    estimacion_pajaros$bosque_text <- HTML(paste0('Bosques: ',
                                                round(estimacion_pajaros$bosque %||% 0), ' aves'))
  }
  estimacion_pajaros$silvopastoriles <- biodiv_area2(area = total_areas_silvopastoriles, region = region, tipo_cobertura = 'silvopastoriles')
  if(!is.null(estimacion_pajaros$silvopastoriles)){
    estimacion_pajaros$silvopastoriles_text <- HTML(paste0('Silvopastoriles: ',
                                                         round(estimacion_pajaros$silvopastoriles %||% 0), ' aves'))
  }
  res$pajaros <- list(estimacion_pajaros$bosque, estimacion_pajaros$silvopastoriles)
  res$pajaros_text <- list(estimacion_pajaros$bosque_text, estimacion_pajaros$silvopastoriles_text)
  res$inputs <- inputs
  #res$pajaros <- NA

  # bosque_primario <- l$bosque_primario %>% bind_rows() %>% drop_na() %>% filter(value > 0)
  # if (region != 'Otras Áreas') {
  #   pajaros_bosque_primario <- biodiv_area2(area = sum(bosque_primario$value, na.rm = TRUE),
  #                                           region = region, tipo_cobertura = 'bosque_secundario')
  #   if (pajaros_bosque_primario != 0) {
  #     pajaros_bosque_primario_text <- HTML(paste0('Bosque primario: ', round(pajaros_bosque_primario), ' aves'))
  #   } else {
  #     pajaros_bosque_primario <- NULL
  #   }
  # } else {
  #   pajaros_bosque_primario <- NULL
  # }
  # bosque_secundario <- l$bosque_secundario %>% bind_rows() %>% drop_na()
  # if (region != 'Otras Áreas') {
  #   pajaros_bosque_secundario <- biodiv_area2(area = sum(bosque_secundario$value, na.rm = TRUE),
  #                                             region = region, tipo_cobertura = 'bosque_secundario')
  #   if (pajaros_bosque_secundario != 0) {
  #     pajaros_bosque_secundario_text <- HTML(paste0('Bosque secundario: ', round(pajaros_bosque_secundario), ' aves'))
  #   } else {
  #     pajaros_bosque_secundario <- NULL
  #   }
  # } else {
  #   pajaros_bosque_secundario <- NULL
  # }
  # potreros <- l$arboles_dispersos %>% bind_rows() %>% drop_na()
  # if (region != 'Otras Áreas') {
  #   pajaros_potreros <- biodiv_area2(area = sum(potreros$value, na.rm = TRUE),
  #                                    region = region, tipo_cobertura = 'silvopastoriles')
  #   if (pajaros_potreros != 0) {
  #     pajaros_potreros_text <- HTML(paste0('Árboles dispersos en potreros: ', round(pajaros_potreros), ' aves'))
  #   } else {
  #     pajaros_potreros_text <- NULL
  #   }
  # } else {
  #   pajaros_potreros_text <- NULL
  # }
  # cercas <- l$cercas_vivas %>% bind_rows() %>% drop_na()
  # if (region != 'Otras Áreas') {
  #   pajaros_cercas <- biodiv_area2(area = sum(cercas$value, na.rm = TRUE),
  #                                  region = region, tipo_cobertura = 'silvopastoriles')
  #   if (pajaros_cercas != 0) {
  #     pajaros_cercas_text <- HTML(paste0('Cercas vivas: ', round(pajaros_cercas), ' aves'))
  #   } else {
  #     pajaros_cercas_text <- NULL
  #   }
  # } else {
  #   pajaros_cercas_text <- NULL
  # }
  # pastoriles <- l$silvopastoriles %>% bind_rows() %>% drop_na()
  # if (region != 'Otras Áreas') {
  #   pajaros_pastoriles <- biodiv_area2(area = sum(pastoriles$value, na.rm = TRUE),
  #                                      region = region, tipo_cobertura = 'silvopastoriles')
  #   if (pajaros_pastoriles != 0) {
  #     pajaros_pastoriles_text <- HTML(paste0('Sistemas silvopastoriles: ', round(pajaros_pastoriles), ' aves'))
  #   } else {
  #     pajaros_pastoriles_text <- NULL
  #   }
  # } else {
  #   pajaros_pastoriles_text <- NULL
  # }




  res

}


