context("Compute")

test_that("Input municipio",{

  library(tidyverse)

  data_mun <- read_csv(system.file('app/data/MunicipiosColombia_geo.csv', package = "GanaderiaSostenible"),
                       locale = readr::locale(encoding = "UTF-8"))

  dta_mun <- paste0(toupper(tolower(data_mun$DEPARTAMEN)), ' - ',
                    toupper(tolower(data_mun$NOMBRE_ENT)))
  l_m <-  setNames(dta_mun, remove_accents(toupper(dta_mun)))

  lugar <- l_m["QUINDIO - MONTENEGRO"]
  # TODO remove old code and test with l_m
  lugar <- "Quindio - Montenegro"
  lugar <- strsplit(lugar, ' - ') %>% unlist()
  departamento <- lugar[1]
  municipio <- lugar[2]
  #region <- regiones_match(departamento = departamento, municipio = municipio)

  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  # Same input as montenegro script
  inputs <- list(
    cercas_vivas = data.frame(year = 2012, value = 2),
    bosque_primario = data.frame(year = 2012, value = 5),
    bosque_secundario = data.frame(year = 2012, value = 10),
    arboles_dispersos = data.frame(year = 2012, value = 0),
    silvopastoriles = data.frame(year = 2012, value = 5)
  )
  region <- regiones_match(departamento = departamento, municipio = municipio)

  old_app_results <- results_old(inputs, departamento, municipio)

  app_results <- app_results(inputs, departamento = departamento, municipio = municipio)

  # Preset 00
  d <- app_results$captura_general
  d_bar <- d %>% filter(Tiempo <= as.numeric(format(Sys.Date(), "%Y"))) %>%
    select(Suelo, carbono) %>%
    dplyr::group_by(Suelo) %>%
    dplyr::summarise(carbono = sum(carbono, na.rm = T))
  # 1 arboles_dispersos      0
  # 2 bosque_primario      879.
  # 3 bosque_secundario    391.
  # 4 cercas_vivas         203.
  # 5 silvopastoriles      155.
  expect_equal(d_bar$carbono, c(0, 878.7931, 390.7690, 202.6864, 155.4565))

  app_results$pajaros
  expect_equal(app_results$pajaros[[1]], biodiv_area2(15, region, "bosque_primario"))
  expect_equal(app_results$pajaros[[2]], biodiv_area2(7, region, "silvopastoriles"))
  expect_equal(app_results$pajaros_text[[1]], HTML("Bosques: 276 aves"))
  expect_equal(app_results$pajaros_text[[2]], HTML("Silvopastoriles: 13 aves"))

  expect_equal(names(old_app_results$captura_general), names(app_results$captura_general))
  expect_equal(names(old_app_results$captura_total), names(app_results$captura_total))

  expect_equal(round(app_results$carbono_capturado_presente), round(1627.705)) # tot_co2_presente + emision.evitada # test_montenegro

  # All NA inputs
  inputs <- list(
    bosque_primario = data.frame(year = NA, value = NA),
    bosque_secundario = data.frame(year = NA, value = NA),
    arboles_dispersos = data.frame(year = NA, value = NA),
    cercas_vivas = data.frame(year = NA, value = NA),
    silvopastoriles = data.frame(year = NA, value = NA)
  )
  old_app_results <- results_old(inputs, departamento, municipio)
  app_results <- app_results(inputs, departamento, municipio)
  expect_null(old_app_results)
  expect_null(app_results)

  # All NA numeric inputs
  inputs <- list(
    bosque_primario = data.frame(year = as.numeric(NA), value = as.numeric(NA)),
    bosque_secundario = data.frame(year = as.numeric(NA), value = as.numeric(NA)),
    arboles_dispersos = data.frame(year = as.numeric(NA), value = as.numeric(NA)),
    cercas_vivas = data.frame(year = as.numeric(NA), value = as.numeric(NA)),
    silvopastoriles = data.frame(year = as.numeric(NA), value = as.numeric(NA))
  )
  str(inputs)
  old_app_results <- results_old(inputs, departamento, municipio)
  app_results <- app_results(inputs, departamento, municipio)
  expect_null(old_app_results)
  expect_null(app_results)


  inputs <- list(
    bosque_primario = data.frame(year = 2010, value = 1),
    bosque_secundario = data.frame(year = NA, value = NA),
    arboles_dispersos = data.frame(year = NA, value = NA),
    cercas_vivas = data.frame(year = NA, value = NA),
    silvopastoriles = data.frame(year = NA, value = NA)
  )
  old_app_results <- results_old(inputs, departamento, municipio)
  app_results <- app_results(inputs, departamento, municipio)

  expect_equal(names(old_app_results$captura_general), names(app_results$captura_general))
  expect_equal(names(old_app_results$captura_total), names(app_results$captura_total))

  expect_equal(nrow(app_results$captura_general), nrow(old_app_results$captura_general))

  ## Tests aves en app_results

  ##

  inputs <- list(
    bosque_primario = data.frame(year = 2012, value = 5)
    )
  result <- app_results(inputs, departamento, municipio)


  ### App viz


  inputs <- list(
    cercas_vivas = data.frame(year = 2012, value = 2),
    bosque_primario = data.frame(year = 2012, value = 5),
    bosque_secundario = data.frame(year = 2012, value = 10),
    arboles_dispersos = data.frame(year = 2012, value = 0),
    silvopastoriles = data.frame(year = 2012, value = 5)
  )
  result <- app_results(inputs, departamento, municipio)

  ## Viz bar

  data <- result$captura_general
  data$carbono <- round(data$carbono, 2)
  data <- data %>%
    filter(Tiempo <= as.numeric(format(Sys.Date(), "%Y"))) %>%
    select(Suelo, carbono)

  viz_bar(data)

  ## Viz lines

  data <- result$captura_general
  type_p <- 'spline'

  min_year <- min(data$Tiempo)
  max_year <- min_year + 20
  data <- data %>%
    select(Ano = Tiempo, Suelo, carbono = Estimacion) %>%
    filter(Ano <= max_year)
  viz_lines(data, type_plot = type_p)

  data <- result$captura_total
  data <- data %>%
    select(Ano = Tiempo, Suelo, carbono = Estimacion) %>%
    filter(Ano <= max_year)
  type_p <- 'area'
  viz_lines(data, type_plot = type_p)


})


