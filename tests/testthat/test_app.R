context("Compute")

test_that("Input municipio",{

  library(tidyverse)

  lugar <- "Quindio - Montenegro"
  lugar <- strsplit(lugar, ' - ') %>% unlist()
  departamento <- lugar[1]
  municipio <- lugar[2]
  #region <- regiones_match(departamento = departamento, municipio = municipio)

  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  inputs <- list(
    bosque_primario = data.frame(year = 2010, value = 5),
    bosque_secundario = data.frame(year = 2010, value = 10),
    arboles_dispersos = data.frame(year = 2010, value = 2),
    cercas_vivas = data.frame(year = 2010, value = 2),
    silvopastoriles = data.frame(year = 2010, value = 5)
  )

  old_app_results <- results_old(inputs, departamento, municipio)

  app_results <- app_results(inputs, departamento = departamento, municipio = municipio)

  expect_equal(app_results$pajaros, old_app_results$pajaros)

  expect_equal(names(old_app_results$captura_general), names(app_results$captura_general))
  expect_equal(names(old_app_results$captura_total), names(app_results$captura_total))

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

})


