context("Compute")

test_that("Regions file is ok",{

  library(tidyverse)

  departamento <- "Quindío"
  municipio <- "Montenegro"
  region <- regiones_match(departamento = departamento, municipio = municipio)
  region
  # unique(info_regiones$Region)[! unique(info_regiones$Region) %in% availableRegiones()]
  # unique(info_regiones$Region)

})


test_that("Región a la cual pertenece un municipio", {
  municipio <- 'Montenegro'
  reg_m <- regiones_match(municipio = municipio)
  expect_equal(reg_m, 'Eje Cafetero')

  departamento <- 'Quindío'
  municipio <- 'Montenegro'
  regiones_match(departamento = departamento, municipio = municipio)

  departamento <- 'Nariño'
  municipio <- 'Potosí'
  reg_p <- regiones_match(departamento, municipio)
  expect_equal(reg_p, 'Otras \u00c1reas')

  municipio <- 'Galapa'
  reg_g <- regiones_match(municipio = municipio)
  expect_equal(reg_g, 'Bajo Magdalena')

  municipio <- 'Manizales'
  reg_g <- regiones_match(municipio = municipio)
  expect_equal(reg_g, 'Eje Cafetero')


  # municipio <- 'Tunja'
  # reg_g <- regiones_match(municipio = municipio)
  # expect_equal(reg_g, 'Boyacá y Santander')

  # municipio <- 'RIOHACHA'
  # departamento <- 'LA GUAJIRA'
  # reg_p <- regiones_match(departamento, municipio)
  # expect_equal(reg_p, 'Valle del Rio Cesar')
})


test_that("Match_municipalities",{

  regiones_match('atlantico', 'Juan de Acosta')
  regiones_match(departamento = NULL, municipio = 'usiacuri')

})



