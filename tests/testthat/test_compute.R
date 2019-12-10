context("Compute")

test_that("Cambio carbono",{
  bosque_carbono <- cambio_carbono('Eje Cafetero', 'bosque_secundario', 20)
  path <- system.file("data_test/test_captura_eje_cafetero.csv", package = "GanaderiaSostenible")
  test_eje_cafetero <- read_csv(path)
  bosque_carbono_eje <- test_eje_cafetero[["bosque_secundario"]]
  expect_equal(bosque_carbono, bosque_carbono_eje)
})

test_that("Factor de emisión", {
  cercas_area <- c(608.1, 1068.8, 0.0, 0.0, 442.9, 1042.9, 447.0 )
  cercas_carbono <- cambio_carbono('Piedemonte del Meta', 'cercas_vivas', 20)
  cercas_factor <- factor_emision(cercas_carbono, 'Piedemonte del Meta')
  path <- system.file("data_test/test_factor_pm.csv", package = "GanaderiaSostenible")
  test_fac_pm <- read_csv(path)
  cercas_fac <- test_fac_pm[["cercas_vivas"]]
  expect_equal(cercas_factor, cercas_fac)
})

test_that("Captura de carbono", {
  pastoriles_area <- c(10.6,	142.5, 0.0,	131.0, 170.0, 222.7, 95.4 )
  pastoriles_carbono <- round(cambio_carbono('Bajo Magdalena', 'silvopastoriles', 20), 1)
  pastoriles_factor <- factor_emision(pastoriles_carbono, 'Bajo Magdalena')
  cb_capturado <- carbono_capturado(pastoriles_area, pastoriles_factor)
  path <- system.file("data_test/test_total_mag.csv", package = "GanaderiaSostenible")
  test_tts_pm <- read_csv(path)
  xx <- test_tts_pm$silvopastoriles
  expect_equal(cb_capturado, c(187.62, 2223.00, 0, 1493.40, 1581.00, 1603.44, 486.54))
})


test_that("Región a la cual pertenece un municipio", {
  municipio <- 'Montenegro'
  reg_m <- regiones_match(municipio = municipio)
  expect_equal(reg_m, 'Eje Cafetero')

  departamento <- 'Nariño'
  municipio <- 'Potosí'
  reg_p <- regiones_match(departamento, municipio)
  expect_equal(reg_p, 'Otras Áreas')

  municipio <- 'Galapa'
  reg_g <- regiones_match(municipio = municipio)
  expect_equal(reg_g, 'Bajo Magdalena')

  municipio <- 'RIOHACHA'
  departamento <- 'LA GUAJIRA'
  reg_p <- regiones_match(departamento, municipio)
  expect_equal(reg_p, 'Valle del Rio Cesar')
})


test_that("Carbono capturado equivalencia en número de carros", {
   num_x <- round(co2_carros(carbono_capturado = 5000))
   expect_equal(num_x, 2212)
})



