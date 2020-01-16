context("Compute")

test_that("Captura carbono extreme input cases",{

  lugar <- "Quindio - Montenegro"
  lugar <- strsplit(lugar, ' - ') %>% unlist()
  departamento <- lugar[1]
  municipio <- lugar[2]

  # SOLO BOSQUE SECUNDARIO WITH INPUT
  inputs <- list(
    bosque_secundario = data.frame(year = 2010, value = 1)
  )
  region <- regiones_match(departamento = departamento, municipio = municipio)

  bs_captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio)
  bs_est_co2 <- estimacion_co2(inputs, departamento = departamento, municipio = municipio)

  # SOLO BOSQUE PRIMARIO WITH INPUT
  inputs <- list(
    bosque_primario = data.frame(year = 2010, value = 1)
  )
  region <- regiones_match(departamento = departamento, municipio = municipio)

  bp_captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio)
  bp_est_co2 <- estimacion_co2(inputs, departamento = departamento, municipio = municipio)

  expect_equal(bs_est_co2$carbono_capturado_cumsum$year, bp_est_co2$carbono_capturado_cumsum$year)

  # SOLO BOSQUE PRIMARION WITH OTHER NAs
  inputs <- list(
    bosque_primario = data.frame(year = 2010, value = 1),
    bosque_secundario = data.frame(year = NA, value = NA),
    arboles_dispersos = data.frame(year = NA, value = NA),
    cercas_vivas = data.frame(year = NA, value = NA),
    silvopastoriles = data.frame(year = NA, value = NA)
  )
  region <- regiones_match(departamento = departamento, municipio = municipio)

  captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio)
  bs2_est_co2 <- estimacion_co2(inputs, departamento = departamento, municipio = municipio)

  expect_equal(bs_est_co2$carbono_capturado_cumsum$year, bs2_est_co2$carbono_capturado_cumsum$year)

  # SOLO CERCAS VIVAS

  municipio <- "Piojó"
  departamento <- "Atlántico"
  inputs <- list(
    cercas_vivas = list(year = 2013, value = 10000 / 3.5)
    #bosque_secundario = list(year = 2013, value = 10000) ## OJO NO HAY FACTOR DE 3.5
  )
  regiones_match(departamento = departamento, municipio = municipio)

  captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  captura_df




})




test_that("Captura carbono",{

  bosque_carbono <- captura_carbono('Eje Cafetero', 'bosque_secundario', 0:20)
  path <- system.file("data_test/test_captura_eje_cafetero.csv", package = "GanaderiaSostenible")
  test_eje_cafetero <- readr::read_csv(path)
  bosque_carbono_eje <- test_eje_cafetero[["bosque_secundario"]]
  expect_equal(bosque_carbono, bosque_carbono_eje)
})



test_that("Captura carbono",{
  bosque_carbono <- captura_carbono('Eje Cafetero', 'bosque_secundario', 0:20)
  path <- system.file("data_test/test_captura_eje_cafetero.csv", package = "GanaderiaSostenible")
  test_eje_cafetero <- readr::read_csv(path)
  bosque_carbono_eje <- test_eje_cafetero[["bosque_secundario"]]
  expect_equal(bosque_carbono, bosque_carbono_eje)
})

test_that("Cambio carbono old", {
  cercas_area <- c(608.1, 1068.8, 0.0, 0.0, 442.9, 1042.9, 447.0 )
  cercas_carbono <- captura_carbono('Piedemonte del Meta', 'arboles_dispersos', 0:20)
  cercas_factor <- factor_emision(cercas_carbono, 'Piedemonte del Meta')
  path <- system.file("data_test/test_factor_pm.csv", package = "GanaderiaSostenible")
  test_fac_pm <- readr::read_csv(path)
  cercas_fac <- test_fac_pm[["arboles_dispersos"]]
  expect_equal(cercas_factor, cercas_fac)
})

test_that("Captura carbono bosque primario works",{

  lugar <- c("QUINDIO", "MONTENEGRO")
  area <- 1

  ## OLD CODE, REMOVE LATER
  captura_primario <- captura_carbono_bosque_primario(departamento = lugar[1], municipio = lugar[2],
                                              area = area, t = 0:1)
  path <- system.file("aux/co2_municipios.csv", package = "GanaderiaSostenible")
  co2_munis <-  suppressMessages(readr::read_csv(path))

  co2 <- co2_munis %>% filter(NOMBRE_ENT == "MONTENEGRO") %>% pull(MeanCO2e)
  expect_equal(c(co2, 0), captura_primario)

})


test_that("Captura de carbono", {
  # pastoriles_area <- c(10.6,	142.5, 0.0,	131.0, 170.0, 222.7, 95.4 )
  # cb_capturado <- carbono_capturado_estimacion(pastoriles_area, anos = c(2013:2019), t_e = 7, region = 'Bajo Magdalena', tipo_cobertura = 'silvopastoriles')
  # path <- system.file("data_test/test_total_mag.csv", package = "GanaderiaSostenible")
  # test_tts_pm <- readr::read_csv(path)
  # xx <- test_tts_pm$silvopastoriles
  # expect_equal(round(cb_capturado$co2), c(54, 753, 320, 992, 1466, 2092, 1905))
})


test_that("Carbono capturado equivalencia en número de carros", {
   num_x <- round(co2_carros(carbono_capturado = 5000))
   expect_equal(num_x, 2212)
})



