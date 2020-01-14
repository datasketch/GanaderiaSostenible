context("Compute")

test_that("Cambio carbono",{
  bosque_carbono <- cambio_carbono('Eje Cafetero', 'bosque_secundario', 20)
  path <- system.file("data_test/test_captura_eje_cafetero.csv", package = "GanaderiaSostenible")
  test_eje_cafetero <- readr::read_csv(path)
  bosque_carbono_eje <- test_eje_cafetero[["bosque_secundario"]]
  expect_equal(bosque_carbono, bosque_carbono_eje)
})

test_that("Factor de emisión", {
  cercas_area <- c(608.1, 1068.8, 0.0, 0.0, 442.9, 1042.9, 447.0 )
  cercas_carbono <- cambio_carbono('Piedemonte del Meta', 'arboles_dispersos', 20)
  cercas_factor <- factor_emision(cercas_carbono, 'Piedemonte del Meta')
  path <- system.file("data_test/test_factor_pm.csv", package = "GanaderiaSostenible")
  test_fac_pm <- readr::read_csv(path)
  cercas_fac <- test_fac_pm[["arboles_dispersos"]]
  expect_equal(cercas_factor, cercas_fac)
})

test_that("Captura carbono bosque primario works",{

  lugar <- c("QUINDIO", "MONTENEGRO")
  area_primario <- 1
  captura_primario <- captura_carbono_bosques(departamento = lugar[1], municipio = lugar[2], area_bosque = area_primario,
                                              anos = 2010, t_e = 10)
  # captura_primario
  # captura_primario$Suelo <- 'Bosque primario'
  # captura_primario$Estimacion  <- cumsum(captura_primario$co2)

})


test_that("Captura de carbono", {
  pastoriles_area <- c(10.6,	142.5, 0.0,	131.0, 170.0, 222.7, 95.4 )
  cb_capturado <- carbono_capturado_estimacion(pastoriles_area, anos = c(2013:2019), t_e = 7, region = 'Bajo Magdalena', tipo_cobertura = 'silvopastoriles')
  path <- system.file("data_test/test_total_mag.csv", package = "GanaderiaSostenible")
  test_tts_pm <- readr::read_csv(path)
  xx <- test_tts_pm$silvopastoriles
  expect_equal(round(cb_capturado$co2), c(54, 753, 320, 992, 1466, 2092, 1905))
})


test_that("Región a la cual pertenece un municipio", {
  municipio <- 'Montenegro'
  reg_m <- regiones_match(municipio = municipio)
  expect_equal(reg_m, 'Eje Cafetero')

  departamento <- 'Nariño'
  municipio <- 'Potosí'
  reg_p <- regiones_match(departamento, municipio)
  expect_equal(reg_p, 'Otras \\u00c1reas')

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





test_that("Biodiversidad works",{

  area <- 10
  region <- "Eje Cafetero"
  pop2 <- biodiv_area(10, "Eje Cafetero", "bosque_secundario")
  pop2
  pop2 <- biodiv_area(1000, region, "bosque_secundario")
  pop2


  areas <- c(100)
  region <- "Eje Cafetero"
  total <- biodiv_area(areas, region, t = 10, tipo_cobertura = "arboles_dispersos")
  total

})

test_that("Viz",{

  areas <- c(100, 30, 23, 459)
  region <- "Eje Cafetero"
  total <- carbono_capturado_estimacion(area = areas, region = region, t_e = 4, anos = 2001:2004, tipo_cobertura = 'bosque_secundario')
  total$Suelo <- 'Bosque primario'
  total <- total %>% select(Suelo, carbono = co2)
  viz_bar(total)


  # need to calculate proyeccion
  d <- carbono_capturado_estimacion(area = areas, region = region, t_e = 20, anos = 2001:2004, tipo_cobertura = 'bosque_secundario')
  d$Suelo <- 'Bosque primario'
  d <- d %>% select(Ano = Tiempo, Suelo, carbono = co2)
  d$carbono <- cumsum(d$carbono)
  viz_lines(d)

})




test_that("Match_municipalities",{

 regiones_match('atlantico', 'Juan de Acosta')
 regiones_match(departamento = NULL, municipio = 'usiacuri')

})



test_that("Carbono_bosque_primario",{
  captura_carbono_bosques(municipio = 'Córdoba', area_bosque = NULL, t_e = 2)
  cb <- captura_carbono_bosques(departamento = 'Nariño', municipio = 'Córdoba', area_bosque = 500, t_e = 1, anos = 2001)
  expect_equal(round(cb$co2), 124618)
})
