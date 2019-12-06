context("Compute")

test_that("Captura works",{

  availableRegiones() # [1] "Eje Cafetero"        "Piedemonte del Meta" "Valle del Rio Cesar" "Otras Áreas"
  availableTipoCobertura() # [1] "arboles_dispersos" "cercas_vivas"      "silvopastoriles"     "bosque_secundario"

  region <- "Eje Cafetero"
  tipo_cobertura <- "arboles_dispersos"
  t <- 0

  captura_carbono(region, tipo_cobertura, t = 3)
  captura_carbono(region, "cercas_vivas", t = 3)

  l <- captura_carbono(region, tipo_cobertura, t = 0:20)

  path <- system.file("dataR/test_captura_eje_cafetero.csv", package = "GanaderiaSostenible")
  test_eje_cafetero <- read_csv(path)
  arboles_dispersos_eje_cafetero <- test_eje_cafetero[["arboles_dispersos"]]
  expect_equal(l, arboles_dispersos_eje_cafetero)

  eje_3_test <- test_eje_cafetero[4,] %>% select(one_of(availableTipoCobertura()))

  coberturas <- c("arboles_dispersos" = 1, "cercas_vivas" = 1,
                  "silvopastoriles" = 1, "bosque_secundario" = 1)
  eje_3 <- captura_areas(coberturas, "Eje Cafetero", t = 3) %>% select(one_of(availableTipoCobertura()))
  expect_equal(eje_3 %>% as_vector(), eje_3_test %>% as_vector())

  coberturas <- c("arboles_dispersos" = 100, "cercas_vivas" = 100,
                  "silvopastoriles" = 100, "bosque_secundario" = 100)
  captura_areas(coberturas, "Eje Cafetero", t = 3)
  coberturas <- c("arboles_dispersos" = 1000, "cercas_vivas" = 100,
                  "silvopastoriles" = 100, "bosque_secundario" = 100)
  captura_areas(coberturas, "Eje Cafetero", t = 3)

  #




})


test_that("Biodiversidad works",{

  area <- 10
  region <- "Eje Cafetero"
  pop2 <- biodiv_area(10, "Eje Cafetero", "bosque_secundario")
  pop2
  pop2 <- biodiv_area(1000, region, "bosque_secundario")
  pop2


  areas <- c("arboles_dispersos" = 100, "cercas_vivas" = 100,
             "silvopastoriles" = 100, "bosque_secundario" = 100)
  region <- "Eje Cafetero"
  total <- biodiv_areas(areas, region, t = 0)
  total

})

test_that("Viz",{

  areas <- c("arboles_dispersos" = 100, "cercas_vivas" = 100,
                  "silvopastoriles" = 100, "bosque_secundario" = 100)
  region <- "Eje Cafetero"
  total <- captura_areas(areas, region, t = 0)

  viz_bar(total)


  # need to calculate proyeccion
  path <- system.file("dataR/test_captura_eje_cafetero.csv", package = "GanaderiaSostenible")
  d  <- read_csv(path)
  d <- captura_areas(areas, region, t = 0:20)
  viz_lines(d)

})




test_that("Match_municipalities",{

 regiones_match('atlantico', 'Juan de Acosta')
 regiones_match(departamento = NULL, municipio = 'usiacuri')

})
