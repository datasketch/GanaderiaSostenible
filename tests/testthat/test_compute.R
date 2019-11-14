context("Compute")

test_that("Compute works",{

  availableRegiones() # [1] "Eje Cafetero"        "Piedemonte del Meta" "Valle del Rio Cesar" "Otras Áreas"
  availableTipoCobertura() # [1] "arboles_dispersos" "cercas_vivas"      "silvopastoril"     "bosque_secundario"

  region <- "Eje Cafetero"
  tipo_cobertura <- "arboles_dispersos"
  t <- 0

  captura_carbono(region, tipo_cobertura, t = 3)
  captura_carbono(region, "cercas_vivas", t = 3)

  l <- lapply(0:20, function(t){
    captura_carbono(region, tipo_cobertura, t = t)
  })
  path <- system.file("test_captura_eje_cafetero.csv", package = "GanaderiaSostenible")
  test_eje_cafetero <- read_csv(path)
  arboles_dispersos_eje_cafetero <- test_eje_cafetero[["arboles_dispersos"]]
  expect_equal(unlist(l), arboles_dispersos_eje_cafetero)

  coberturas <- c("arboles_dispersos" = 100, "cercas_vivas" = 100,
                  "silvopastoril" = 100, "bosque_secundario" = 100)

  eje_3_test <- test_eje_cafetero[4,-1] %>% as_vector()
  eje_3 <- captura_areas(coberturas, "Eje Cafetero", t = 3)[[2]]
  expect_equal(sum(eje_3), sum(eje_3_test))


  #

  library(highcharter)

  source("inst/viz.R")
  coberturas <- c("arboles_dispersos" = 100, "cercas_vivas" = 100,
                  "silvopastoril" = 100, "bosque_secundario" = 100)
  region <- "Eje Cafetero"
  total <- captura_areas(coberturas, region, t = 0)

  viz_bar(total)

  # need to calculate proyeccion
  path <- system.file("test_captura_eje_cafetero.csv", package = "GanaderiaSostenible")
  coberturas_proyeccion  <- read_csv(path)

  viz_lines(coberturas_proyeccion)



})
