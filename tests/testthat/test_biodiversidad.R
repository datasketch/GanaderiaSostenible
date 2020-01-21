context("Biodiversidad")

test_that("Biodiversidad works",{

  area <- 10
  region <- "Eje Cafetero"
  pop2 <- biodiv_area(10, "Eje Cafetero", "bosque_secundario")
  pop2
  pop2 <- biodiv_area(1000, region, "bosque_secundario")
  pop2


  area <- 10
  region <- "Bajo Magdalena"
  tipo_cobertura <- "bosque_secundario"
  pop <- biodiv_area(10, region, "bosque_secundario")
  pop2
  pop2 <- biodiv_area(1000, region, "bosque_secundario")
  pop2

  areas <- c(100)
  region <- "Eje Cafetero"
  total <- biodiv_area(areas, region, t = 10, tipo_cobertura = "arboles_dispersos")
  total

})

test_that("Biodiversidad aves 2", {




  region <- "Bajo Magdalena"
  tipo_cobertura <- "bosque_secundario"

  area <- 10
  region <- "Bajo Magdalena"
  tipo_cobertura <- "bosque_secundario"
  especies1 <- biodiv_area(area, region, tipo_cobertura)
  especies1
  especies2 <- biodiv_area2(area, region, tipo_cobertura)
  especies2

  biodiv_area2(0, region, tipo_cobertura)
  biodiv_area2(NA, region, tipo_cobertura)
  #biodiv_area2(NULL, region, tipo_cobertura)

  expect_equal(0,biodiv_area2(0, "Eje Cafetero", "bosque_primario"))
  expect_equal(as.numeric(NA), biodiv_area2(NA, region, tipo_cobertura))

  expect_equal(biodiv_area2(1, "Eje Cafetero", "bosque_secundario"),biodiv_area2(1, "Eje Cafetero", "bosque_primario"))
  expect_equal(biodiv_area2(1, "Eje Cafetero", "silvopastoriles"),biodiv_area2(1, "Eje Cafetero", "arboles_dispersos"))
  expect_equal(biodiv_area2(1, "Eje Cafetero", "cercas_vivas"),biodiv_area2(1, "Eje Cafetero", "arboles_dispersos"))

  expect_equal(0, biodiv_area2(area = 0, region = region, tipo_cobertura = 'silvopastoriles'))

  ##
  expect_equal(26,round(biodiv_area2(1, "Eje Cafetero", "bosque_primario")))
  expect_equal(4,round(biodiv_area2(1, "Eje Cafetero", "silvopastoriles")))
  expect_equal(691,round(biodiv_area2(301, "Eje Cafetero", "bosque_primario")))
  expect_equal(38,round(biodiv_area2(1001, "Eje Cafetero", "silvopastoriles")))


  # cargar paquete
  library (sars) # Species area relationship
  ### Ejemplo para predecir número de especies
  # # cargar las funciones Area - especies
  load(system.file("biodiversity/Funciones_area_especies.RData", package = "GanaderiaSostenible"))
  # Predecir numero de especies proporcionando el area para cada region.
  # Note que el area esta en metros (1ha=10.000 m2)
  # sar_pred(Bajo_Magdalena_bosque_secundario, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
  # sar_pred(Bajo_Magdalena_silvopastoriles, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
  # sar_pred(Boyaca_y_Santander_bosque_secundario, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
  # sar_pred(Boyaca_y_Santander_silvopastoriles, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
  # sar_pred(Eje_Cafetero_bosque_secundario, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
  # sar_pred(Eje_Cafetero_silvopastoriles, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
  # sar_pred(Piedemonte_del_Meta_bosque_secundario, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
  # sar_pred(Piedemonte_del_Meta_silvopastoriles, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
  # sar_pred(Valle_del_Rio_Cesar_bosque_secundario, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
  # sar_pred(Valle_del_Rio_Cesar_silvopastoriles, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha


  #sar_pred(Eje_Cafetero_bosque_secundario, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha

  x <- sars::sar_pred(Eje_Cafetero_bosque_secundario, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
  expect_equal(x$Prediction[1], biodiv_area2(1, "Eje Cafetero", "bosque_secundario"))





})
