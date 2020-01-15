context("Montenegro")

test_that("TNC Example",{
  #Parametros
  b1=0.064
  b2=1.964
  t=0:20
  pastos_eje<-1.3 #factor emisión pastos eje cafetero
  pastos_pdm<-2.6 #factor emisión pastos piedemonte meta
  pastos_vrc<-0.5 #factor emision pastos valle del rio cesar
  pastos_mag<-0.5 #factor emision pastos magdalena
  pastos_bys<-1.3 #factor emision pastos boyaca y santander

  #Ejemplo Manizales (Eje Cafetero) con implementaciones de bosque secundario 10 hectareas,
  #bosque primario 5 hectareas y 5 hectareas de silvopastoriles, implementadas en el 2013.

  area.bsec=10
  area.bprim=5
  area.silvo=5
  km.cerca=2
  area.cv=km.cerca*3.5 #factor de conversion km a hectareas

  captura.bsec<-111.51*((1-exp(-b1*t))^b2)*(44/12)*0.5*area.bsec #Esta es la ecuacion de bosques secundarios para todo el pais
  captura.bsec0 <- captura.bsec
  captura.bsec[1]<-area.bsec*pastos_eje #se considera el contenido de carbono de la pastura en t=0.
  cambio.bsec<-captura.bsec[2:length(t)]-captura.bsec[1:(length(t)-1)] #Este vector debe coincidir con lo reportado en la pestaña Cambio_C_BS_EJE columna AB
  captura.presente.bsec <- sum(cambio.bsec[1:8]) #Total captura de carbono en bosque secundario
  captura.proyectada.bsec <- sum(cambio.bsec)

  captura.silvo <- (7.698+2.7437*t)*area.silvo #esta es la ecuacion que corresponde a los silvopastoriles en eje cafetero
  captura.silvo0 <- captura.silvo
  captura.silvo[1]<-area.silvo*pastos_eje #se considera el contenido de carbono de la pastura en t=0.
  cambio.silvo<-captura.silvo[2:length(t)]-captura.silvo[1:(length(t)-1)] #Este vector debe coincidir con lo reportado en la pestaña Cambio_C_SSPi_EJE columna AB
  captura.presente.silvo <- sum(cambio.silvo[1:8]) #Total captura de carbono en silvopastoriles
  captura.proyectada.silvo <- sum(cambio.silvo)

  captura.cv <- (4.1633+2.8991*t)*area.cv #esta es la ecuacion que corresponde a las cercas vivas en eje cafetero
  captura.cv0 <- captura.cv
  captura.cv[1]<-area.cv*pastos_eje #se considera el contenido de carbono de la pastura en t=0.
  cambio.cv<-captura.cv[2:length(t)]-captura.cv[1:(length(t)-1)] #Este vector debe coincidir con lo reportado en la pestaña Cambio_C_CV_EJE columna AB
  captura.presente.cv <- sum(cambio.cv[1:8]) #Total captura de carbono en cercas vivas
  captura.proyectada.cv <- sum(cambio.cv)

  #Este valor debe coincidir con lo reportado por la calculadora en la pestaña "captura_co2"
  tot_co2_presente <- captura.presente.bsec+captura.presente.silvo+captura.presente.cv

  #calculos para bosque primario
  library(rgdal)
  co2byMuni<-readOGR(system.file("aux/carbono_updated/MunicipiosColombia_geo.shp", package = "GanaderiaSostenible"))

  emision.evitada <- co2byMuni@data$MeanCO2e[co2byMuni@data$NOMBRE_ENT=="MONTENEGRO"]*area.bprim

  #Este valor es lo que se deberia reportar como total en el panel de resultados
  tot_co2_presente2 <- captura.presente.bsec+captura.presente.silvo+emision.evitada
  #Este valor es lo que se deberia reportar como total en el panel de resultados avanzados
  tot_co2_proyeccion <- captura.proyectada.bsec+captura.proyectada.silvo+emision.evitada

  ## TEST FUNCTIONS WITH TNC EXAMPLES

  region <- "Eje Cafetero"
  tipo_cobertura <- "cercas_vivas"

  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  inputs <- list(
    cercas_vivas = list(year = 2012, value = 2),
    bosque_primario= list(year = 2012, value = 5),
    bosque_secundario = list(year = 2012, value = 10),
    silvopastoriles = list(year = 2012, value = 5)
  )

  # Bosque secundario
  area <- inputs$bosque_secundario$value
  captura_bosque_secundario <- captura_carbono(region, "bosque_secundario", t = 0:20) * area
  expect_equal(captura_bosque_secundario, captura.bsec0)
  cambio_bosque_secundario <- cambio_carbono(captura_bosque_secundario, region, area = area)
  expect_equal(cambio.bsec, cambio_bosque_secundario)

  # Cercas vivas
  area <- inputs$cercas_vivas$value * 3.5
  captura_cercas <- captura_carbono(region, "cercas_vivas", t = 0:20) * area
  expect_equal(captura_cercas, captura.cv0)
  cambio_cercas <- cambio_carbono(captura_cercas, region, area = area)
  expect_equal(cambio.cv, cambio_cercas)

  # Silvopastoriles
  area <- inputs$silvopastoriles$value
  captura_silvopastoriles <- captura_carbono(region, "silvopastoriles", t = 0:20) * area
  expect_equal(captura_silvopastoriles, captura.silvo0)
  cambio_silvopastoriles <- cambio_carbono(captura_silvopastoriles, region, area = area)
  expect_equal(cambio.silvo, cambio_silvopastoriles)

  # Bosque primario
  area <- inputs$bosque_primario$value
  captura_bosque_primario <- captura_carbono_bosque_primario(departamento = 'Quindío',
                                                             municipio = 'MONTENEGRO', area = area)
  expect_equal(emision.evitada, captura_bosque_primario)



  inputs <- list(
    cercas_vivas = list(year = 2012, value = 2),
    bosque_primario= list(year = 2012, value = 5),
    bosque_secundario = list(year = 2012, value = 10),
    silvopastoriles = list(year = 2012, value = 5)
  )

  captura_df <- estimacion_co2_tidy(inputs, departamento = "Quindio", municipio = "Montenegro")
  co2_est <- estimacion_co2(inputs, departamento = "Quindio", municipio = "Montenegro", this_year = 2020)


  expect_equal(co2_est$carbono_capturado_presente, tot_co2_presente)

})





