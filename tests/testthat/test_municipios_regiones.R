context("Compute")

test_that("Manizales",{

  lugar <- "Caldas - Manizales"
  lugar <- strsplit(lugar, ' - ') %>% unlist()
  region <- regiones_match(departamento = lugar[1], municipio = lugar[2])

  fecha_hoy <- 2020
  bosque_primario <- tribble(
    ~año, ~valor,
    2010,   1
  )

  annio_0_pr <- bosque_primario$año[1]
  captura_primario <- captura_carbono_bosques(departamento = lugar[1], municipio = lugar[2], area_bosque = bosque_primario$valor,
                                              anos = bosque_primario$año, t_e = (fecha_hoy -  annio_0_pr) + 10)
  captura_primario$Suelo <- 'Bosque primario'
  captura_primario$Estimacion  <- captura_primario$co2
  if (region != 'Otras Áreas') {
    pajaros_bosque_primario <- biodiv_area(area = sum(bosque_primario$valor, na.rm = T), region = region, tipo_cobertura = 'bosque_secundario')
    if (pajaros_bosque_primario != 0) {
      pajaros_bosque_primario <- HTML(paste0('Bosque primario: ', round(pajaros_bosque_primario), ' aves'))
    } else {
      pajaros_bosque_primario <- NULL
    }
  } else {
    pajaros_bosque_primario <- NULL
  }

  # bosque_carbono <- cambio_carbono('Eje Cafetero', 'bosque_secundario', 20)
  # path <- system.file("data_test/test_captura_eje_cafetero.csv", package = "GanaderiaSostenible")
  # test_eje_cafetero <- readr::read_csv(path)
  # bosque_carbono_eje <- test_eje_cafetero[["bosque_secundario"]]
  # expect_equal(bosque_carbono, bosque_carbono_eje)

})
