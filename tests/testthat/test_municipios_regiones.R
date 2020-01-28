context("Captura computations work for all regions")

test_that("Captura Eje Cafetero",{

  region <- "Eje Cafetero"
  regiones <- readr::read_csv(system.file("helpers", "regiones.csv", package = "GanaderiaSostenible"))

  lugar <- regiones %>% filter(Region == region) %>% sample_n(1) %>% select(-1) %>% as.list()
  municipio <- lugar[1]
  departamento <- lugar[2]

  ## 01 HERRAMIENTA PARA LA ESTIMACIÓN DE CAPTURA DE CO2 EJE CERCAS VIVAS UN AÑO
  tot_c02 <- 260561
  res_cambio_acumulado <- c(57624, 86615, 115606, 144597, 173588, 202579, 231570, 260561)


  inputs <- list(
    cercas_vivas = list(year = 2013, value = 10/3.5) ## OJO NO HAY FACTOR DE 3.5
  )
  captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  captura_df


  inputs <- list(
    cercas_vivas = list(year = 2013, value = 10000 / 3.5) ## OJO NO HAY FACTOR DE 3.5
  )
  captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  captura_df

  expect_equal(cumsum(captura_df$cambio), res_cambio_acumulado)
  expect_equal(sum(captura_df$cambio), tot_c02)
  expect_equal(sum(captura_df$cambio), tot_c02)

  est_co2 <- estimacion_co2(inputs, departamento = departamento, municipio = municipio)
  expect_equal(round(est_co2$carbono_capturado_presente), round(tot_c02))


  ## 02 HERRAMIENTA PARA LA ESTIMACIÓN DE CAPTURA DE CO2 EJE ÁRBOLES DISPERSOS UN AÑO
  tot_c02 <- 299927
  res_cambio_acumulado <- c(69382, 102317, 135252, 168187, 201122, 234057, 266992, 299927)
  inputs <- list(
    arboles_dispersos = list(year = 2013, value = 10000)
  )
  captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  captura_df
  expect_equal(cumsum(captura_df$cambio), res_cambio_acumulado)
  expect_equal(sum(captura_df$cambio), tot_c02)

  est_co2 <- estimacion_co2(inputs, departamento = departamento, municipio = municipio)
  expect_equal(round(est_co2$carbono_capturado_presente), round(tot_c02))

  ## 03 HERRAMIENTA PARA LA ESTIMACIÓN DE CAPTURA DE CO2 EJE SILVOPASTORILES UN AÑO
  tot_c02 <- 283476
  res_cambio_acumulado <- c(91417, 118854, 146291, 173728, 201165, 228602, 256039, 283476)
  inputs <- list(
    silvopastoriles = list(year = 2013, value = 10000)
  )
  captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  captura_df
  expect_equal(cumsum(captura_df$cambio), res_cambio_acumulado)
  expect_equal(sum(captura_df$cambio), tot_c02)

  est_co2 <- estimacion_co2(inputs, departamento = departamento, municipio = municipio)
  expect_equal(round(est_co2$carbono_capturado_presente), round(tot_c02))

  ## 04 HERRAMIENTA PARA LA ESTIMACIÓN DE CAPTURA DE CO2 EJE CERCAS VIVAS DOS AÑOS
  tot_c02 <- 361850.5
  res_cambio_acumulado <- c(57624, 86615, 144418, 187904.5, 231391, 274877.5, 318364, 361850.5)

  inputs <- list(
    cercas_vivas =
      tribble(
        ~year, ~value,
        2013,   10000 / 3.5,
        2015,   5000 / 3.5
      )
  )

  captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  est_co2_tot <- estimacion_co2(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  captura_df_tot <- est_co2_tot$carbono_capturado_cumsum
  expect_equal(cumsum(captura_df_tot$cambio), res_cambio_acumulado) ## OJO FACTOR DE 3.5
  expect_equal(sum(captura_df_tot$cambio), tot_c02)

  est_co2 <- estimacion_co2(inputs, departamento = departamento, municipio = municipio)
  expect_equal(round(est_co2$carbono_capturado_presente), round(tot_c02))

  ## 05 HERRAMIENTA PARA LA ESTIMACIÓN DE CAPTURA DE CO2 EJE ÁRBOLES DISPERSOS DOS AÑOS
  tot_c02 <- 416955.5
  res_cambio_acumulado <- c(69382, 102317, 169943, 219345.5, 268748, 318150.5, 367553, 416955.5)

  inputs <- list(
    arboles_dispersos =
      tribble(
        ~year, ~value,
        2013,   10000,
        2015,   5000
      )
  )

  captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  est_co2_tot <- estimacion_co2(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  captura_df_tot <- est_co2_tot$carbono_capturado_cumsum
  expect_equal(cumsum(captura_df_tot$cambio), res_cambio_acumulado)
  expect_equal(est_co2_tot$carbono_capturado_total, tot_c02)

  est_co2 <- estimacion_co2(inputs, departamento = departamento, municipio = municipio)
  expect_equal(round(est_co2$carbono_capturado_presente), round(tot_c02))

  ## 06 HERRAMIENTA PARA LA ESTIMACIÓN DE CAPTURA DE CO2 EJE SILVOPASTORILES DOS AÑOS
  tot_c02 <-  397777.0
  res_cambio_acumulado <- c(91417, 118854, 191999.5, 233155, 274310.5, 315466, 356621.5, 397777)

  inputs <- list(
    silvopastoriles =
      tribble(
        ~year, ~value,
        2013,   10000,
        2015,   5000
      )
  )

  captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  est_co2_tot <- estimacion_co2(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  captura_df_tot <- est_co2_tot$carbono_capturado_cumsum
  expect_equal(cumsum(captura_df_tot$cambio), res_cambio_acumulado)
  expect_equal(est_co2_tot$carbono_capturado_total, tot_c02)

  est_co2 <- estimacion_co2(inputs, departamento = departamento, municipio = municipio)
  expect_equal(round(est_co2$carbono_capturado_presente), round(tot_c02))

  ## 07 HERRAMIENTA PARA LA ESTIMACIÓN DE CAPTURA DE CO2 EJE TODOS UN AÑO

  tot_c02 <- 1170199.6
  res_cambio_acumulado <- c(214107.5, 326636, 450582.2, 583536.2, 723507.4, 868833.1, 1018119.7, 1170199.6)

  inputs <- list(
    bosque_secundario = list(year = 2013, value = 10000),
    cercas_vivas = list(year = 2013, value = 10000 / 3.5),
    arboles_dispersos = list(year = 2013, value = 10000),
    silvopastoriles = list(year = 2013, value = 10000)
  )
  captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  est_co2_tot <- estimacion_co2(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  captura_df_tot <- est_co2_tot$carbono_capturado_cumsum
  #expect_equal(captura_df_tot$cambio_acumulado, res_cambio_acumulado)
  expect_true( sum( (captura_df_tot$cambio_acumulado - res_cambio_acumulado)^2 ) < 0.01) # Differencia <1%
  expect_equal(est_co2_tot$carbono_capturado_total, tot_c02)

  est_co2 <- estimacion_co2(inputs, departamento = departamento, municipio = municipio)
  expect_equal(round(est_co2$carbono_capturado_presente), round(tot_c02))

  ## 08 HERRAMIENTA PARA LA ESTIMACIÓN DE CAPTURA DE CO2 EJE TODOS DOS AÑOS.xlsx

  tot_c02 <- 1604616.1
  res_cambio_acumulado <- c(214107.5, 326636, 557635.9, 746854.2, 948798.5, 1160601.2, 1379873.5, 1604616.1)

  inputs <- list(
    bosque_secundario = list(year = 2013, value = 10000),
    cercas_vivas = list(year = 2013, value = 10000 / 3.5),
    arboles_dispersos = list(year = 2013, value = 10000),
    silvopastoriles = list(year = 2013, value = 10000),
    bosque_secundario = list(year = 2015, value = 5000),
    cercas_vivas = list(year = 2015, value = 5000 / 3.5),
    arboles_dispersos = list(year = 2015, value = 5000),
    silvopastoriles = list(year = 2015, value = 5000)
  )
  captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  est_co2_tot <- estimacion_co2(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  captura_df_tot <- est_co2_tot$carbono_capturado_cumsum
  #expect_equal(captura_df_tot$cambio_acumulado, res_cambio_acumulado)
  expect_true( sum( (captura_df_tot$cambio_acumulado - res_cambio_acumulado)^2 ) < 0.01) # Differencia <1%
  expect_equal(round(est_co2_tot$carbono_capturado_total), round(tot_c02))

  est_co2 <- estimacion_co2(inputs, departamento = departamento, municipio = municipio)
  expect_equal(round(est_co2$carbono_capturado_presente), round(tot_c02))

})

test_that("Captura Bajo Magdalena",{

  ## 10 HERRAMIENTA PARA LA ESTIMACIÓN DE CAPTURA DE CO2 MAG TODOS DOS AÑOS

  region <- "Bajo Magdalena"
  regiones <- readr::read_csv(system.file("helpers" , "regiones.csv", package = "GanaderiaSostenible"))

  lugar <- regiones %>% filter(Region == region) %>% sample_n(1) %>% select(-1) %>% as.list()
  municipio <- lugar[1]
  departamento <- lugar[2]


  tot_c02 <- 2893687.6
  res_cambio_acumulado <- c(271878.5, 510977, 897432.4, 1276505.7, 1668305, 2069962.7, 2479090, 2893687.6)

  inputs <- list(
    bosque_secundario = list(year = 2013, value = 10000),
    cercas_vivas = list(year = 2013, value = 10000 / 3.5),
    arboles_dispersos = list(year = 2013, value = 10000),
    silvopastoriles = list(year = 2013, value = 10000),
    bosque_secundario = list(year = 2015, value = 5000),
    cercas_vivas = list(year = 2015, value = 5000 / 3.5),
    arboles_dispersos = list(year = 2015, value = 5000),
    silvopastoriles = list(year = 2015, value = 5000)
  )
  captura_df <- estimacion_co2_tidy(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  est_co2_tot <- estimacion_co2(inputs, departamento = departamento, municipio = municipio, t_max = 8)
  captura_df_tot <- est_co2_tot$carbono_capturado_cumsum
  #expect_equal(captura_df_tot$cambio_acumulado, res_cambio_acumulado)
  expect_true( sum( (captura_df_tot$cambio_acumulado - res_cambio_acumulado)^2 ) < 0.01) # Differencia <1%
  expect_equal(round(est_co2_tot$carbono_capturado_total), round(tot_c02))

  est_co2 <- estimacion_co2(inputs, departamento = departamento, municipio = municipio)
  expect_equal(round(est_co2$carbono_capturado_presente), round(tot_c02))

})

test_that("Municipios específicos",{

  municipio <- "Pasto"
  departamento <- "Nariño"
  expect_null(biodiv_area2(1, "Otras \u00c1reas", "bosque_primario"))
  inputs <- list(
    bosque_primario = list(year = 2000, value = 10)
  )
  res <- app_results(inputs, departamento, municipio)
  expect_null(res$pajaros[[1]])
  expect_null(res$pajaros[[2]])

})




