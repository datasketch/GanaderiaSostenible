context("Compute")

test_that("Input municipio",{

  lugar <- "Quindio - Montenegro"
  lugar <- strsplit(lugar, ' - ') %>% unlist()
  departamento <- lugar[1]
  municipio <- lugar[2]
  region <- regiones_match(departamento = departamento, municipio = municipio)
  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  inputs <- list(
    bosque_primario = list(year = current_year - 8, value = 5),
    bosque_secundario = list(year = current_year - 8, value = 10),
    arboles_dispersos = list(year = current_year - 8, value = 2),
    cercas_vivas = list(year = current_year - 8, value = 2),
    silvopastoriles = list(year = current_year - 8, value = 5)
  )





  inputs <- list(
    bosque_primario= list(
      list(year = 2010, value = 10),
      list(year = 2002, value = 2)
      ),
    bosque_secundario = list(year = current_year - 8, value = 10),
    potreros = list(year = current_year - 8, value = 2),
    cercas_vivas = list(year = current_year - 8, value = 2),
    silvopastoriles = list(year = current_year - 8, value = 5)
  )



})


