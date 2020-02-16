## code to prepare `DATASET` dataset goes here
#usethis::use_data("DATASET")


path <- system.file("helpers", "co2_municipios.csv", package = "GanaderiaSostenible")
data_mun <-  suppressMessages(readr::read_csv(path))

#stringi::stri_escape_unicode(c("hola é", "ñ as"))
library(tidyverse)

data_mun <- data_mun %>%
  mutate(NOMBRE_ENT = stringi::stri_escape_unicode(NOMBRE_ENT),
         DEPARTAMEN = stringi::stri_escape_unicode(DEPARTAMEN))
write_csv(data_mun, "inst/helpers/co2_municipios.csv")
#stringi::stri_escape_unicode(c("hola é", "ñ as"))

