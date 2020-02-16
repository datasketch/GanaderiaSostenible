## code to prepare `DATASET` dataset goes here
#usethis::use_data("DATASET")


# path <- system.file("helpers", "co2_municipios2.csv", package = "GanaderiaSostenible")
path <- "inst/helpers/co2_municipios2.csv"
data_mun <-  suppressMessages(readr::read_csv(path))

#stringi::stri_escape_unicode(c("hola é", "ñ as"))
library(tidyverse)

# data_mun <- data_mun %>%
#   mutate(NOMBRE_ENT = stringi::stri_escape_unicode(NOMBRE_ENT),
#          DEPARTAMEN = stringi::stri_escape_unicode(DEPARTAMEN))

data_mun <- data_mun %>%
  mutate(NOMBRE_ENT = mop::remove_accents(tolower(NOMBRE_ENT)),
         DEPARTAMEN = mop::remove_accents(tolower(DEPARTAMEN)))

write_csv(data_mun, "inst/helpers/co2_municipios.csv")
#stringi::stri_escape_unicode(c("hola é", "ñ as"))

