
# cargar paquete
library (sars) # Species area relationship

# cargar las funciones Area - especies
load("inst/Funciones_area_especies.RData")


# Predecir numero de especies proporcionando el area para cada region.
# Note que el area esta en metros (1ha=10.000 m2)
sar_pred(Bajo_Magdalena_bosque_secundario, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
sar_pred(Bajo_Magdalena_silvopastoriles, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha

sar_pred(Boyaca_y_Santander_bosque_secundario, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
sar_pred(Boyaca_y_Santander_silvopastoriles, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha

sar_pred(Eje_Cafetero_bosque_secundario, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
sar_pred(Eje_Cafetero_silvopastoriles, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha

sar_pred(Piedemonte_del_Meta_bosque_secundario, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
sar_pred(Piedemonte_del_Meta_silvopastoriles, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha

sar_pred(Valle_del_Rio_Cesar_bosque_secundario, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha
sar_pred(Valle_del_Rio_Cesar_silvopastoriles, area = c(10000, 100000, 1000000, 10000000)) # predict 1ha, 10ha, 100ha, 1000ha




