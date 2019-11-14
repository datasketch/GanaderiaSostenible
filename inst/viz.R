

viz_bar <- function(total){
  highchart() %>%
    hc_title(text = "Gráfico",
             style = list(fontSize = "15px")) %>%
    hc_chart(type = "treemap") %>%
    hc_xAxis(categories = total$tipo_cobertura) %>%
    hc_add_series(total$captura, name = "tipo_cobertura", showInLegend = FALSE)
}


viz_lines <- function(d){
  highchart() %>%
    hc_xAxis(categories = d$t) %>%
    hc_add_series(name = "Bosque Secundario", data = d$bosque_secundario) %>%
    hc_add_series(name = "Árboles Dispersos", data = d$arboles_dispersos) %>%
    hc_add_series(name = "Cercas Vivas", data = d$cercas_vivas) %>%
    hc_add_series(name = "Silvopastoriles", data = d$silvopastoriles)



}
