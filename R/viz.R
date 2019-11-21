

#' @export
viz_bar <- function(total){

  highchart() %>%
    hc_chart(type = "column",
             #spacing = c(0,0,0,0),
             margin = c(0,0,0,0)) %>%
    hc_title(floating = TRUE, y = -50) %>%
    hc_xAxis(enabled = FALSE, categories = "", gridLineWidth = 0, lineWidth = -1, offset = 0, labels = list(enabled = FALSE)) %>%
    hc_yAxis(enabled = FALSE, gridLineWidth = 0, lineWidth = -1,  offset = 0, labels = list(enabled = FALSE)) %>%
    #hc_series(list(name="A",data=c(1))) %>%
    hc_add_series(data = total$bosque_secundario[1], name ="Bosque Secundario", showInLegend = FALSE ,
                  dataLabels = list(align = "left", format = "{y}<br>{series.name}", style = list(textOutline = "0px contrast"))) %>%
    hc_add_series(data = total$arboles_dispersos[1], name ="Árboles Dispersos", showInLegend = FALSE ,
                  dataLabels = list(align = "left", format = "{y}<br>{series.name}", style = list(textOutline = "0px contrast"))) %>%
    hc_add_series(data = total$cercas_vivas[1], name ="Cercas Vivas", showInLegend = FALSE ,
                  dataLabels = list(align = "left", format = "{y}<br>{series.name}", style = list(textOutline = "0px contrast"))) %>%
    hc_add_series(data = total$silvopastoriles[1], name ="Silvopastoriles", showInLegend = FALSE ,
                  dataLabels = list(align = "left", format = "{y}<br>{series.name}", style = list(textOutline = "0px contrast"))) %>%
    hc_plotOptions(series = list(
      dataLabels = list(enabled = TRUE, inside = TRUE),
      pointWidth = 200,
      stacking = "normal",
      enableMouseTracking = TRUE)) %>%
    hc_add_theme(thm)

}


#' @export
viz_lines <- function(d){
  highchart() %>%
    hc_chart(type = "line") %>%
    hc_xAxis(categories = d$t) %>%
    hc_add_series(name = "Bosque Secundario", data = d$bosque_secundario) %>%
    hc_add_series(name = "Árboles Dispersos", data = d$arboles_dispersos) %>%
    hc_add_series(name = "Cercas Vivas", data = d$cercas_vivas) %>%
    hc_add_series(name = "Silvopastoriles", data = d$silvopastoriles) %>%
    hc_plotOptions(line = list(
      marker = list(enabled = FALSE))) %>%
    hc_add_theme(thm)
}

thm <- hc_theme(colors = c('#dbefa5','#b0d361', '#3d894d', '#2e4856'))

thm <- hc_theme(
  colors = c('#dbefa5','#b0d361', '#3d894d', '#2e4856'),
  chart = list(
    backgroundColor = "#ffffff"
  ),
  title = list(
    style = list(
      color = '#333333',
      fontFamily = "Erica One"
    )
  ),
  subtitle = list(
    style = list(
      color = '#666666',
      fontFamily = "Shadows Into Light"
    )
  ),
  legend = list(
    itemStyle = list(
      #fontFamily = 'Tangerine',
      color = 'black'
    ),
    itemHoverStyle = list(
      color = 'gray'
    )
  )
)

