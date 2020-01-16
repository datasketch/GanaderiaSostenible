hcoptslang <- getOption("highcharter.lang")
hcoptslang$contextButtonTitle <- 'Descargar Imagen'
hcoptslang$printChart <- "Imprimir Gr\\u00e1fico"
hcoptslang$downloadJPEG <- "Descarga en JPEG"
hcoptslang$downloadPNG <- "Descarga en PNG"
hcoptslang$downloadPDF <- "Descarga en PDF"
hcoptslang$downloadSVG <- "Descarga en SVG"
hcoptslang$thousandsSep <- " "
hcoptslang$decimalPoint <- "."


#' Bar visualization of carbon capture
#'
#' @param data Data frame with carbon capture for each land coverage type
#' @return None
#' @examples
#' \dontrun{
#' viz_bar()
#' }
#' @import highcharter
#' @export
viz_bar <- function(data) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  data <- data %>% dplyr::filter(carbono != 0)

  level_key <- c(cercas_vivas = "Cercas Vivas", bosque_primario = "Bosque Primario",
                 bosque_secundario = "Bosque Secundario", arboles_dispersos = "Árboles Dispersos",
                 silvopastoriles = "Silvopastoriles")
  data$Suelo <- recode(data$Suelo, !!!level_key)


  d <- data %>%
    dplyr::group_by(Suelo) %>%
    dplyr::summarise(carbono = sum(carbono, na.rm = T))


  d <- d %>% dplyr::ungroup() %>%
    dplyr::mutate(porcentaje = (carbono / sum(carbono, na.rm = TRUE)) * 100)

  colors <- c(`Bosque Secundario` = '#dbefa5', `Cercas Vivas` = '#ea640d',
              `Silvopastoriles` = '#fdbc00', `Bosque Primario` = '#3d894d',
              `Árboles Dispersos` = '#2e4856')

  series <- purrr::map(as.character(unique(d$Suelo)), function(i) {
    d0 <- d %>%
      dplyr::filter(Suelo %in% i)
    l0 <- list("name" = as.character(i),
               "color" = unname(colors[i]),
               "data" = list(d0$carbono)
    )
  })


  options(highcharter.lang = hcoptslang)

  highchart() %>%
    hc_chart(type =  "column",
             margin = c(0,0,0,0),
             padding = c(0,0,0,0)) %>%
    hc_xAxis(
      categories = list('temporal'),
      title = list(text = ' '),
      type= 'category',
      lineWidth= 0,
      labels = list(enabled = FALSE)
    ) %>%
    hc_yAxis(maxRange = 100,
             gridLineWidth = 0,
             tickLength = 0,
             lineWidth= 0,
             labels = list(enabled = T)) %>%
    hc_add_series_list(
      series
    ) %>%
    hc_plotOptions(column = list(stacking = "percentage",
                                 pointPadding = 0,
                                 groupPadding = 0,
                                 pointWidth = 370,
                                 dataLabels = list(
                                   enabled = T,
                                   inside = T,
                                   align = "left",
                                   format = " {point.percentage:.2f}% {series.name} Captura: {point.y} (tCO2e)",
                                   style = list(
                                     color = 'contrast',
                                     fontSize = '11px',
                                     textDecoration= 'none',
                                     textShadow = 'none',
                                     textOutline = 'none'
                                   )
                                 ))) %>%
    hc_tooltip(useHTML=TRUE,
               headerFormat = '<b>{series.name}</b><br/>',
               pointFormat = 'Captura de carbono: {point.y} (tCO2e)<br/> Porcentaje: {point.percentage:.2f}') %>%
    hc_legend(
      enabled = F
    )%>%
    hc_add_theme(thm)  %>%
    hc_exporting(enabled = TRUE, buttons= list(
      contextButton= list(
        symbol= 'url(https://cdn1.iconfinder.com/data/icons/feather-2/24/download-32.png)',
        height= 30,
        width= 33,
        symbolSize= 24,
        symbolX= 30,
        symbolY= 30,
        menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
      )
    ))

}

#' Lines visualization of carbon capture
#'
#' @param data Data frame with carbon capture for each land coverage type and year
#' @param type_plot Plot type
#' @return None
#' @examples
#' \dontrun{
#' viz_lines()
#' }
#' @export
viz_lines <- function(data, type_plot = "spline") {


  if (is.null(data)) {
    stop("Load an available dataset")
  }

  level_key <- c(cercas_vivas = "Cercas Vivas", bosque_primario = "Bosque Primario",
                 bosque_secundario = "Bosque Secundario", arboles_dispersos = "Árboles Dispersos",
                 silvopastoriles = "Silvopastoriles")
  data$Suelo <- recode(data$Suelo, !!!level_key)

  d <- data %>%
    dplyr::group_by(Ano, Suelo) %>%
    dplyr::summarise(total = sum(carbono, na.rm = T)) %>%
    tidyr::spread(Suelo, total) %>%
    tidyr::gather(Suelo, total, -Ano)

  colors <- c(`Bosque Secundario` = '#dbefa5', `Cercas Vivas` = '#ea640d',
              `Silvopastoriles` = '#fdbc00', `Bosque Primario` = '#3d894d',
              `Árboles Dispersos` = '#2e4856')

  series <- purrr::map(unique(d[[2]]), function(i) {
    d0 <- d %>%
      dplyr::filter(Suelo %in% i)
    l0 <- list("name" = as.character(i),
               "color" = unname(colors[i]),
               "data" = d0$total,
               "marker" = list(symbol = 'circle'))
  })


 # p_i <- grep(format(Sys.Date(), '%Y'), data$Ano) - 1
 # if (identical(p_i, integer(0))) p_i <- 2020

 p_i <- as.numeric(format(Sys.Date(), '%Y')) -  min(data$Ano)

 options(highcharter.lang = hcoptslang)

 highchart() %>%
    hc_chart(type =  type_plot
    ) %>%
    hc_xAxis(
      categories = purrr::map(as.character(unique(d$Ano)), function(z) z),
      type= 'category',
      plotLines = list(list(value = p_i, color = "black",
                            dashStyle = "shortdash", zIndex = 5, width = 2
      ))
    ) %>%
    hc_yAxis(title = list(text = 'Captura de carbono (tCO2e)')) %>%
    hc_add_series_list(series) %>%
    hc_tooltip(useHTML=TRUE,
               headerFormat = '<b>{series.name}</b><br/>',
               pointFormat = 'Captura de carbono: {point.y:.2f} (tCO2e)<br/>') %>%
    hc_legend( align = 'center',
               verticalAlign = 'top',
               backgroundColor = 'transparent',
               symbolWidth = 3) %>%
    hc_add_theme(thm)  %>%
   hc_exporting(enabled = TRUE, buttons= list(
     contextButton= list(
       symbol= 'url(https://cdn1.iconfinder.com/data/icons/feather-2/24/download-32.png)',
       height= 30,
       width= 33,
       symbolSize= 24,
       symbolX= 30,
       symbolY= 30,
       menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
     )
   ))


}



thm <- hc_theme(
  colors = c('#dbefa5', '#ea640d', '#fdbc00', '#3d894d', '#2e4856'),
  chart = list(
    backgroundColor = "#ffffff"
  ),
  title = list(
    style = list(
      color = '#333333',
      fontFamily = "Ubuntu"
    )
  ),
  subtitle = list(
    style = list(
      color = '#666666',
      fontFamily = "Ubuntu"
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

