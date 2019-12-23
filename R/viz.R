global_options <- function(marksMil, marksDec){

  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- marksMil
  hcoptslang$decimalPoint <- marksDec
  options(highcharter.lang = hcoptslang)
}

#' @export
viz_bar <- function(data) {

  if (is.null(data)) {
    stop("Load an available dataset")
  }

  data <- data %>% filter(carbono != 0)

  d <- data %>%
    dplyr::group_by(Suelo) %>%
    dplyr::summarise(carbono = sum(carbono, na.rm = T))


  d <- d %>% ungroup() %>%
    dplyr::mutate(porcentaje = (carbono / sum(carbono, na.rm = TRUE)) * 100)



  series <- purrr::map(as.character(unique(d$Suelo)), function(i) {
    d0 <- d %>%
      dplyr::filter(Suelo %in% i)
    l0 <- list("name" = as.character(i),
               "data" = list(d0$carbono)
    )
  })


  global_options(' ', '.')

  highchart() %>%
    hc_chart(type =  "column",
             height = 550,
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
                                   format = " {point.percentage:.2f}%<br/>{series.name}<br/>Captura: {point.y} (tCO2e)",
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
    hc_add_theme(thm)

}


#' @export
viz_lines <- function(data, type_plot = "spline") {


  if (is.null(data)) {
    stop("Load an available dataset")
  }

  d <- data %>%
    dplyr::group_by(Año, Suelo) %>%
    dplyr::summarise(total = sum(carbono, na.rm = T)) %>%
    tidyr::spread(Suelo, total) %>%
    tidyr::gather(Suelo, total, -Año)



  series <- purrr::map(unique(d[[2]]), function(i) {
    d0 <- d %>%
      dplyr::filter(Suelo %in% i)
    l0 <- list("name" = as.character(i),
               "data" = d0$total,
               "marker" = list(symbol = 'circle'))
  })

  global_options(' ', '.')
 p_i <- grep( format(Sys.Date(), '%Y'), data$Año) - 1
 if (identical(p_i, integer(0))) p_i <- 2019

 highchart() %>%
    hc_chart(type =  type_plot
    ) %>%
    hc_xAxis(
      categories = purrr::map(as.character(unique(d$Año)), function(z) z),
      type= 'category',
      plotLines = list(list(value = p_i, color = "black",
                            dashStyle = "shortdash", zIndex = 5, width = 2
      ))
    ) %>%
    hc_yAxis(title = list(text = 'Captura de carbono (tCO2e)')) %>%
    hc_add_series_list(series) %>%
    hc_tooltip(useHTML=TRUE,
               headerFormat = '<b>{series.name}</b><br/>',
               pointFormat = 'Captura de carbono: {point.y} (tCO2e)<br/>') %>%
    hc_legend( align = 'center',
               verticalAlign = 'top',
               backgroundColor = 'transparent',
               symbolWidth = 3) %>%
    hc_add_theme(thm)


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

