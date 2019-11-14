library(dsAppLayout)
library(tidyverse)
library(highcharter)
library(dsCustom)
# library(GanaderiaSostenible)

source("compute.R")
source("viz.R")

styles <- '
h2,h3,h4,label,span{
  color: #8096a3;
}
.app-container {
 background-color: #F2F7F9;
}
.top-olive {
	border-top: 2px solid #b0d361;
}
.text-olive {
	color: #b0d361;
}
.icon-close--olive line {
	stroke: #b0d361;
}

.btn:hover {
    background: #89a53d;
}
.btn {
    border-radius: 2px;
    color:
    #fff;
    font-size: 13px;
    font-weight: 800;
    letter-spacing: 0.7px;
    padding: 10px;
    text-decoration: none;
    text-transform: uppercase;
    transition: background 0.1s linear;
    background-color: #b0d361;
    display: block;
    width: 100%;
}
input[type="radio"]:active, input[type="radio"]:checked:active, input[type="radio"]:checked {
    background-color: #2e4856;
}
input[type="radio"] {
    -webkit-appearance: none;
    background-color: #E5EBEE;
    border-radius: 50%;
    border: 3px solid #E5EBEE;
    cursor: pointer;
    margin: 0px;
    outline: none;
    padding: 3px;
    position: relative;
    transition: all 0.1s;
}
hr {
    border-width: 4px;
    border-style: none none dotted;
    border-color: #ddd;
}
.img-center{
    display: block;
    margin: 0 auto;
}
.input-autosuggest {
    border: 1px solid #dddddd;
    border-radius: 2px;
    overflow: hidden;
    padding: 5px 0;
    background-color: #ffffff;
    margin-bottom: 10px;
}
.selectize-input{
  border-radius: 2px;
}
.form-group input, .form-group textarea{
  padding: 7px;
}
'

municipios <- read_csv("municipios.csv")[[2]]

ui <- dsAppPanels( styles = styles,
                   verbatimTextOutput("debug"),
                   panel(title = "Información de ayuda", color = "olive", collapsed = TRUE, width =  400,
                         head = h2("Head"),
                         body = div(
                           h2("Metodología"),
                           p("Si quieres conocer la forma en que se recogieron los datos, las fórmulas utilizadas para los cálculos
            y las fuentes bibliográficas descarga nuestra metodología"),
                           actionButton("download_methodology", "Descargar Metodología"),
                           hr(),
                           h2("Glosario"),
                           img(src="captura_carbono.png", width = 60),
                           h3("CAPTURA DE CARBONO O CO2"),
                           p("Algunos organismos como las plantas, por medio de la
                              fotosíntesis, capturan el carbono que se encuentra en
                              la atmósfera y así ayudan a reducir la concentración de
                              gases de efecto invernadero que forman parte de las
                              mayores causas del calentamiento global."),
                           img(src="arboles_dispersos.png", width = 60),
                           h3("ÁRBOLES DISPERSOS"),
                           p("Lorem ipsum dolor sit amet, consectetur adipiscing elit.
                             Suspendisse tempor nisl eget arcu euismod pulvinar. Ut eleifend euismod sapien
                             eget laoreet. Morbi consectetur enim ut mi sodales."),
                           img(src="bosque_primario.png", width = 60),
                           h3("BOSQUE PRIMARIO"),
                           p("Áreas de bosque maduro que no han sido intervenidas
                              por los humanos en los últimos 30 años y cuyo uso es
                              de conservación estricta. Actividades como la
                              extracción de madera, la cacería y las prácticas
                              agropecuarias están restringidas en estas áreas."),
                           img(src="bosque_secundario.png", width = 60),
                           h3("BOSQUE SECUNDARIO"),
                           p("Áreas de bosque en proceso de regeneración natural
                              en respuesta a daños causados por actividades del ser
                              humano. Su uso es de conservación estricta y
                              actividades como la extracción de madera, la cacería y
                              las prácticas agropecuarias están restringidas en estas
                              áreas."),
                           img(src="cercas_vivas.png", width = 60),
                           h3("CERCAS VIVAS"),
                           p("Áreas de bosque maduro que no han sido intervenidas
                              por los humanos en los últimos 30 años y cuyo uso es
                              de conservación estricta. Actividades como la
                              extracción de madera, la cacería y las prácticas
                              agropecuarias están restringidas en estas áreas."),
                           img(src="sistemas_silvopastoriles.png", width = 60),
                           h3("SISTEMAS SILVOPASTORILES"),
                           p("Áreas de bosque maduro que no han sido intervenidas
                              por los humanos en los últimos 30 años y cuyo uso es
                              de conservación estricta. Actividades como la
                              extracción de madera, la cacería y las prácticas
                              agropecuarias están restringidas en estas áreas.")
                         ),
                         footer = NULL
                   ),
                   panel(title = "INFORMACIÓN DEL PREDIO", color = "olive", collapsed = FALSE,
                         head = NULL,
                         body = list(
                           h2("RESULTADOS"),
                           radioButtons("result_type", "", choices = c("Carbono", "Biodiversidad"), inline = TRUE),
                           h2("UBICACIÓN"),
                           searchInput("municipio", data = municipios, placeholder = "Buscar municipio"),
                           radioButtons("data_type", "TIPO DE DATO", choices = c("Área Total", "Porcentaje de área"), inline = TRUE),
                           div(style = "display:flex;",
                               div(style = "padding:0 0px;",selectizeInput("init_year", "AÑO INICIAL", choices = 1980:2010)),
                               div(style = "padding:0 10px;", numericInput("n_years", "NÚMERO AÑOS", value = 1, width = '50px'))
                           ),
                           box(title = "BOSQUE SECUNDARIO", collapsed = FALSE,
                               uiOutput("controls_bosque_secundario")
                           ),
                           box(title = "ÁRBOLES DISPERSOS", collapsed = FALSE,
                               uiOutput("controls_arboles_dispersos")
                           ),
                           box(title = "CERCAS VIVAS", collapsed = FALSE,
                               uiOutput("controls_cercas_vivas")
                           ),
                           box(title = "SISTEMAS SILVOPASTORILES", collapsed = FALSE,
                               uiOutput("controls_silvopastoriles")
                           )
                         ),
                         footer = list(
                           box(title = "TOTAL",
                               uiOutput("controls_total")
                           )
                         )
                   ),
                   panel(title = "RESULTADOS", color = "olive", collapsed = FALSE,
                         head = NULL,
                         body = list(
                           uiOutput("result_viz")
                         ),
                         footer = list(
                           actionButton("download_chart", "DESCARGAR GRÁFICA")
                         )
                   ),
                   panel(title = "RESULTADOS AVANZADOS", color = "olive", collapsed = FALSE,
                         head = NULL,
                         body = list(
                           uiOutput("result_adv_text"),
                           hr(),
                           uiOutput("result_adv_viz")
                         ),
                         footer = list(
                           div(style = "display:flex;",
                               actionButton("download_chart2", "DESCARGAR GRÁFICA"),
                               actionButton("download_tables", "DESCARGAR TABLAS")
                           )
                         )
                   )
)

server <- function(input, output, session) {

  output$debug <- renderText({
    #str(result())
    # input$result_type
  })

  region <- reactive({
    municipio <- input$municipio
    "Eje Cafetero"
  })

  output$controls_bosque_secundario <- renderUI({
    lapply(1:input$n_years, function(i){
      textInput(paste0("bosque_secundario_",i), "", value = 10)
    })
  })
  output$controls_arboles_dispersos <- renderUI({
    lapply(1:input$n_years, function(i){
      textInput(paste0("arboles_dispersos_",i), "", value = 10)
    })
  })
  output$controls_cercas_vivas <- renderUI({
    lapply(1:input$n_years, function(i){
      textInput(paste0("cercas_vivas_",i), "", value = 10)
    })
  })
  output$controls_silvopastoriles <- renderUI({
    lapply(1:input$n_years, function(i){
      textInput(paste0("silvopastoriles_",i), "", value = 10)
    })
  })

  result <- reactive({

    n_years <- input$n_years
    res_bosque_secundario <- lapply(1:n_years, function(i){
      input[[paste0("bosque_secundario_",i)]]
    })
    res_arboles_dispersos <- lapply(1:n_years, function(i){
      input[[paste0("arboles_dispersos_",i)]]
    })
    res_cercas_vivas <- lapply(1:n_years, function(i){
      input[[paste0("cercas_vivas_",i)]]
    })
    res_silvopastoriles <- lapply(1:n_years, function(i){
      input[[paste0("silvopastoriles_",i)]]
    })

    coberturas <- c(
      arboles_dispersos = as.numeric(unlist(res_arboles_dispersos)),
      bosque_secundario = as.numeric(unlist(res_bosque_secundario)),
      cercas_vivas = as.numeric(unlist(res_cercas_vivas)),
      silvopastoril = as.numeric(unlist(res_silvopastoriles))
    )
    region <- region()
    total <- captura_areas(coberturas, region, t = 0)
    total_amount <- sum(total$captura)
    total_amount_cars <- round(total_amount/10)

    # need to calculate proyeccion
    path <- system.file("test_captura_eje_cafetero.csv", package = "GanaderiaSostenible")
    coberturas_proyeccion  <- read_csv(path)

    list(
      text = glue("{total_amount_cars} carros por un año de CO2"),
      text_adv = glue("{total_amount} toneladas CO2 equivalente"),
      coberturas = total,
      coberturas_proyeccion = coberturas_proyeccion
    )

  })


  output$result_viz <- renderUI({
    if(is.null(input$result_type)) return()
    if(input$result_type == "Carbono"){
      result <- result()
      text <- result$text
      viz <- list(
        div(class = "result-text",
            h3("CONTAMINACIÓN EVITADA "),
            h2(text)
        ),
        hr(),
        highchartOutput("result_viz_carbono")
      )
    }
    if(input$result_type == "Biodiversidad"){
      viz <- list(
        h3("Especies conservadas"),
        h2("450 Especies de Aves"),
        img(src = "aves.png", class = "img-center", width = "150px"),
        hr(),
        h3("Especies conservadas"),
        h2("450 Especies de Aves"),
        img(src = "escarabajos.png", class = "img-center", width = "150px")
      )
    }
    viz
  })

  output$result_viz_carbono <- renderHighchart({
    if(is.null(result())) return()
    total <- result()
    h <- viz_bar(total$coberturas)
    h
  })

  output$result_adv_text <- renderUI({
    result <- result()
    text <- result$text_adv
    div(class = "result-text",
        h3("CONTAMINACIÓN EVITADA "),
        h2(text)
    )
  })

  output$result_adv_viz <- renderUI({
    highchartOutput("result_adv_viz_chart")
  })
  output$result_adv_viz_chart <- renderHighchart({
    if(is.null(result())) return()
    d <- result()$coberturas_proyeccion
    h <- viz_lines(d)
    h
  })

}

shinyApp(ui, server)

