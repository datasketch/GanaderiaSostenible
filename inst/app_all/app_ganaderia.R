library(dsAppLayout)
library(tidyverse)
library(highcharter)
library(dsCustom)
library(GanaderiaSostenible)

styles <- '
@import url("https://fonts.googleapis.com/css?family=Open+Sans:400,600&display=swap");
*,
*:before,
*:after{
  box-sizing: inherit;
}

button:focus { 
  outline-style: none; 
}

html {
  box-sizing: border-box;
  font-family: "Open Sans", sans-serif;
}

h3,h4,label,span{
  color: #8096a3;
}
p {
  font-size: 11pt;
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
    color: #fff;
    font-size: 13px;
    font-weight: 800;
    letter-spacing: 0.7px;
    padding: 10px;
    text-decoration: none;
    text-transform: uppercase;
    transition: background 0.1s linear;
    background-color: #b0d361;
    border: 1px solid #b0d361;
    display: block;
    width: 100%;
    cursor: pointer;
}

.btn#download_methodology {
  width: 130px !important;
  border-radius: 3px;
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
   border-width: 2px;
   border-style: none none dotted;
   border-color: #ddd;
   margin: 30px 0px;
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



.topbar,.modal-title {
    padding: 20px 55px;
    background-color: #2e4856;
    font-size: 31px;
    color: #fff;
}

.top_title {
margin-left: 30px;
display: flex;
}


.title-filters {
 color: #8096a3;
 font-weight: 600;
 letter-spacing: 1px;
 margin-bottom: 11px;
}

.modal-wrapper {
  height: auto;
}
.modal-title {
  margin: 10px 30px;
}
.modal-content {
  margin: 10px 30px;
}

.panel-title {
    color: #B0D361;
    font-size: 15px;
}

h2 {
  color: #8096a3;
    margin: 15px 0px;
    letter-spacing: 1.7px;
    text-transform: uppercase;
    font-size: 21px;
}

.titlte-info {
    color: #B0D361;
    font-weight: 600;
    margin: 7px 0px;
    letter-spacing: 0.7px;
    font-size: 15px;
}

.paraph-info {
 margin: 0px 0px;
}

::-webkit-scrollbar{
  width: 8px;
  height: 8px;
}

::-webkit-scrollbar-track{
  background: #E6E6E6;
}

::-webkit-scrollbar-thumb{
  background: #B0D361;
}

::-webkit-scrollbar:focus {
    overflow: scroll;
    display:block;
    background: #B0D361;
}

.panel#info_panel {
 width: 550px;
}

#id-but-mod {
 background: transparent;
 border: 0px;
 margin-left: 6px;
 cursor: pointer;
}

.topbar__img {
  height: 110px;
  width: 70px;
}

.topbar-modal,.tex_sub {
  font-size: 31px;
  color: #fff;
}

.modal-title {
    margin: 0px !important;
}

.top_line  {
border-left: 1px solid #ffffff;
margin-left: 10px;
font-weight: 700;
}

.contenido-modal {
 padding: 20px 50px;
}

.par-modal {
display:flex;
align-items: center;
}

.modal-wrapper {
    height: 600px;
    width: 65%;
    overflow: auto;
}

.form-group,.selectize-control,.control-label,.shiny-options-group,.margin-button-null {
margin: 0px !important;
}

#add_primario_2,#add_secundario_2,#add_potreros_2,#add_cercas_2,#add_pastoriles_2 {
    border-radius: 50%;
    background-color: #8096a3;
    border: 1px solid #8096a3;
    width: 20px;
    cursor: pointer;
    height: 20px;
    padding: 0 !important;
    text-align: center;
}

.text-btn {
 color: #8096a3;
font-size: 15px;
margin-right: 10px;
font-weight: 600;
white-space: nowrap;
margin: 10px 10px 10px 0px;
}

.mas-anios-primario,.mas-anios-secundario,.mas-anios-potreros,.mas-anios-cercas,.mas-anios-pastoriles {
    display: flex;
    flex-wrap: wrap;
    justify-content: space-between;
}

div[id^="annios_"] {
 width: 48% !important;
}

.add-anio {
  justify-content: center;
  align-items: center;
  display: inline-flex;
}

.selectize-input,.input-autosuggest,.input-autosuggest input {
  border: 1px solid #F0F4F5 !important;
  background: #F0F4F5 !important; 
  color: #333333;
  font-family: inherit;
  font-size: 13px;
  line-height: 20px;
}

.radio-inline {
 font-weight: 400 !important;
}


#anio_inicial_primario,#anio_inicial_secundario,#anio_inicial_potreros,#anio_inicial_cercas,#anio_inicial_pastoriles {
 width: 152px !important;
}

.collapsible {
 position: relative;
 margin-top: 15px;
}

.collapsible:before {
     content: "";
    position: absolute;
    top: 50%;
    left: 0;
    width: 100%;
    height: calc(30px + 100%);
    z-index: -1;
    background-color: #eef3f5;
    transform: translateY(-50%);
}

#remove-padding {
  padding: 0px !important;
  background: #f0f5f7;
}

.box-collapsible-content {
    overflow: auto !important;
}

.box-collapsible-trigger {
    margin: 10px 0px 0px 0px;
}

.margin-panel-info {
  margin: 0px 10px;
}

.flex-filters {
 display: flex;
 align-items: center;
 width: 100px;
}





'
source('info.R')
data_mun <- read_csv('data/MunicipiosColombia_geo.csv')
ui <- dsAppPanels( styles = styles,
                   header =  div(style="", class="topbar",
                                 img(class="topbar__img", src = "img/logo_GCS.png"),
                                 HTML("<div class = 'top_title'> HERRAMIENTA <div class = 'top_line'> <div style = 'margin-left: 10px;'> ESTIMACIÓN DE BIODIVERSIDAD Y <span class = 'tex_sub'>CAPTURA DE CO<sub>2</sub></span></div></div></div>"),
                                 modalBtn(id = 'id-but-mod', modal_id = 'info_modal', label = HTML('<i class="fa fa-info-circle" style="font-size:31px;color:#fff"></i>'))
                   ),
                   modal(id  = "info_modal",
                         title = div(class = 'topbar-modal',
                                     HTML("<div class = 'top_title' style = 'align-items: center;'> HERRAMIENTA <div class = 'top_line'> <div style = 'margin-left: 10px;'> ESTIMACIÓN DE BIODIVERSIDAD Y <span class = 'tex_sub'>CAPTURA DE CO<sub>2</sub></span></div></div></div>")
                         ),
                         body = div(
                           text_modal()
                         )
                   ),
                   panel(
                     id = "info_panel",
                     title = "Información de ayuda", color = "olive", collapsed = T,
                     body = div(
                       text_info()
                     )
                   ),
                   panel(
                     title = "INFORMACIÓN DEL PREDIO", color = "olive", collapsed = F, width = 400, id = 'panel-info', id_body = 'remove-padding',
                     body = (
                       div(
                         div(style = 'background: #ffffff;',
                             div(class = 'margin-panel-info',
                                 div(class = 'title-filters', 'UBICACIÓN'),
                                 uiOutput('buscador'),
                                 br(),
                             )),
                         box(title = div(class = 'title-filters', 'BOSQUE PRIMARIO'), collapsed = 'T', 
                             body =     div(class = 'panel-primario',
                                            uiOutput('anio_inicial_primario'),
                                            div(class="mas-anios-primario",
                                                uiOutput('annios_primario0'),
                                                uiOutput('annios_primario1')
                                            ),
                                            uiOutput('add_anio_primario')
                             )),
                         box(title = div(class = 'title-filters', 'BOSQUE SECUNDARIO'), collapsed = 'F', 
                             body =     div(class = 'panel-secundario',
                                            uiOutput('anio_inicial_secundario'),
                                            div(class="mas-anios-secundario",
                                                uiOutput('annios_secundario0'),
                                                uiOutput('annios_secundario1')
                                            ),
                                            uiOutput('add_anio_secundario')
                             )),
                         box(title = div(class = 'title-filters', 'ÁRBOLES DISPERSOS EN POTREROS'), collapsed = 'F', 
                             body =     div(class = 'panel-potreros',
                                            uiOutput('anio_inicial_potreros'),
                                            div(class="mas-anios-potreros",
                                                uiOutput('annios_potreros0'),
                                                uiOutput('annios_potreros1')
                                            ),
                                            uiOutput('add_anio_potreros')
                             )),
                         box(title = div(class = 'title-filters', 'CERCAS VIVAS'), collapsed = 'F', 
                             body =     div(class = 'panel-cercas',
                                            uiOutput('anio_inicial_cercas'),
                                            div(class="mas-anios-cercas",
                                                uiOutput('annios_cercas0'),
                                                uiOutput('annios_cercas1')
                                            ),
                                            uiOutput('add_anio_cercas')
                             )),
                         box(title = div(class = 'title-filters', 'SISTEMAS SILVOPASTORILES INTENSIVOS'), collapsed = 'F', 
                             body =     div(class = 'panel-pastoriles',
                                            uiOutput('anio_inicial_pastoriles'),
                                            div(class="mas-anios-pastoriles",
                                                uiOutput('annios_pastoriles0'),
                                                uiOutput('annios_pastoriles1')
                                            ),
                                            uiOutput('add_anio_pastoriles')
                             ))
                       )
                     )
                   ),
                   panel(
                     title = "RESULTADOS", color = "olive", collapsed = F, width = 450,
                     body = div(
                       div(class = 'title-filters margin-button-null', 'RESULTADOS'),
                       uiOutput('resultados'),
                       uiOutput('vista_resultados')
                     )#,
                     # footer = list(
                     #   actionButton("download_chart", "DESCARGAR GRÁFICA")
                     # )
                   ),
                   panel(
                     title = "RESULTADOS AVANZADOS", color = "olive", collapsed = F,  
                     body = div(uiOutput('vista_avanzados'))
                       #verbatimTextOutput('blala'),
                        #        highchartOutput('viz_lineas'))
                   )
                   
)

server <- function(input, output, session) {
  
  # panel de filtros
  
  
  output$buscador <- renderUI({
    
    dta_mun <- paste0(Hmisc::capitalize(tolower(data_mun$DEPARTAMEN)), ' - ', Hmisc::capitalize(tolower(data_mun$NOMBRE_ENT)))
    l_m <-  setNames(dta_mun, toupper(dta_mun))
    searchInput('name_mun', l_m, 'Búsqueda por municipio')
  })
  
  output$resultados <- renderUI({
    radioButtons('id_resultados', ' ', c('Biodiversidad', 'Captura de carbono'), inline = T)
  })
  
  
  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(z) {
    output[[paste0('anio_inicial_', z)]] <- renderUI({
      selectizeInput(paste0('id_anio', z), ' ', 1800:3000, selected = NULL, multiple = T,  options = list(
        placeholder = "Año inicial",
        plugins= list('remove_button'),
        maxItems = 1)
      )
    })
  })
  
  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(z) {
    map(0:1, function(i) {
      output[[paste0('annios_', z, i)]] <- renderUI({
        div(class = 'flex-filters',
            HTML(paste0('<span class = "text-btn">', paste0('Año ', i, '</span>'))),
            textInput(paste0('id_anios_', z, i), ' ', value = NULL)
        )
      })
    })
  })
  
  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(i) {
    output[[paste0('add_anio_', i)]] <- renderUI({
      div(class = 'add-anio',
          HTML('<div class = "text-btn"> Agregar año</div>'), 
          actionButton(paste0('add_', i, '_2'), HTML('+')))
    })
  })
  
  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(z) {
    observe({
      if (is.null(click_i[[z]])) click_i[[z]] <- 1
      map(click_i[[z]], function(i) {
        output[[paste0('annios_', z, i)]] <- renderUI({
          div(class = 'flex-filters',
              HTML(paste0('<span class = "text-btn">Año ', i, '</span>')),
              textInput(paste0('id_anios_', z, i), ' ',  value = NULL)
          )
        })
      })
    })
  })
  click_i <- reactiveValues(primario = 1, secundario = 1, potreros = 1, cercas = 1, pastoriles = 1)
  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(i) {
    observeEvent(input[[paste0('add_', i, '_2')]], {
      click_i[[i]] <- click_i[[i]] + 1
      insertUI(paste0('.mas-anios-', i), ui = uiOutput(paste0('annios_', i, click_i[[i]])), multiple = T)
    })  
  })  
  
  result <- reactive({
    
    lugar <- input$name_mun  
    if (is.null(lugar)) return()
    if (lugar == "") return()
    
    lugar <- str_split(lugar, ' - ') %>% unlist()
    region <- regiones_match(departamento = lugar[1], municipio = lugar[2])
    
    l <-  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(z) {
      n_years <- 0:click_i[[z]]
      map(n_years, function(i) {
        as.numeric(input[[paste0("id_anios_", z, i)]])
      })
    })
    print(l)
    names(l) <- c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles')
    
    bosque_primario <- unlist(l$primario)
    anio_primario <- input$id_anioprimario
    if (is.null(anio_primario)) anio_primario <- 0
    anio_primario <- as.numeric(anio_primario)
    captura_primario <- captura_carbono_bosques(departamento = lugar[1], municipio = lugar[2], area_bosque = bosque_primario)
    captura_primario <- data.frame(Año = anio_primario:(anio_primario + length(captura_primario) - 1), 
                                   Suelo = 'Bosque primario',
                                   carbono = captura_primario)
    captura_primario$Suelo <- as.character(captura_primario$Suelo)
    
    bosque_secundario <- unlist(l$secundario)
    anio_secundario <- input$id_aniosecundario
    if (is.null(anio_secundario)) anio_secundario <- 0
    anio_secundario <- as.numeric(anio_secundario)
    cb_carbono <- cambio_carbono(region = region, tipo_cobertura = 'bosque_secundario', t_f = length(bosque_secundario))
    fc_emision <- factor_emision(cb_carbono = cb_carbono, region = region)
    captura_secundario <- carbono_capturado(area = bosque_secundario, factor_emision = fc_emision)
    captura_secundario <- data.frame(Año = anio_secundario:(anio_secundario + length(captura_secundario) - 1), 
                                   Suelo = 'Bosque secundario',
                                   carbono = captura_secundario)
    captura_secundario$Suelo <- as.character(captura_secundario$Suelo)
    
    potreros <- unlist(l$potreros)
    anio_potreros <- input$id_aniopotreros
    if (is.null(anio_potreros)) anio_potreros <- 0
    anio_potreros <- as.numeric(anio_potreros)
    cb_carbono <- cambio_carbono(region = region, tipo_cobertura = 'arboles_dispersos', t_f = length(potreros))
    fc_emision <- factor_emision(cb_carbono = cb_carbono, region = region)
    captura_potreros <- carbono_capturado(area = potreros, factor_emision = fc_emision)
    captura_potreros <- data.frame(Año = anio_potreros:(anio_potreros + length(captura_potreros) - 1), 
                                   Suelo = 'Árboles dispersos',
                                   carbono = captura_potreros)
    captura_potreros$Suelo <- as.character(captura_potreros$Suelo)
    
    cercas <- unlist(l$cercas)
    anio_cercas <- input$id_aniocercas
    if (is.null(anio_cercas)) anio_cercas <- 0
    anio_cercas <- as.numeric(anio_cercas)
    cb_carbono <- cambio_carbono(region = region, tipo_cobertura = 'cercas_vivas', t_f = length(cercas))
    fc_emision <- factor_emision(cb_carbono = cb_carbono, region = region)
    captura_cercas <- carbono_capturado(area = cercas, factor_emision = fc_emision)
    captura_cercas <- data.frame(Año = anio_cercas:(anio_cercas + length(captura_cercas) - 1), 
                                 Suelo = 'Cercas vivas',
                                 carbono = captura_cercas)
    captura_cercas$Suelo <- as.character(captura_cercas$Suelo)
    
    pastoriles <- unlist(l$pastoriles)
    print(pastoriles)
    anio_pastoriles <- input$id_aniopastoriles
    if (is.null(anio_pastoriles)) anio_pastoriles <- 0
    anio_pastoriles <- as.numeric(anio_pastoriles)
    cb_carbono <- cambio_carbono(region = region, tipo_cobertura = 'silvopastoriles', t_f = length(pastoriles))
    print(cb_carbono)
    fc_emision <- factor_emision(cb_carbono = cb_carbono, region = region)
    print(fc_emision)
    captura_pastoriles <- carbono_capturado(area = pastoriles, factor_emision = fc_emision)
    print(captura_pastoriles)
    captura_pastoriles <- data.frame(Año = anio_pastoriles:(anio_pastoriles + length(captura_pastoriles) - 1), 
                                   Suelo = 'Silvopastoriles',
                                   carbono = captura_pastoriles)
    captura_pastoriles$Suelo <- as.character(captura_pastoriles$Suelo)
    
   bind_rows(captura_primario, captura_secundario, captura_potreros, captura_cercas, captura_pastoriles)
    
    
    
  })
  
  output$blala <- renderPrint({
    result()
  })
  
  output$viz_porcentaje <- renderHighchart({
    if (is.null(input$name_mun)) return()
    data <- result()[,-1]
    if (sum(data$carbono) == 0) return()
    data$carbono <- round(data$carbono, 2) 
    viz_bar(data)
  })
  
  output$vista_resultados <- renderUI({
    
    if (is.null(input$name_mun)) return()
    if (input$name_mun == "") return('Ingrese el municipio de ubicación de las categorías de uso del suelo')
    
    data <- result()[,-1]
    if (sum(data$carbono) == 0) return('Ingrese el área (en hectáreas) implementada por año en cada una de las categorías de uso del suelo')
    options(scipen = 9999)
    co2_car <- format(round(co2_carros(sum(data$carbono))), big.mark = ',', small.mark = '.')
    
    
    div(
      HTML(paste0('<div>CONTAMINACIÓN EVITADA</div><div>', co2_car, ' carros</div>')),
      highchartOutput('viz_porcentaje')
    )
    
  })
  
  output$viz_lineas <- renderHighchart({
    if (is.null(input$name_mun)) return()
    data <- result()
    if (sum(data$carbono) == 0) return()
    viz_lines(data)
  })
  
  output$vista_avanzados <- renderUI({
    
    if (is.null(input$name_mun)) return()
    
    data <- result()
    if (sum(data$carbono) == 0) return()
    options(scipen = 9999)
    car_tot <- format(round(sum(data$carbono)), big.mark = ',', small.mark = '.')
    
    
    div(
      HTML(paste0('<div>CARBONO TOTAL EQUIVALENTE</div><div>', car_tot, ' tCO<sub>2</sub>e</div>')),
      highchartOutput('viz_lineas')
    )
    
  })
  
}

shinyApp(ui, server)

