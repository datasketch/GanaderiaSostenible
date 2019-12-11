library(dsAppLayout)
library(tidyverse)
library(highcharter)
library(dsCustom)
library(GanaderiaSostenible)
library(geoloc)

styles <- '
@import url("https://fonts.googleapis.com/css?family=Ubuntu:400,500,700&display=swap");
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
    text-align: center;
}

#download_lineas {
 background:  #89a53d;
}

.btn#download_methodology {
  width: 130px !important;
  border-radius: 3px;
}


.content-intro {
  text-align: center;
  padding: 50px 50px;
}

.text-intro {
    text-transform: uppercase;
    font-size: 13pt;
    color: #8096a3;
    letter-spacing: 0.7px;
    font-weight: bold;
    margin-top: 11px;
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
 font-weight: 700;
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
    font-weight: 700;
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



#resultados-padding {
  padding-bottom: 0px !important;
}

/*
#remove-padding {
  padding: 0px !important;
  background: #f0f5f7;
}*/

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

#panel-info {
 padding-bottom: 0px !important;
}

.panel-footer {
padding: 0px !important;
}

.title-sliders {
 color: #8096a3;
 font-family: inherit;
 letter-spacing: 1px;
 text-transform: uppercase; 
}

.title-viz {
    color: #8096a3;
    font-family: inherit;
    letter-spacing: 1px;
    font-weight: 700;
    font-size: 13px;
}

.subtitle-viz {
font-size: 23px;
    color: #8096a3;
    font-weight: 700;
    font-family: inherit;
}

.info-tool {
 display: inline-flex;
}

.tooltip-inf {
 cursor: pointer;
 position: relative;
 margin-left: 3px;
}

.tooltip-inf .tooltiptext {
  visibility: hidden;
background-color: #fafafa;
    color: #2E4856;
    position: absolute;
    z-index: 9999;
    top: 0;
    padding: 1rem;
    border: 1px solid #ccc;
    font-weight: 400;
    letter-spacing: normal;
    font-size: 0.75rem;
    width: 140px;
    text-align: left;
}

.tooltip-inf:hover .tooltiptext {
  visibility: visible;
}

.irs-bar-edge,.irs-bar {
    background-color: #2e4856 !important;
    border: 1px solid #424242 !important;
}

.irs-slider {
    border: 1px solid #424242 !important;
   background-color: #2e4856 !important;
}

.irs.js-irs-0,.irs-with-grid {
 margin: 10px 10px !important;
     max-width: 500px !important;
}

.result-slider {
 max-width: 500px;
}

#slider_area_pastoriles {
border-top: 2px dotted #cccccc;
margin-top: 15px;
max-width: 500px;
}

#gpsubicacion {
    width: 130px;
    padding: 5px;
    background-color: #8096a3;
    border: 0px;
}

.ubicacion {
display: flex;
    justify-content: space-between;
    align-items: baseline;
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
                     title = "Información de ayuda", color = "olive", collapsed = T, show_footer = FALSE,
                     body = div(
                       text_info()
                     )
                   ),
                   panel(
                     title = "INFORMACIÓN DEL PREDIO",show_footer = FALSE, color = "olive", collapsed = F, width = 400, id = 'panel-info', id_body = 'remove-padding',
                     body = (
                       div(
                         div(style = 'background: #ffffff;',
                             div(class = 'margin-panel-info',
                                 div(class = "ubicacion",
                                 div(class = 'title-filters', 'UBICACIÓN'),
                                 geoloc::button_geoloc("gpsubicacion", HTML('<i class="fas fa-map-marker-alt"></i> Activar GPS')),
                                 ),
                                 uiOutput('buscador'),
                                 br()
                             )),
                         box(title = div(class = 'title-filters', 'BOSQUE PRIMARIO'), collapsed = FALSE,
                                         div(class = 'panel-primario',
                                             div(style = "width: 152px !important;",
                                             selectizeInput('id_anioprimario', ' ', 1990:3000, selected = NULL, multiple = T,  options = list(
                                               placeholder = "Año inicial",
                                               plugins= list('remove_button'),
                                               maxItems = 1)
                                             )),
                                            div(class="mas-anios-primario",
                                                uiOutput('annios_primario0'),
                                                uiOutput('annios_primario1')
                                            ),
                                             uiOutput('add_anio_primario'),
                                            br()
                             )),
                         box(title = div(class = 'title-filters', 'BOSQUE SECUNDARIO'), collapsed = T, 
                                   div(class = 'panel-secundario',
                                            uiOutput('anio_inicial_secundario'),
                                            div(class="mas-anios-secundario",
                                                uiOutput('annios_secundario0'),
                                                uiOutput('annios_secundario1')
                                            ),
                                            uiOutput('add_anio_secundario'),
                                       br()
                             )),
                         box(title = div(class = 'title-filters', 'ÁRBOLES DISPERSOS EN POTREROS'), collapsed = T, 
                                   div(class = 'panel-potreros',
                                            uiOutput('anio_inicial_potreros'),
                                            div(class="mas-anios-potreros",
                                                uiOutput('annios_potreros0'),
                                                uiOutput('annios_potreros1')
                                            ),
                                            uiOutput('add_anio_potreros'),
                                       br()
                             )),
                         box(title = div(class = 'title-filters', 'CERCAS VIVAS'), collapsed = T, 
                                 div(class = 'panel-cercas',
                                            uiOutput('anio_inicial_cercas'),
                                            div(class="mas-anios-cercas",
                                                uiOutput('annios_cercas0'),
                                                uiOutput('annios_cercas1')
                                            ),
                                            uiOutput('add_anio_cercas'),
                                     br()
                             )),
                         box(title = div(class = 'title-filters', 'SISTEMAS SILVOPASTORILES INTENSIVOS'), collapsed = T, 
                                    div(class = 'panel-pastoriles',
                                            uiOutput('anio_inicial_pastoriles'),
                                            div(class="mas-anios-pastoriles",
                                                uiOutput('annios_pastoriles0'),
                                                uiOutput('annios_pastoriles1')
                                            ),
                                            uiOutput('add_anio_pastoriles'),
                                        br()
                             )
                     ))
                   )),
                   panel(
                     title = "RESULTADOS", color = "olive", collapsed = F, width = 450, id = 'resultados-padding',
                     body = div(
                       #div(class = 'title-filters margin-button-null', 'RESULTADOS'),
                       #verbatimTextOutput('aver'),
                       uiOutput('resultados'),
                       uiOutput('vista_resultados')
                     ),
                     footer = list(
                       downloadButton("download_bar", "DESCARGAR GRÁFICA")
                     )
                   ),
                   panel(
                     title = "RESULTADOS AVANZADOS", color = "olive", collapsed = F,  id = 'resultados-padding',
                     body =  div(uiOutput('vista_avanzados')),
                     footer = div( style = "display:flex;",
                                   downloadButton("download_lineas", "DESCARGAR GRÁFICA"),
                                   downloadButton("download_data", "DESCARGAR DATOS")
                     )
                   )
                   
)

server <- function(input, output, session) {
  
  # panel de filtros
  output$aver <- renderPrint({
    #input$gpsubicacion
    list(
    req(input$gpsubicacion_lon),
    req(input$gpsubicacion_lat)
    )
  })
  
  output$buscador <- renderUI({
    
    dta_mun <- paste0(Hmisc::capitalize(tolower(data_mun$DEPARTAMEN)), ' - ', Hmisc::capitalize(tolower(data_mun$NOMBRE_ENT)))
    l_m <-  setNames(dta_mun, toupper(dta_mun))
    searchInput('name_mun', l_m, 'Búsqueda por municipio')
  })
  
  output$resultados <- renderUI({
    radioButtons('id_resultados', ' ', c('Captura de carbono', 'Biodiversidad'), inline = T)
  })
  
  
  map(c('secundario', 'potreros', 'cercas', 'pastoriles'), function(z) {
    output[[paste0('anio_inicial_', z)]] <- renderUI({
      selectizeInput(paste0('id_anio', z), ' ', 1990:3000, selected = NULL, multiple = T,  options = list(
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
  
  
  output$blabla <- renderPrint({
    result()  
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
    names(l) <- c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles')
    
    bosque_primario <- unlist(l$primario)
    bosque_primario[is.na(bosque_primario)] <- 0
    anio_primario <- input$id_anioprimario
    if (is.null(anio_primario)) anio_primario <- 0
    anio_primario <- as.numeric(anio_primario)
    captura_primario <- captura_carbono_bosques(departamento = lugar[1], municipio = lugar[2], area_bosque = cumsum(bosque_primario))
    captura_primario <- data.frame(Suelo = 'Bosque primario',
                                   carbono = captura_primario)
    captura_primario$Suelo <- as.character(captura_primario$Suelo)
    estimacion_primario <- captura_carbono_bosques(departamento = lugar[1], municipio = lugar[2], area_bosque = c(cumsum(bosque_primario), rep(sum(bosque_primario), 20-length(bosque_primario))))
    estimacion_primario <- data.frame(Año = anio_primario + 0:19,
                                      Suelo = 'Bosque primario',
                                      carbono = estimacion_primario)
    estimacion_primario$Suelo <- as.character(estimacion_primario$Suelo)
    if (region != 'Otras Áreas') {
    pajaros_bosque_primario <- biodiv_area(area = sum(bosque_primario, na.rm = T), region = region, tipo_cobertura = 'bosque_secundario')
    } else {
    pajaros_bosque_primario <- NULL
    }
    
    bosque_secundario <- unlist(l$secundario)
    anio_secundario <- input$id_aniosecundario
    if (is.null(anio_secundario)) anio_secundario <- 0
    anio_secundario <- as.numeric(anio_secundario)
    captura_secundario <- carbono_capturado_estimacion(area = bosque_secundario, region = region, tipo_cobertura = 'bosque_secundario', t_e = length(bosque_secundario))
    captura_secundario <- data.frame(Suelo = 'Bosque secundario',
                                     carbono = captura_secundario$co2)
    captura_secundario$Suelo <- as.character(captura_secundario$Suelo)
    estimacion_secundario <- carbono_capturado_estimacion(area = bosque_secundario, region = region, tipo_cobertura = 'bosque_secundario', t_e = 20)
    estimacion_secundario <- data.frame(Suelo = 'Bosque secundario',
                                        carbono = estimacion_secundario$co2)
    estimacion_secundario$Año <- anio_secundario + 0:19
    estimacion_secundario$Suelo <- as.character(estimacion_secundario$Suelo)
    if (region != 'Otras Áreas') {
      pajaros_bosque_secundario <- biodiv_area(area = sum(bosque_secundario, na.rm = T), region = region, tipo_cobertura = 'bosque_secundario')
    } else {
      pajaros_bosque_secundario <- NULL
    }
    
    potreros <- unlist(l$potreros)
    anio_potreros <- input$id_aniopotreros
    if (is.null(anio_potreros)) anio_potreros <- 0
    anio_potreros <- as.numeric(anio_potreros)
    captura_potreros <- carbono_capturado_estimacion(area = potreros, region = region, tipo_cobertura = 'arboles_dispersos', t_e = length(potreros))
    captura_potreros <- data.frame(Suelo = 'Árboles dispersos',
                                   carbono = captura_potreros$co2)
    captura_potreros$Suelo <- as.character(captura_potreros$Suelo)
    estimacion_potreros <- carbono_capturado_estimacion(area = potreros, region = region, tipo_cobertura = 'arboles_dispersos', t_e = 20)
    estimacion_potreros <- data.frame(Año = anio_potreros + 0:19,
                                      Suelo = 'Árboles dispersos',
                                      carbono = estimacion_potreros$co2)
    estimacion_potreros$Suelo <-  as.character(estimacion_potreros$Suelo)
    if (region != 'Otras Áreas') {
      pajaros_potreros <- biodiv_area(area = sum(potreros, na.rm = T), region = region, tipo_cobertura = 'silvopastoriles')
    } else {
      pajaros_potreros <- NULL
    }
    
    
    cercas <- unlist(l$cercas)
    anio_cercas <- input$id_aniocercas
    if (is.null(anio_cercas)) anio_cercas <- 0
    anio_cercas <- as.numeric(anio_cercas)
    captura_cercas <- carbono_capturado_estimacion(area = cercas, region = region, tipo_cobertura = 'cercas_vivas', t_e = length(cercas))
    captura_cercas <- data.frame( Suelo = 'Cercas vivas',
                                  carbono = captura_cercas$co2)
    captura_cercas$Suelo <- as.character(captura_cercas$Suelo)
    estimacion_cercas <- carbono_capturado_estimacion(area = cercas, region = region, tipo_cobertura = 'cercas_vivas', t_e = 20)
    estimacion_cercas <- data.frame(Año = anio_cercas + 0:19,
                                    Suelo = 'Cercas vivas',
                                    carbono = estimacion_cercas$co2)
    estimacion_cercas$Suelo <-  as.character(estimacion_cercas$Suelo)
    if (region != 'Otras Áreas') {
      pajaros_cercas <- biodiv_area(area = sum(cercas, na.rm = T), region = region, tipo_cobertura = 'silvopastoriles')
    } else {
      pajaros_cercas <- NULL
    }
    
    pastoriles <- unlist(l$pastoriles)
    anio_pastoriles <- input$id_aniopastoriles
    if (is.null(anio_pastoriles)) anio_pastoriles <- 0
    anio_pastoriles <- as.numeric(anio_pastoriles)
    captura_pastoriles <- carbono_capturado_estimacion(area = pastoriles, region = region, tipo_cobertura = 'silvopastoriles', t_e = length(pastoriles))
    captura_pastoriles <- data.frame(Suelo = 'Silvopastoriles',
                                     carbono = captura_pastoriles$co2)
    captura_pastoriles$Suelo <- as.character(captura_pastoriles$Suelo)
    estimacion_pastoriles <- carbono_capturado_estimacion(area = pastoriles, region = region, tipo_cobertura = 'silvopastoriles', t_e = 20)
    estimacion_pastoriles <- data.frame(Año = anio_pastoriles + 0:19,
                                        Suelo = 'Silvopastoriles',
                                        carbono = estimacion_pastoriles$co2)
    estimacion_pastoriles$Suelo <-  as.character(estimacion_pastoriles$Suelo)
    if (region != 'Otras Áreas') {
      pajaros_pastoriles <- biodiv_area(area = sum(pastoriles, na.rm = T), region = region, tipo_cobertura = 'silvopastoriles')
    } else {
      pajaros_pastoriles <- NULL
    }
    
    captura_general <- bind_rows(captura_primario, captura_secundario, captura_potreros, captura_cercas, captura_pastoriles)
    estimacion_general <- bind_rows(estimacion_primario, estimacion_secundario, estimacion_potreros, estimacion_cercas, estimacion_pastoriles)
    estimacion_pajaros <- list(pajaros_bosque_primario, pajaros_bosque_secundario, pajaros_potreros, pajaros_cercas, pajaros_pastoriles)
    list("region" = region,"captura_general" = captura_general, "estimacion_general" = estimacion_general,  "pajaros" = estimacion_pajaros)
    
  })
  
  
  plot_bar <- reactive({
    if (is.null(input$name_mun)) return()
    data <- result()$captura_general
    if (sum(data$carbono) == 0) return()
    data$carbono <- round(data$carbono, 2)
    viz_bar(data)
  })
  
  output$viz_porcentaje <- renderHighchart({
    print(plot_bar())
  })
  
  output$total_aves <- renderUI({
   
    if (all(is.null(unlist(result()$pajaros)))) {
      tx <- 'No hay resultados para este municipio'
    } else {
      tx <- div(
        HTML(paste0('<div style = "text-align:center;"><div class = "title-viz">ESPECIES CONSERVADAS</div><div class = "subtitle-viz">',  round(sum(unlist(result()$pajaros), na.rm = T)), ' ESPECIES DE AVES</div></div></div>')),
        tags$img(style = "text-align: center; padding: 0px 20px;margin-top:20px;", src = "img/aves.png")
        )
    }    
    
    tx
    
  })
  
  output$vista_resultados <- renderUI({
    
    if (is.null(input$name_mun)) return()
    if (input$name_mun == "") return(HTML('<div class = "content-intro"><img style = "width:78px;" src = "img/placeholder.png"><div class = "text-intro">Llena los campos de información de tú predio</div></div>'))
    
    data <- result()$captura_general
    if (sum(data$carbono) == 0)  return(HTML('<div class = "content-intro"><img style = "width:78px;" src = "img/placeholder.png"><div class = "text-intro">Llena los campos de información de tú predio</div></div>'))
    options(scipen = 9999)
    
    id_res <- input$id_resultados
    if (is.null(id_res)) return()
    
    if (id_res == 'Biodiversidad') {
      uiOutput('total_aves')
    } else {
    co2_car <- format(round(co2_carros(sum(data$carbono))), big.mark = ',', small.mark = '.')
    div(
      HTML(paste0('<div style = "text-align:center;"><div class = "title-viz">CONTAMINACIÓN EVITADA</div><div class = "info-tool subtitle-viz">', co2_car, ' carros <div class="tooltip-inf"> <i class="fa fa-info-circle"></i><span class="tooltiptext">El cálculo se realiza según la distancia promedio recorrida en grandes ciudades durante un año (12500 km), por un carro promedio de motor 1.5 litros.</span</div></div></div></div>')),
      highchartOutput('viz_porcentaje')
    )
    }
    
  })
  
  plot_lineas <- reactive({
    if (is.null(input$name_mun)) return()
    data <- result()$estimacion_general
    if (sum(data$carbono) == 0) return()
    viz_lines(data)
  })
  
  output$viz_lineas <- renderHighchart({
    plot_lineas()
  })
  
  output$slider_area <- renderUI({
    if (all(is.null(unlist(result()$pajaros)))) {
      "No hay información disponible para este municipio"
    } else {
      div(HTML('<div class = "title-sliders">Área bosque primario o secundario</div>'),
    sliderInput('id_area_primario', ' ', min = 1000, max = 1000000, value = 3000, step = 100)
      )
    }
  })
  
  output$slider_area_pastoriles <- renderUI({
    if (all(is.null(unlist(result()$pajaros)))) {
      "No hay información disponible para este municipio"
    } else {
      div(HTML('<div class = "title-sliders" style="margin-top:15px;">Área Silvopastorales, cercas vivas <br/>o árboles dispersos</div>'),
      sliderInput('id_pastoriles', ' ', min = 1000, max = 1000000, value = 5000, step = 100)
      )
    }
  })
  
  output$text_aves <- renderUI({
    # if (all(is.null(unlist(result()$pajaros)))) {
    #   txt <- HTML("No hay información disponible para este municipio")
    # } else {
    area_bosque <- input$id_area_primario
    if (is.null(area_bosque)) return()
    region <- result()$region
    aves_bosques <- round(biodiv_area(area = area_bosque, region = region, tipo_cobertura = 'bosque_secundario'))
    txt <- HTML(paste0('<p class = "result-slider">Por cada  <span style="color: #2e4856;font-size: 18px;">', area_bosque, ' hectáreas </span> de más en bosques primarios o secundarios se podrían conservar <span style="color: #2e4856;font-size: 18px;">', aves_bosques, ' aves</span>.</p>' ))
    
    txt
  })
  
  output$text_aves_pastoriles <- renderUI({
   
    area_otras <- input$id_pastoriles
    if (is.null(area_otras)) return()
    region <- result()$region
    aves_otras <- round(biodiv_area(area = area_otras, region = region, tipo_cobertura = 'silvopastoriles'))
    txt <- HTML(paste0('<p class = "result-slider">
                Por cada  <span style="color: #2e4856;font-size: 18px;">', area_otras, ' hectáreas</span> de más en suelos silvopastorales, cercas vivas ó árboles dispersos se podrían conservar <span style="color: #2e4856;font-size: 18px;">', aves_otras, ' aves</span>.</p>' ))
    
    txt
  })
  
  output$vista_avanzados <- renderUI({
    
    if (is.null(input$name_mun)) return()
    if (input$name_mun == "") return(HTML('<div class = "content-intro" style = "margin-top:45px;"><img style = "width:78px;" src = "img/placeholder.png"><div class = "text-intro">Llena los campos de <br/> información de tú predio</div></div>'))
    data <- result()$estimacion_general
    
    if (sum(data$carbono) == 0)  return(HTML('<div class = "content-intro" style = "margin-top:45px;"><img style = "width:78px;" src = "img/placeholder.png"><div class = "text-intro">Llena los campos de <br/> información de tú predio</div></div>'))
    
    options(scipen = 9999)
    
    id_res <- input$id_resultados
    if (is.null(id_res)) return()
    
    if (id_res == 'Biodiversidad') {
      div(
        uiOutput('slider_area'),
        uiOutput('text_aves'),
        uiOutput('slider_area_pastoriles'),
        uiOutput('text_aves_pastoriles')
      )
    } else {
    car_tot <- format(round(sum(data$carbono)), big.mark = ',', small.mark = '.')

    div(
      HTML(paste0('<div style = "text-align:center;"><div class = "title-viz">PROYECCIÓN CAPTURA DE CARBONO </div><div class = "subtitle-viz">', car_tot, ' tCO<sub>2</sub>e</div></div>')),
      highchartOutput('viz_lineas')
    )
    }
    
  })
  
  output$download_bar <- downloadHandler(
    filename = function() {
      ext <- ifelse(input$id_resultados == 'Biodiversidad', '.txt', '.html')
      paste('plot-', Sys.Date(), ext, sep='')
    },
    content = function(file) {
      if (input$id_resultados == 'Biodiversidad') {
      write_lines('No hay gráfico disponible', file)
      } else {
      htmlwidgets::saveWidget(plot_bar(), file)
      }
    }
  )
  
  
  output$download_lineas <- downloadHandler(
    filename = function() {
      ext <- ifelse(input$id_resultados == 'Biodiversidad', '.txt', '.html')
      paste('plot-lineas', Sys.Date(), ext, sep='')
    },
    content = function(file) {
      if (input$id_resultados == 'Biodiversidad') {
        write_lines('No hay gráfico disponible', file)
      } else {
        htmlwidgets::saveWidget(plot_lineas(), file)
      }
    }
  )
  
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste('data', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      if (input$id_resultados == 'Biodiversidad') {
        write_lines('No hay data disponible', file)
      } else {
        data <- as.data.frame(result()$estimacion_general)
        write_csv(data, file)
      }
    }
  )
  
  
  
  
  
}

shinyApp(ui, server)

