library(shinypanels)
library(tidyverse)
library(highcharter)
library(GanaderiaSostenible)
#library(geoloc)



# .layout-container {
#   height: 100%;
#   display: flex;
#   flex-direction: column;
# }
#
# .layout-panels {
#   background-color: #fbfdfe;
#     padding: 30px;
#   flex-grow: 1;
#   flex-shrink: 1;
#   overflow-x: auto;
# }
#
# .app-container {
#   display: flex;
#   height: 100%;
# }
#
# .app-container:after {
#   content: "";
#   flex: 0 0 30px;
# }




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
p li{
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

.input-autosuggest input{
margin-bottom: 0px !important;
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
  font-size: 14px;
  color: #fff;
}

.top_title {
  margin-left: 24px;
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
  font-size: 14px;
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

#add_primario_1,#add_secundario_1,#add_potreros_1,#add_cercas_1,#add_pastoriles_1 {
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

/*
.mas-anios-primario,.mas-anios-secundario,.mas-anios-potreros,.mas-anios-cercas,.mas-anios-pastoriles {
    display: flex;
    flex-wrap: wrap;
    justify-content: space-between;
}


div[id^="annios_"] {
 width: 48% !important;
}
*/

input {
     margin-right: 10px !important;
     margin-bottom: 10px !important;
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

.anios-valor {
  display: flex;
  justify-content: space-between;
  width: 50%;
}


#resultados-padding {
  padding-bottom: 0px !important;
}

/*#remove-padding {
  padding: 0px !important;
  background: #f0f5f7;
}
.margin-panel-info {
  margin: 0px 10px;
}
#anio_inicial_primario,#anio_inicial_secundario,#anio_inicial_potreros,#anio_inicial_cercas,#anio_inicial_pastoriles {
 width: 152px !important;
}*/

.box-collapsible-content {
    overflow: auto !important;
}

.box-collapsible-trigger {
    margin: 10px 0px 0px 0px;
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
font-size: 21px;
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

#warning_years {
  color: orange;
  font-size: small;
}

@media only screen and (min-width: 768px) {
  .topbar, .tex_sub, .modal-title {
    font-size: 20px;
  }
}

@media only screen and (min-width: 1024px) {
  .topbar, .tex_sub, .modal-title {
    font-size: 32px;
  }
}


.topbar {
    padding: 20px 55px;
    background-color: #2e4856;
    font-size: 14px;
    color: #fff;
    overflow: hidden;
}

.top_title {
  margin-left: 24px;
  display: flex;
}

.topbar__img {
  height: auto;
  width: 72px;
}

.top_line {
  border-left: 1px solid #ffffff;
  margin-left: 10px;
  font-weight: 700;
}


.topbar-modal, .tex_sub {
  font-size: 14px;
  color: #fff;
}

@media only screen and (min-width: 768px) {
  .topbar, .tex_sub {
    font-size: 20px;
  }
}

@media only screen and (min-width: 1024px) {
  .topbar, .tex_sub {
    font-size: 32px;
  }
}

.layout-panels {
    min-height: 400px;
}

'
source('info.R')
data_mun <- read_csv('data/MunicipiosColombia_geo.csv')
ui <- panelsPage( styles = styles,
                  header =  div(style="", class="topbar",
                                img(class="topbar__img", src = "img/logo_GCS.png"),
                                HTML("<div class = 'top_title'> HERRAMIENTA <div class = 'top_line'>
                                      <div style = 'margin-left: 10px;'> ESTIMACIÓN DE BIODIVERSIDAD, CAPTURA<span class = 'tex_sub'>
                                      Y EMISIONES EVITADAS DE CO<sub>2</sub></span></div></div></div>"),
                                modalButton(id = 'id-but-mod', modal_id = 'info_modal',
                                            label = HTML('<i class="fa fa-info-circle" style="font-size:31px;color:#fff"></i>'))
                  ),
                  modal(id  = "info_modal",
                        title = div(class = 'topbar-modal',
                                    HTML("<div class = 'top_title' style = 'align-items: center;'> HERRAMIENTA
                                         <div class = 'top_line'> <div style = 'margin-left: 10px;'> ESTIMACIÓN DE BIODIVERSIDAD Y
                                         <span class = 'tex_sub'>CAPTURA DE CO<sub>2</sub></span></div></div></div>")
                        ),
                        body = div(
                          text_modal()
                        )
                  ),
                  panel(
                    id = "info_panel",
                    title = "Información de ayuda", color = "olive", collapsed = TRUE, show_footer = FALSE,
                    body = div(
                      uiOutput("debug_container"),
                      text_info()
                    )
                  ),
                  panel(
                    title = HTML(paste0('INFORMACIÓN DEL PREDIO<div class = "info-tool"> <div class="tooltip-inf">
                     <i class="fa fa-info-circle"></i><span class="tooltiptext" style = "width: 340px !important;text-transform: lowercase;">
                                         <span style = "color: #2E4856;text-transform: uppercase;">A</span>
                                         active el gps para determinar su ubicación o ingrese el municipio en el cuál tiene sus predios, luego diríjase
                                         a la cobertura de interés, ingrese el año inicial en el que empezó la implementación del suelo y el número total
                                         de hectáreas de este terreno, sí desde el año inicial hasta la actualidad ha agregado más terreno, de click en
                                         agregar año donde se desplegará un nuevo cuadro, allí debe ingresar el año en el que agregó más hectáreas a su suelo y
                                         poner el número adicional.</span</div></div></div>')),
                    show_footer = FALSE, color = "olive", collapsed = FALSE, width = 450, id = 'panel-info', id_body = 'remove-padding',
                    body = (
                      div(
                        div(style = 'background: #ffffff;',
                            div(class = 'margin-panel-info',
                                div(class = "ubicacion",
                                    div(class = 'title-filters', 'UBICACIÓN')
                                    # geoloc::button_geoloc("gpsubicacion", HTML('<i class="fas fa-map-marker-alt"></i> Activar GPS')),
                                ),
                                uiOutput('buscador'),
                                br()
                            )),
                        uiOutput("warning_years"),
                        box(title = div(class = 'title-filters', 'BOSQUE PRIMARIO'), collapsed = FALSE,
                            div(class = 'panel-primario',
                                div(class="mas-anios-primario",
                                    div(class = "anios-valor",
                                        numericInput(paste0('aniosprimario0'), 'Año', value = NULL, min = 2000, max = 2020, width = "80px"),
                                        textInput(paste0('id_anios_primario0'), 'Área', value = NULL, placeholder = 'Hectáreas')
                                    )
                                ),
                                uiOutput('add_anio_primario'),
                                br()
                            )),
                        box(title = div(class = 'title-filters', 'BOSQUE SECUNDARIO'), collapsed = TRUE,
                            div(class = 'panel-secundario',
                                div(class="mas-anios-secundario",
                                    div(class = "anios-valor",
                                        numericInput(paste0('aniossecundario0'), 'Año', value = NULL, min = 2000, max = 2020, width = "80px"),
                                        textInput(paste0('id_anios_secundario0'), 'Área', value = NULL, placeholder = 'Hectáreas')
                                    )
                                ),
                                uiOutput('add_anio_secundario'),
                                br()
                            )),
                        box(title = div(class = 'title-filters', 'ÁRBOLES DISPERSOS EN POTREROS'), collapsed = TRUE,
                            div(class = 'panel-potreros',
                                div(class="mas-anios-potreros",
                                    div(class = "anios-valor",
                                        numericInput(paste0('aniospotreros0'), 'Año', value = NULL, min = 2000, max = 2020, width = "80px"),
                                        textInput(paste0('id_anios_potreros0'), 'Área', value = NULL, placeholder = 'Hectáreas')
                                    )
                                ),
                                uiOutput('add_anio_potreros'),
                                br()
                            )),
                        box(title = div(class = 'title-filters', 'CERCAS VIVAS'), collapsed = TRUE,
                            div(class = 'panel-cercas',
                                div(class="mas-anios-cercas",
                                    div(class = "anios-valor",
                                        numericInput(paste0('anioscercas0'), 'Año', value = NULL, min = 2000, max = 2020, width = "80px"),
                                        textInput(paste0('id_anios_cercas0'), 'Longitud', value = NULL, placeholder = 'Kilómetros')
                                    )
                                ),
                                uiOutput('add_anio_cercas'),
                                br()
                            )),
                        box(title = div(class = 'title-filters', 'SISTEMAS SILVOPASTORILES INTENSIVOS'), collapsed = TRUE,
                            div(class = 'panel-pastoriles',
                                div(class="mas-anios-pastoriles",
                                    div(class = "anios-valor",
                                        numericInput(paste0('aniospastoriles0'), 'Año', value = NULL,  min = 2000, max = 2020, width = "80px"),
                                        textInput(paste0('id_anios_pastoriles0'), 'Área', value = NULL, placeholder = 'Hectáreas')
                                    )
                                ),
                                uiOutput('add_anio_pastoriles'),
                                br()
                            )
                        ))
                    )),
                  panel(
                    title = HTML(paste0('RESULTADOS<div class = "info-tool"> <div class="tooltip-inf"> <i class="fa fa-info-circle"></i>
                     <span class="tooltiptext" style = "width: 340px !important;text-transform: lowercase;">
                     <span style = "color: #2E4856;text-transform: uppercase;">S</span>i selecciona captura de carbono, podrá ver el resumen
                     del total de carbono capturado por tipo de terreno hasta el día de hoy, el gráfico le muestra el porcentaje de captura según
                     el tipo de suelo, es decir, si tiene dos tipos de terrenos el gráfico le mostrará cuánto captura en porcentaje cada uno. <br/>
                                <span style = "color: #2E4856;text-transform: uppercase;">S</span>i selecciona biodiversidad podrá ver la estimación
                                         del número de aves protegidas por terreno. </span</div></div></div>')),
                    color = "olive", collapsed = FALSE, width = 350,show_footer = FALSE, id = 'resultados-padding',
                    body = div(
                      #verbatimTextOutput('aver'),
                      uiOutput('resultados'),
                      uiOutput('vista_resultados')
                    )
                  ),
                  panel(
                    title =  HTML(paste0('RESULTADOS AVANZADOS<div class = "info-tool"> <div class="tooltip-inf"> <i class="fa fa-info-circle"></i>
                                          <span class="tooltiptext" style = "width: 310px !important;text-transform: lowercase;">
                                          <span style = "color: #2E4856;text-transform: uppercase;">A</span>cá podrá ver una proyección de la captura de carbono
                                          por terreno, además si da click en el cuadro inferior del gráfico podrá ver la proyección del total capturado.
                                          <span style = "color: #2E4856;text-transform: uppercase;">S</span>i selecciona Biodiversidad en resultados, podrá deslizar
                                          el círculo para conocer el número de especies protegidas por hectáreas.</span</div></div></div>')),
                    color = "olive", collapsed = FALSE, show_footer = FALSE, id = 'resultados-padding',
                    body =  div(uiOutput('vista_avanzados'))
                  )

)

server <- function(input, output, session) {



  # # panel de filtros
  # output$aver <- renderPrint({
  #   #input$gpsubicacion
  #   list(
  #   req(input$gpsubicacion_lon),
  #   req(input$gpsubicacion_lat)
  #   )
  # })

  output$debug_container <- renderUI({
    if(!.GlobalEnv$.debug) return()
    div(class="debug", verbatimTextOutput("debug"))
  })

  output$debug <- renderText({

    lugar <- input$name_mun
    lugar <- "Quindío - Montenegro"

    if (is.null(lugar)) return()
    if (lugar == "") return()

    lugar <- str_split(lugar, ' - ') %>% unlist()
    departamento <- lugar[1]
    municipio <- lugar[2]

    l <-  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(z) {
      n_years <- 0:click_i[[z]]
      annios <- c()
      hectareas <- c()
      map(n_years, function(i) {
        input_0 <- as.numeric(input[[paste0('anios', z, '0')]])
        annios <- as.numeric(input[[paste0('anios', z, i)]])
        if (sum(is.null(annios)) >= 1) annios <- input_0 + i
        hectareas <- as.numeric(input[[paste0("id_anios_", z, i)]])
        data.frame(year = annios, value = hectareas)
      }) %>% bind_rows()
    })
    names(l) <- availableTipoCobertura() #c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles')
    #current_year <- as.numeric(format(Sys.Date(), "%Y"))
    inputs <- l

    if(!is.null(.GlobalEnv$.preset)){
      presets <- list(
        "00" = list(
          cercas_vivas = data.frame(year = 2012, value = 2),
          bosque_primario = data.frame(year = 2012, value = 5),
          bosque_secundario = data.frame(year = 2012, value = 10),
          arboles_dispersos = data.frame(year = 2012, value = 0),
          silvopastoriles = data.frame(year = 2012, value = 5)
        ),
        "01" = list(
          cercas_vivas = list(year = 2013, value = 10000 / 3.5) ## OJO NO HAY FACTOR DE 3.5
        ),
        "02" = list(
          arboles_dispersos = list(year = 2013, value = 10000 / 3.5) ## OJO NO HAY FACTOR DE 3.5
        )
      )
      inputs <- presets[[.GlobalEnv$.preset]]
    }

    string <- capture.output(str(c(l, inputs)))
    # string <- capture.output(str(inputs))

    #res <- results_old(inputs, departamento = departamento, municipio = municipio)
    res <- app_results(inputs,  departamento = departamento, municipio = municipio)
    string <- capture.output(str(res))

    out <- list(inputs = inputs, result = res)
    string <- capture.output(str(out))

    string %>% paste0(collapse = "\n")

  })




  output$buscador <- renderUI({
    dta_mun <- paste0(Hmisc::capitalize(tolower(data_mun$DEPARTAMEN)), ' - ', Hmisc::capitalize(tolower(data_mun$NOMBRE_ENT)))
    l_m <-  setNames(dta_mun, toupper(dta_mun))
    searchInput('name_mun', l_m, 'Búsqueda por municipio')
  })

  output$resultados <- renderUI({
    radioButtons('id_resultados', ' ', c('Captura y emisiones evitadas', 'Biodiversidad'), inline = T)
  })

  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(i) {
    output[[paste0('add_anio_', i)]] <- renderUI({
      div(class = 'add-anio',
          HTML('<div class = "text-btn"> Agregar año</div>'),
          actionButton(paste0('add_', i, '_1'), HTML('+')))
    })
  })



  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(z) {
    observe({
      if (is.null(click_i[[z]])) click_i[[z]] <- 1
      map(click_i[[z]], function(i) {
        output[[paste0('annios_', z, i)]]  <- renderUI({
          if(z == "cercas"){
            return(
              div(class = "anios-valor",
                  numericInput(paste0('anios', z, i), 'Año', value = NULL, min = 2000, max = 2020),
                  textInput(paste0('id_anios_',  z, i), 'Longitud', value = NULL, placeholder = 'Kilómetros')
              )
            )
          }
          div(class = "anios-valor",
                  numericInput(paste0('anios', z, i), 'Año', value = NULL, min = 2000, max = 2020),
              # textInput(paste0('anios', z, i), ' ', value = NULL, placeholder = 'Año'),
              textInput(paste0('id_anios_',  z, i), 'Área', value = NULL, placeholder = 'Hectáreas')
          )

        })
      })
    })
  })


  click_i <- reactiveValues(primario = 0, secundario = 0, potreros = 0, cercas = 0, pastoriles = 0)

  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(i) {
    observeEvent(input[[paste0('add_', i, '_1')]], {
      click_i[[i]] <- click_i[[i]] + 1
      insertUI(paste0('.mas-anios-', i), ui = uiOutput(paste0('annios_', i, click_i[[i]])), multiple = T)
    })
  })

  output$aver <- renderPrint({
    result()
  })


  result <- reactive({

    if(!is.null(.GlobalEnv$.preset)){
      lugar <- "Quindío - Montenegro"

    }else{
      lugar <- input$name_mun
      if (is.null(lugar)) return()
      if (lugar == "") return()
    }

    lugar <- str_split(lugar, ' - ') %>% unlist()
    departamento <- lugar[1]
    municipio <- lugar[2]
    region <- regiones_match(departamento = lugar[1], municipio = lugar[2])

    l <-  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(z) {
      n_years <- 0:click_i[[z]]
      annios <- c()
      hectareas <- c()
      map(n_years, function(i) {
        input_0 <- as.numeric(input[[paste0('anios', z, '0')]])
        annios <- as.numeric(input[[paste0('anios', z, i)]])
        if (sum(is.null(annios)) >= 1) annios <- input_0 + i
        hectareas <- as.numeric(input[[paste0("id_anios_", z, i)]])
        data.frame(year = annios, value = hectareas)
      })  %>% bind_rows()
    })
    names(l) <- availableTipoCobertura() #c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles')
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    inputs <- l

    if(!is.null(.GlobalEnv$.preset)){
      presets <- list(
        "00" = list(
          cercas_vivas = data.frame(year = 2012, value = 2),
          bosque_primario = data.frame(year = 2012, value = 5),
          bosque_secundario = data.frame(year = 2012, value = 10),
          arboles_dispersos = data.frame(year = 2012, value = 0),
          silvopastoriles = data.frame(year = 2012, value = 5)
        ),
        "01" = list(
          cercas_vivas = list(year = 2013, value = 10000 / 3.5) ## OJO NO HAY FACTOR DE 3.5
        ),
        "02" = list(
          arboles_dispersos = list(year = 2013, value = 10000 / 3.5) ## OJO NO HAY FACTOR DE 3.5
        )
      )
      inputs <- presets[[.GlobalEnv$.preset]]
    }

    str(inputs)
    #res <- results_old(inputs, departamento = departamento, municipio = municipio)
    res <- app_results(inputs,  departamento = departamento, municipio = municipio)
    str(res)
    res
  })

  # plot_bar <- reactive({
  # })

  output$viz_porcentaje <- renderHighchart({
    if (is.null(input$name_mun)) return()
    data <- result()$captura_general
    if (sum(data$carbono) == 0) return()
    data$carbono <- round(data$carbono, 2)
    data <- data %>%
      filter(Tiempo <= as.numeric(format(Sys.Date(), "%Y"))) %>%
      select(Suelo, carbono)

    viz_bar(data)
    #plot_bar()
  })

  output$total_aves <- renderUI({

    if (all(is.null(unlist(result()$pajaros)))) {
      tx <- div(style = "text-align:center;", tags$img(src = 'img/informacion.png', style = "transform: rotate(90deg);"),
                HTML('<div class = "subtitle-viz">No hay información disponible para esta ubicación</div>'))
    } else {
      tx <- div(
        HTML('<div style = "text-align:center;">
             <div class = "title-viz">RIQUEZA ESPERADA DE ESPECIES POR TIPO DE COBERTURA</div></div>'),
        HTML(gsub("NULL", "", paste0('<div style = "text-align:center;"><div class = "subtitle-viz">',
                                     result()$pajaros_text, collapse = "</div></div>"))),
        tags$img(style = "text-align: center; padding: 0px 20px;margin-top:20px;", src = "img/aves.png")
      )
    }

    tx

  })

  min_year <- reactive({
    if(is.null(result())) return()
    data <- result()$captura_general
    min(data$Tiempo)
  })

  output$warning_years <- renderUI({
    if(is.null(min_year())) return()
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    message("min year")
    str(min_year())
    min_year <- min_year()
    smaller_year <- current_year - 20
    if(min_year() >= smaller_year){
      return()
    }
    div(id="warning_years", HTML(warning_text))
  })


  output$vista_resultados <- renderUI({

    if (is.null(input$name_mun)) return()

    if (input$name_mun == "") return(HTML('<div class = "content-intro"><img style = "width:78px;" src = "img/placeholder.png">
                                          <div class = "text-intro">Llena los campos de información de tú predio</div></div>'))

    data <- result()$captura_general
    #str(min(data$Tiempo))

    if (sum(data$carbono) == 0)  return(HTML('<div class = "content-intro"><img style = "width:78px;" src = "img/placeholder.png">
                                             <div class = "text-intro">Llena los campos de información de tú predio</div></div>'))

    options(scipen = 9999)

    id_res <- input$id_resultados
    if (is.null(id_res)) return()

    if (id_res == 'Biodiversidad') {
      uiOutput('total_aves')
    } else {
      total_tco2e <- format(round(result()$carbono_capturado_presente), big.mark = ' ', small.mark = '.')
      co2_car <- format(round(co2_carros(sum(data$carbono))), big.mark = ' ', small.mark = '.')
      div(
        HTML(paste0('<div style = "text-align:center;">
                        <div class = "title-viz"></div>
                          <div class = "subtitle-viz">', total_tco2e," tCO<sub>2</sub>e
                            <br>
                          <div class = 'info-tool'>",co2_car,' carros
                        <div class="tooltip-inf">
                          <i class="fa fa-info-circle"></i>
                          <span class="tooltiptext">El cálculo se realiza según la distancia promedio recorrida en grandes ciudades
                            durante un año (12500 km), por un carro promedio de motor 1.5 litros.
                          </span>
                        </div>
                        </div>
                    </div>
                   ')),
        highchartOutput('viz_porcentaje', height = 450)
      )
    }

  })
  #
  # plot_lineas <- reactive({
  # })

  output$viz_lineas <- renderHighchart({
    # plot_lineas()
    if(is.null(result())) return()
    if(is.null(min_year())) return()
    if (is.null(input$name_mun)) return()
    data <- result()$captura_general
    type_p <- 'spline'
    if (input$id_lines) {
      data <- result()$captura_total
      type_p <- 'area'
    }

    if (sum(data$Estimacion) == 0) return()

    #data$carbono <- round(cumsum(data$carbono), 2)
    max_year <- min_year() + 20
    data <- data %>%
      select(Ano = Tiempo, Suelo, carbono = Estimacion) %>%
      filter(Ano <= max_year)

    viz_lines(data, type_plot = type_p)
  })
  # #
  output$slider_area <- renderUI({
    if (all(is.null(unlist(result()$pajaros)))) return()
    div(HTML('<div class = "title-sliders">Área bosque primario o secundario</div>'),
        sliderInput('id_area_primario', ' ', min = 0, max = 1000, value = 0, step = 10)
    )

  })

  output$slider_area_pastoriles <- renderUI({
    if (all(is.null(unlist(result()$pajaros)))) {
      div(style = "text-align:center;", tags$img(src = 'img/informacion.png', style = "transform: rotate(90deg);margin-top:30px;"),
          HTML('<div class = "subtitle-viz">No hay información disponible para esta ubicación</div>'))
    } else {
      div(HTML('<div class = "title-sliders" style="margin-top:15px;">Área Silvopastorales, cercas vivas <br/>o árboles dispersos</div>'),
          sliderInput('id_pastoriles', ' ', min = 0, max = 1000, value = 0, step = 10)
      )
    }
  })

  output$text_aves <- renderUI({
    # if (all(is.null(unlist(result()$pajaros)))) {
    #   txt <- HTML("No hay información disponible para este municipio")
    # } else {
    area_bosque <- input$id_area_primario
    inputs <- result()$inputs
    area_bosque <- area_bosque + sum(inputs$bosque_primario$value, na.rm = TRUE) + sum(inputs$bosque_secundario$value, na.rm = TRUE)
    if (is.null(area_bosque)) return()
    region <- result()$region
    aves_bosques <- round(biodiv_area2(area = area_bosque, region = region, tipo_cobertura = 'bosque_secundario'))
    txt <- HTML(paste0('<p class = "result-slider">Por cada  <span style="color: #2e4856;font-size: 18px;">', input$id_area_primario,
                       ' hectáreas </span> de más en bosques primarios o secundarios se podrían conservar
                       <span style="color: #2e4856;font-size: 18px;">', aves_bosques, ' aves</span>.</p>' ))

    txt
  })

  output$text_aves_pastoriles <- renderUI({

    area_otras <- input$id_pastoriles
    inputs <- result()$inputs
    area_otras <- area_otras + sum(inputs$arboles_dispersos$value, na.rm = TRUE) +
      sum(inputs$cercas_vivas$value, na.rm = TRUE) + sum(inputs$silvopastoriles$value, na.rm = TRUE)
    if (is.null(area_otras)) return()
    region <- result()$region
    aves_otras <- round(biodiv_area2(area = area_otras, region = region, tipo_cobertura = 'silvopastoriles'))
    txt <- HTML(paste0('<p class = "result-slider">
                Por cada  <span style="color: #2e4856;font-size: 18px;">', input$id_pastoriles, ' hectáreas</span>
                       de más en suelos silvopastorales, cercas vivas ó árboles dispersos se podrían conservar
                       <span style="color: #2e4856;font-size: 18px;">', aves_otras, ' aves</span>.</p>' ))

    txt
  })

  output$vista_avanzados <- renderUI({

    if (is.null(input$name_mun)) return()
    if (input$name_mun == "") return(HTML('<div class = "content-intro" style = "margin-top:45px;">
                                          <img style = "width:78px;" src = "img/placeholder.png">
                                          <div class = "text-intro">Llena los campos de <br/> información de tú predio</div>
                                          </div>'))

    data <- result()$captura_general

    if (sum(data$carbono) == 0)  return(HTML('<div class = "content-intro" style = "margin-top:45px;">
                                             <img style = "width:78px;" src = "img/placeholder.png">
                                             <div class = "text-intro">Llena los campos de <br/> información de tú predio</div>
                                             </div>'))

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
      car_tot <- format(round(result()$carbono_capturado_total), big.mark = ' ', small.mark = '.')

      div(
        HTML(paste0('<div style = "text-align:center">
                    <div class = "title-viz">PROYECCIÓN DE CAPTURA Y EMISIONES EVITADAS DE CO<sub>2</sub> (A 20 AÑOS)</div>
                    <div class = "subtitle-viz">', car_tot, ' tCO<sub>2</sub>e</div>
                    </div>')),
        highchartOutput('viz_lineas', height = 450),
        checkboxInput('id_lines', 'Ver resultados por total', value = FALSE)
      )
    }

  })


}

onStop(function() {
  cat("Doing application cleanup\n")
  .GlobalEnv$.preset <- NULL
  .GlobalEnv$.debug <- FALSE
  rm(list=c(.debug, .preset), envir=.GlobalEnv)
})

shinyApp(ui, server)

