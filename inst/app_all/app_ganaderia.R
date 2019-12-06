library(dsAppLayout)
library(tidyverse)
library(highcharter)
library(dsCustom)

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

#add_primario_3,#add_secundario_3,#add_potreros_3,#add_cercas_3,#add_pastoriles_3 {
    border-radius: 50%;
    background-color: #8096a3;
    border: 1px solid #8096a3;
    width: 20px;
    cursor: pointer;
    height: 20px;
    padding: 0 !important;
    text-align: center;
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

.width-annio-uno {
width: 110px !important;
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
}

.box-collapsible-content {
    overflow: auto !important;
}

.margin-panel-info {
  margin: 0px 10px;
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
                     title = "INFORMACIÓN DEL PREDIO", color = "olive", collapsed = F, width = 400, id_body = 'remove-padding',
                     body = (
                       div(
                         div(class = 'margin-panel-info',
                           div(class = 'title-filters', 'UBICACIÓN'),
                           uiOutput('deptos'),
                           uiOutput('buscador'),
                           div(class = 'title-filters margin-button-null', 'RESULTADOS'
                           ),
                           uiOutput('resultados')
                         ),
                  box(title = div(class = 'title-filters', 'BOSQUE PRIMARIO'), collapsed = 'F', 
                      body =     div(class = 'panel-primario',
                                     div(class = 'width-annio-uno',
                                         uiOutput('anio_inicial_primario')),
                                     div(class = 'anios-terrenos',
                                         div(class="mas-anios-primario",
                                             uiOutput('annios_primario0'),
                                             uiOutput('annios_primario1'),
                                             uiOutput('annios_primario2')
                                         ),
                                         uiOutput('add_anio_primario'))
                      )),
                  box(title = div(class = 'title-filters', 'BOSQUE SECUNDARIO'), collapsed = 'F', 
                      body =     div(class = 'panel-secundario',
                                     div(class = 'width-annio-uno',
                                         uiOutput('anio_inicial_secundario')),
                                     div(class = 'anios-terrenos',
                                         div(class="mas-anios-secundario",
                                             uiOutput('annios_secundario0'),
                                             uiOutput('annios_secundario1'),
                                             uiOutput('annios_secundario2')
                                         ),
                                         uiOutput('add_anio_secundario'))
                      )),
                  box(title = div(class = 'title-filters', 'ÁRBOLES DISPERSOS EN POTREROS'), collapsed = 'F', 
                      body =     div(class = 'panel-potreros',
                                     div(class = 'width-annio-uno',
                                         uiOutput('anio_inicial_potreros')),
                                     div(class = 'anios-terrenos',
                                         div(class="mas-anios-potreros",
                                             uiOutput('annios_potreros0'),
                                             uiOutput('annios_potreros1'),
                                             uiOutput('annios_potreros2')
                                         ),
                                         uiOutput('add_anio_potreros'))
                      )),
                  box(title = div(class = 'title-filters', 'CERCAS VIVAS'), collapsed = 'F', 
                      body =     div(class = 'panel-cercas',
                                     div(class = 'width-annio-uno',
                                         uiOutput('anio_inicial_cercas')),
                                     div(class = 'anios-terrenos',
                                         div(class="mas-anios-cercas",
                                             uiOutput('annios_cercas0'),
                                             uiOutput('annios_cercas1'),
                                             uiOutput('annios_cercas2')
                                         ),
                                         uiOutput('add_anio_cercas'))
                      )),
                  box(title = div(class = 'title-filters', 'SISTEMAS SILVOPASTORILES INTENSIVOS'), collapsed = 'F', 
                      body =     div(class = 'panel-pastoriles',
                                     div(class = 'width-annio-uno',
                                         uiOutput('anio_inicial_pastoriles')),
                                     div(class = 'anios-terrenos',
                                         div(class="mas-anios-pastoriles",
                                             uiOutput('annios_pastoriles0'),
                                             uiOutput('annios_pastoriles1'),
                                             uiOutput('annios_pastoriles2')
                                         ),
                                         uiOutput('add_anio_pastoriles'))
                      ))
                     )
                     )
                  ),
                   panel(
                     title = "RESULTADOS", color = "olive", collapsed = F, width = 450,
                     body = 'hola'
                   ),
                   panel(
                     title = "RESULTADOS AVANZADOS", color = "olive", collapsed = F,  
                     body = 'hola'
                   )
                   
)

server <- function(input, output, session) {
  
  # panel de filtros
  
  output$deptos <- renderUI({
    dta_dep <- unique(data_mun$DEPARTAMEN)
    l_d <- setNames(dta_dep, Hmisc::capitalize(tolower(dta_dep)))
    
    selectizeInput('name_depto',  ' ', l_d, selected = NULL, multiple = T, options = list(
      placeholder = "Todos los departamentos",
      plugins= list('remove_button'),
      maxItems = 1)
    )
  })
  
  output$buscador <- renderUI({
    
    dep_sel <- input$name_depto
    
    if (is.null(dep_sel)) {
      d_m <- data_mun
    } else {
      d_m <- data_mun %>% 
        filter(DEPARTAMEN %in% dep_sel)
    }
    dta_mun <- d_m$NOMBRE_ENT
    l_m <-  setNames(Hmisc::capitalize(tolower(dta_mun)), dta_mun)
    searchInput('name_mun', l_m, 'Búsqueda por municipio')
  })
  
  output$resultados <- renderUI({
    radioButtons('id_resultados', ' ', c('Biodiversidad', 'Captura de carbono'), inline = T)
  })
  
  
  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(z) {
    output[[paste0('anio_inicial_', z)]] <- renderUI({
      selectizeInput(paste0('id_primario_anio', z), ' ', 1990:2020, selected = NULL, multiple = T,  options = list(
        placeholder = "Año inicial",
        plugins= list('remove_button'),
        maxItems = 1)
      )
    })
  })
  
  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(z) {
    map(0:2, function(i) {
      output[[paste0('annios_', z, i)]] <- renderUI({
        textInput(paste0('id_anios_', z, i), paste0('Año ', i), 'dadas')
      })
    })
  })
  
  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(i) {
    output[[paste0('add_anio_', i)]] <- renderUI({
      div(class = 'add-anio',
          'Agregar año', 
          actionButton(paste0('add_', i, '_3'), HTML('<div class = "text-btn"> + </div>')))
    })
  })
  
  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(z) {
    observe({
      if (is.null(click_i[[z]])) click_i[[z]] <- 2
      map(click_i[[z]], function(i) {
        output[[paste0('annios_', z, i)]] <- renderUI({
          textInput(paste0('id_anios_', z, i), paste0('Año ', i), 'dadas')
        })
      })
    })
  })
  click_i <- reactiveValues(primario = 2, secundario = 2)
  map(c('primario', 'secundario', 'potreros', 'cercas', 'pastoriles'), function(i) {
    observeEvent(input[[paste0('add_', i, '_3')]], {
      click_i[[i]] <- click_i[[i]] + 1
      insertUI(paste0('.mas-anios-', i), ui = uiOutput(paste0('annios_', i, click_i[[i]])), multiple = T)
    })  
  })  
  
  
  
  
  
}

shinyApp(ui, server)

