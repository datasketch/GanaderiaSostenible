text_info <- function() {
tx <- div(
h2("Metodología"),
p("Si quieres conocer la forma en que se recogieron los datos, las fórmulas utilizadas para los cálculos
            y las fuentes bibliográficas descarga nuestra metodología"),
actionButton("download_methodology", "Descargar"),
hr(),
h2("Glosario"),

div(style = "display:flex;",
    img(src="img/captura_carbono.png", width = 100, height = 100, style="margin: 10px 20px 10px 10px;"),
    div(
        div(class = "titlte-info", "CAPTURA DE CARBONO O CO2"),
        p(class = "paraph-info", "Algunos organismos como las plantas, por medio de la
                              fotosíntesis, capturan el carbono que se encuentra en
                              la atmósfera y así ayudan a reducir la concentración de
                              gases de efecto invernadero que forman parte de las
                              mayores causas del calentamiento global.")
    )
),
div(style = "display:flex;",
    img(src="img/arboles_dispersos.png", width = 100, height = 100, style="margin: 10px 20px 10px 10px;"),
    div(
        div(class = "titlte-info","ÁRBOLES DISPERSOS"),
    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit.
                             Suspendisse tempor nisl eget arcu euismod pulvinar. Ut eleifend euismod sapien
                             eget laoreet. Morbi consectetur enim ut mi sodales.")
    )
),
div(style = "display:flex;",
    img(src="img/bosque_primario.png", width = 100, height = 100, style="margin: 10px 20px 10px 10px;"),
    div(
        div(class = "titlte-info","BOSQUE PRIMARIO"),
    p("Áreas de bosque maduro que no han sido intervenidas
                              por los humanos en los últimos 30 años y cuyo uso es
                              de conservación estricta. Actividades como la
                              extracción de madera, la cacería y las prácticas
                              agropecuarias están restringidas en estas áreas.")
    )
),
div(style = "display:flex;",
    img(src="img/bosque_secundario.png", width = 100, height = 100, style="margin: 10px 20px 10px 10px;"),
    div(
        div(class = "titlte-info","BOSQUE SECUNDARIO"),
    p("Áreas de bosque en proceso de regeneración natural
                              en respuesta a daños causados por actividades del ser
                              humano. Su uso es de conservación estricta y
                              actividades como la extracción de madera, la cacería y
                              las prácticas agropecuarias están restringidas en estas
                              áreas.")
    )
),
div(style = "display:flex;",
    img(src="img/cercas_vivas.png", width = 100, height = 100, style="margin: 10px 20px 10px 10px;"),
    div(
        div(class = "titlte-info","CERCAS VIVAS"),
    p("Áreas de bosque maduro que no han sido intervenidas
                              por los humanos en los últimos 30 años y cuyo uso es
                              de conservación estricta. Actividades como la
                              extracción de madera, la cacería y las prácticas
                              agropecuarias están restringidas en estas áreas.")
    )
),
div(style = "display:flex;",
    img(src="img/sistemas_silvopastoriles.png", width = 100, height = 100, style="margin: 10px 20px 10px 10px;"),
    div(
        div(class = "titlte-info","SISTEMAS SILVOPASTORILES"),
    p("Áreas de bosque maduro que no han sido intervenidas
                              por los humanos en los últimos 30 años y cuyo uso es
                              de conservación estricta. Actividades como la
                              extracción de madera, la cacería y las prácticas
                              agropecuarias están restringidas en estas áreas.")
    )
)
)
 tx 
}


text_modal <- function(){
    
    div(class = "contenido-modal",
        div(class = "par-modal",
        img(class="topbar__img", src="img/logo_GCS.png"),
            p( style = "margin-left:15px;font-size:17px;", "Esta es la herramienta de estimación de biodiversidad y captura de carbono del 
            proyecto Ganadería Colombiana Sostenible. Esta aplicación te permitirá calcular la
            cantidad de carbono atmosférico que estás contribuyendo a reducir al implementar
            sistemas silvopastoriles en tus predios. También podrás estimar el número de especies
            de aves y escarabajos que estás ayudando a preservar y hacer proyecciones futuras
             sobre el uso de tus suelos.")
        ),
        div(class = "par-modal",
        div( style = "margin-top: 15px;",
            div(style = "color:#2e4856;font-size:21px;font-weight: 700;","¿POR QUÉ ES IMPORTANTE?"),
            p(style = "margin-right:15px;font-size:17px;","
              Los sistemas tradicionales ganaderos han sido comúnmente asociados con 
              altas emisiones de gases de efecto invernadero (como el dióxido de carbono
              y el metano), que son parte de las mayores causas del calentamiento global.
              Por medio de la fotosíntesis, los árboles y arbustos ayudan a reducir una
              parte del carbono atmosférico al almacenarlo en sus organismos. Es por esto que
              el proyecto Ganadería Colombiana Sostenible busca promover, en
              fincas ganaderas colombianas, la adopción de sistemas silvopastoriles
              cercas vivas y árboles dispersos en potreros, así como conservar áreas
              de bosques primarios y secundarios.")
        ),
        img(style = "width: 150px;", src="img/bosque_primario.png")
        )
        )
}
