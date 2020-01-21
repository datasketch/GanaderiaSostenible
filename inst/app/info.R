text_info <- function() {
  tx <- div(
    h2("Guía de usuario"),
    p("Guía de usuario"),
    p("Para estimar la captura actual y proyectada de su predio siga los siguientes pasos:"),
    tags$ol(
      tags$li("Seleccione el municipio en el que se encuentra su predio."),
      tags$li("Ingrese las áreas de las conversiones de pasturas a otras coberturas en su predio
                y el año en el que se implementaron o se planean implementar. En el caso de conversiones
                que suceden en varios años, discrimine el área por año utilizando el botón “Agregar año”.
              Advertencia: ingrese únicamente las conversiones que se hayan realizado durante los últimos 20 años."),
      tags$li("En la medida que ingrese la información el panel de resultados se actualizará presentando la
              estimación de carbono y de riqueza de aves a tiempo presente por tipo de cobertura."),
      tags$li("El panel de resultados avanzados desplegará la captura de carbono a través del tiempo
              (total y discriminada por tipo de cobertura)."),
      tags$li("Para biodiversidad, el panel de resultados avanzados desplegará un control deslizante
              a través del cual podrá estimar el incremento de aves con la implementación de sistemas
              silvopastoriles y la protección de bosques.")
    ),
    h2("Metodología"),
    p("Si quieres conocer la forma en que se recogieron los datos, las fórmulas utilizadas para los cálculos
            y las fuentes bibliográficas descarga nuestra metodología"),
    tags$a(href = "https://datasketch.github.io/landing-gcs/paper.pdf", tags$button(class = "text-btn", "Descargar")),
    hr(),
    h2("Glosario"),

    div(style = "display:flex;",
        img(src="img/arboles_dispersos.png", width = 100, height = 100, style="margin: 10px 20px 10px 10px;"),
        div(
          div(class = "titlte-info","ÁRBOLES DISPERSOS"),
          p("Son áreas de pastos naturales o mejorados que tienen árboles dispersos aleatoriamente (cuando son producto de la
          regeneración natural) o sistemáticamente (cuando han sido plantados). En este tipo de cobertura hay menos de 25
          árboles por hectárea. Los arboles dispersos funcionan para las aves como el equivalente a una “piedra de salto”
          que les permite percharse y descansar mientras se mueven de un sitio a otro.")
        )
    ),
    div(style = "display:flex;",
        img(src="img/bosque_primario.png", width = 100, height = 100, style="margin: 10px 20px 10px 10px;"),
        div(
          div(class = "titlte-info","BOSQUE PRIMARIO"),
          p("Son áreas de bosque maduro que no han sido intervenidas por los humanos en los últimos 30 años y cuyo uso es de conservación
      estricta. Actividades como la extracción de madera, la cacería y las prácticas agropecuarias están restringidas en estas áreas.
      Los bosques conservados albergan una alta diversidad de aves.")
        )
    ),
    div(style = "display:flex;",
        img(src="img/bosque_secundario.png", width = 100, height = 100, style="margin: 10px 20px 10px 10px;"),
        div(
          div(class = "titlte-info","BOSQUE SECUNDARIO"),
          p("Son áreas de bosque en proceso de regeneración natural en respuesta a daños causados por actividades del ser humano.
    Su uso es de conservación estricta y actividades como la extracción de madera, la cacería y las prácticas agropecuarias están
    restringidas en estas áreas. Los bosques conservados albergan una alta diversidad de aves.")
        )
    ),
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
        img(src="img/cercas_vivas.png", width = 100, height = 100, style="margin: 10px 20px 10px 10px;"),
        div(
          div(class = "titlte-info","CERCAS VIVAS"),
          p("Son límites prediales conformados por árboles, arbustos o palmas dispuestos de manera lineal que reemplazan los
      postes de madera muerta o cemento y que contribuyen a reducir la erosión del suelo, a incrementar la diversidad biológica
      y a mejorar el ciclo de nutrientes y las reservas de carbono. Las cercas vivas funcionan para las aves como elemento
      conector o corredor entre diferentes lugares.")
        )
    ),
    div(style = "display:flex;",
        img(src="img/sistemas_silvopastoriles.png", width = 100, height = 100, style="margin: 10px 20px 10px 10px;"),
        div(
          div(class = "titlte-info","SISTEMAS SILVOPASTORILES"),
          p("Son áreas que combinan especies de plantas forrajeras como pastos y leguminosas con arbustos y árboles en alta densidad.
      Mientras las plantas forrajeras proveen de alimento al ganado, los árboles y arbustos ayudan a reducir la erosión del suelo,
      a incrementar la diversidad biológica y a mejorar las reservas de carbono.")
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
