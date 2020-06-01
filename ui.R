

# run loadStataData.Rmd
# run polarPlot.Rmd
#source("global.R")

ui <- navbarPage(
  theme = shinytheme("simplex"),
  title = img(src = "flag.png", width = 70),
  # header = tags$head(includeHTML((
  #   "google-analytics.html"
  # ))),
  windowTitle = "COVID-19 Mexico",
  # ++++++++++++++++++++++
  # TAB 1 : HOME PAGE ----
  # ++++++++++++++++++++++
  
  tabPanel(
    h4("Inicio"),
    fluidRow(h1(
      strong("OBSERVATORIO PARA LA CONTENCIÓN DEL COVID-19 EN AMÉRICA LATINA "),
      align = "center",
      style = "color: black"
    )),
    br(),
    br(),
    br(),
    fluidRow(column(
      width = 12,
      align = "center",
      br(),
      p(
        "Un observatorio de políticas públicas y datos de salud por estado para México, elaborado por por el Instituto de Estudios Avanzados de las Américas y el Departamento de Ciencias de la Salud Pública de la Facultad de Medicina Miller de la Universidad de Miami.",
        style = "font-size: 24px"
      )
    )),
    fluidRow(
      column(
        width = 12,
        align = "center",
        img(src = "um.png"),
        br(),
        br(),
        tags$style(".img {
                            margin-left:35px;
                            margin-right:35px;
                          }"),
        div(
          img(src = "mia.png", width = 400, class = "img"),
          img(
            src = "Com_Logo.png",
            width = 400,
            height = 140,
            class = "img"
          ),
          img(
            src = "arts.png",
            width = 375,
            height = 170,
            class = "img"
          ),
          
          img(
            src = "dphs.png",
            width = 400,
            height = 140,
            class = "img"
          )
          
        ),
        br(),
        h4(strong("en colaboración con:")),
        br(),
        fluidRow(
          column(width = 1, offset = 2),
          column(
            width = 2,
            align = "center",
            fluidRow(img(src = "UNAM.png", width = 200)),
            br(),
            br(),
            fluidRow(img(src = "cicsaanahuac.png", width = 200)),
            br(),
            br(),
            fluidRow(img(src = "cide.png", width = 150))
          ),
          column(width = 1, offset = 1),
          column(
            width = 2,
            align = "center",
            fluidRow(img(src = "mexicosocial.png", width = 200)),
            br(),
            br(),
            fluidRow(img(src = "smsp.png", width = 200)),
            br(),
            br(),
            fluidRow(img(src = "Tufts_univ.png", width = 200)),
            br(),
            br(),
            fluidRow(img(src = "LOGO-TAP.png", width = 200))
          )
        )
      )
    ),
    fluidRow(),
    fluidRow(),
    fluidRow(),
    br(),
    br(),
    br(),
    fluidRow(
      h6(
        "Esto se desarrolla como una aplicación de código abierto y se
           publica bajo la licencia MIT",
        align = "center",
        style = "color: black"
      )
    )
    
  ),
  # +++++++++++++++++++++++++
  # TAB 2 : SUMMARY PAGE ----
  # +++++++++++++++++++++++++
  tabPanel(
    h4("Resumen"),
    fluidRow(h1(
      strong("Resumen"), align = "center", style = "color: black"
    )),
    h4(
      "Estos datos se actualizaron por última vez el 15 de mayo de 2020.",
      align = "center"
    ),
    fluidRow(h4(
      strong(
        "Haga doble clic en el nombre de un estado en la leyenda para mostrarlo solo. Haga doble clic nuevamente para mostrar toda la información."
      ),
      align = "center",
      style = "color: red"
    )),
    fluidRow(h4(
      strong(
        "Haga clic en el nombre de un estado en la leyenda para ocultar o mostrar un estado."
      ),
      align = "center",
      style = "color: red"
    )),
    br(),
    br(),
    fluidRow(column(
      offset = 1,
      width = 10,
      tabBox(
        id = "index",
        width = 12,
        title = h3(
          "Índice de política pública ajustado por tiempo",
          align = "center"
        ),
        height = "500px",
        tabPanel("Gráfico", width = 12,
                 plotlyOutput("summaryIndexPlot")) #,
        #tabPanel("Tabla de datos", width = 12,
        #         dataTableOutput("index_table"))
      )
    )),
    
    
    fluidRow(column(
      offset = 1,
      width = 10,
      tabBox(
        id = "index",
        width = 12,
        title = h3(
          "Movilidad",
          align = "center"
        ),
        height = "500px",
        tabPanel("Gráfico", width = 12,
                 plotlyOutput("summaryMobilityPlot")) #,
        #tabPanel("Tabla de datos", width = 12,
        #         dataTableOutput("index_table"))
      )
    )),
    
    
    
    fluidRow(column(
      offset = 1,
      width = 10,
      tabBox(
        id = "deaths",
        width = 12,
        title = h3("Muertes per cápita", align = "center"),
        height = "500px",
        tabPanel("Gráfico",
                 plotlyOutput("summaryDeathsPerCapitaPlot")) #,
        #tabPanel("Tabla de datos",
        #dataTableOutput("death_table"))
      )
    )),
    br(),
    br(),
    br(),
    h4(
      strong(
        "Las siguientes gráficas muestran la adopción de los siete tipos de medidas preventivas considerados en el índice de política pública, por entidad federativa. Las líneas cortas representan adopción parcial de medidas en esta área. Las líneas largas representan adopción total."
      ),
      align = "center",
      style = "color: red"
    ),
    
    fluidRow(column(
      width = 12,
      align = "center",
      sliderInput(
        "spider",
        h4(strong(tags$i("elige un día:"))),
        min = 1,
        max = latest,
        value = latest,
        step = 1,
        animate =
          animationOptions(interval = 300, loop = FALSE)
      )
    )),
    
    fluidRow(column(
      width = 12,
      imageOutput("spiderPlot"),
      align = "center"
    )),
    
    h4(
      "Estos datos se actualizaron por última vez el 15 de mayo de 2020.",
      align = "center"
    )
    
    
    
  ),
  # ++++++++++++++++++++++++++++++
  # TAB 3 : SINGLE STATE PAGE ----
  # ++++++++++++++++++++++++++++++
  tabPanel(
    h4("Por estado"),
    fluidRow(h1(
      strong("Resumen por estado "),
      align = "center",
      style = "color: black"
    )),
    h4(
      "Estos datos se actualizaron por última vez el 15 de mayo de 2020.",
      align = "center"
    ),
    fluidRow(h4(
      strong(
        "Las líneas grises muestran el valor mínimo y máximo que se obtuvo en todos los estados por día."
      ),
      align = "center",
      style = "color: red"
    )),
    br(),
    fluidRow(column(
      width = 3,
      offset = 1,
      align = "left",
      selectInput(
        inputId = "refPlace",
        label = "Seleccione un estado:",
        choices = stateNames,
        selected = 1
      )
    )),
    br(),
    br(),
    fluidRow(
      column(
        offset = 1,
        width = 10,
        plotlyOutput("indexAdjTimePlot")
      )),    
    fluidRow(
      column(
        offset = 1,
        width = 10,
        plotlyOutput("mobilityPlot")
      ))
    
    
  ),
  # ++++++++++++++++++++++
  # TAB 4 : PLOTS     ----
  # ++++++++++++++++++++++
  # tabPanel(
  #   h4("Estado múltiple"),
  #   column(width = 6,
  #          fluidRow(
  #            selectInput(inputId = "state1",
  #                        label = "Select a State:",
  #                        choices = unique(mexico$`State Name`),
  #                        selected = "Chiapas")
  #          ),
  #          fluidRow(
  #            checkboxGroupInput(inputId = "vars1",
  #                               label = "Select Measure(s)",
  #                               choices = c("Hospital Beds" = "Hospital_Beds",
  #                                           "Hospital Beds/Capita" = "Hospital_Beds_per_capita",
  #                                           "ICU Beds" = "ICU_Beds",
  #                                           "ICU Beds/Capita" = "ICUBeds_per_capita"),
  #                               selected = "Hospital_Beds")
  #          ),
  #          fluidRow(
  #            plotOutput("plot1")
  #          )
  #   ),
  #   column(width = 6,
  #          fluidRow(
  #            selectInput(inputId = "state2",
  #                        label = "Select a State:",
  #                        choices = unique(mexico$`State Name`),
  #                        selected = "Veracruz")
  #          ),
  #          fluidRow(
  #            checkboxGroupInput(inputId = "vars2",
  #                               label = "Select Measure(s)",
  #                               choices = c("Hospital Beds" = "Hospital_Beds",
  #                                           "Hospital Beds/Capita" = "Hospital_Beds_per_capita",
  #                                           "ICU Beds" = "ICU_Beds",
  #                                           "ICU Beds/Capita" = "ICUBeds_per_capita"),
  #                               selected = "Hospital_Beds")
  #          ),
  #          fluidRow(
  #            plotOutput("plot2")
  #          ),
  #          fluidRow(
  #            plotlyOutput("refplot")
  #          )
  #   )
  # ),
  
  # ++++++++++++++++++++++
  # TAB 5 : MAP       ----
  # ++++++++++++++++++++++
  
  tabPanel(
    h4("Mapa"),
    h2("Mapa del índice de políticas ajustado por tiempo"),
    br(),
    # radioButtons(inputId = "week", label = "Seleccione una semana:",
    #              choices = c("2020-03-09" = "2020-03-09",
    #                          "2020-03-16" = "2020-03-16",
    #                          "2020-03-23" = "2020-03-23",
    #                          "2020-03-30" = "2020-03-30",
    #                          "2020-04-06" = "2020-04-06",
    #                          "2020-04-13" = "2020-04-13",
    #                          "2020-04-20" = "2020-04-20",
    #                          "2020-04-27" = "2020-04-27",
    #                          "2020-05-01" = "2020-05-01"),
    #              selected = "2020-04-27", inline = TRUE),
    column(
      width = 12,
      align = "center",
      sliderInput(
        inputId = "week",
        label = "Seleccione una semana:",
        min = lubridate::ymd("2020-03-09"),
        max = lubridate::ymd("2020-05-15"),
        value = lubridate::ymd("2020-05-15"),
        step = NULL,
        ticks = TRUE,
        width = '1000px',
        sep = ",",
        dragRange = TRUE
      ),
      br(),
      leafletOutput("map", height = 900)
    )
  ),
  
  # ++++++++++++++++++++++
  # TAB 6 : METHODS   ----
  # ++++++++++++++++++++++
  tabPanel(
    h4("Metodología"),
    br(),
    h3(
      strong("Observatorio Estatal para la Contención del COVID-19"),
      align = "center"
    ),
    br(),
    br(),
    column(
      width = 10,
      offset = 1,
      align = "left",
      p(
        'El Observatorio Estatal para la Contención del COVID-19 (ObsContenCOVID) 
        es una plataforma que presenta información sistemática y actualizada día 
        con día sobre las medidas que los gobiernos estatales en México han adoptado 
        para fomentar el distanciamiento social, reducir la movilidad e informar a 
        la población sobre la enfermedad. La herramienta incluye también información 
        sobre el grado en que estas medidas han sido observadas por los ciudadanos, 
        con base en el movimiento registrado entre la población.',
        style = "font-size:150%"
      ),
      p(
        'La primera aportación de este Observatorio es el cálculo de un índice que toma como base en',
        tags$i('EL Oxford COVID-19 Government Response Tracker'),
        '(OxCGRT),
una herramienta desarrollada por la Universidad Oxford que consiste en un listado de medidas al alcance de los gobiernos para disminuir la
velocidad de transmisión del virus.  Utilizamos el OxCGRT para identificar las siete 
        medidas preventivas relevantes a la situación de México: el cierre de escuelas; 
        la suspensión de actividades laborales presenciales; la cancelación de eventos públicos; 
        la suspensión del transporte público; el desarrollo de campañas informativas; la restricción 
        de viajes y viajeros dentro del estado; y el control de viajes y viajeros internacionales. 
        OxCGRT califica estas medidas en una forma ordinal y presenta una suma simple de ellas a 
        través del tiempo, generando así un índice. ',
        style = "font-size:150%"
      ),
      p('ObsContenCOVID parte del marco OxCGRT y codifica la respuesta de los gobiernos estatales en México, 
        con base en la información disponible en las páginas oficiales de cada una de las entidades 
        federativas (Ver Anexo metodológico). Para cada estado, cada una de las siete políticas públicas 
        mencionadas es medida en forma diaria, a partir de la fecha de aparición del primer caso oficialmente 
        reconocido en el país (el 27 de febrero). La variables incluidas son: i) Cierre de escuelas, 
        ii) Suspensión en área laboral, iii) Cancelación eventos públicos, iv) Suspensión transporte público, 
        v) Desarrollo de Campañas informativas, vi) Restricción de viajes dentro del estado, y 
        vii) Control de viajes internacionales. De manera similar a la metodología de OxCGRT, 
        cada medida preventiva es calificada con una escala ordinal entre 0 y 1, en donde “0” implica 
        que no existe registro de la aplicación de dicha medida en el estado, “0.5” registro parcial y/o no 
        obligatoria y “1” que dicha política se implementó de manera total y obligatoria en la entidad. 
        Cabe señalar que cada dato pasa por tres revisiones independientes, antes de ser agregado a la 
        base de datos. De tal manera, cada estado recibe una calificación entre 0 y 7, que resulta de la 
        suma de las distintas dimensiones y se actualiza diariamente, conforme cambian las medidas adoptadas.',
        style = "font-size:150%"
      ),
      
      p('Como segundo paso, ObsContenCOVID retoma el índice agregado para cada estado y lo pondera con base 
        en la fecha de entrada en vigor de cada una de las medidas en la entidad respectiva, nuevamente tomando 
        como base el 27 de febrero, fecha del primer caso registrado. Se presentan dos formas de ponderar a 
        través del tiempo: una que aplica un peso uniforme por día, y otra que da un mayor peso a los días 
        más cercanos al 27 de febrero. Esta estrategia permite ajustar el índice estatal para que refleje 
        no sólo la cantidad y rigor de las medidas adoptadas, sino su adopción a tiempo. Uno de los hallazgos 
        importantes de este ejercicio es que el ritmo de implementación de medidas preventivas ha variado de 
        manera considerable en el país, lo cual puede tener implicaciones importantes en el ritmo e intensidad 
        de propagación de la enfermedad.',
        style = "font-size:150%"
      ),
      p('Finalmente, como tercer y último paso el índice compuesto es ponderado y ajustado con base en el 
        éxito de las medidas en la contención de la movilidad poblacional en la entidad federativa utilizando 
        información de Retargetly Meta-data Movilidad México GPS Mobile App sobre la reducción efectiva de la 
        movilidad en cada uno de los estados, relativo a la movilidad convencional observada tres semanas previo 
        al inicio de la contingencia. Esta ponderación es crucial para capturar la eficacia de las políticas públicas.',
        style = "font-size:150%"
      ),
      p('En resumen, el índice estatal del ObcContenCOVID es un retrato diario, para todas y cada una de las entidades 
        federativas, de: 1) la amplitud de las medidas adoptadas por los gobiernos estatales conforme a recomendaciones 
        internacionales; 2) la oportunidad en su adopción; y 3) la respuesta poblacional, en términos de reducción de 
        la movilidad. Es basado en este índice, que toma valores de 0 a 100 y puede ser interpretado como un porcentaje, 
        que se hace un ranqueo de las entidades federativas.',
        style = "font-size:150%"
      ),
      p('Con ello ofrecemos una herramienta valiosa para revisar no sólo lo hecho hasta aquí, sino 
        también lo que se está haciendo ahora mismo, en este momento crítico, para la contención de la 
        epidemia. Nuestro objetivo al ponerlo al alcance del público y sobre todo de las autoridades estatales 
        es proveer información oportuna, que contribuya a tomar mejores decisiones hacia el futuro.',
        style = "font-size:150%"
      ),
      br(),
      br(),
      h4(strong("Limitaciones"), align = "center"),
      p(
        'El ejercicio que aquí se presenta y el ObsContenCOVID en general, tiene limitaciones y no debe tomarse como un juicio
      último o de valor sobre la actuación de los gobiernos, ni como una predicción de
      los patrones geográficos de la enfermedad. ',
        style = "font-size:120%"
      ),
      p(
        'A continuación compartimos algunos puntos a tomar en consideración:',
        style = "font-size:120%"
      ),
      p(
        tags$ul(
          style = "font-size:120%",
          tags$li(
            'Los siete tipos de medidas preventivas consideradas tiene el mismo peso en la construcción del índice.
                       Se podría argumentar que no todas las medidas de política pública tienen la misma eficacia y por lo tanto,
                       deberían de tener distinto peso en la escala de la Universidad Oxford.  Sin embargo, la evidencia sobre qué tipo
                       de intervención funciona mejor para abatir el COVID-19 es limitada en este momento.  Por esta razón,
                       se realiza una agregación simple de intervenciones que nos permite capturar, en un solo índice, la
                       amplitud de las disposiciones adoptadas en las distintas entidades como parte de la política pública de
                       prevención del contagio, sin aplicar un juicio sobre la eficacia potencial de cada uno.'
          ),
          tags$li(
            'El índice clasifica en forma ordinal, y por ende aproximada, la eficacia de cada una de las siete
                       acciones para promover el distanciamiento físico. Por el momento, no es factible tener una medición
                       más precisa de cada una de ellas, como el porcentaje de cobertura.'
          ),
          tags$li(
            'El índice compuesto se diseñó para ponderar el tiempo y la movilidad poblacional y se
                       planea analizar otras formas de diseñar el índice. Según el análisis hecho hasta el momento,
                       los cambios en la ponderación tienen sólo un impacto marginal en la posición de los estados.
                       Se podría argumentar que el impacto es mayor de lo que se reporta aquí.'
          ),
          tags$li(
            'La base de datos incluye la información oficial actualizada del número de casos diagnosticados y de
                       muertes por COVID-19. Sin embargo, se debe tratar estos datos como muy preliminares y sujetos a ajustes importantes.
                       Con base en la experiencia internacional, es de esperarse que estos indicadores sufran cambios al alza con la ampliación
                       de las pruebas y la reclasificación de personas fallecidas por otras causas.  Por esta razón, es difícil medir el impacto
                       de las medidas capturadas en el índice  sobre la salud de la población.'
          ),
          tags$li(
            'Es imposible en este momento establecer una relación causal entre el impacto del COVID-19 en la mortalidad y
                       la política pública. Una vez que se cuente con una serie de tiempo más larga y datos más precisos sobre el
                       acceso a los servicios de salud, el diagnóstico y la mortalidad se podrán analizar dichos aspectos causales'
          ),
          tags$li(
            'Es importante tomar en cuenta que las diferentes entidades del país enfrentan una realidad distinta.
                       En cada estado, puede ser necesario adaptar la respuesta pública dependiendo de las circunstancias locales.
                       Especialmente importante es que los gobiernos tomen las decisiones económicas necesarias para que la población más
                       necesitada tenga las condiciones para cumplir con el distanciamiento físico'
          ),
          tags$li(
            'El índice se concentra solo en las medidas de distanciamiento físico, sin considerar otros aspectos
                       relevantes de la respuesta de los gobiernos estatales a la pandemia, como la distribución de insumos,
                       la reconversión hospitalaria o las medidas económicas para mitigar el impacto entre la población pobre y los negocios'
          ),
          tags$li(
            'Además, se podría decir que al ponderar con base en la fecha del primer caso nacional, asume que todos los
                       estados debieron actuar al mismo tiempo. Es decir "penaliza" o "premia" a todos por igual. Esto es correcto dada
                       la información limitada sobre los lugares de entrada del virus o los focos iniciales. Desde otra perspectiva, sin
                       embargo, actuar más tarde en algunos estados puede haber sido lo "correcto" con base en los fuertes costos económicos
                       de detener la actividad y los distintos ritmos de exposición al virus entre los estados'
          )
        )
      )
      
    )
  ),
  tabPanel(
    h4("Contacto"),
    br(),
    h3(
      "Son bienvenidos los comentarios, propuestas de ajuste y contribuciones al
       ObsContenCOVID, mismos que pueden ser enviados a Héctor Arreola-Ornelas (harreola@me.com)
       y Profesor Mike Touchton (miketouchton@miami.edu)",
      align = "center",
      style = "color:green"
    ),
    br(),
    br(),
    fluidRow(column(
      offset = 1,
      width = 10,
      align = "center",
      tags$img(
        src = "mike.jpg",
        height = "300px",
        width  = "300px"
      )
    )),
    fluidRow(column(
      offset = 1,
      width = 10,
      align = "center",
      
      h4(
        HTML(
          "Mike Touchton, Ph.D <br>
                          Profesor Asociado de Ciencias Políticas y Jefe de la Facultad de Salud Global <br>
                          en el Instituto de Estudios Avanzados de las Américas (Universidad de Miami) <br>
                          miketouchton@miami.edu"
        )
      )
    )),
    br(),
    fluidRow(column(
      offset = 1,
      width = 10,
      align = "center",
      p(
        'El Dr. Touchton tiene experiencia amplia en el diseño y administración
                  de evaluaciones de impacto de políticas. Su investigación enfatiza
                  la gobernanza de políticas, la prestación de servicios, y los
                  resultados de salud en el Sur Global, especialmente para las
                  poblaciones vulnerables y marginadas. Es autor de dos docenas de
                  artículos de revistas academicas y dos libros, más recientemente,',
        tags$i('Democracy at Work: Pathways to Well-being in Brazil'),
        '(Cambridge University Press 2019).',
        style = "font-size:160%"
      )
    )),
    #end mike
    
    
    br(),
    hr(),
    br(),    
    
    fluidRow(column(
      offset = 1,
      width = 10,
      align = "center",
      tags$img(
        src = "hector.png",
        height = "300px",
        width  = "300px"
      )
    )),
    fluidRow(column(
      offset = 1,
      width = 10,
      align = "center",
      
      h4(
        HTML(
          "Mtro. Héctor Arreola-Ornelas <br>
                          Director ejecutivo <br>
                          Tómatelo a Pecho A.C. <br>
                          harreola@me.com"
        )
      )
    )),
    br(),
    fluidRow(column(
      offset = 1,
      width = 10,
      align = "center",
      p(
        'Director ejecutivo de Tómatelo a Pecho A.C., e investigador visitante en el Instituto
                 de Estudios Avanzados para las Américas e investigador asociado del Centro de
                 Investigación en Ciencias de la Salud de la Universidad Anáhuac. Maestro en
                 Economía de la Salud. Sus líneas de investigación abarcan protección financiera,
                 políticas y sistemas de salud, economía laboral, evaluación económica y sistemas de
                 salud y cáncer. Es autor de 61 documentos de investigación y 12 capítulos en libros.
                 Pertenece al Sistema Nacional de Investigadores en México.',
        style = "font-size:160%"
      )
    )),
    #end hector
    
    
    
  ),
  # ++++++++++++++++++++++
  # TAB 1 : DOWNLOADS ----
  # ++++++++++++++++++++++
  tabPanel(title = h4("Documentación"),
           br(),
           fluidRow(
             column(
               width = 6,
               align = "center",
               fluidRow(
                 selectInput(inputId = "slide",
                             label = "Seleccione una versión:",
                             choices = c("29 Abril" = "april",
                                         "03 Mayo" = "may"),
                             selected = "may")
               ),
               br(),
               br(),
               uiOutput("slide_png"),
               br(),
               uiOutput("slide_pdf")
             ),
             column(
               width = 6,
               align = "center",
               fluidRow(
                 selectInput(inputId = "pr",
                             label = "Seleccione una versión:",
                             choices = c("29 Abril" = "april",
                                         "03 Mayo" = "may",
                                         "15 Mayo" = "may2"),
                             selected = "may2")
               ),
               br(),
               br(),
               uiOutput("pr_png"),
               br(),
               uiOutput("pr_pdf")
             )
           ))
)

