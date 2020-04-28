source("global.R")

ui <- navbarPage(
  
  theme = shinytheme("simplex"),
  title = img(src = "flag.png", width = 70),
  windowTitle = "COVID-19 Mexico",
  # ++++++++++++++++++++++
  # TAB 1 : HOME PAGE ----
  # ++++++++++++++++++++++
  
  tabPanel(
    h4("Inicio"),
    fluidRow(
      h1(strong("OBSERVATORIO PARA LA CONTENCIÓN DEL COVID-19 EN AMERICA LATINA "), align = "center", style = "color: black")
    ),
    br(),
    br(),
    br(),
    fluidRow(
      column(width = 12,
             align = "center",
             br(),
             p("Un observatorio de políticas públicas y datos de salud por 
               estado para México por el Instituto de Estudios Avanzados de las 
               Américas de Miami y el Departamento de Ciencias de la Salud 
               Pública de la Facultad de Medicina Miller de la Universidad 
               de Miami", style = "font-size: 24px")
      )),
    fluidRow(
      column(width = 12,
             align = "center",
             img(src = "um.png"),
             br(),
             br(),
             tags$style(".img {
                            margin-left:35px;
                            margin-right:35px;
                          }"),
             div(img(src = "mia.png", width = 400, class="img"),
                 img(src = "arts.png", width= 375, height = 170, class="img"),
                 img(src = "dphs.png", width = 400, height = 140, class="img")),
            br(),
             h4(strong("en colaboración con:")),
             br(),
             fluidRow(
               column(width = 1, offset = 2),
               column(width = 2,
                    align = "center",
                    fluidRow(
                      img(src = "mexicosocial.png", width = 200)
                    ),
                    br(),
                    br(),
                    fluidRow(
                      img(src = "cide.png", width = 100)
                    ),
                    br(),
                    br(),
                    fluidRow(
                      img(src = "UNAM.png", width = 100)
                    )
             ),
             column(width = 1, offset = 1),
             column(width = 2,
                    align = "center",
                    fluidRow(
                      img(src = "Tufts_univ.png", width = 200)
                    ),
                    br(),
                    br(),
                    fluidRow(
                      img(src = "cicsaanahuac.png", width = 200)
                    ),
                    br(),
                    br(),
                    fluidRow(
                      img(src = "smsp.png", width = 200)
                    )
             ))
      )
    ),
    fluidRow(),
    fluidRow(),
    fluidRow(),
    br(),
    br(),
    br(),
    fluidRow(
      h6("Esto se desarrolla como una aplicación de código abierto y se 
           publica bajo la licencia MIT", align = "center", 
         style = "color: black")
    )
    
  ),
  # +++++++++++++++++++++++++
  # TAB 2 : SUMMARY PAGE ----
  # +++++++++++++++++++++++++
  tabPanel(
    h4("Resumen"),
    fluidRow(
      h1(strong("Resumen"), align = "center", style = "color: black")
    ),
    fluidRow(
      h4(strong("Haga doble clic en el nombre de un estado en la leyenda para mostrarlo solo. Haga doble clic nuevamente para mostrar toda la información."), align = "center", style = "color: red")
    ),
    fluidRow(
      h4(strong("Haga clic en el nombre de un estado en la leyenda para ocultar o mostrar un estado."), align = "center", style = "color: red")
    ),
    br(),
    br(),
    fluidRow(
      column(offset = 3, width = 12,
      tabBox(id = "index",
             title = h3("Índice de política pública ajustado por tiempo y movilidad", align = "center"),
             height = "500px",
             tabPanel("Grafico", width = 12,
                      plotlyOutput("summaryIndexPlot")),
             tabPanel("Tabla de datos", width = 12,
                      dataTableOutput("index_table")))
      )),
    fluidRow(
      column(offset = 3, width = 12,
      tabBox(id = "deaths",
             title = h3("Muertes per capita", align = "center"),
             height = "500px",
             tabPanel("Grafico",
                      plotlyOutput("summaryDeathsPerCapitaPlot")),
             tabPanel("Tabla de datos",
                      dataTableOutput("death_table")))
      )),    
    br(),    
    br(),    
    br(),
    h4(strong("Las líneas moradas cortas representan respuestas parciales. La línea roja larga representa respuestas completas."), align = "center", style = "color: red"),
    
    fluidRow(
      column(width =12, align = "center",
     sliderInput("spider", "Pick a day:",
                 min = 1, max = 57,
                 value = 1, step = 1,
                 animate =
                   animationOptions(interval = 300, loop = TRUE)))),
    
    fluidRow(
      column(
        width =12,
        imageOutput("spiderPlot"),
        align = "center")
    ), 

    p("Estos datos se actualizaron por última vez el 23 de abril de 2020.", align = "center")
    
    
    
  ),
  # ++++++++++++++++++++++++++++++
  # TAB 3 : SINGLE STATE PAGE ----
  # ++++++++++++++++++++++++++++++
  tabPanel(
    h4("Por estado"),
    fluidRow(
      h1(strong("Resumen por estado "), align = "center", style = "color: black")
    ),
    
    fluidRow(
      h4(strong("Las líneas grises muestran el valor mínimo y máximo que se obtuvo en todos los estados por día."), align = "center", style = "color: red")
    ),
    br(),
    fluidRow(
      column(width = 3,
             offset = 1,
             align = "left",
             selectInput(inputId = "refPlace",
                         label = "Seleccione un estado:",
                         choices = stateNames,
                         selected = 1))
    ),
    br(),
    br(),
    fluidRow(
      column(offset = 1, width = 5, plotlyOutput("indexAdjTimeMobilityPlot")),
      column(width = 5, plotlyOutput("mobilityPlot"))
    )
    
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
    h2("Mapa del índice de políticas ajustado por tiempo y movilidad"),
    br(),
    leafletOutput("map", height = 925)
  ),
  tabPanel(
    h4("Metodología"),
    br(),
    h3(strong("Observatorio Estatal para la Contención del COVID-19"), 
       align = "center"),
    br(),
    br(),
    column(width = 10,
           offset = 1,
           align = "left",
           p('El Observatorio Estatal para la Contención del COVID-19 (ObsContenCOVID) 
    es una plataforma que presenta información sistemática y actualizada día con día sobre las 
    medidas que los gobiernos estatales en México han adoptado para fomentar el distanciamiento social, reducir la movilidad e 
    informar a la población sobre la enfermedad. La herramienta incluye también información sobre el grado en que estas medidas 
    han sido observadas por los ciudadanos, con base en el movimiento registrado entre la población.', style = "font-size:150%"),
           p('La primera aportación de este Observatorio es el cálculo de un índice que toma como base en', tags$i('EL Oxford COVID-19 Government Response Tracker'), '(OxCGRT), 
una herramienta desarrollada por la Universidad Oxford que consiste en un listado de medidas al alcance de los gobiernos para disminuir la 
velocidad de transmisión del virus [5].  Utilizamos el  OxCGRT para identificar las siete medidas preventivas relevantes a la situación de México: el cierre de escuelas; 
la suspensión de actividades laborales presenciales; la cancelación de eventos públicos; la suspensión del transporte público; el desarrollo de campañas 
informativas; la restricción de viajes y viajeros dentro del estado; y el control de viajes y viajeros internacionales.   OxCGRT califica estas medidas en una 
forma ordinal y presenta una suma simple de ellas a través del tiempo, generando así un índice.', style = "font-size:150%"),
           p('ObsContenCOVID parte del marco OxCGRT y codifica la respuesta de los gobiernos estatales en México, con base en la información disponible en las páginas 
oficiales de cada una de las entidades federativas (Ver Anexo metodológico). Para cada estado, cada una de las siete políticas públicas mencionadas es medida 
en forma diaria, a partir de la fecha de aparición del primer caso oficialmente reconocido en el país (el 27 de febrero). 
La variables incluidas son: i) Cierre de escuelas, ii) Suspensión en área laboral, iii) Cancelación eventos públicos, iv) Suspensión transporte público, 
v) Desarrollo de Campañas informativas, vi) Restricción de viajes dentro del estado, y vii) Control de viajes internacionales. 
De manera similar a la metodología de OxCGRT, cada medida preventiva es calificada con una escala ordinal entre 0 y 1, en donde “0” implica que no existe registro 
de la aplicación de dicha medida en el estado, “0.5” registro parcial y/o no obligatoria y “1” que dicha política se implementó de manera total y 
obligatoria en la entidad. Cabe señalar que cada dato pasa por tres revisiones independientes, antes de ser agregado a la base de datos. De tal manera, 
cada estado recibe una calificación entre 0 y 7, que resulta de la suma de las distintas dimensiones y se actualiza diariamente, conforme cambian las medidas adoptadas.',
             style = "font-size:150%"),
           
           p('Como segundo paso, ObsContenCOVID retoma el índice agregado para cada estado y lo pondera con base en la fecha de entrada en 
      vigor de cada una de las medidas en la entidad respectiva, nuevamente tomando como base el 27 de febrero, fecha del primer caso registrado. 
      Se presentan dos formas de ponderar a través del tiempo: una que aplica un peso uniforme por día, y otra que da un mayor peso a los días más cercanos 
      al 27 de febrero. Esta estrategia permite ajustar el índice estatal para que refleje no sólo la cantidad y rigor de las medidas adoptadas, sino 
      su adopción a tiempo. Uno de los hallazgos importantes de este ejercicio es que el ritmo de implementación de medidas preventivas ha variado 
      de manera considerable en el país, lo cual puede tener implicaciones importantes en el ritmo e intensidad de propagación de la enfermedad.', style = "font-size:150%"),
           p('Finalmente, como tercer y último paso el índice compuesto es ponderado y ajustado con base en el éxito de las medidas en la contención de 
      la movilidad poblacional en la entidad federativa utilizando información de Retargetly Meta-data Movilidad México GPS Mobile App 
      sobre la reducción efectiva de la movilidad en cada uno de los estados, relativo a la movilidad convencional observada tres semanas previo 
      al inicio de la contingencia.[2] Esta ponderación es crucial para capturar la eficacia de las políticas públicas.', style = "font-size:150%"),
           p('En resumen, el índice estatal del ObcContenCOVID es un retrato diario, para todas y cada una de las entidades federativas, 
      de: 1) la amplitud de las medidas adoptadas por los gobiernos estatales conforme a recomendaciones internacionales; 
      2) la oportunidad en su adopción; y 3) la respuesta poblacional, en términos de reducción de la movilidad. 
      Es basado en este índice, que toma valores de 0 a 1003 y puede ser interpretado como un porcentaje, 
      que se hace un ranqueo de las entidades federativas.', style = "font-size:150%"),
           p('Con ello ofrecemos una herramienta valiosa para revisar no sólo lo hecho hasta aquí, sino también lo que se está 
      haciendo ahora mismo, en este momento crítico, para la contención de la epidemia. Nuestro objetivo al ponerlo al 
      alcance del público y sobre todo de las autoridades estatales es proveer información oportuna, que contribuya a tomar 
      mejores decisiones hacia el futuro. ', style = "font-size:150%"),
           br(),
           br(),
           h4(strong("Limitaciones"), align = "center"),
           p('El ejercicio que aquí se presenta y el ObsContenCOVID en general, tiene limitaciones y no debe tomarse como un juicio 
      último o de valor sobre la actuación de los gobiernos, ni como una predicción de 
      los patrones geográficos de la enfermedad. ', style = "font-size:120%"),
           p('A continuación compartimos algunos puntos a tomar en consideración:', style = "font-size:120%"),
           p(
             tags$ul(style = "font-size:120%",
               tags$li('Los siete tipos de medidas preventivas consideradas tiene el mismo peso en la construcción del índice. 
                       Se podría argumentar que no todas las medidas de política pública tienen la misma eficacia y por lo tanto, 
                       deberían de tener distinto peso en la escala de la Universidad Oxford.  Sin embargo, la evidencia sobre qué tipo 
                       de intervención funciona mejor para abatir el COVID-19 es limitada en este momento.  Por esta razón, 
                       se realiza una agregación simple de intervenciones que nos permite capturar, en un solo índice, la 
                       amplitud de las disposiciones adoptadas en las distintas entidades como parte de la política pública de 
                       prevención del contagio, sin aplicar un juicio sobre la eficacia potencial de cada uno.'),
               tags$li('El índice clasifica en forma ordinal, y por ende aproximada, la eficacia de cada una de las siete 
                       acciones para promover el distanciamiento físico. Por el momento, no es factible tener una medición 
                       más precisa de cada una de ellas, como el porcentaje de cobertura.'),
               tags$li('El índice compuesto se diseñó para ponderar el tiempo y la movilidad poblacional y se 
                       planea analizar otras formas de diseñar el índice. Según el análisis hecho hasta el momento, 
                       los cambios en la ponderación tienen sólo un impacto marginal en la posición de los estados.  
                       Se podría argumentar que el impacto es mayor de lo que se reporta aquí.'),
               tags$li('La base de datos incluye la información oficial actualizada del número de casos diagnosticados y de 
                       muertes por COVID-19. Sin embargo, se debe tratar estos datos como muy preliminares y sujetos a ajustes importantes. 
                       Con base en la experiencia internacional, es de esperarse que estos indicadores sufran cambios al alza con la ampliación 
                       de las pruebas y la reclasificación de personas fallecidas por otras causas.  Por esta razón, es difícil medir el impacto 
                       de las medidas capturadas en el índice  sobre la salud de la población.'),
               tags$li('Es imposible en este momento establecer una relación causal entre el impacto del COVID-19 en la mortalidad y 
                       la política pública. Una vez que se cuente con una serie de tiempo más larga y datos más precisos sobre el 
                       acceso a los servicios de salud, el diagnóstico y la mortalidad se podrán analizar dichos aspectos causales'),
               tags$li('Es importante tomar en cuenta que las diferentes entidades del país enfrentan una realidad distinta. 
                       En cada estado, puede ser necesario adaptar la respuesta pública dependiendo de las circunstancias locales. 
                       Especialmente importante es que los gobiernos tomen las decisiones económicas necesarias para que la población más 
                       necesitada tenga las condiciones para cumplir con el distanciamiento físico'),
               tags$li('El índice se concentra solo en las medidas de distanciamiento físico, sin considerar otros aspectos 
                       relevantes de la respuesta de los gobiernos estatales a la pandemia, como la distribución de insumos, 
                       la reconversión hospitalaria o las medidas económicas para mitigar el impacto entre la población pobre y los negocios'),
               tags$li('Además, se podría decir que al ponderar con base en la fecha del primer caso nacional, asume que todos los 
                       estados debieron actuar al mismo tiempo. Es decir "penaliza" o "premia" a todos por igual. Esto es correcto dada 
                       la información limitada sobre los lugares de entrada del virus o los focos iniciales. Desde otra perspectiva, sin 
                       embargo, actuar más tarde en algunos estados puede haber sido lo "correcto" con base en los fuertes costos económicos 
                       de detener la actividad y los distintos ritmos de exposición al virus entre los estados')
             )
           )
           
    )
  ),
  tabPanel(
    h4("Contacto"),
    br(),
    h4("Son bienvenidos los comentarios, propuestas de ajuste y contribuciones al
       ObsContenCOVID, mismos que pueden ser enviados a Héctor Arreola-Ornelas (harreola@me.com) 
       y Profesor Mike Touchton (miketouchton@miami.edu)", align = "center"),
    br(),
    br(),
    fluidRow(
      column(width = 3,
             tags$img(src = "mike.jpg", height = "300px", width  = "300px"), 
             offset = 1,
             align = "center"), 
      column(width = 6,
             fluidRow(
               h4(HTML("Mike Touchton, Ph.D <br> 
                          Profesor Asociado de Ciencias Políticas y Jefe de la Facultad de Salud Global <br>
                          en el Instituto de Estudios Avanzados de las Américas (Universidad de Miami) <br>
                          miketouchton@miami.edu")
               ) 
             ),
             br(),
             fluidRow( 
               p('El Dr. Touchton tiene experiencia amplia en el diseño y administración 
                  de evaluaciones de impacto de políticas. Su investigación enfatiza 
                  la gobernanza de políticas, la prestación de servicios, y los 
                  resultados de salud en el Sur Global, especialmente para las 
                  poblaciones vulnerables y marginadas. Es autor de dos docenas de 
                  artículos de revistas academicas y dos libros, más recientemente,', 
                 tags$i('Democracy at Work: Pathways to Well-being in Brazil'), '(Cambridge University Press 2019).', 
                 style = "font-size:160%")
             )
      )
    ), #end mike 
    br(),
    br(),
    fluidRow(
      column(width = 3,
             tags$img(src = "mike.jpg", height = "300px", width  = "300px"), 
             offset = 1,
             align = "center"), 
      column(width = 6,
             fluidRow(
               h4(HTML("Héctor Arreola-Ornelas, Ph.D <br> 
                          Profesor Asociado de Ciencias Políticas y Jefe de la Facultad de Salud Global <br>
                          en el Instituto de Estudios Avanzados de las Américas (Universidad de Miami) <br>
                          harreola@me.com")
               ) 
             ),
             br(),
             fluidRow( 
               p('El Dr. Touchton tiene experiencia amplia en el diseño y administración 
                  de evaluaciones de impacto de políticas. Su investigación enfatiza 
                  la gobernanza de políticas, la prestación de servicios, y los 
                  resultados de salud en el Sur Global, especialmente para las 
                  poblaciones vulnerables y marginadas. Es autor de dos docenas de 
                  artículos de revistas academicas y dos libros, más recientemente,', 
                 tags$i('Democracy at Work: Pathways to Well-being in Brazil'), '(Cambridge University Press 2019).', 
                 style = "font-size:160%")
             )
      )
    ) #end hector
  )
)

server <- function(input, output, session) {
  
  
  # Summary plots / Index ----
  
  output$summaryIndexPlot <- renderPlotly({
    gg <- ggplot(data = mexico) + 
      #ggtitle("Policy Index Adjusted for Time and Mobility") +
      geom_line(data = refIndexTimeMob, aes(x=`Days Since the First Case (in Mexico)`, 
                                            y = Smallest), color = "gray") +
      geom_line(data = refIndexTimeMob, aes(x=`Days Since the First Case (in Mexico)`, 
                                            y = Average), color = "black") +
      geom_line(data = refIndexTimeMob, aes(x=`Days Since the First Case (in Mexico)`, 
                                            y = Largest), color = "gray") +
      theme_few(base_size = 20) +
      theme(legend.title = element_blank(),
            panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), 
            legend.background = element_rect(fill = "transparent"), # get rid of legend bg
            legend.box.background = element_rect(fill = "transparent")) +
      geom_point(aes(x=`Days Since the First Case (in Mexico)`, 
                     y = `Policy Index Adj Time Mobility`, 
                     group = `State Name`,
                     color = `State Name`,
                     shape = 4,
      ),
      size = 2) +
      scale_shape_identity() +
      xlab("Días transcurridos desde el primer caso (en México)") +
      ylab("Índice de política")
    
    
    ggplotly(gg, tooltip=c("x", "y", "group")) 
  })
  
  mexico_latest <- mexico %>% 
    group_by(`State Name`) %>%
    slice(which.max(as.Date(Date, '%Y-%m-%d')))
  
  output$index_table <- renderDataTable({
    mexico_latest %>% 
      mutate(`Policy Index Adj Time Mobility` = round(`Policy Index Adj Time Mobility`,2)) %>% 
      select(`State Name`, `Policy Index Adj Time Mobility`) %>% 
      rename(`Índice de política pública ajustado por tiempo y movilidad` = `Policy Index Adj Time Mobility`) %>%    
      rename(Estado = `State Name`) %>% 
      datatable(.,
                rownames = FALSE)
  })  
  
  # Summary plots / DeathPerCapita ----
  
  output$summaryDeathsPerCapitaPlot <- renderPlotly({
    gg <- ggplot(data = mexico) + 
      #ggtitle("Deaths per capita") +
      geom_line(data = refDeathPerCapita, aes(x=`Days Since the First Case (in Mexico)`, 
                                            y = Smallest * 1000000), color = "gray") +
      geom_line(data = refDeathPerCapita, aes(x=`Days Since the First Case (in Mexico)`, 
                                            y = Average * 1000000), color = "black") +
      geom_line(data = refDeathPerCapita, aes(x=`Days Since the First Case (in Mexico)`, 
                                            y = Largest * 1000000), color = "gray") +
      theme_few(base_size = 20) +
      theme(legend.title = element_blank(),
            panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), 
            legend.background = element_rect(fill = "transparent"), # get rid of legend bg
            legend.box.background = element_rect(fill = "transparent")) +
      geom_point(aes(x=`Days Since the First Case (in Mexico)`, 
                     y = `Deaths per capita` * 1000000, 
                     group = `State Name`,
                     color = `State Name`,
                     shape = 21),
                 size = 2) +
      scale_shape_identity() +
      ylab("Muertes per capita") +
      xlab("Días transcurridos desde el primer caso (en México)")
    
    ggplotly(gg, tooltip=c("x", "y", "group"))
  })
  
  output$death_table <- renderDataTable({
    
  mexico %>% 
    select(Date, `State Name`, `Deaths per capita`) %>% 
    group_by(`State Name`) %>%
    slice(which.max(as.Date(Date, '%Y-%m-%d'))) %>% 
    mutate(`Deaths per capita` = round(`Deaths per capita` * 1000000, .01)) %>% 
      select(`State Name`, `Deaths per capita`) %>% 
      rename(`Muertes per capita * 1,000,000` = `Deaths per capita`) %>%    
      rename(Estado = `State Name`) %>% 
      datatable(.,
                rownames = FALSE)
  })  
  
  
  
  
  # Single State Plot / Index ----
  
  output$indexAdjTimeMobilityPlot <- renderPlotly({
    # should be reactive
    place <- mexico %>% 
      filter(`State Name` == input$refPlace) %>% 
      filter(!is.na(`Policy Index Adj Time Mobility`))
    
    ggplot() + 
      ggtitle("Índice de política pública ajustado por tiempo y movilidad") +
      theme_few(base_size = 20) +
      theme(plot.title = element_text(size = 20),
            panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), 
            legend.background = element_rect(fill = "transparent"), # get rid of legend bg
            legend.box.background = element_rect(fill = "transparent")) +
      geom_line(data = refIndexTimeMob, 
                aes(x=`Days Since the First Case (in Mexico)`, 
                    y = Smallest), 
                color = "gray", 
                size = 1) + 
      geom_line(data = refIndexTimeMob, 
                aes(x=`Days Since the First Case (in Mexico)`, 
                    y = Largest), 
                color = "gray", 
                size = 1)  +
      geom_point(data = place, 
                aes(x=`Days Since the First Case (in Mexico)`, 
                    y = `Policy Index Adj Time Mobility`), 
                color = "orange",
                size = 2,
                shape = 4) +
      ylab("Índice")  +
      xlab("Días transcurridos desde el primer caso (en México)")
  }) 

  # Single State Plot / Death Per Capita Plot ----
  # 
  # output$deathPerCapitaPlot <- renderPlotly({
  #   place <- mexico %>% 
  #     filter(`State Name` == input$refPlace) %>% 
  #     filter(!is.na(`Policy Index Adj Time Mobility`))
  #   
  #   gg <- ggplot() + 
  #     ggtitle("Deaths per Capita") +
  #     theme_few(base_size = 25) +
  #     theme(legend.title = element_blank()) +
  #     geom_line(data = refDeathPerCapita, 
  #               aes(x=`Days Since the First Case (in Mexico)`, 
  #                   y = Smallest), 
  #               color = "gray", 
  #               size = 1) + 
  #     geom_line(data = refDeathPerCapita, 
  #               aes(x=`Days Since the First Case (in Mexico)`, 
  #                   y = Largest), 
  #               color = "gray", 
  #               size = 1)  +
  #     geom_point(data = place, 
  #                aes(x=`Days Since the First Case (in Mexico)`, 
  #                    y = `Deaths per capita`), 
  #                color = "orange",
  #                size = 2,
  #                shape = 21) +
  #     ylab("Deaths")
  #   
  #   ggplotly(gg, tooltip=c("x", "y", "group"))
  # }) 
  
  
 
  # Single State Plot / Mobility Plot ----
  
  output$mobilityPlot <- renderPlotly({
    place <- mexico %>% 
      filter(`State Name` == input$refPlace) %>% 
      filter(!is.na(`Mobility Index`))
    
    gg <- ggplot() + 
      ggtitle("Movilidad") +
      theme_few(base_size = 20) +
      theme(legend.title = element_blank(),
            panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), 
            legend.background = element_rect(fill = "transparent"), # get rid of legend bg
            legend.box.background = element_rect(fill = "transparent")) +
      geom_line(data = refMobilityIndex, 
                aes(x=`Days Since the First Case (in Mexico)`, 
                    y = Smallest), 
                color = "gray", 
                size = 1) + 
      geom_line(data = refMobilityIndex, 
                aes(x=`Days Since the First Case (in Mexico)`, 
                    y = Largest), 
                color = "gray", 
                size = 1)  +
      geom_point(data = place, 
                 aes(x=`Days Since the First Case (in Mexico)`, 
                     y =  `Mobility Index`), 
                 color = "orange",
                 size = 2) +
      ylab("Movilidad") +
      xlab("Días transcurridos desde el primer caso (en México)")
      expand_limits(x = 0, y = 0)
    
    ggplotly(gg, tooltip=c("x", "y", "group"))
  }) 
  
  
  
  
  # # Single State Plot / Case Per Capita Plot ----
  # 
  # output$casePlot <- renderPlotly({
  #   place <- mexico %>% 
  #     filter(`State Name` == input$refPlace) %>% 
  #     filter(!is.na(`Policy Index Adj Time Mobility`))
  #   
  #   gg <- ggplot() + 
  #     ggtitle("Cases") +
  #     theme_few(base_size = 25) +
  #     theme(legend.title = element_blank()) +
  #     geom_line(data = refCasesPerCapita, 
  #               aes(x=`Days Since the First Case (in Mexico)`, 
  #                   y = Smallest), 
  #               color = "gray", 
  #               size = 1) + 
  #     geom_line(data = refCasesPerCapita, 
  #               aes(x=`Days Since the First Case (in Mexico)`, 
  #                   y = Largest), 
  #               color = "gray", 
  #               size = 1)  +
  #     geom_point(data = place, 
  #                aes(x=`Days Since the First Case (in Mexico)`, 
  #                    y = `Cases per capita`), 
  #                color = "orange",
  #                size = 1) +
  #     ylab("Cases per capita")
  #   
  #   ggplotly(gg, tooltip=c("x", "y", "group"))
  # }) 
  # 
  
    ##### multi-state plots #####
  
  # plot1_dat <- reactive({
  #   req(input$state1)
  #   mexico %>% 
  #     filter(`State Name` == input$state1)
  # })
  # 
  # plot2_dat <- reactive({
  #   req(input$state2)
  #   mexico %>% 
  #     filter(`State Name` == input$state2) 
  # })
  # 
  # 
  # output$plot1 <- renderPlot({
  #   #browser()
  #   p <- ggplot(data = plot1_dat()) + 
  #     ggtitle("Beds Through Time") +
  #     theme_few(base_size = 20) +
  #     theme(legend.title = element_blank()) +
  #     geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
  #                    y = input$vars1[1]),
  #                color = "red"
  #     ) +
  #     ylab("Measure") +
  #     xlab("Movilidad")
  #   
  #   if(length(input$vars1) > 1){
  #     p <- p + geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
  #                             y = input$vars1[2]),
  #                         color = "green"
  #     )
  #   }else if(length(input$vars1) > 2){
  #     p <- p + geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
  #                             y = input$vars1[3]),
  #                         color = "orange"
  #     )
  #   }else if(length(input$vars1) > 3){
  #     p <- p + geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
  #                             y = input$vars1[4]),
  #                         color = "blue"
  #     )
  #   }
  #   p
  # }) 
  # 
  # output$plot2 <- renderPlot({
  #   
  #   #browser()
  #   p <- ggplot(data = plot2_dat()) + 
  #     ggtitle("Beds Through Time") +
  #     theme_few(base_size = 20) +
  #     theme(legend.title = element_blank()) +
  #     geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
  #                    y = input$vars2[1]),
  #                color = "red"
  #     ) +
  #     labs(y = "Measure")
  #   
  #   if(length(input$vars2) > 1){
  #     p <- p + geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
  #                             y = input$vars2[2]),
  #                         color = "green"
  #     )
  #   }else if(length(input$vars2) > 2){
  #     p <- p + geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
  #                             y = input$vars2[3]),
  #                         color = "orange"
  #     )
  #   }else if(length(input$vars2) > 3){
  #     p <- p + geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
  #                             y = input$vars2[4]),
  #                         color = "blue"
  #     )
  #   }
  #   p
  # }) 
  
  
  # output$refplot <- renderPlotly({
  #   state1_label <- input$state1
  #   state2_label <- input$state2
  #   label1_pos <- max(plot1_dat()$`Policy Index Adjusted for Time`, na.rm = TRUE)
  #   label2_pos <- max(plot2_dat()$`Policy Index Adjusted for Time`, na.rm = TRUE)
  #   
  #   x <- ggplot() + 
  #     #theme_few() +
  #     geom_line(data = State_Name1(), 
  #               aes(x=`Days Since the First Case (in Mexico)`, 
  #                   y = `Policy Index Adjusted for Time`, group = `State Name`), color = "orange") +
  #     geom_text(data = State_Name1(), x = 45, y = label1_pos + 5, 
  #               label = state1_label, color = "orange") +
  #     geom_line(data = State_Name2(), 
  #               aes(x=`Days Since the First Case (in Mexico)`, 
  #                   y = `Policy Index Adjusted for Time`, group = `State Name`), color = "gray") +
  #     geom_text(data = State_Name2(), x = 45, y = label2_pos + 5, 
  #               label = state2_label, color = "gray") +
  #     ylim (0, 45) + 
  #     theme_minimal()
  #   
  #   ggplotly(x)
  # }) 
  
  # Map ----
  
  
  output$map <- renderLeaflet({
    
    mexico_latest <- mexico %>% 
      group_by(`State Name`) %>%
      slice(which.max(as.Date(Date, '%Y-%m-%d')))
    
    #join data to spdf
    mexico_states_covid <- sp::merge(mexico_states, mexico_latest[c("State Name","Policy Index Adj Time Mobility")],
                                     by.x = "ADMIN_NAME", by.y = "State Name")
    
    #bins <- c(0, 75, 150, 225, 300, 375, 450, 525, 600, Inf)
    pal <- colorBin("BrBG", domain = mexico_latest$`Policy Index Adj Time Mobility`)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g índice de política",
      mexico_states_covid$ADMIN_NAME, mexico_states_covid$`Policy Index Adj Time Mobility`
    ) %>% lapply(htmltools::HTML)
    
    leaflet(data = mexico_states_covid) %>% 
      addProviderTiles("Esri.WorldGrayCanvas") %>% 
      addPolygons(
        fillColor = ~pal(`Policy Index Adj Time Mobility`),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~`Policy Index Adj Time Mobility`, opacity = 0.7, title = NULL,
                position = "bottomright"
      ) %>% 
      setView(lng = -103.2,
              lat = 23.95,
              zoom = 6)

  })
# Spider plot ----    
    
    output$spiderPlot <- renderImage({
   
        return(list(
          src = paste0("www/polarBarChart", input$spider, ".png"),
          contentType = "image/png",
          alt = "Face"
        ))
   
    }, deleteFile = FALSE)
}

shinyApp(ui, server)