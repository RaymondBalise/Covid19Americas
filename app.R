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
      h1(strong("Observatorio de Políticas Públicas de COVID-19 
                en América Latina"), align = "center", style = "color: black")
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
             h4("en colaboración con:"),
             br(),
             img(src = "mia.jpg", width = 400),
             br(),
             img(src = "arts.jpeg", width= 450),
             br(),
             img(src = "miller.png", width = 450, height = 220)
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
      h4(strong("Click a state name in the legend to hide or show it."), align = "center", style = "color: red")
    ),
    br(),
    br(),
    fluidRow(
      column(offset = 3, width = 12,
      tabBox(id = "index",
             title = h3("Índice de política pública ajustado por tiempo y movilidad", align = "center"),
             height = "500px",
             tabPanel("Plot", width = 12,
                      plotlyOutput("summaryIndexPlot")),
             tabPanel("Table", width = 12,
                      dataTableOutput("index_table")))
      )),
    fluidRow(
      column(offset = 3, width = 12,
      tabBox(id = "deaths",
             title = h3("Muertes per capita", align = "center"),
             height = "500px",
             tabPanel("Plot",
                      plotlyOutput("summaryDeathsPerCapitaPlot")),
             tabPanel("Table",
                      dataTableOutput("death_table")))
      )),    
    br(),    
    br(),    
    br(),
    h4(strong("Las líneas moradas cortas representan respuestas parciales. La línea roja larga representa respuestas completas."), align = "center", style = "color: red"),
    
    fluidRow(
      column(
        width =12,
        img(src = "polarBarChart.png"), 
        align = "center")
    ), 
    
    p("These data were last updated on 2020-04-15", align = "center")
    
    
    
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
      h4(strong("Las líneas grises muestran los valores más pequeños o más grandes de todos los estados."), align = "center", style = "color: red")
    ),
    br(),
    fluidRow(
      column(width = 6, offset = 3, 
             selectInput(inputId = "refPlace",
                         label = "Seleccione un estado:",
                         choices = mexico$`State Name`,
                         selected = 1))
    ),
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
    leafletOutput("map", height = 900)
  ),
  tabPanel(
    h4("Metodología")
  ),
  tabPanel(
    h4("Contacto")
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
      select(`State Name`, `Policy Index Adj Time Mobility`)  %>% 
      datatable()
  },
  options = list(
    dom = 't'
  ),
  rownames = FALSE
  )
  
  # Summary plots / DeathPerCapita ----
  
  output$summaryDeathsPerCapitaPlot <- renderPlotly({
    gg <- ggplot(data = mexico) + 
      #ggtitle("Deaths per capita") +
      geom_line(data = refDeathPerCapita, aes(x=`Days Since the First Case (in Mexico)`, 
                                            y = Smallest), color = "gray") +
      geom_line(data = refDeathPerCapita, aes(x=`Days Since the First Case (in Mexico)`, 
                                            y = Average), color = "black") +
      geom_line(data = refDeathPerCapita, aes(x=`Days Since the First Case (in Mexico)`, 
                                            y = Largest), color = "gray") +
      theme_few(base_size = 20) +
      theme(legend.title = element_blank(),
            panel.background = element_rect(fill = "transparent"), # bg of the panel
            plot.background = element_rect(fill = "transparent", color = NA), 
            legend.background = element_rect(fill = "transparent"), # get rid of legend bg
            legend.box.background = element_rect(fill = "transparent")) +
      geom_point(aes(x=`Days Since the First Case (in Mexico)`, 
                     y = `Deaths per capita`, 
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
    
    mexico_latest %>% 
      mutate(`Deaths per capita` = round(`Deaths per capita`,2)) %>% 
      select(`State Name`, `Deaths per capita`)  %>% 
      datatable()
  },
  options = list(
    dom = 't'
  ),
  rownames = FALSE
  )
  
  # Single State Plot / Index ----
  
  output$indexAdjTimeMobilityPlot <- renderPlotly({
    # should be reactive
    place <- mexico %>% 
      filter(`State Name` == input$refPlace) %>% 
      filter(!is.na(`Policy Index Adj Time Mobility`))
    
    ggplot() + 
      ggtitle("Índice de política pública ajustado por tiempo y movilidad") +
      theme_few(base_size = 20) +
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
      ggtitle("Mobility") +
      theme_few(base_size = 20) +
      theme(legend.title = element_blank()) +
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
    pal <- colorBin("YlOrRd", domain = mexico_latest$`Policy Index Adj Time Mobility`)
    
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
      )
    
  })
}

shinyApp(ui, server)