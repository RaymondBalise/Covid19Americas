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
      column(width = 6,
             align = "left",
             br(),
             p("Un observatorio de políticas públicas y datos de salud por 
               estado para México por el Instituto de Estudios Avanzados de las 
               Américas de Miami y el Departamento de Ciencias de la Salud 
               Pública de la Facultad de Medicina Miller de la Universidad 
               de Miami", style = "font-size: 24px")
      ),
      column(width = 6,
             align = "center",
             img(src = "um.png"),
             h4("en colaboración con:"),
             br(),
             img(src = "LOGO-TAP.png"),
             br(),
             img(src = "dphs.png")
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
    h4("El Sumario"),
    fluidRow(
      h1(strong("Summary"), align = "center", style = "color: black")
    ),
    fluidRow(
      h4(strong("Double click a state name in the legend to show only it.  Double click it again to show all."), align = "center", style = "color: gray")
    ),
    fluidRow(
      h4(strong("Click a state name in the legend to hide or show it."), align = "center", style = "color: gray")
    ),
    br(),
    fluidRow(
      column(width = 6, offset = 3, plotlyOutput("summaryIndexPlot",height = "200%"))),    
    br(),    
    br(),    
    br(),
    fluidRow(
      column(width = 6, offset = 3, plotlyOutput("summaryDeathsPerCapitaPlot",height = "200%"))
    )
    
  ),
  # ++++++++++++++++++++++++++++++
  # TAB 3 : SINGLE STATE PAGE ----
  # ++++++++++++++++++++++++++++++
  tabPanel(
    h4("Estado único"),
    fluidRow(
      h1(strong("Summary of a Single State"), align = "center", style = "color: black")
    ),
    
    fluidRow(
      h4(strong("Gray lines show the smallest or largest values acorss all states."), align = "center", style = "color: gray")
    ),
    br(),
    fluidRow(
      column(width = 6, offset = 3, 
             selectInput(inputId = "refPlace",
                         label = "Select a State:",
                         choices = mexico$`State Name`,
                         selected = 1))
    ),
    fluidRow(
      column(width = 6, offset = 3, plotOutput("indexPlot")),
      column(width = 6, offset = 3, plotOutput("casePlot"))
    )
    
  ),
  # ++++++++++++++++++++++
  # TAB 4 : PLOTS     ----
  # ++++++++++++++++++++++
  tabPanel(
    h4("Estado múltiple"),
    column(width = 6,
           fluidRow(
             selectInput(inputId = "state1",
                         label = "Select a State:",
                         choices = unique(mexico$`State Name`),
                         selected = "Chiapas")
           ),
           fluidRow(
             checkboxGroupInput(inputId = "vars1",
                                label = "Select Measure(s)",
                                choices = c("Hospital Beds" = "Hospital_Beds",
                                            "Hospital Beds/Capita" = "Hospital_Beds_per_capita",
                                            "ICU Beds" = "ICU_Beds",
                                            "ICU Beds/Capita" = "ICUBeds_per_capita"),
                                selected = "Hospital_Beds")
           ),
           fluidRow(
             plotOutput("plot1")
           )
    ),
    column(width = 6,
           fluidRow(
             selectInput(inputId = "state2",
                         label = "Select a State:",
                         choices = unique(mexico$`State Name`),
                         selected = "Veracruz")
           ),
           fluidRow(
             checkboxGroupInput(inputId = "vars2",
                                label = "Select Measure(s)",
                                choices = c("Hospital Beds" = "Hospital_Beds",
                                            "Hospital Beds/Capita" = "Hospital_Beds_per_capita",
                                            "ICU Beds" = "ICU_Beds",
                                            "ICU Beds/Capita" = "ICUBeds_per_capita"),
                                selected = "Hospital_Beds")
           ),
           fluidRow(
             plotOutput("plot2")
           ),
           fluidRow(
             plotlyOutput("refplot")
           )
    )
  ),
  
  # ++++++++++++++++++++++
  # TAB 5 : MAP       ----
  # ++++++++++++++++++++++
  
  tabPanel(
    h4("La Carta"),
    leafletOutput("map", height = 900)
  )
)

server <- function(input, output, session) {
  
  
  # Summary plots / Index ----
  
  output$summaryIndexPlot <- renderPlotly({
    gg <- ggplot(data = mexico) + 
      ggtitle("Policy Index Adjusted for Time and Mobility Through Time") +
      theme_few(base_size = 20) +
      theme(legend.title = element_blank()) +
      geom_point(aes(x=`Days Since the First Case (in Mexico)`, 
                     y = `Policy Index Adj Time Mobility`, 
                     group = `State Name`,
                     color = `State Name`,
                     shape = 4,
      ),
      size = 2) +
      scale_shape_identity() +
      geom_smooth(aes(x=`Days Since the First Case (in Mexico)`, 
                      y = `Policy Index Adj Time Mobility`),
                  method = "gam",
                  formula = y ~ s(x, bs = "cs"),  # shrinkage version of cubic spline base
                  se=FALSE)
    
    ggplotly(gg, tooltip=c("x", "y", "group"))
  })
  
  # Summary plots / DeathPerCapita ----
  
  output$summaryDeathsPerCapitaPlot <- renderPlotly({
    gg <- ggplot(data = mexico) + 
      ggtitle("Deaths per capita") +
      theme_few(base_size = 20) +
      theme(legend.title = element_blank()) +
      geom_point(aes(x=`Days Since the First Case (in Mexico)`, 
                     y = `Deaths per capita`, 
                     group = `State Name`,
                     color = `State Name`,
                     shape = 21),
                 size = 2) +
      scale_shape_identity() 
    
    ggplotly(gg, tooltip=c("x", "y", "group"))
  })
  
  # Single State Plot / Index ----
  
  output$indexPlot <- renderPlot({
    # should be reactive
    place <- mexico %>% 
      filter(`State Name` == input$refPlace) %>% 
      filter(!is.na(`Policy Index Adj Time Mobility`))
    
    ggplot() + 
      ggtitle("Policy Index Adjusted for Time and Mobility Through Time") +
      theme_few(base_size = 25) +
      geom_line(data = refIndexTimeMob, 
                aes(x=`Days Since the First Case (in Mexico)`, 
                    y = Smallest), 
                color = "gray", 
                size = 2) + 
      geom_line(data = refIndexTimeMob, 
                aes(x=`Days Since the First Case (in Mexico)`, 
                    y = Largest), 
                color = "gray", 
                size = 2)  +
      geom_line(data = place, 
                aes(x=`Days Since the First Case (in Mexico)`, 
                    y = `Policy Index Adj Time Mobility`), 
                color = "orange",
                size = 2) +
      ylab("Policy Index Adj Time Mobility") 
  }) 
  
  # Single State Plot / Case Plot ----
  
  output$casePlot <- renderPlot({
    place <- mexico %>% 
      filter(`State Name` == input$refPlace) %>% 
      filter(!is.na(`Policy Index Adj Time Mobility`))
    
    ggplot() + 
      ggtitle("Cases through time") +
      theme_few(base_size = 25) +
      theme(legend.title = element_blank()) +
      geom_line(data = refCases, 
                aes(x=`Days Since the First Case (in Mexico)`, 
                    y = Smallest), 
                color = "gray", 
                size = 2) + 
      geom_line(data = refCases, 
                aes(x=`Days Since the First Case (in Mexico)`, 
                    y = Largest), 
                color = "gray", 
                size = 2)  +
      geom_point(data = place, 
                 aes(x=`Days Since the First Case (in Mexico)`, 
                     y = `Cases per capita`), 
                 color = "orange",
                 size = 2) +
      ylab("Cases per capita")
  }) 
  
  
  # Single State Plot / Death Per Capita Plot ----
  
  output$deathPerCapitaPlot <- renderPlot({
    place <- mexico %>% 
      filter(`State Name` == input$refPlace) %>% 
      filter(!is.na(`Policy Index Adj Time Mobility`))
    
    ggplot() + 
      ggtitle("Cases through time") +
      theme_few(base_size = 25) +
      theme(legend.title = element_blank()) +
      geom_line(data = refDeaths, 
                aes(x=`Days Since the First Case (in Mexico)`, 
                    y = Smallest), 
                color = "gray", 
                size = 2) + 
      geom_line(data = refDeaths, 
                aes(x=`Days Since the First Case (in Mexico)`, 
                    y = Largest), 
                color = "gray", 
                size = 2)  +
      geom_point(data = place, 
                 aes(x=`Days Since the First Case (in Mexico)`, 
                     y = `Deaths per capita`), 
                 color = "orange",
                 size = 2) +
      ylab("Deaths per capita")
  }) 
  
  ##### multi-state plots#####
  
  
  plot1_dat <- reactive({
    req(input$state1)
    mexico %>% 
      filter(`State Name` == input$state1)
  })
  
  plot2_dat <- reactive({
    req(input$state2)
    mexico %>% 
      filter(`State Name` == input$state2) 
  })
  
  
  output$plot1 <- renderPlot({
    #browser()
    p <- ggplot(data = plot1_dat()) + 
      ggtitle("Beds Through Time") +
      theme_few(base_size = 20) +
      theme(legend.title = element_blank()) +
      geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
                     y = input$vars1[1]),
                 color = "red"
      ) +
      labs(y = "Measure")
    
    if(length(input$vars1) > 1){
      p <- p + geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
                              y = input$vars1[2]),
                          color = "green"
      )
    }else if(length(input$vars1) > 2){
      p <- p + geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
                              y = input$vars1[3]),
                          color = "orange"
      )
    }else if(length(input$vars1) > 3){
      p <- p + geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
                              y = input$vars1[4]),
                          color = "blue"
      )
    }
    p
  }) 
  
  output$plot2 <- renderPlot({
    
    #browser()
    p <- ggplot(data = plot2_dat()) + 
      ggtitle("Beds Through Time") +
      theme_few(base_size = 20) +
      theme(legend.title = element_blank()) +
      geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
                     y = input$vars2[1]),
                 color = "red"
      ) +
      labs(y = "Measure")
    
    if(length(input$vars2) > 1){
      p <- p + geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
                              y = input$vars2[2]),
                          color = "green"
      )
    }else if(length(input$vars2) > 2){
      p <- p + geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
                              y = input$vars2[3]),
                          color = "orange"
      )
    }else if(length(input$vars2) > 3){
      p <- p + geom_point(aes(x =`Days Since the First Case (in Mexico)`, 
                              y = input$vars2[4]),
                          color = "blue"
      )
    }
    p
  }) 
  
  
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
    
    leaflet(data = mexico_states) %>% 
      addProviderTiles("Esri.WorldGrayCanvas") %>% 
      addPolygons(
        weight = .5,
        opacity = 1,
        fillOpacity = 0.5,
        label = ~ADMIN_NAME
      )
  })
}

shinyApp(ui, server)