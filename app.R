source("global.R")

ui <- navbarPage(
  
  theme = shinytheme("simplex"),
  title = h4("COVID-19 Mexico"),
  # ----------------------
  # TAB 1 : HOME PAGE
  # ----------------------
  tabPanel(
    h4("Home"),
    fluidRow(
      h1(strong("COVID-19 Mexico"), align = "center", style = "color: black")
    ),
    br(),
    fluidRow(
      column(width = 12, align = "center",
        img(src = "ttsp.jpg", height = 500, width = 800)
        )
    )
    
  ),
  # ----------------------
  # TAB 2 : PLOTS
  # ----------------------
  tabPanel(
    h4("Plots"),
    column(width = 6,
           fluidRow(
             selectInput(inputId = "state1",
                         label = "Select a State:",
                         choices = unique(mexico$place),
                         selected = "Chiapas")
           ),
           fluidRow(
             plotOutput("plot1")
           )
    ),
    column(width = 6,
           fluidRow(
             selectInput(inputId = "state2",
                         label = "Select a State:",
                         choices = unique(mexico$place),
                         selected = "Veracruz")
           ),
           fluidRow(
             plotOutput("plot2")
           ),
           fluidRow(
             plotlyOutput("refplot")
           )
    )
  ),
  
  # ----------------------
  # TAB 3 : MAP
  # ----------------------
  
  tabPanel(
    h4("Map"),
    leafletOutput("map", height = 900)
  )
)

server <- function(input, output, session) {
  place1 <- reactive({
    mexico %>% 
      filter(place == input$state1)
  })
  place2 <- reactive({
    mexico %>% 
      filter(place == input$state2)
  })
  
  output$plot1 <- renderPlot({
    ggplot(place1()) +
      geom_bar(aes(`Total Cases`)) +
      theme_minimal()
  }) 
  
  output$plot2 <- renderPlot({
    ggplot(place2()) +
      geom_bar(aes(`Total Cases`)) +
      theme_minimal()
  }) 
  
  output$refplot <- renderPlotly({
    state1_label <- input$state1
    state2_label <- input$state2
    label1_pos <- max(place1()$`Index Adjusted for Time`, na.rm = TRUE)
    label2_pos <- max(place2()$`Index Adjusted for Time`, na.rm = TRUE)
    
    x <- ggplot() + 
      #theme_few() +
      geom_line(data = place1(), 
                aes(x=`Days Since the first case (in Mexico)`, 
                    y = `Index Adjusted for Time`, group = place), color = "orange") +
      geom_text(data = place1(), x = 45, y = label1_pos + 5, 
                label = state1_label, color = "orange") +
      geom_line(data = place2(), 
                aes(x=`Days Since the first case (in Mexico)`, 
                    y = `Index Adjusted for Time`, group = place), color = "gray") +
      geom_text(data = place2(), x = 45, y = label2_pos + 5, 
                label = state2_label, color = "gray") +
      ylim (0, 45) + 
      theme_minimal()
    
    ggplotly(x)
  }) 
  
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