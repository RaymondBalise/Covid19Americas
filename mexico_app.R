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
                         choices = mexico$place)
           ),
           fluidRow(
             plotOutput("plot1")
           )
    ),
    column(width = 6,
           fluidRow(
             selectInput(inputId = "state2",
                         label = "Select a State:",
                         choices = mexico$place)
           ),
           fluidRow(
             plotOutput("plot2")
           )
    )
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
}

shinyApp(ui, server)