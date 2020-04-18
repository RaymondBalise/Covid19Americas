source("global.R")

# analysis data
analysis <- read_rds("data/analysis.rds") %>% 
  arrange(place, `Days Since the first case (in Mexico)`) %>% 
  mutate(place = factor(place)) %>% 
  mutate(place = factor(place, levels = rev(levels(place))))
  
# reference states
ref <- analysis %>% 
  filter(place %in% c("Chiapas", "Nuevo Leon")) %>% 
  filter(!is.na(`Index Adjusted for Time`))

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
    column(width = 10, offset = 1,
      fluidRow(
        selectInput(inputId = "refPlace",
                    label = "Select a State:",
                    choices = mexico$place)
      ),
      fluidRow(
        plotOutput("refPlot")
      ),
      fluidRow(
        plotOutput("casePlot")
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
  
  output$refPlot <- renderPlot({
    # should be reactive
    place <- analysis %>% 
      filter(place == input$refPlace) %>% 
      filter(!is.na(`Index Adjusted for Time`))

    ggplot() + 
      ggtitle("Index through time") +
      theme_few(base_size = 25) +
      geom_line(data = place, 
                aes(x=`Days Since the first case (in Mexico)`, 
                    y = `Index Adjusted for Time`, 
                    group = place), 
                color = "orange",
                size = 2) +
      geom_line(data = ref, 
                aes(x=`Days Since the first case (in Mexico)`, 
                    y = `Index Adjusted for Time`, 
                    group = place), 
                color = "gray", 
                size = 2) +
      geom_text(data = ref, x = 49, y = 17, label = "Chiapas", color = "gray") +
      geom_text(data = ref, x = 48.5, y = 45, label = "Nuevo Leon", color = "gray") +
      ylim (0, 45) 
  }) 
  
  output$casePlot <- renderPlot({
    place <- analysis %>% 
      filter(place == input$refPlace) %>% 
      filter(!is.na(`Index Adjusted for Time`))
    
  ggplot() + 
    ggtitle("Cases through time") +
    theme_few(base_size = 25) +
    theme(legend.title = element_blank()) +
    geom_point(data = place, 
               aes(x=`Days Since the first case (in Mexico)`, 
                   y = `Cases per million inhabitants`, group = place), 
               color = "orange") +
    geom_smooth(data = ref, 
                aes(x=`Days Since the first case (in Mexico)`, 
                    y = `Cases per million inhabitants`, group = place),
                method = 'loess',
                formula = 'y ~ x', 
                color = "gray",
                se = FALSE, 
                size = 2) +
    geom_text(data = ref, x = 49, y = 7, label = "Chiapas", color = "gray") +
    geom_text(data = ref, x = 48.5, y = 22, label = "Nuevo Leon", color = "gray")
}) 
}

shinyApp(ui, server)