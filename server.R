


server <- function(input, output, session) {
  #documentation
  output$slide_png <- renderUI({
    
    fluidRow(img(
      src = paste0("slideshow_", input$slide,".png"),
      width = 550,
      height = 400
    )
    )
    
  })
  
  output$slide_pdf <- renderUI({
    
    fluidRow(
      tags$a(
        href = paste0('slideshow_',input$slide,'.pdf'),
        class = "btn",
        icon("download"),
        'haga click aquí para descargar',
        style = "margin: auto; width: 550px;color: green;
                             font-size:18px"
      )
      
    )
  })
  
  output$pr_png <- renderUI({
    fluidRow(img(
      src = paste0("press_release_", input$pr,".png"),
      width = 650,
      height = 400
    ))
  })
  
  output$pr_pdf <- renderUI({
    fluidRow(
      tags$a(
        href = paste0("press_release_", input$pr, ".pdf"),
        class = "btn",
        icon("download"),
        'haga click aquí para descargar',
        style = "margin: auto; width: 650px;color: green;
                             font-size:18px"
      )
    )
  })
  # Summary plots / Index ----
  
  output$summaryIndexPlot <- renderPlotly({
    gg <-
      mexico %>%
      mutate(`Policy Index Adj Time` = round(`Policy Index Adjusted for Time`, 1)) %>%
      ggplot() +
      #ggtitle("Policy Index Adjusted for Time") +
      geom_line(
        data = refIndexTime, # fix this name
        aes(x = `Days Since the First Case (in Mexico)`,
            y = Smallest),
        color = "gray"
      ) +
      geom_line(
        data = refIndexTime,
        aes(x = `Days Since the First Case (in Mexico)`,
            y = Average),
        color = "black"
      ) +
      geom_line(
        data = refIndexTime,
        aes(x = `Days Since the First Case (in Mexico)`,
            y = Largest),
        color = "gray"
      ) +
      theme_few(base_size = 20) +
      theme(
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent")
      ) +
      geom_point(
        aes(
          x = `Days Since the First Case (in Mexico)`,
          y = `Policy Index Adj Time`,
          group = `State Name`,
          color = `State Name`,
          shape = 4,
        ),
        size = 2
      ) +
      scale_shape_identity() +
      xlab("Días desde el primer caso en México") +
      ylab("Índice de política")
    
    
    ggplotly(gg, tooltip = c("x", "y", "group"))
  })
  
  mexico_latest <- mexico %>%
    group_by(`State Name`) %>%
    slice(which.max(as.Date(Date, '%Y-%m-%d')))
  
  output$index_table <- renderDataTable({
    mexico_latest %>%
      mutate(`Policy Index Adj Time` = round(`Policy Index Adjusted for Time`, 1)) %>%
      select(`State Name`, `Policy Index Adj Time`) %>%
      rename(`Índice de política pública ajustado por tiempo` = `Policy Index Adj Time`) %>%
      rename(Estado = `State Name`) %>%
      datatable(.,
                rownames = FALSE)
  })
  
  # Summary plots / Mobility ----
  
  output$summaryMobilityPlot <- renderPlotly({
    gg <-
      mexico %>%
      mutate(`Mobility Index` = round(`Mobility Index`, 1)) %>%
      ggplot() +
      #ggtitle("Mobility Index") +
      geom_line(
        data = refMobilityIndex, 
        aes(x = `Days Since the First Case (in Mexico)`,
            y = Smallest),
        color = "gray"
      ) +
      geom_line(
        data = refMobilityIndex,
        aes(x = `Days Since the First Case (in Mexico)`,
            y = Average),
        color = "black"
      ) +
      geom_line(
        data = refMobilityIndex,
        aes(x = `Days Since the First Case (in Mexico)`,
            y = Largest),
        color = "gray"
      ) +
      theme_few(base_size = 20) +
      theme(
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent")
      ) +
      geom_point(
        aes(
          x = `Days Since the First Case (in Mexico)`,
          y = `Mobility Index`,
          group = `State Name`,
          color = `State Name`,
          shape = 4,
        ),
        size = 2
      ) +
      scale_shape_identity() +
      xlab("Días desde el primer caso en México") +
      ylab("Movilidad")
    
    
    ggplotly(gg, tooltip = c("x", "y", "group"))
  })
  
  mexico_latest <- mexico %>%
    group_by(`State Name`) %>%
    slice(which.max(as.Date(Date, '%Y-%m-%d')))
  
  output$index_table <- renderDataTable({
    mexico_latest %>%
      mutate(`Policy Index Adj Time` = round(`Policy Index Adjusted for Time`, 1)) %>%
      select(`State Name`, `Policy Index Adj Time`) %>%
      rename(`Índice de política pública ajustado por tiempo` = `Policy Index Adj Time`) %>%
      rename(Estado = `State Name`) %>%
      datatable(.,
                rownames = FALSE)
  })
  
  
  
  
  
  # Summary plots / DeathPerCapita ----
  
  output$summaryDeathsPerCapitaPlot <- renderPlotly({
    gg <-
      mexico %>%
      mutate(`Deaths per capita * 1000000` = round(`Deaths per capita` * 1000000, 1)) %>%
      ggplot() +
      #ggtitle("Deaths per capita") +
      geom_line(
        data = refDeathPerCapita,
        aes(x = `Days Since the First Case (in Mexico)`,
            y = Smallest * 1000000),
        color = "gray"
      ) +
      geom_line(
        data = refDeathPerCapita,
        aes(x = `Days Since the First Case (in Mexico)`,
            y = Average * 1000000),
        color = "black"
      ) +
      geom_line(
        data = refDeathPerCapita,
        aes(x = `Days Since the First Case (in Mexico)`,
            y = Largest * 1000000),
        color = "gray"
      ) +
      theme_few(base_size = 20) +
      theme(
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent")
      ) +
      geom_point(
        aes(
          x = `Days Since the First Case (in Mexico)`,
          y = `Deaths per capita * 1000000`,
          group = `State Name`,
          color = `State Name`,
          shape = 21
        ),
        size = 2
      ) +
      scale_shape_identity() +
      ylab("Muertes per cápita") +
      xlab("Días desde el primer caso en México")
    
    ggplotly(gg, tooltip = c("x", "y", "group"))
  })
  
  output$death_table <- renderDataTable({
    mexico %>%
      select(Date, `State Name`, `Deaths per capita`) %>%
      group_by(`State Name`) %>%
      slice(which.max(as.Date(Date, '%Y-%m-%d'))) %>%
      mutate(`Deaths per capita * 1000000` = round(`Deaths per capita` * 1000000, 1))  %>%
      select(`State Name`, `Deaths per capita * 1000000`) %>%
      rename(`Muertes per capita * 1,000,000` = `Deaths per capita * 1000000`) %>%
      rename(Estado = `State Name`) %>%
      datatable(.,
                rownames = FALSE)
  })
  
  
  
  
  # Single State Plot / Index ----
  
  output$indexAdjTimePlot <- renderPlotly({
    # should be reactive
    place <- mexico %>%
      filter(`State Name` == input$refPlace) %>%
      filter(!is.na(`Policy Index Adjusted for Time`))
    
    ggplot() +
      ggtitle("Índice de política pública ajustado por tiempo") +
      theme_few(base_size = 20) +
      theme(
        plot.title = element_text(size = 20),
        panel.background = element_rect(fill = "transparent"),
        # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent")
      ) +
      geom_line(
        data = refIndexTime,
        aes(x = `Days Since the First Case (in Mexico)`,
            y = Smallest),
        color = "gray",
        size = 1
      ) +
      geom_line(
        data = refIndexTime,
        aes(x = `Days Since the First Case (in Mexico)`,
            y = Largest),
        color = "gray",
        size = 1
      )  +
      geom_point(
        data = place,
        aes(x = `Days Since the First Case (in Mexico)`,
            y = `Policy Index Adjusted for Time`),
        color = "orange",
        size = 2,
        shape = 4
      ) +
      ylab("Índice")  +
      xlab("Días desde el primer caso en México")
  })
  
  # Single State Plot / Mobility Plot ----
  
  output$mobilityPlot <- renderPlotly({
    place <- mexico %>%
      filter(`State Name` == input$refPlace) %>%
      filter(!is.na(`Policy Index Adjusted for Time`))
    
    gg <- ggplot() +
      ggtitle("Movilidad") +
      theme_few(base_size = 20) +
      theme(
        legend.title = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"),
        # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent")
      ) +
      geom_line(
        data = refMobilityIndex,
        aes(x = `Days Since the First Case (in Mexico)`,
            y = Smallest),
        color = "gray",
        size = 1
      ) +
      geom_line(
        data = refMobilityIndex,
        aes(x = `Days Since the First Case (in Mexico)`,
            y = Largest),
        color = "gray",
        size = 1
      )  +
      geom_point(
        data = place,
        aes(x = `Days Since the First Case (in Mexico)`,
            y =  `Mobility Index`),
        color = "orange",
        size = 2
      ) +
      ylab("Movilidad") +
      xlab("Días desde el primer caso en México")
    expand_limits(x = 0, y = 0)
    
    ggplotly(gg, tooltip = c("x", "y", "group"))
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
    
    # input <-NULL
    # input$week <- "2020-05-01"
    
    mexico_latest <- mexico %>%
      group_by(`State Name`) %>%
      filter(Date == input$week)
    
    #join data to spdf
    # mexico_states_covid <- sp::merge(mexico_states, mexico_latest[c("State Name","Policy Index Adj Time Mobility")],
    #                                  by.x = "ADMIN_NAME", by.y = "State Name")
    #
    mexico_states_covid <-
      left_join(mexico_states,
                mexico_latest[c("State Name", "Policy Index Adjusted for Time")],
                by = c("ADMIN_NAME" = "State Name"))
    
    #bins <- c(0, 75, 150, 225, 300, 375, 450, 525, 600, Inf)
    pal <-
      colorBin("BrBG", domain = mexico_latest$`Policy Index Adjusted for Time`)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g índice de política",
      mexico_states_covid$ADMIN_NAME,
      mexico_states_covid$`Policy Index Adjusted for Time`
    ) %>% lapply(htmltools::HTML)
    
    leaflet(data = mexico_states_covid) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(
        fillColor = ~ pal(`Policy Index Adjusted for Time`),
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
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~ `Policy Index Adjusted for Time`,
        opacity = 0.7,
        title = NULL,
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