library(shiny)
library(conflicted)
suppressMessages(conflict_prefer("filter", "dplyr"))
suppressPackageStartupMessages(library(tidyverse))
library(shinythemes)
suppressMessages(conflict_prefer("last_plot", "plotly"))
suppressMessages(conflict_prefer("layout", "plotly"))
library(plotly)
library(DT)
suppressMessages(conflict_prefer("dataTableOutput", "DT"))
suppressMessages(conflict_prefer("renderDataTable", "DT"))
library(ggthemes)
library(shinydashboard)
library(leaflet)
library(sp)
library(reactable)

mexico <- read_rds("./data/analysis.rds")

latest <- max(mexico[ "Days Since the First Case (in Mexico)"])


mexico_states <- read_rds("./geo/mexico_states.rds") 
mexico_states <- sf::st_as_sf(mexico_states) %>% 
  mutate(ADMIN_NAME = case_when(ADMIN_NAME == "Mexico" ~ "Estado de México",
                                ADMIN_NAME =="Distrito Federal" ~ "Ciudad de México",
                                ADMIN_NAME == "Michoacan" ~ "Michoacán",
                                ADMIN_NAME == "Nuevo Leon" ~ "Nuevo León",
                                ADMIN_NAME == "Queretaro" ~ "Querétaro",
                                ADMIN_NAME == "San Luis Potosi" ~ "San Luis Potosí",
                                ADMIN_NAME == "Yucatan" ~ "Yucatán",
                                TRUE ~ as.character(ADMIN_NAME)))

stateNames <- mexico %>% 
  select(`State Name`) %>% 
  distinct() %>% 
  arrange(`State Name`)

makeLimits <- function(data, x, grouping) {
  data %>% 
    filter(!is.na({{x}})) %>% 
    group_by({{grouping}}) %>% 
    summarise(Smallest = min({{x}}, na.rm=TRUE), 
              Average = mean({{x}}, na.rm=TRUE),
              Largest = max({{x}}, na.rm=TRUE))
}

refIndexTime <- makeLimits(mexico, 
                              `Policy Index Adjusted for Time`, 
                              `Days Since the First Case (in Mexico)`)


refCasesPerCapita <- makeLimits(mexico, 
                              `Cases per capita`,
                              `Days Since the First Case (in Mexico)`)


refDeathPerCapita <- makeLimits(mexico, 
                                `Deaths per capita`,
                                `Days Since the First Case (in Mexico)`)


refMobilityIndex <- makeLimits(mexico, 
                                `Mobility Index`,
                                `Days Since the First Case (in Mexico)`)


