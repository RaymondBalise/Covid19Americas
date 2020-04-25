library(shiny)

library(conflicted)

suppressMessages(conflict_prefer("filter", "dplyr"))
suppressPackageStartupMessages(library(tidyverse))

library(shinythemes)

suppressMessages(conflict_prefer("last_plot", "plotly"))
suppressMessages(conflict_prefer("layout", "plotly"))
library(plotly)

library(ggthemes)

library(leaflet)
library(sp)

mexico <- read_rds("./data/analysis.rds")

mexico_states <- read_rds("./geo/mexico_states.rds")



makeLimits <- function(data, x, grouping) {
  data %>% 
    filter(!is.na({{x}})) %>% 
    group_by({{grouping}}) %>% 
    summarise(Smallest = min({{x}}, na.rm=TRUE), 
              Average = mean({{x}}, na.rm=TRUE),
              Largest = max({{x}}, na.rm=TRUE))
}

refIndexTimeMob <- makeLimits(mexico, 
                              `Policy Index Adj Time Mobility`, 
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


latest <- max(mexico[ "Days Since the First Case (in Mexico)"])
polar <- mexico %>% 
  filter(`Days Since the First Case (in Mexico)`== latest) %>% 
  select(`State Name`, School_Closure,
         Business_Closure,
         Public_Events_Cancelled,
         Public_Transit_Suspended,
         Information_Campaign,
         Internal_Travel_Control,
         International_Travel_Controls) %>% 
  pivot_longer(-`State Name`, names_to = "Restriction", values_to = "amount")
