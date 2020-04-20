library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly)

library(ggthemes)

library(leaflet)


mexico <- read_rds("./data/analysis.rds")

mexico_states <- read_rds("./geo/mexico_states.rds")

ref <- mexico %>% 
  filter(`State Name` %in% c("Chiapas", "Nuevo León")) %>% 
  filter(!is.na(`Policy Index Adjusted for Time`))

refIndex <- mexico %>% 
  filter(!is.na(`Policy Index Adjusted for Time`)) %>% 
  group_by(`Days Since the First Case (in Mexico)`) %>% 
  summarise(Smallest = min(`Policy Index Adjusted for Time`, na.rm=TRUE), 
            Largest = max(`Policy Index Adjusted for Time`, na.rm=TRUE))

refCases <- mexico %>% 
  filter(!is.na(`Cases per capita`)) %>% 
  group_by(`Days Since the First Case (in Mexico)`) %>% 
  summarise(Smallest = min(`Cases per capita`, na.rm=TRUE), 
            Largest = max(`Cases per capita`, na.rm=TRUE))
