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

refIndexTimeMob <- mexico %>% 
  filter(!is.na(`Policy Index Adj Time Mobility`)) %>% 
  group_by(`Days Since the First Case (in Mexico)`) %>% 
  summarise(Smallest = min(`Policy Index Adj Time Mobility`, na.rm=TRUE), 
            Largest = max(`Policy Index Adj Time Mobility`, na.rm=TRUE))

refCases <- mexico %>% 
  filter(!is.na(`Cases per capita`)) %>% 
  group_by(`Days Since the First Case (in Mexico)`) %>% 
  summarise(Smallest = min(`Cases per capita`, na.rm=TRUE), 
            Largest = max(`Cases per capita`, na.rm=TRUE))

refDeaths <- mexico %>% 
  filter(!is.na(`Deaths per capita`)) %>% 
  group_by(`Days Since the First Case (in Mexico)`) %>% 
  summarise(Smallest = min(`Deaths per capita`, na.rm=TRUE), 
            Largest = max(`Deaths per capita`, na.rm=TRUE))