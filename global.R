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