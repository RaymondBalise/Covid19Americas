library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly)
library(leaflet)


mexico <- read_rds("./data/analysis.rds")

mexico_states <- read_rds("./geo/mexico_states.rds")
