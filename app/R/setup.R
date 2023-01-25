library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(leaflet)
# library(cshapes)
# reactlog::reactlog_enable()

# country_shapes <- readRDS("data/shapes.rds")

country_shapes <- cshapes::cshp(date = as_date("2019-12-31"), 
              useGW = TRUE, dependencies = TRUE) %>% 
  select(-capname, -caplong, -caplat, -b_def)

country_names <- sort(country_shapes$country_name)
