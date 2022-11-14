# remotes::install_github('matthewjrogers/rairtable', ref = 'dev')
### development versions handles multi-select fields without error
library(tidyverse)
library(rairtable)
library(RSQLite)

### the api key needs to be set only once, is saved to the local R environment
### if it needs to be set again, replace string and un-comment below accordingly
# set_airtable_api_key("ACTUAL_API_KEY_GOES_HERE", install = TRUE)
# readRenviron("~/.Renviron")
# Sys.getenv("AIRTABLE_API_KEY")

### download data from Airtable 
base_id <- "appHsoHAemKITZgMF"
to_download <- c(
  "Amnesties" = "export", 
  "Trials" = "export", 
  "Accused" = "export",
  "Truth Commissions" = "export", 
  "Reparations" = "export", 
  "Country-Years" = "cy_table", 
  "Countries" = "countries_table", 
  "metadata" = "metadata_table") 
tjet <- lapply(names(to_download), function(table) {
  airtable(table, base_id, view = to_download[table]) %>%
    read_airtable(id_to_col = TRUE)
})
save(tjet, file = "tjet.RData")
# load("tjet.RData")

### TO DO

## have to deal with 
## - multi-select fields: either turn into binary in Airtable or transform in R
## - linked record fields: make sure the needed data is included, 
##   as linked record fields download as the record identifiers 

## variable transformations 
## - binary variables from checkbox fields

## create and save SQLite DB
