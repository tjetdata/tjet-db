# remotes::install_github('matthewjrogers/rairtable', ref = 'dev')
### development versions handles multi-select fields without error
library(tidyverse)
library(rairtable)
library(RSQLite)

### the api key needs to be set only once, and is saved to the local R environment
### if it needs to be set again, replace string below accordingly and uncomment
# set_airtable_api_key("ACTUAL_API_KEY_GOES_HERE", install = TRUE)
# readRenviron("~/.Renviron")
# Sys.getenv("AIRTABLE_API_KEY")

### download data from Airtable 
base_id <- "appHsoHAemKITZgMF"
to_download <- c("Amnesties" = "amnesties_table_full", 
  "Trials" = "trials_table_full", 
  "Accused" = "accused_table_full",
  "Truth Commissions" = "tcs_table_full", 
  "Reparations" = "reparations_table_full", 
  "Countries" = "countries_table",
  "Transitions" = "cy_table",
  "metadata" = "metadata_table")
tjet <- lapply(names(to_download), function(table) {
  airtable(table, base_id, view = to_download[table]) %>%
    read_airtable(id_to_col = TRUE)
})
save(tjet, file = "tjet.RData")
# load("tjet.RData")

## have to deal with 
## - multi-select fields: either turn into binary in Airtable or transform in R
## - linked record fields: make sure the needed data is included, as linked record fields downlaod as the record identifiers 

