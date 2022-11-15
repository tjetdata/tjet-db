# remotes::install_github('matthewjrogers/rairtable', ref = 'dev')
### development versions handles multi-select fields without error
library(tidyverse)
library(rairtable)
library(RSQLite)
library(parallel)

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
names(tjet) <- str_replace(
  str_replace(
    str_to_lower(
      names(to_download)
    ), fixed("-"), "")
  , fixed(" "), "")
save(tjet, file = "tjet.RData")
# load("tjet.RData")

### TO DO

## have to deal with 
## - multi-select fields: either turn into binary in Airtable or transform in R
## - linked record fields: make sure the needed data is included, 
##   as linked record fields download as the record identifiers 
## - variable transformations 
##   - binary variables from checkbox fields
## - clean up dataframes so that they can be added to the sqlite db 
## - then complete below

## create SQLite DB
# file.remove("tjet.db")
conn <- dbConnect(RSQLite::SQLite(), "tjet.db")
lapply(names(tjet)[7:8], function(table_name) {
  print(table_name)
  dbWriteTable(conn, table_name, tjet[[table_name]])
})
dbListTables(conn) 
dbListFields(conn, "metadata")
dbReadTable(conn, "metadata")
dim(dbGetQuery(conn, "SELECT * FROM metadata"))

result <- dbSendQuery(conn, "SELECT * FROM metadata")
dbFetch(result)
dbClearResult(result)
dbDisconnect(conn)
