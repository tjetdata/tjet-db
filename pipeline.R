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

lapply(tjet, dim)

rename <- function(names_to_change) {
  str_replace(
    str_replace(
      str_to_lower(
        names_to_change
      ), fixed("-"), "")
    , fixed(" "), ""
  )
}
names(tjet) <- rename(names(to_download))
save(tjet, file = "tjet.RData")
# load("tjet.RData")

### subsetting tables
new <- tjet$metadata %>% 
  filter(incl_prod == 1 & incl_data != "decide" & table_name != "Vetting") %>%
  select(table_name) %>%
  unlist(use.names = FALSE) %>%
  unique() %>%
  rename() %>%
  lapply(FUN = function(tab_name) {
    cat(tab_name, ":\n", sep = "")
    select_vars <- tjet$metadata %>% 
      mutate(table_name = rename(table_name)) %>% 
      filter(incl_prod == 1 & incl_data != "decide" & table_name == tab_name) %>% 
      select(field_name) %>%
      unlist(use.names = FALSE)
    print(select_vars[!(select_vars %in% names(tjet[[tab_name]]))])
    tjet[[tab_name]] %>% 
      select(all_of(select_vars))
  })
  
### TO DO

# in TCs not coming through... why?
# `grantAmnesty`, `neverOperated`, `yearBeginOperatingUnknown`, and `yearPassedUnknown`

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
