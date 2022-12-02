# devtools::install_github("bergant/airtabler")
# library(airtabler)
# remotes::install_github('matthewjrogers/rairtable', ref = 'dev')
### development version handles multi-select fields without error
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
names(to_download) <- to_download <- c(
  "Amnesties",
  "Trials",
  "Accused",
  "TruthCommissions",
  "Reparations",
  "Vettings",
  "CountryYears",
  "Countries",
  "Transitions",
  "Conflicts",
  "Dyads",
  "select_options",
  "metadata")
tjet[13] <- lapply(to_download[13], function(table) {
  cat(table, "\n") 
  airtable(table, base_id) %>%
    read_airtable(id_to_col = TRUE)
})
# rename_labels <- function(names_to_change) {
#   str_replace(
#     str_replace(
#       str_to_lower(
#         names_to_change
#       ), fixed("-"), "")
#     , fixed(" "), ""
#   )
# }
# names(tjet) <- rename_labels(names(to_download))
save(tjet, file = "tjet.RData")
# load("tjet.RData")

### subsetting tables
db <- map(names(tjet)[-length(tjet)], function(tab_name) {
    # cat(tab_name, "\n", sep = "")
    select_vars <- tjet$metadata %>%
      filter(incl_prod == 1 & table_name == tab_name) %>%
      select(field_name) %>%
      unlist(use.names = FALSE)
    missing_cols <- select_vars[!(select_vars %in% names(tjet[[tab_name]]))]
    tjet[[tab_name]] %>%
      ### adding empty fields as NA columns for now
      mutate(!!!setNames(rep(NA, length(missing_cols)), missing_cols)) %>%
      select(all_of(select_vars)) %>%
      tibble()
  })
names(db) <- names(tjet)[-length(tjet)]
# object.size(db)

### checkboxes to binary
checkbox_to_binary <- function(col) {
  ifelse(is.na(col), 0, 1)
}
names(tables) <- tables <- names(db)[-length(db)]
db[tables] <- map(tables, function(tab) {
  fields <- tjet$metadata %>%
    filter(incl_prod == 1 & 
             incl_data == "checkmark to binary" & 
             table_name == tab) %>%
    select(field_name) %>% 
    unlist(use.names = FALSE)
  db[[tab]][, fields] <- map(db[[tab]][, fields], checkbox_to_binary)
  return(db[[tab]])
})

### multiselects
# make_named_list <- function(lst) {
#   if(!is.null(lst)) 
#     names(lst) <- lst %>% 
#       str_replace_all(fixed(" "), "_")
#   return(lst)
# }

### FROM HERE

tjet$metadata %>%
  filter(incl_prod == 1 & incl_data != "include as is") %>%
  select(-airtable_record_id, -ID, -table_ID, -field_ID, -last_modified, -created, -field_description, -field_options) %>%
  select(field_type, incl_data) %>%
  table()

### TO DO
## - automate variable transformations for all 
##   - binary variables from checkbox fields
##   - multi-select fields into dummies for data downloads 
##     - come up with naming scheme
##   - for relational DB for browse, we need a separate table for multiselect options 
##     - should create this in Airtable 

### create SQLite DB
## file.remove("tjet.db")
# conn <- dbConnect(RSQLite::SQLite(), "tjet.db")
# lapply(names(tjet)[7:8], function(table_name) {
#   print(table_name)
#   dbWriteTable(conn, table_name, tjet[[table_name]])
# })
# dbListTables(conn) 
# dbListFields(conn, "metadata")
# dbReadTable(conn, "metadata")
# dim(dbGetQuery(conn, "SELECT * FROM metadata"))
# 
# result <- dbSendQuery(conn, "SELECT * FROM metadata")
# dbFetch(result)
# dbClearResult(result)
# dbDisconnect(conn)
