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
tjet <- lapply(to_download, function(table) {
  cat(table, "\n") 
  airtable(table, base_id) %>%
    read_airtable(id_to_col = TRUE)
})
save(tjet, file = "tjet.RData")
# load("tjet.RData")

### prodDB tables
names(select_tables) <- select_tables <- names(tjet)[-length(tjet)]
db <- map(select_tables, function(tab_name) {
    # cat(tab_name, "\n", sep = "")
    select_vars <- tjet$metadata %>%
      filter(incl_prod == 1 & table_name == tab_name) %>%
      select(field_name) %>%
      unlist(use.names = FALSE)
    select_vars <- c("airtable_record_id", select_vars)
    missing_cols <- select_vars[!(select_vars %in% names(tjet[[tab_name]]))]
    tjet[[tab_name]] %>%
      ### adding empty fields as NA columns for now until they are recoded by RAs
      mutate(!!!setNames(rep(NA, length(missing_cols)), missing_cols)) %>%
      select(all_of(select_vars)) %>%
      tibble()
})

### transforming checkboxes to binary
checkbox_to_binary <- function(col) {
  ifelse(is.na(col), 0, 1)
}
names(select_tables) <- select_tables <- names(db)[-length(db)]
db[select_tables] <- map(select_tables, function(tab) {
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
## don't need this anymore, now that multiselects are turned into separate tables 
## but keeping sample code for now
# make_named_list <- function(lst) {
#   if(!is.null(lst))
#     names(lst) <- lst %>%
#       str_replace_all(fixed(" "), "_")
#   return(lst)
# }
# db$Reparations %>% 
#   select(reparationID, legalBasis) %>% 
#   rowwise() %>%
#   mutate(legalBasis = list(make_named_list(legalBasis))) %>%
#   ungroup() %>%
#   unnest_wider(legalBasis, names_sep = "_", simplify = FALSE) %>%
#   mutate(legalBasis_Domestic_law = ifelse(is.na(legalBasis_Domestic_law), 0, 1)) %>%
#   print(n = Inf)

map(select_tables, function(tab) {
  fields <- tjet$metadata %>%
    filter(incl_prod == 1 & 
             incl_data == "transform: multiple" & 
             table_name == tab) %>%
    select(field_name) %>%
    unlist(use.names = FALSE)
  tab <- "Amnesties"
  
### FROM HERE
  
  db$select_options %>%
    filter(enactedHow_set) %>%
    select(label, pkey, enactedHow_Amnesties) %>%
    print(n = 15)
  
})

## checking transformations to perform
tjet$metadata %>%
  filter(incl_prod == 1) %>%
  select(-airtable_record_id, -ID, -table_ID, -field_ID, -last_modified, -created, -field_description, -field_options) %>%
  select(field_type, incl_data) %>%
  table()

### TO DO
## - for browse, we need separate tables for one-to-many relationships (i.e. multiselect options) 
## - turn multi-select fields into dummies for data downloads 
##   - needs a consistent naming scheme
## - set all PKs and FKs in DB
##   - any special cosniderations for multipleLookupValues & multipleRecordLinks

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
