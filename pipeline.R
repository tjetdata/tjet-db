# remotes::install_github('matthewjrogers/rairtable', ref = 'dev')
### the development version handles multi-select fields without error
library(tidyverse)
library(rairtable)
library(RSQLite)
# library(parallel)

### the api key needs to be set only once, is saved to the local R environment
### if it needs to be set again, replace string and un-comment below to run once
# set_airtable_api_key("ACTUAL_API_KEY_GOES_HERE", install = TRUE)
# readRenviron("~/.Renviron")
# Sys.getenv("AIRTABLE_API_KEY")

### setup
base_id <- "appHsoHAemKITZgMF"
names(to_download) <- to_download <- c("Amnesties",
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
tableIDs <- c("Amnesties" = "amnestyID",
              "Trials" = "trialID",
              "Accused" = "accusedID",
              "TruthCommissions" = "truthcommissionID",
              "Reparations" = "reparationID",
              "Vettings" = "vettingID",
              "CountryYears" = "countryyearID",
              "Countries" = "ccode",
              "Transitions" = "transitionID",
              "Conflicts" = "conflict_id",
              "Dyads" = "dyad_id")

### download data from Airtable 
tjet <- lapply(to_download, function(table) {
  cat(table, "\n") 
  airtable(table, base_id) %>%
    read_airtable(id_to_col = TRUE)
})
tjet$Accused$trialID <- as.integer(tjet$Accused$trialID)
save(tjet, file = "tjet.RData")
# load("tjet.RData")

### prodDB tables
names(select_tables) <- select_tables <- names(tjet)[!names(tjet) %in% c("select_options", "metadata")]
db <- map(select_tables, function(tab_name) {
    # cat(tab_name, "\n", sep = "")
    select_vars <- tjet$metadata %>%
      filter(incl_prod == 1 & 
               table_name == tab_name & 
               incl_data != "transform: multiple") %>%
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
db[select_tables] <- map(select_tables, function(tab_name) {
  fields <- tjet$metadata %>%
    filter(incl_prod == 1 & 
             incl_data == "checkmark to binary" & 
             table_name == tab_name) %>%
    select(field_name) %>% 
    unlist(use.names = FALSE)
  db[[tab_name]][, fields] <- map(db[[tab_name]][, fields], checkbox_to_binary)
  return(db[[tab_name]])
})

### multiselect fields
db[["labels"]] <- tjet$select_options %>%
  select(labelID, label) %>% 
  tibble()

multi_selects <- map(select_tables, function(tab_name) {
  names(fields) <- fields <- tjet$metadata %>%
    filter(incl_prod == 1 & 
             incl_data == "transform: multiple" & 
             table_name == tab_name) %>%
    select(field_name) %>%
    unlist(use.names = FALSE)
  map(fields, function(field) {
    to_filter_on <- paste(field, "set", sep = "_")
    to_select <- paste(field, tab_name, sep = "_")
    tjet$select_options %>%
      filter(if_all(to_filter_on)) %>%
      select(all_of(c("labelID", to_select))) %>%
      unnest_longer(all_of(to_select)) %>%
      left_join(tjet[[tab_name]] %>%
                  select(all_of(c("airtable_record_id", tableIDs[[tab_name]]))),
                by = setNames("airtable_record_id", to_select)) %>%
      select(all_of(c("labelID", tableIDs[[tab_name]]))) %>%
      drop_na()
  })
}) %>% 
  unlist(recursive = FALSE) 
names(multi_selects) <- str_replace(names(multi_selects), fixed("."), "_")
db <- c(db, multi_selects)



### FROM HERE

tjet$metadata %>%
  filter(incl_prod == 1 & 
           table_name != "select_options") %>%
  select(-airtable_record_id, -ID, -table_ID, -field_ID, -last_modified, -created, -field_description, -field_options) %>%
  select(field_type, incl_data) %>%
  table()

tjet$metadata %>%
  filter(incl_prod == 1 & 
           incl_data == "include as key" &
           table_name != "select_options" & 
           field_type %in% c("multipleLookupValues", "multipleRecordLinks")) %>%
  select(table_name, field_name, field_type)



db$Trials %>% 
  select(trialID, ccode_Victim1) %>% 
  unnest_wider(ccode_Victim1, names_sep = "", simplify = TRUE) %>% 
  rename(ccode_Victim1 = "ccode_Victim11") 
# %>%
  # select(ccode) %>% unlist() %>% class()


# 12        Amnesties         ccode multipleLookupValues
# 2       Reparations         ccode multipleLookupValues
# 7          Vettings         ccode multipleLookupValues
# 9      CountryYears         ccode multipleLookupValues
# 10 TruthCommissions         ccode multipleLookupValues
# 11           Trials   ccode_Trial multipleLookupValues
# 4            Trials ccode_Accused multipleLookupValues



# 1            Trials  ccode_Crime1 multipleLookupValues
# 3            Trials ccode_Victim1 multipleLookupValues
# 5            Trials ccode_Victim3 multipleLookupValues
# 6            Trials  ccode_Crime3 multipleLookupValues
# 8            Trials ccode_Victim2 multipleLookupValues
# 13           Trials  ccode_Crime2 multipleLookupValues



### TO DO
## - turn multi-select fields into dummies for data downloads (needs a consistent naming scheme)
## - set all PKs and FKs in DB (how to deal with multipleLookupValues & multipleRecordLinks?)

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
