library(tidyverse)
load("tjet.RData")

names(select_tables) <- select_tables <- names(tjet)[!names(tjet) %in% c("select_options", "metadata")]
db <- map(c("Trials", "Accused", "TruthCommissions"), function(tab_name) {
    select_vars <- tjet$metadata %>%
      filter(incl_prod == 1 & table_name == tab_name) %>%
      select(field_name) %>%
      unlist(use.names = FALSE)
    first <- c(select_vars[str_detect(select_vars, fixed("ID"))], select_vars[str_detect(select_vars, fixed("ccode"))])
    select_vars <- c(select_vars[select_vars %in% first], select_vars[!select_vars %in% first])
    tjet[[tab_name]] %>%
      tibble() %>%
      select(any_of(select_vars)) %>% 
      filter(invalid == 0) %>%
      select(-invalid)
})

db[["codebook"]] <- tjet$metadata %>% 
    filter(incl_prod == 1 & table_name %in% c("Trials", "Accused", "TruthCommissions")) %>% 
  select(table_name, field_name, field_type, field_description, field_options)
  
db[["labels"]] <- tjet$select_options %>%
  select(airtable_record_id, labelID, label) %>% 
  tibble()

### transforming checkboxes to binary
checkbox_to_binary <- function(col) {
  if_else(is.na(col), 0, 1)
}

db[select_tables] <- map(select_tables, function(tab_name) {
  fields <- tjet$metadata %>%
    filter(table_name == tab_name & incl_prod == 1 & 
             incl_data == "transform: checkmark to binary") %>%
    select(field_name) %>% 
    unlist(use.names = FALSE)
  fields <- fields[fields %in% names(db[[tab_name]])]
  db[[tab_name]][, fields] <- map(db[[tab_name]][, fields], checkbox_to_binary)
  return(db[[tab_name]])
})

### dealing with multipleLookupValues

### FROM HERE > 

db[c("Trials", "TruthCommissions")] <- map(c("Trials", "TruthCommissions"), function(tab_name) {
  fields <- tjet$metadata %>%
    filter(table_name == tab_name & 
             incl_prod == 1 &
             incl_data == "include as key" &
             field_type == "multipleLookupValues") %>%
    select(field_name) %>%
    unlist(use.names = FALSE)
  db[[tab_name]] %>%   
    unnest_wider(all_of(fields), names_sep = "", simplify = TRUE) %>%
    rename_with(.cols = all_of(paste(fields, "1", sep = "")), .fn = ~ fields)
}) 




names(db$TruthCommissions[unlist(map(db$TruthCommissions, class)) == "list"])
db$TruthCommissions$ucdpDyadID
names(db$Trials[unlist(map(db$Trials, class)) == "list"])
db$Trials$ucdpDyadID

### multiselect fields

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
      filter(.data[[to_filter_on]] == 1) %>%
      select(all_of(c("labelID", to_select))) %>%
      unnest_longer(all_of(to_select)) %>%
      left_join(tjet[[tab_name]] %>%
                  select(all_of(c("airtable_record_id", pkeys[[tab_name]]))),
                by = setNames("airtable_record_id", to_select)) %>%
      select(all_of(c("labelID", pkeys[[tab_name]]))) %>%
      drop_na()
  })
}) %>% 
  unlist(recursive = FALSE) 
names(multi_selects) <- str_replace(names(multi_selects), fixed("."), "_")
db <- c(db, multi_selects)




### dealing with keys in multipleRecordLinks
## the approaches below differ by whether the relationship is one-to-one or one-to-many
## should simplify the code below with one function

db[["Trials"]] <- db$Trials %>%
  unnest_longer(all_of(c("ucdpConflictID", "ucdpDyadID"))) %>%
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet$Conflicts %>% select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(airtable_record_id = "ucdpDyadID") %>%
  left_join(tjet$Dyads %>% select(airtable_record_id, dyad_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id",
         ucdpDyadID = "dyad_id")

## truth commissions have one-to-many links

db[["TruthCommissions_Conflicts"]] <- db$TruthCommissions %>% 
  select(truthcommissionID, ucdpConflictID) %>% 
  unnest_longer(ucdpConflictID) %>% 
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet$Conflicts %>% select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id") %>% 
  drop_na()

db[["TruthCommissions_Dyads"]] <- db$TruthCommissions %>% 
  select(truthcommissionID, ucdpDyadID) %>% 
  unnest_longer(ucdpDyadID) %>% 
  rename(airtable_record_id = "ucdpDyadID") %>%
  left_join(tjet$Dyads %>% select(airtable_record_id, dyad_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>% 
  rename(ucdpConflictID = "dyad_id") %>% 
  drop_na()

db[["TruthCommissions"]] <- db$TruthCommissions %>% 
  select(-ucdpConflictID, -ucdpDyadID)



### dummies from multi-select fields
## (may not need this if we can get it with SQL queries)
## sample code, would have to be expanded and generalized

# make_named_list <- function(lst) {
#   if(!is.null(lst))
#     names(lst) <- lst %>%
#       str_replace_all(fixed(" "), "_")
#   return(lst)
# }
# db$Reparations %>%
#   select(reparationID, legalBasis) %>%
#   # rowwise() %>%
#   # mutate(legalBasis = list(make_named_list(legalBasis))) %>%
#   # ungroup() %>%
#   unnest_wider(legalBasis, names_sep = "_", simplify = FALSE) %>%
#   mutate(legalBasis_1 = ifelse(is.na(legalBasis_1), 0, 1),
#          legalBasis_2 = ifelse(is.na(legalBasis_2), 0, 1),
#          legalBasis_3 = ifelse(is.na(legalBasis_3), 0, 1))
