library(tidyverse)
library(here)

load(here("data/tjet.RData"))
pkeys <- c(
  "Amnesties" = "amnestyID",
  "Trials" = "trialID",
  "Accused" = "accusedID",
  "TruthCommissions" = "truthcommissionID",
  "Reparations" = "reparationID",
  "Vettings" = "vettingID",
  "Countries" = "ccode",
  "Transitions" = "transitionID",
  "Conflicts" = "conflict_id",
  "Dyads" = "dyad_id"
)

### check metadata table for non-existing fields

names(tables) <- tables <- names(pkeys)
map(tables, function(tab) {
  exclude <- c("Amnesties", "Vetting", "Dyads", "Conflicts", "invalidExplain")
  meta <- tjet$metadata %>% 
    filter(table_name == tab) %>% 
    select(field_name) %>%
    unlist(use.names = FALSE)
  meta <- meta[!meta %in% exclude] 
  meta[!meta %in% names(tjet[[tab]])]
})

### prodDB tables

names(select_tables) <-
  select_tables <-
  names(tjet)[!names(tjet) %in% c("select_options", "metadata")]

db <- map(select_tables, function(tab_name) {
  select_vars <- tjet$metadata %>%
    filter(incl_prod == 1 &
             incl_data != "transform: multiple" &
             ## include these if creating dummies
             table_name == tab_name) %>%
    select(field_name) %>%
    unlist(use.names = FALSE)
  first <-
    c(select_vars[str_detect(select_vars, fixed("ID"))], select_vars[str_detect(select_vars, fixed("ccode"))])
  select_vars <-
    c(select_vars[select_vars %in% first], select_vars[!select_vars %in% first])
  missing_cols <-
    select_vars[!(select_vars %in% names(tjet[[tab_name]]))]
  tjet[[tab_name]] %>%
    tibble() %>%
    ### adding empty fields as NA columns for now until they are recoded by RAs
    mutate(!!!setNames(rep(NA, length(missing_cols)), missing_cols)) %>%
    select(all_of(select_vars))
})

names(drop_invalids) <-
  drop_invalids <- 
  c("Amnesties",
    "Trials",
    "Accused",
    "TruthCommissions",
    "Reparations",
    "Vettings")

db[drop_invalids] <- map(drop_invalids, function(tab_name) {
  db[[tab_name]] %>%
    ## will need to change this later to selecting only valids
    filter(invalid != 1 | is.na(invalid)) # %>% select(-invalid)
})

### transforming checkboxes to binary

checkbox_to_binary <- function(col) {
  ifelse(is.na(col), 0, 1)
}

db[select_tables] <- map(select_tables, function(tab_name) {
  fields <- tjet$metadata %>%
    filter(table_name == tab_name &
             incl_prod == 1 &
             incl_data == "transform: checkmark to binary") %>%
    select(field_name) %>%
    unlist(use.names = FALSE)
  db[[tab_name]][, fields] <-
    map(db[[tab_name]][, fields], checkbox_to_binary)
  return(db[[tab_name]])
})

### dealing with multipleLookupValues (& multipleRecordLinks)

names(select_tables) <- select_tables <- tjet$metadata %>%
  filter(
    incl_prod == 1 &
      incl_data == "include as key" &
      table_name != "select_options" &
      field_type == "multipleLookupValues"
  ) %>%
  select(table_name) %>%
  unlist(use.names = FALSE) %>%
  unique()

db[select_tables] <- map(select_tables, function(tab_name) {
  fields <- tjet$metadata %>%
    filter(
      table_name == tab_name &
        incl_prod == 1 &
        incl_data == "include as key" &
        field_type == "multipleLookupValues"
    ) %>%
    select(field_name) %>%
    unlist(use.names = FALSE)
  db[[tab_name]] %>%
    unnest_wider(all_of(fields), names_sep = "", simplify = TRUE) %>%
    rename_with(.cols = all_of(paste(fields, "1", sep = "")), .fn = ~ fields)
})

### ccodes for Crimes and Victims

crimes <- c("ccode_Crime1", "ccode_Crime2", "ccode_Crime3")
db[["Trials_Crimes"]] <- map(crimes, function(var) {
  db$Trials %>%
    select(all_of(c("trialID", var))) %>%
    rename(ccode_Crime = all_of(var)) %>%
    drop_na()
}) %>%
  bind_rows()

victims <- c("ccode_Victim1", "ccode_Victim2", "ccode_Victim3")
db[["Trials_Victims"]] <- map(victims, function(var) {
  db$Trials %>%
    select(all_of(c("trialID", var))) %>%
    rename(ccode_Victim = all_of(var)) %>%
    drop_na()
}) %>%
  bind_rows()

db[["Trials"]] <- db[["Trials"]] %>%
  select(!all_of(c(crimes, victims)))

### multiselect fields

db[["labels"]] <- tjet$select_options %>%
  select(labelID, label) %>%
  tibble()

multi_selects <- map(select_tables, function(tab_name) {
  # cat(tab_name)
  # tab_name = "Amnesties"
  names(fields) <- fields <- tjet$metadata %>%
    filter(incl_prod == 1 &
             incl_data == "transform: multiple" &
             table_name == tab_name) %>%
    select(field_name) %>%
    unlist(use.names = FALSE)
  map(fields, function(field) {
    # field = "whatCrimes"
    to_filter_on <- paste(field, "set", sep = "_")
    to_select <- paste(field, tab_name, sep = "_")
    tjet$select_options %>%
      filter(.data[[to_filter_on]] == 1) %>%
      select(all_of(c("labelID", to_select))) %>%
      unnest_longer(all_of(to_select)) %>%
      left_join(tjet[[tab_name]] %>%
                  select(all_of(c(
                    "airtable_record_id", pkeys[[tab_name]]
                  ))),
                by = setNames("airtable_record_id", to_select)) %>%
      select(all_of(c("labelID", pkeys[[tab_name]]))) %>%
      drop_na()
  })
}) %>%
  unlist(recursive = FALSE)
names(multi_selects) <-
  str_replace(names(multi_selects), fixed("."), "_")
db <- c(db, multi_selects)

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

### dealing with keys in multipleRecordLinks
## the approaches below differ by whether the relationship is one-to-one or one-to-many
## should simplify the code below with one function

# db[["CountryYears"]] <- db$CountryYears %>%
#   unnest_longer(transitionID) %>%
#   rename(airtable_record_id = "transitionID") %>%
#   left_join(tjet$Transitions %>% 
#               select(airtable_record_id, transitionID),
#             by = "airtable_record_id") %>%
#   select(-airtable_record_id)

db[["Reparations"]] <- db$Reparations %>%
  unnest_longer(ucdpConflictID) %>%
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet$Conflicts %>% 
              select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id")

db[["Trials"]] <- db$Trials %>%
  unnest_longer(all_of(c("ucdpConflictID", "ucdpDyadID"))) %>%
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet$Conflicts %>% 
              select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(airtable_record_id = "ucdpDyadID") %>%
  left_join(tjet$Dyads %>% 
              select(airtable_record_id, dyad_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id",
         ucdpDyadID = "dyad_id")

db[["Amnesties"]] <- db$Amnesties %>%
  unnest_longer(all_of(c("ucdpConflictID", "ucdpDyadID"))) %>%
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet$Conflicts %>% 
              select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(airtable_record_id = "ucdpDyadID") %>%
  left_join(tjet$Dyads %>% 
              select(airtable_record_id, dyad_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id",
         ucdpDyadID = "dyad_id")

db[["Vettings"]] <- db$Vettings %>%
  unnest_longer(all_of(c("ucdpConflictID", "ucdpDyadID"))) %>%
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet$Conflicts %>% 
              select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(airtable_record_id = "ucdpDyadID") %>%
  left_join(tjet$Dyads %>% 
              select(airtable_record_id, dyad_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id",
         ucdpDyadID = "dyad_id")

### at the moment the trial-accused link is one-to-many, but in theory, I could become many-to-many
# range(unlist(map(db$Accused$trialID, length)))

db[["Accused"]] <- db$Accused %>%
  unnest_longer(trialID) %>%
  rename(airtable_record_id = "trialID") %>%
  left_join(tjet$Trials %>% 
              select(airtable_record_id, trialID),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) 

## truth commissions have one-to-many links

db[["TruthCommissions_Conflicts"]] <- db$TruthCommissions %>%
  select(truthcommissionID, ucdpConflictID) %>%
  unnest_longer(ucdpConflictID) %>%
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet$Conflicts %>%
              select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id") %>%
  drop_na()

db[["TruthCommissions_Dyads"]] <- db$TruthCommissions %>%
  select(truthcommissionID, ucdpDyadID) %>%
  unnest_longer(ucdpDyadID) %>%
  rename(airtable_record_id = "ucdpDyadID") %>%
  left_join(tjet$Dyads %>%
              select(airtable_record_id, dyad_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "dyad_id") %>%
  drop_na()

db[["TruthCommissions"]] <- db$TruthCommissions %>%
  select(-ucdpConflictID,-ucdpDyadID)

## consistent ID field names

db[["Conflicts"]] <- db$Conflicts %>%
  rename(ucdpConflictID = "conflict_id")

db[["Dyads"]] <- db$Dyads %>%
  rename(ucdpConflictID = "conflict_id",
         ucdpDyadID = "dyad_id")

save(db, file = here("data/tjetdb.RData"))

### TO DO
## - multi-select fields into dummies for data downloads 
##   - needs a consistent naming scheme
