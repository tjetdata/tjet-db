### packages
library(tidyverse)
library(readxl)
library(writexl)

### load the raw data tables
load(here::here("data", "tjet.RData"), verbose = TRUE)
map(tjet, names)

### ID column names
pkeys <- c(
  "Amnesties" = "amnestyID",
  "Trials" = "trialID",
  "Accused" = "accusedID",
  "CourtLevels" = "CLID", 
  "TruthCommissions" = "truthcommissionID",
  "Reparations" = "reparationID",
  "Vettings" = "vettingID",
  "Countries" = "ccode",
  "Transitions" = "transitionID",
  "Conflicts" = "conflict_id",
  "Dyads" = "dyad_id",
  "ICC" = "ccode_cow", 
  "Investigations" = "pkey", 
  "TJETmembers" = "pkey")

### note that both bases have Countries, Transitions, Conflicts and Dyads tables
### but these should be the same; ideally the two bases should be combined and 
### using only one version each but Airtable makes this difficult

### exclude these tables from the production database
exclude <- c("metadata", "select_options", "Experts", "NGOs", "Legal", 
             "ConflictDyadSpells", "UCDPcountries", "Mallinder", "Rozic", 
             "Challenges", "VettingComparison", "ICDB", "BIcomparison", 
             "AdHocHybrid", "Ethiopia", "ArgCausas", "ArgAccused", "ArgCLs", 
             "ArgCharges", "BIcomparison", "BIcaseIDlink", "TI", "newTI",
             "Southey", "Filipa", "CorrectionsCandidates")

### check metadata table for non-existing fields
### can use this to determine which fields can be deleted from dev DB
cat("These fields listed in 'metadata' are missing in the Airtable download.")
map(names(to_download), function(basename) {
  cat("Base:", basename, "\n\n")
  names(tables) <- tables <- 
    to_download[[basename]][!to_download[[basename]] %in% exclude]
  result <- map(tables, function(tab) {
    meta <- tjet[[basename]]$metadata %>%
      filter(table_name == tab) %>%
      select(field_name) %>%
      unlist(use.names = FALSE)
    meta[!meta %in% names(tjet[[basename]][[tab]])] 
  }) 
  result[map(result, length) > 0] %>% 
    print()
  return()
})

### create prodDB tables
db <- map(names(to_download), function(basename) { # basename = "MegaBase"
  names(select_tables) <- select_tables <- 
    to_download[[basename]][!to_download[[basename]] %in% exclude]
  map(select_tables, function(tab_name) { # tab_name = "Amnesties"
    select_vars <- tjet[[basename]]$metadata %>%
      filter(incl_prod == 1 & 
               ### doing multi-select fields separately
               incl_data != "transform: multiple" &
               ### include these later if creating dummies
               table_name == tab_name) %>%
      arrange(order)  %>%
      select(field_name) %>%
      unlist(use.names = FALSE)
    ### order of fields
    first <-
      c(select_vars[str_detect(select_vars, fixed("ID"))], 
        select_vars[str_detect(select_vars, fixed("ccode"))])
    select_vars <-
      c(select_vars[select_vars %in% first], 
        select_vars[!select_vars %in% first])
    ### adding empty fields as NA for now until they are properly coded
    missing_cols <-
      select_vars[!(select_vars %in% names(tjet[[basename]][[tab_name]]))]
    tjet[[basename]][[tab_name]] %>%
      tibble() %>%
      mutate(!!!setNames(rep(NA, length(missing_cols)), missing_cols)) %>%
      select(all_of(select_vars))
  })
})

names(db) <- names(to_download)

### for tracking valid rows in data tables
dim_orig <- map(db, function(dat) {
  map_vec(dat, nrow)
})

### filtering out invalid records (more removal for other reasons below)
drop_invalids <- c("Amnesties", "Trials", "Accused", 
                   "TruthCommissions", "Reparations", "Vettings")
db <- map(names(to_download), function(basename) {
  drop_invalids <- drop_invalids[drop_invalids %in% names(db[[basename]])]
  base <- db[[basename]]
  base[drop_invalids] <- map(drop_invalids, function(tab_name) {
    base[[tab_name]] %>%
      filter(is.na(invalid) | invalid == 0)
  })
  return(base)
})
names(db) <- names(to_download)

### checking how many records left
dim_drop <- map(db, function(dat) {
  map_vec(dat, nrow)
})
cbind(orig = dim_orig[[1]], drop = dim_drop[[1]])
cbind(orig = dim_orig[[2]], drop = dim_drop[[2]])

### transforming checkboxes to binary
db <- map(names(to_download), function(basename) {
  names(select) <- select <- 
    to_download[[basename]][!to_download[[basename]] %in% exclude]
  map(select, function(tab_name) {
    fields <- tjet[[basename]]$metadata %>%
      filter(table_name == tab_name &
               incl_prod == 1 &
               incl_data == "transform: checkmark to binary") %>%
      select(field_name) %>%
      unlist(use.names = FALSE)
    db[[basename]][[tab_name]][, fields] <-
      map(db[[basename]][[tab_name]][, fields], \(col) { ifelse(is.na(col), 0, 1) } )
    return(db[[basename]][[tab_name]])
  })
})
names(db) <- names(to_download)

### including fields with multipleLookupValues
db <- map(names(to_download), function(basename) {
  names(select) <- select <- tjet[[basename]]$metadata %>%
    filter(
      incl_prod == 1 &
        incl_data == "include as key" &
        table_name != "select_options" &
        field_type == "multipleLookupValues"
    ) %>%
    select(table_name) %>%
    unlist(use.names = FALSE) %>%
    unique()
  base <- db[[basename]]
  base[select] <- map(select, function(tab_name) {
    fields <- tjet[[basename]]$metadata %>%
      filter(
        table_name == tab_name &
          incl_prod == 1 &
          incl_data == "include as key" &
          field_type == "multipleLookupValues"
      ) %>%
      select(field_name) %>%
      unlist(use.names = FALSE)
    db[[basename]][[tab_name]] %>%
      unnest_wider(all_of(fields), names_sep = "", simplify = TRUE) %>%
      rename_with(.cols = all_of(paste(fields, "1", sep = "")), .fn = ~ fields)
  })
  return(base)
})
names(db) <- names(to_download)

rm(to_download) 

### ccode tables for Crimes and Victims

crimes <- c("ccode_Crime1", "ccode_Crime2", "ccode_Crime3")
victims <- c("ccode_Victim1", "ccode_Victim2", "ccode_Victim3")

db[["Prosecutions"]][["Trials_Crimes"]] <- map(crimes, function(var) {
  db[["Prosecutions"]][["Trials"]] %>%
    select(all_of(c("trialID", var))) %>%
    rename(ccode_Crime = all_of(var)) %>%
    drop_na()
}) %>%
  bind_rows()

db[["Prosecutions"]][["Trials_Victims"]] <- map(victims, function(var) {
  db[["Prosecutions"]]$Trials %>%
    select(all_of(c("trialID", var))) %>%
    rename(ccode_Victim = all_of(var)) %>%
    drop_na()
}) %>%
  bind_rows()

db[["Prosecutions"]][["Trials"]] <- 
  db[["Prosecutions"]][["Trials"]] %>%
  select(!all_of(c(crimes, victims)))

### tables for multiselect fields

db[["MegaBase"]][["labels"]] <- 
  tjet[["MegaBase"]][["select_options"]] %>%
  select(labelID, label) %>%
  tibble()

names(select) <- select <- tjet[["MegaBase"]][["metadata"]] %>%
  filter(
    incl_prod == 1 &
      incl_data == "include as key" &
      table_name != "select_options" &
      field_type == "multipleLookupValues"
  ) %>%
  select(table_name) %>%
  unlist(use.names = FALSE) %>%
  unique()

multi_selects <- map(select, function(tab_name) {
  # cat(tab_name)
  names(fields) <- fields <- tjet[["MegaBase"]][["metadata"]] %>%
    filter(incl_prod == 1 &
             incl_data == "transform: multiple" &
             table_name == tab_name) %>%
    select(field_name) %>%
    unlist(use.names = FALSE)
  map(fields, function(field) {
    # cat(field)
    to_filter_on <- paste(field, "set", sep = "_")
    to_select <- paste(field, tab_name, sep = "_")
    tjet[["MegaBase"]]$select_options %>%
      filter(.data[[to_filter_on]] == 1) %>%
      select(all_of(c("labelID", to_select))) %>%
      unnest_longer(all_of(to_select)) %>%
      left_join(tjet[["MegaBase"]][[tab_name]] %>%
                  select(all_of(c("airtable_record_id", pkeys[[tab_name]]))),
                by = setNames("airtable_record_id", to_select)) %>%
      select(all_of(c("labelID", pkeys[[tab_name]]))) %>%
      drop_na()
  })
}) %>%
  unlist(recursive = FALSE)
names(multi_selects) <- str_replace(names(multi_selects), fixed("."), "_")
db[["MegaBase"]] <- c(db[["MegaBase"]], multi_selects)

### checking numbers of records again to ensure integrity
dim_now <- map(db, function(dat) {
  map_vec(dat, nrow)
})
cbind(orig = dim_orig[[1]], drop = dim_drop[[1]], 
      now = dim_now[[1]][names(dim_orig[[1]])])
cbind(orig = dim_orig[[2]], drop = dim_drop[[2]], 
      now = dim_now[[2]][names(dim_orig[[2]])])

### recoding keys in multipleRecordLinks (from Airtable record IDs)
### approaches differ by whether the relationship is one-to-one or one-to-many
### could simplify code below with functions

db[["MegaBase"]][["Reparations"]] <- 
  db[["MegaBase"]][["Reparations"]] %>%
  unnest_longer(ucdpConflictID, keep_empty = TRUE) %>%
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet[["MegaBase"]]$Conflicts %>% 
              select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id")

db[["Prosecutions"]][["Trials"]] <- 
  db[["Prosecutions"]][["Trials"]] %>%
  unnest_longer(all_of(c("ucdpConflictID", "ucdpDyadID")), 
                keep_empty = TRUE) %>%
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet[["Prosecutions"]]$Conflicts %>% 
              select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(airtable_record_id = "ucdpDyadID") %>%
  left_join(tjet[["Prosecutions"]]$Dyads %>% 
              select(airtable_record_id, dyad_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id",
         ucdpDyadID = "dyad_id")

db[["MegaBase"]][["Vettings"]] <- 
  db[["MegaBase"]][["Vettings"]] %>%
  unnest_longer(all_of(c("ucdpConflictID", "ucdpDyadID")), 
                keep_empty = TRUE) %>%
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet[["MegaBase"]]$Conflicts %>% 
              select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(airtable_record_id = "ucdpDyadID") %>%
  left_join(tjet[["MegaBase"]]$Dyads %>% 
              select(airtable_record_id, dyad_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id",
         ucdpDyadID = "dyad_id")

### the trial-accused link is one-to-many 
### (it should be many-to-many but the original DB was not designed to accomodate this)

db[["Prosecutions"]][["Accused"]] <- 
  db[["Prosecutions"]][["Accused"]] %>%
  unnest_longer(trialID, keep_empty = TRUE) %>%
  rename(airtable_record_id = "trialID") %>%
  left_join(tjet[["Prosecutions"]][["Trials"]] %>% 
              select(airtable_record_id, trialID),
            by = "airtable_record_id") %>%
  select(-airtable_record_id)

db[["Prosecutions"]][["CourtLevels"]] <- 
  db[["Prosecutions"]][["CourtLevels"]] %>%
  unnest_longer(accusedID, keep_empty = TRUE) %>%
  rename(airtable_record_id = "accusedID") %>%
  left_join(tjet[["Prosecutions"]][["Accused"]] %>% 
              select(airtable_record_id, accusedID),
            by = "airtable_record_id") %>%
  select(-airtable_record_id)

### truth commissions and amnesties have one-to-many links

db[["MegaBase"]][["TruthCommissions_Conflicts"]] <- 
  db[["MegaBase"]][["TruthCommissions"]] %>%
  select(truthcommissionID, ucdpConflictID) %>%
  unnest_longer(ucdpConflictID) %>%
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet[["MegaBase"]]$Conflicts %>%
              select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id") %>%
  drop_na()

db[["MegaBase"]][["TruthCommissions_Dyads"]] <- 
  db[["MegaBase"]][["TruthCommissions"]] %>%
  select(truthcommissionID, ucdpDyadID) %>%
  unnest_longer(ucdpDyadID) %>%
  rename(airtable_record_id = "ucdpDyadID") %>%
  left_join(tjet[["MegaBase"]]$Dyads %>%
              select(airtable_record_id, dyad_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpDyadID = "dyad_id") %>%
  drop_na()

db[["MegaBase"]][["TruthCommissions"]] <- 
  db[["MegaBase"]][["TruthCommissions"]] %>%
  select(-ucdpConflictID,-ucdpDyadID)

db[["MegaBase"]][["Amnesties_Conflicts"]] <- 
  db[["MegaBase"]][["Amnesties"]] %>%
  select(amnestyID, ucdpConflictID) %>%
  unnest_longer(ucdpConflictID) %>%
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet[["MegaBase"]]$Conflicts %>%
              select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id") %>%
  drop_na()

db[["MegaBase"]][["Amnesties_Dyads"]] <- 
  db[["MegaBase"]][["Amnesties"]] %>%
  select(amnestyID, ucdpDyadID) %>%
  unnest_longer(ucdpDyadID) %>%
  rename(airtable_record_id = "ucdpDyadID") %>%
  left_join(tjet[["MegaBase"]]$Dyads %>%
              select(airtable_record_id, dyad_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpDyadID = "dyad_id") %>%
  drop_na()

db[["MegaBase"]][["Amnesties"]] <- 
  db[["MegaBase"]][["Amnesties"]] %>%
  select(-ucdpConflictID,-ucdpDyadID)

### using consistent ID field names 

db[["MegaBase"]][["Conflicts"]] <- 
  db[["MegaBase"]][["Conflicts"]] %>%
  rename(ucdpConflictID = "conflict_id")

db[["MegaBase"]][["Dyads"]] <- 
  db[["MegaBase"]][["Dyads"]] %>%
  rename(ucdpConflictID = "conflict_id",
         ucdpDyadID = "dyad_id")

db[["Prosecutions"]][["Conflicts"]] <- 
  db[["Prosecutions"]][["Conflicts"]] %>%
  rename(ucdpConflictID = "conflict_id")

db[["Prosecutions"]][["Dyads"]] <- 
  db[["Prosecutions"]][["Dyads"]] %>%
  rename(ucdpConflictID = "conflict_id",
         ucdpDyadID = "dyad_id")

### other multi-select fields (lookup fields as list columns of length one)

db[["Prosecutions"]][["Accused"]] <- 
  db[["Prosecutions"]][["Accused"]] %>% 
  unnest_longer(all_of(c("lastGuiltyYear", "lastVerdictYear", 
                         "lastVerdict", "lastSentencingTime", 
                         "lastSentencingArrangement")), keep_empty = TRUE) %>%
  distinct() 

### other multi-select fields: 
### lookup fields as list columns of length greater than one

db[["Prosecutions"]][["Trials"]] <- 
  db[["Prosecutions"]][["Trials"]] %>%
  rowwise() %>%
  mutate(membership = str_flatten(membership, collapse =", ")) %>% 
  ungroup()

db[["Prosecutions"]][["Trials_lastVerdict"]] <- 
  db[["Prosecutions"]][["Trials"]] %>%
  select(trialID, lastVerdict) %>% 
  unnest_longer(lastVerdict, keep_empty = TRUE) %>% 
  filter(!is.na(lastVerdict)) 

db[["Prosecutions"]][["Trials_lastSentencingTime"]] <- 
  db[["Prosecutions"]][["Trials"]] %>%
  select(trialID, lastSentencingTime) %>% 
  unnest_longer(lastSentencingTime, keep_empty = TRUE) %>% 
  filter(!is.na(lastSentencingTime)) 

db[["Prosecutions"]][["Trials"]] <- 
  db[["Prosecutions"]][["Trials"]] %>%
  select(-lastVerdict, -lastSentencingTime)

### fixing missing Trials endYear & cleaning description
db[["Prosecutions"]][["Trials"]] <- 
  db[["Prosecutions"]][["Trials"]] %>% 
  mutate(yearEnd = ifelse(is.na(yearEnd) & ongoing == 1, 2024, yearEnd), 
         yearEnd = ifelse(is.na(yearEnd) & ongoing == 0 & CLs_final_year > 1970, 
                          CLs_final_year, yearEnd), 
         yearEnd = ifelse(is.na(yearEnd) & ongoing == 0, yearStart, yearEnd), 
         caseDescription = str_squish(caseDescription), 
         charnum = nchar(caseDescription),
         caseDescription = ifelse(str_sub(caseDescription, charnum, charnum) == ".", 
                                  str_sub(caseDescription, 1, charnum - 1), 
                                  caseDescription)) %>% 
  select(-charnum) 

### fair trials measure for later 
fair_trials <- db[["MegaBase"]][["Transitions"]] %>%
  filter(!is.na(fairFlawed)) %>% 
  mutate(fair_postautocratic_trials = ifelse(fairFlawed == "fair", 1, 0)) %>%
  select(ccode, year, fair_postautocratic_trials) %>% 
  arrange(ccode, year) %>% 
  mutate(before = ifelse(year < 1970, 1, 0)) %>%  
  group_by(ccode, before) %>%
  mutate(year_max = max(year)) %>%
  ungroup() %>% 
  filter(before == 0 | (before == 1 & year == year_max)) %>% 
  mutate(year = ifelse(before == 1, 1970, year), 
         year = ifelse(ccode == 115, 1975, year), 
         year = ifelse(ccode == 349, 1992, year), 
         year = ifelse(ccode == 591, 1976, year)
         ) %>% 
  select(ccode, year, fair_postautocratic_trials) %>% 
  distinct() 

db_years_mech <- 1970:2024
db_years_trials <- 1970:2020
db_years_cy <- 1970:2024

### formatting transitions table for website 
db[["MegaBase"]][["Transitions"]] <-
  db[["MegaBase"]][["Transitions"]] %>% 
  filter(trans == 1) %>% 
  mutate(p5 = case_when(is.na(p5_year) | trans_year_begin < p5_year ~ 0, 
                        trans_year_begin >= p5_year ~ 1),
         bmr = case_when(is.na(bmr_year) | trans_year_begin < bmr_year ~ 0, 
                         trans_year_begin >= bmr_year ~ 1),
         ert = case_when(is.na(ert_year) | trans_year_begin < ert_year ~ 0, 
                         trans_year_begin >= ert_year ~ 1), 
         nsupport = p5 + bmr + ert) %>% 
  rowwise() %>% 
  mutate(sources = str_flatten(c(
           case_when(!is.na(p5_year) ~ paste("Polity5 (", p5_year, ")", sep = "")),
           case_when(!is.na(bmr_year) ~ paste("BMR (", bmr_year, ")", sep = "")),
           case_when(!is.na(ert_year) ~ paste("VDem-ERT (", ert_year, ")", sep = ""))),
           collapse = " & ", na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(trans_year_begin %in% db_years_cy) %>%
  filter(!(ccode == 265 & trans_year_begin == 1990)) %>%
  select(transitionID, ccode, trans_year_begin, nsupport, sources, p5_year, ert_year, bmr_year) %>%
  arrange(trans_year_begin, ccode)

### filter out non-HRs policies & events, 
### state agents and opposition members only for domestic trials
### check again?: amnesties, TCs, reparations, vettings

db[["Prosecutions"]][["Trials"]] <- 
  db[["Prosecutions"]][["Trials"]] %>% 
  filter(generalOrSpecific == "event") %>% 
  filter(IntraConfl == 1 | humanRights == 1 | HRs_charges > 0)

db[["Prosecutions"]][["Accused"]] <- 
  db[["Prosecutions"]][["Accused"]] %>% 
  filter(trialID %in% db[["Prosecutions"]][["Trials"]]$trialID) 

db[["Prosecutions"]][["CourtLevels"]] <- 
  db[["Prosecutions"]][["CourtLevels"]] %>%
  filter(accusedID %in% db[["Prosecutions"]][["Accused"]]$accusedID)

### filtering out domestic trials that don't involve state agents or opposition members as accused

trials_to_include <- db[["Prosecutions"]][["Accused"]] %>% 
  filter(stateAgent == 1 | opposedToGovernment == 1) %>% 
  select(trialID) %>% 
  distinct() %>% 
  arrange(trialID) %>% 
  unlist(use.names = FALSE)

db[["Prosecutions"]][["Trials"]] <- 
  db[["Prosecutions"]][["Trials"]] %>% 
  filter((trialType %in% c("domestic", "don't know") & 
            trialID %in% trials_to_include) | 
           trialType %in% c("foreign", "international", "international (hybrid)"))

rm(trials_to_include) 

### filtering out TCs that didn't meet definition criteria or didn't operate

db[["MegaBase"]][["TruthCommissions"]] <-
  db[["MegaBase"]][["TruthCommissions"]] |> 
  filter(temporaryBodyReport == 1 & # authorizedByState == 1 & 
           focusedPast == 1 & investigatePatternAbuse == 1 & neverOperated == 0) |> 
  select(-temporaryBodyReport, -focusedPast, # -authorizedByState, 
         -investigatePatternAbuse, -neverOperated) 

### filtering out amnesties that don't meet crime criteria

## checking amnesties
# crime_labels <- db[["MegaBase"]][["Amnesties_whatCrimes"]] %>%
#   select(labelID) %>%
#   distinct() %>%
#   unlist(use.names = FALSE)
# db[["MegaBase"]][["labels"]] %>%
#   filter(labelID %in% crime_labels)
# rm(crime_labels)

keep_amnesties <- db[["MegaBase"]][["Amnesties_whatCrimes"]] %>% 
  filter(labelID %in% c(60, 88, 9, 143, 132) ) %>%
  select(amnestyID) %>% 
  distinct() %>% 
  unlist(use.names = FALSE)

db[["MegaBase"]][["Amnesties"]] <- 
  db[["MegaBase"]][["Amnesties"]] %>% 
  filter(amnestyID %in% keep_amnesties)

rm(keep_amnesties)

### UN investigations

dl_invest <- db[["MegaBase"]][["Investigations"]] %>%
  rename(beg = year_beg, 
         end = year_end) %>% 
  select(ccode_cow, beg, end, name, mandate_en, mandate_fr, 
         uninv_dompros, uninv_intlpros, uninv_evcoll, 
         secgen, unsc, cohr, unga, ohchr, hrc, source) %>% 
  filter(beg %in% db_years_mech) %>%
  mutate(
    ccode_cow = ifelse(ccode_cow == 860 & beg == 1999, 850, ccode_cow),
    ccode_cow = ifelse(ccode_cow == 541 & beg == 1973, 235, ccode_cow)
  ) %>% 
  arrange(name, beg) %>%
  filter(!(ccode_cow == 490 & str_detect(name, " & "))) %>% 
  # mutate(name = str_split(name, " & ") ) %>% 
  # unnest(name, keep_empty = TRUE) %>% 
  # distinct() %>%
  mutate(country = str_split_i(name, "_", 1),
         country = str_remove(country, as.character(beg)),
         country = str_replace(country, "CAR", "Central African Republic"),
         country = str_replace(country, "DRC", "Democratic Republic of the Congo"),
         country = str_replace(country, "DPRK", "North Korea"),
         country = str_replace(country, "Lebanon2005", "Lebanon"),
         country = str_replace(country, "Syria2011", "Syria"),
         country = str_replace(country, "Myanmar2017", "Myanmar"),
         country = str_replace(country, "Myanmar2018", "Myanmar"),
         country = str_replace(country, "SouthAfrica", "South Africa"),
         mandate = str_split_i(name, "_", 2), 
         mandate = ifelse(is.na(mandate) & ccode_cow == 93 & unsc == 1, "UNSC", mandate), 
         mandate = ifelse(mandate == "rep" & ccode_cow == 860 & secgen == 1, "SECGEN", mandate), 
         mandate = str_squish(mandate),
         mandate = str_replace(mandate, "UNSC", "UN Security Council"),
         mandate = str_replace(mandate, "SECGEN", "UN Secretary General"),
         mandate = str_replace(mandate, "UNSG", "UN Secretary General"),
         mandate = str_replace(mandate, "UNGA", "UN General Assembly"),
         mandate = str_replace(mandate, "GA ", "UN General Assembly "),
         mandate = str_replace(mandate, "OHCHR", "Office of the UN High Commissioner for Human Rights"),
         mandate = str_replace(mandate, "HCHR", "Office of the UN High Commissioner for Human Rights"),
         mandate = str_replace(mandate, "HRC ", "UN Human Rights Council "),
         mandate = str_replace(mandate, "COHR ", "UN Commission on Human Rights "),
         mandate = str_replace(mandate, "CHR ", "UN Commission on Human Rights "),
         mandate = str_replace(mandate, " res ", " resolution "),
         mandate = str_replace(mandate, " dec ", " decision "),
         mandate = str_replace(mandate, " rep ", " report "),
         mandate = str_replace(mandate, fixed("/ "), "/"),
         goals = case_when(uninv_intlpros == 1 & uninv_evcoll == 1 & uninv_dompros == 1 ~ "support international prosecutions; collect evidence; encourage domestic prosecutions", 
                           uninv_intlpros == 1 & uninv_evcoll == 1 ~ "support international prosecutions; collect evidence",
                           uninv_intlpros == 1 & uninv_dompros == 1 ~ "support international prosecutions; encourage domestic prosecutions", 
                           uninv_evcoll == 1 & uninv_dompros == 1 ~ "collect evidence; encourage domestic prosecutions", 
                           uninv_intlpros == 1 ~ "support international prosecutions", 
                           uninv_evcoll == 1 ~ "collect evidence",
                           uninv_dompros == 1 ~ "encourage domestic prosecutions") 
         ) %>% 
  select(country, ccode_cow, beg, end, mandate, mandate_en, mandate_fr, goals, 
         uninv_dompros, uninv_evcoll, uninv_intlpros, source) 
  # group_by(ccode_cow, mandate, beg) %>%
  # filter(n() > 1) %>% 
  # print(n = Inf)

### investigations where mandate differs
# dl_invest %>%
#   filter(mandate != mandate_en | is.na(mandate_en))

db[["MegaBase"]][["Investigations"]] <- dl_invest |>
  rowwise() |>  
  mutate(uninv = 1, 
         year = list(beg:end)) |>  
  unnest(year) |>  
  mutate(uninv_beg = ifelse(beg == year, 1, 0)) |> 
  group_by(ccode_cow, year) |>  
  reframe(uninv_beg = sum(uninv_beg), 
          uninv = sum(uninv), 
          across(all_of(c("uninv_dompros", "uninv_evcoll", "uninv_intlpros")), max)) |> 
  left_join(dl_invest |> 
              select(ccode_cow, beg, uninv_dompros, uninv_evcoll, uninv_intlpros) |> 
              group_by(ccode_cow, beg) %>%
              reframe(uninv_dompros_beg = max(uninv_dompros),
                      uninv_evcoll_beg = max(uninv_evcoll),
                      uninv_intlpros_beg = max(uninv_intlpros)),
            by = c("ccode_cow" = "ccode_cow", "year" = "beg") ) |> 
  select(ccode_cow, year, uninv_beg, uninv, uninv_dompros_beg, uninv_dompros, 
         uninv_evcoll_beg, uninv_evcoll, uninv_intlpros_beg, uninv_intlpros) 

### compare numbers of records again
dim_last <- map(db, function(dat) {
  map_vec(dat, nrow)
})
cbind(orig = dim_orig[[1]], 
      drop = dim_drop[[1]], 
      now = dim_now[[1]][names(dim_orig[[1]])],
      last = dim_last[[1]][names(dim_orig[[1]])])
cbind(orig = dim_orig[[2]], 
      drop = dim_drop[[2]], 
      now = dim_now[[2]][names(dim_orig[[2]])],
      last = dim_last[[2]][names(dim_orig[[2]])])

### combining the relevant tables from MegaBase and Prosecutions into one list 
db <- c(db[["MegaBase"]], 
        db[["Prosecutions"]][c("Trials", "Accused", "CourtLevels", 
                               "Trials_Crimes", "Trials_Victims", 
                               "Trials_lastVerdict", 
                               "Trials_lastSentencingTime")])

### cleaning up workspace environment
rm(exclude, select, dim_drop, dim_last, dim_now, dim_orig, 
   drop_invalids, crimes, victims, multi_selects, pkeys)

### creating country list as basis for country-year dataset
countrylist <- db[["Countries"]] %>% 
  mutate(beg = as.integer(str_sub(begin_date, 1, 4)), 
         end = as.integer(str_sub(end_date, 1, 4)), 
         beg = ifelse(beg <= 1970, 1970, beg),
         beg = ifelse(country == "Timor-Leste", 1999, beg), # was 2002, special case
         beg = ifelse(country == "Kosovo", 1999, beg), # was 2008, special case
         beg = ifelse(country == "Montenegro", 1996, beg), # was 2006, special case
         beg = ifelse(country == "Slovenia", 1991, beg), # was 2006, special case
         beg = ifelse(country == "Andorra", 1994, beg), # was 1993
         beg = ifelse(country == "Antigua and Barbuda", 1982, beg), # was 1981 but not included here
         beg = ifelse(country == "Brunei", 1985, beg), # was 1984 but not included here
         beg = ifelse(country == "Kiribati", 1997, beg),
         beg = ifelse(country == "Liechtenstein", 1991, beg), # was 1990
         beg = ifelse(country == "Marshall Islands", 1992, beg), # was 1991
         beg = ifelse(country == "Micronesia", 1992, beg),
         beg = ifelse(country == "Monaco", 1994, beg), # was 1993
         beg = ifelse(country == "Palau", 1995, beg), # was 1994 but not included here
         beg = ifelse(country == "Slovenia", 1991, beg),
         beg = ifelse(country == "Saint Kitts and Nevis", 1984, beg), # was 1983 but not included here
         end = ifelse(country == "Serbia and Montenegro", 2005, end),
         end = ifelse(country == "German Federal Republic (West)", 1989, end),
         end = ifelse(country == "Yemen Arab Republic (North)", 1989, end),
         end = ifelse(end == 2020, 2024, end), ### to extend years 
         region_sub_un = ifelse(is.na(intregion), subregion, intregion),
         region = ifelse(region_wb == "Middle East & North Africa" & 
                           region %in% c("Asia", "Africa"), "MENA", region)) %>% 
  mutate(across(starts_with("txt_"), str_trim) ) %>%
  mutate(across(starts_with("auto_"), str_trim) ) %>%
  select(country, country_fr, include, ccode, ccode_case, ccode_ksg, m49, isoa3, 
         country_id_vdem, beg, end, micro_ksg, region, region_sub_un, region_wb, 
         focus, factsheet, starts_with("txt_"), starts_with("auto_")) %>% 
  rename("tjet_focus" = "focus") %>% 
  arrange(country)

### duplicate ccodes
# countrylist %>%
#   select(country, beg, end, ccode, ccode_case) %>%
#   rename("country_case" = "country") %>%
#   group_by(ccode_case) %>%
#   mutate(n = n()) %>%
#   filter(n > 1 & ccode == ccode_case & end < 2020)

countrylist <- countrylist %>%
  left_join(countrylist %>% 
              select(country, country_fr, ccode) %>%
              filter(!country %in% 
                       c("Soviet Union", "Yugoslavia", "Serbia and Montenegro") ) %>%
              rename("country_case" = "country", 
                     "country_case_fr" = "country_fr"),
          by = c("ccode_case" = "ccode")) %>% 
  select(country, country_case, include, country_fr, country_case_fr, ccode, 
         ccode_case, ccode_ksg, m49, isoa3, country_id_vdem, beg, end, 
         micro_ksg, region, region_sub_un, region_wb, 
         tjet_focus, factsheet, starts_with("txt_"), starts_with("auto_")) 

### ccode_case / country_case for the countries on the 2020 map that data are matched to
# countrylist %>%
#   filter(country != country_case | ccode != ccode_case |
#            ccode %in% c(255, 260, 265, 315, 316, 345, 365, 678, 679, 680, 816, 817)) %>%
#   select(country_case, ccode_case, country, ccode, beg, end) %>%
#   arrange(country_case, end)

### clean up text fields 
tabs <- c("Amnesties", "Reparations", "TruthCommissions", 
          "Vettings", "Trials", "Accused") 
db[tabs] <- map(tabs, function(tab) {
  text_fields <- names(db[[tab]])[map(db[[tab]], class) == "character"]
  db[[tab]][text_fields] <- map(db[[tab]][text_fields], str_squish)
  return(db[[tab]])
})

### preparing amnesties list for merging into country-year dataset
amnesties <- db[["Amnesties"]] %>% 
  arrange(ccode, amnestyYear) %>%  
  group_by(ccode, amnestyYear) %>%
  reframe(amnesties = n(),
          amnesties_SGBV = max(SGBV)) %>%
  rename("year" = "amnestyYear") %>% 
  filter(year %in% db_years_mech)

### preparing reparations list for merging into country-year dataset
reparations <- db[["Reparations"]] %>%
  arrange(ccode, yearCreated) %>%  
  mutate(SGBV = ifelse(harmsSexualViolence == 1 | 
                         genderCrimes == "yes" | 
                         lgbtqCrimes == "yes", 1, 0) ) %>%    
  group_by(ccode, yearCreated) %>%
  reframe(reparations = n(),
          # genderAttentive = max(genderAttentive), 
          reparations_SGBV = max(SGBV)) %>% 
  rename("year" = "yearCreated") %>% 
  filter(year %in% db_years_mech)

### preparing TCs list for merging into country-year dataset
tcs <- db[["TruthCommissions"]] %>%
  arrange(ccode, yearPassed) %>%  
  group_by(ccode, yearPassed) %>%
  reframe(tcs = n(),
          tcs_SGBV = max(SGBV)) %>% 
  rename("year" = "yearPassed") %>% 
  filter(year %in% db_years_mech)

### preparing vettings list for merging into country-year dataset
vettings <- db[["Vettings"]] %>%
  filter(is.na(alterationOf)) %>% 
  arrange(ccode, yearStart) %>%  
  group_by(ccode, yearStart) %>%  
  reframe(vettings = n()) %>% 
  rename("year" = "yearStart") %>% 
  filter(year %in% db_years_mech)

### preparing trials list for merging into country-year dataset
trials <- db[["Trials"]] %>%
  rename("ccode" = "ccode_Accused") %>% 
  arrange(ccode, yearStart) %>%
  mutate(SGBV = ifelse(rape_Accused == 1 | sexualViolence_Accused == 1 | 
                         otherSGBV_Accused == 1, 1, 0) ) %>% 
  select(ccode, trialType, yearStart, SGBV) %>% 
  rename("year" = "yearStart") %>% 
  filter(year %in% db_years_trials)

### subsetting and coding domestic trials total count measures for website
domestic <- trials %>% 
  filter(trialType %in% c("domestic", "don't know")) %>% 
  group_by(ccode, year) %>%
  reframe(trials_domestic = n(),
          trials_domestic_SGBV = max(SGBV))

### subsetting and coding foreign trials total count measures for website
foreign <- trials %>% 
  filter(trialType == "foreign") %>% 
  group_by(ccode, year) %>%
  reframe(trials_foreign = n(),
          trials_foreign_SGBV = max(SGBV))

### subsetting and coding intl trials total count measures for website
intl <- trials %>% 
  filter(trialType %in% c("international", "international (hybrid)")) %>% 
  group_by(ccode, year) %>%
  reframe(trials_intl = n(),
          trials_intl_SGBV = max(SGBV))

### preparing conflicts list for merging into country-year dataset
confllist <- read_csv("conflicts/confl_dyads.csv", show_col_types = FALSE) %>% 
  select(location, gwno_loc, ep_start_year, ep_end_year) %>% 
  rowwise() %>% 
  mutate(year = list(ep_start_year:ep_end_year)) %>% 
  ungroup() %>% 
  unnest_longer(year) %>% 
  select(-ep_start_year, -ep_end_year) %>% 
  distinct() %>% 
  mutate(conflict = 1) %>% 
  filter(year %in% db_years_cy)

### transitions dataset in country-year format

translist <- read_csv("transitions/transitions_new_revised.csv",
                      show_col_types = FALSE) %>% 
  full_join(countrylist %>% 
              select(country, beg, end), 
            by = "country") %>%
  arrange(country, year) %>% 
  filter(year >= beg & year <= end) %>% 
  mutate(transition = ifelse(is.na(trans_year), 0, 1), 
         dem_polity = ifelse(polity_p5 >= 6, 1, 0),
         dem_vdem = ifelse(str_detect(str_to_lower(v2x_regime_amb), "democracy"), 1, 0), 
         dem_all = rowSums(across(all_of(c("dem_bmr", "dem_polity", 
                                           "dem_vdem"))), na.rm = TRUE)/3) %>% 
  select(country, ccode, year, transition, dem_bmr, dem_polity, dem_vdem, dem_all) %>%
  group_by(ccode) %>% 
  mutate(finite_check = sum(!is.na(dem_polity)), 
         dem_polity_min = ifelse(finite_check > 0, min(dem_polity, na.rm = TRUE), NA), 
         dem_polity_max = ifelse(finite_check > 0, max(dem_polity, na.rm = TRUE), NA), 
         finite_check = sum(!is.na(dem_bmr)), 
         dem_bmr_min = ifelse(finite_check > 0, min(dem_bmr, na.rm = TRUE), NA), 
         dem_bmr_max = ifelse(finite_check > 0, max(dem_bmr, na.rm = TRUE), NA), 
         finite_check = sum(!is.na(dem_vdem)), 
         dem_vdem_min = ifelse(finite_check > 0, min(dem_vdem, na.rm = TRUE), NA), 
         dem_vdem_max = ifelse(finite_check > 0, max(dem_vdem, na.rm = TRUE), NA), 
         sources = 3 - (is.na(dem_polity_max) + 
                          is.na(dem_bmr_max) + is.na(dem_vdem_max)),
         regime = max(transition, na.rm = TRUE), 
         dem_prop = sum(dem_all, na.rm = TRUE) / n()) %>% 
  ungroup() %>% 
  mutate(context_bmr = case_when(dem_bmr_min == 0 & dem_bmr_max == 0 ~ 0,
                                 dem_bmr_min == 1 & dem_bmr_max == 1 ~ 1),
         context_polity = case_when(dem_polity_min == 0 & dem_polity_max == 0 ~ 0,
                                    dem_polity_min == 1 & dem_polity_max == 1 ~ 1),
         context_vdem = case_when(dem_vdem_min == 0 & dem_vdem_max == 0 ~ 0,
                                  dem_vdem_min == 1 & dem_vdem_max == 1 ~ 1),
         context_dem = rowSums(across(all_of(c("context_bmr", "context_polity", 
                                               "context_vdem"))), na.rm = TRUE),
         regime = case_when(regime == 1 ~ "transitional", 
                            regime == 0 & 
                              context_dem == 0 & sources > 0 ~ "autocratic",
                            regime == 0 & (context_dem == sources | 
                                             context_dem > 1) ~ "democratic",
                            regime == 0 & dem_prop > 0.5 ~ "democratic"), 
         ## India is the only ambiguous case by this algorithm, hence the last line
         reg_democ = ifelse(regime == "democratic", 1, 0), 
         reg_autoc = ifelse(regime == "autocratic", 1, 0), 
         reg_trans = ifelse(regime == "transitional", 1, 0)
  ) %>% 
  rename(regime_sample = regime) %>% 
  select(country, ccode, year, transition, dem_bmr, dem_polity, dem_vdem, 
         regime_sample, reg_democ, reg_autoc, reg_trans) |> 
  filter(year <= 2020) 

### democratic reversions data for merging 
reversions <- read_csv("transitions/transitions_new_revised.csv",
         show_col_types = FALSE) %>% 
  select(country, country_id_vdem, year, reg_type, reg_start_year, reg_end_year, 
         reg_age, reg_trans, dem_spell_id, dem_reversion, v2x_regime_ert) %>% 
  rename(reg_type_vdem = reg_type,
         reg_age_vdem = reg_age,
         reg_start_year_vdem = reg_start_year,  
         reg_end_year_vdem = reg_end_year,
         reg_trans_vdem = reg_trans,
         dem_spell_id_vdem = dem_spell_id) %>% 
  filter(!is.na(country_id_vdem)) 

### creating country-year dataset and merging in mechanisms count measures
db[["CountryYears"]] <- map(countrylist$country , function(ctry) {
  beg <- countrylist %>%
    filter(country == ctry) %>%
    select(beg) %>%
    unlist(use.names = FALSE)
  end <- countrylist %>%
    filter(country == ctry) %>%
    select(end) %>%
    unlist(use.names = FALSE)
  expand_grid(country = ctry, year = beg:end) %>%
    tibble()
}) %>%
  do.call(rbind, .) %>% 
  full_join(countrylist, by = "country") %>%
  mutate(cyID = paste(ccode, year, sep = "-")) %>% 
  full_join(translist, by = c("country", "ccode", "year") ) %>%
  full_join(confllist, by = c("ccode_ksg" = "gwno_loc", "year" = "year") ) %>% 
  mutate(conflict = ifelse(is.na(conflict), 0, 1)) %>% 
  group_by(ccode_case) %>%
  mutate(conflict = max(conflict) ) %>%
  ungroup() %>% 
  mutate(
    country_label = ifelse(end < 2020, 
      paste(country, " [-", end, ", included on ", country_case, " page]", sep = ""), country), 
    country_label_fr = ifelse(end < 2020,
      paste(country_fr, " [-", end, ", inclus dans la page ", country_case_fr, "]", sep = ""),
      country_fr)
    ) %>% 
  select(cyID, country, country_fr, country_label, country_label_fr, year, 
         beg, end, ccode, ccode_case, country_case, ccode_ksg, country_id_vdem, 
         tjet_focus, region, regime_sample, reg_democ, reg_autoc, reg_trans, 
         conflict, transition) %>% 
# db[["CountryYears"]] |> ### Montenegro 2004
#   select(country, country_case, ccode, ccode_case, year) |>
#   mutate(base = 1) |>
#   full_join(domestic |>
#               mutate(domestic = 1),
#             by = c("ccode", "year")) |>
#   filter(is.na(base))
  left_join(amnesties, by = c("ccode", "year") ) %>% 
  mutate(amnesties = ifelse(is.na(amnesties), 0, amnesties),
         amnesties_SGBV = ifelse(is.na(amnesties_SGBV), 0, amnesties_SGBV)) %>% 
  left_join(reparations, by = c("ccode", "year") ) %>% 
  mutate(reparations = ifelse(is.na(reparations), 0, reparations),
         reparations_SGBV = ifelse(is.na(reparations_SGBV), 0, reparations_SGBV)) %>% 
  left_join(tcs, by = c("ccode", "year") ) %>% 
  mutate(tcs = ifelse(is.na(tcs), 0, tcs),
         tcs_SGBV = ifelse(is.na(tcs_SGBV), 0, tcs_SGBV)) %>% 
  left_join(vettings, by = c("ccode", "year") ) %>% 
  mutate(vettings = ifelse(is.na(vettings), 0, vettings)) %>% 
  left_join(domestic, by = c("ccode", "year") ) %>% 
  mutate(trials_domestic = ifelse(is.na(trials_domestic), 0, trials_domestic),
         trials_domestic_SGBV = ifelse(is.na(trials_domestic_SGBV), 0, trials_domestic_SGBV)) %>% 
  left_join(intl, by = c("ccode", "year") ) %>% 
  mutate(trials_intl = ifelse(is.na(trials_intl), 0, trials_intl),
         trials_intl_SGBV = ifelse(is.na(trials_intl_SGBV), 0, trials_intl_SGBV)) %>% 
  left_join(foreign, by = c("ccode", "year") ) %>% 
  mutate(trials_foreign = ifelse(is.na(trials_foreign), 0, trials_foreign),
         trials_foreign_SGBV = ifelse(is.na(trials_foreign_SGBV), 0, trials_foreign_SGBV)) 

db[["CountryYears"]] |> 
  reframe(
    amnesties = sum(amnesties), 
    reparations = sum(reparations), 
    tcs = sum(tcs), 
    trials_domestic = sum(trials_domestic), 
    trials_intl = sum(trials_intl), 
    trials_foreign = sum(trials_foreign), 
    vettings = sum(vettings)) 
  
## country labels in map table for special cases
# db[["CountryYears"]] %>%
#   filter(ccode_case %in% c(316, 255, 345, 365, 816, 679) ) %>%
#   select(country, country_label, country_label_fr) %>%
#   distinct()
# db[["CountryYears"]] %>%
#   select(country, country_label, country_case,
#          beg, end, ccode, ccode_case, ccode_ksg, country_id_vdem) %>%
#   filter(country != country_label) %>%
#   distinct()

### countries table for database
db[["Countries"]] <- countrylist %>% 
  mutate(beg = ifelse(beg < 1970, 1970, beg)) %>% 
  select(country, country_case, include, country_fr, ccode, ccode_case, 
         ccode_ksg, beg, end, m49, isoa3, micro_ksg, region, region_sub_un, 
         region_wb, tjet_focus, starts_with("txt_"), starts_with("auto_"))

### data definitions codebook
db[["codebook"]] <- read_csv(here::here("data", "tjet_codebook.csv"), 
                          show_col_types = FALSE, guess_max = 2000) %>% tibble()
attr(db[["codebook"]], "spec") <- NULL
attr(db[["codebook"]], "problems") <- NULL
# attributes(db[["codebook"]]) 

### field meta data for website tables
db[["fields_meta"]] <- read_csv(here::here("data", "tjet_fields_meta.csv"), 
                                show_col_types = FALSE) %>% tibble()
attr(db[["fields_meta"]], "spec") <- NULL
attr(db[["fields_meta"]], "problems") <- NULL
# attributes(db[["fields_meta"]]) 

### translations table for website 
db[["translations"]] <- read_csv(here::here("data", "tjet_translations.csv"), 
                                show_col_types = FALSE) %>% tibble()
attr(db[["translations"]], "spec") <- NULL
attr(db[["translations"]], "problems") <- NULL
# attributes(db[["translations"]]) 

### conflict dyads lookup table for database
db[["ConflictDyads"]] <- read_csv(here::here("conflicts", "confl_dyads.csv"), 
                             show_col_types = FALSE) %>% 
  tibble() %>%
  filter(ep_start_year <= 2024 & ep_end_year >= 1970)

attr(db[["ConflictDyads"]], "spec") <- NULL
attr(db[["ConflictDyads"]], "problems") <- NULL
# attributes(db[["ConflictDyads"]]) 

### surveys metadata 
db[["SurveysMeta"]] <- db[["SurveysMeta"]] %>%
  select(-country_fr) %>% 
  unnest(country) %>% 
  rename("airtable_record_id" = "country") %>% 
  left_join(tjet[["MegaBase"]][["Countries"]] %>% 
              select(airtable_record_id, country, country_fr),
            by = "airtable_record_id") %>%
  mutate(results_tables = map(results_tables, function(x) x[["filename"]])) %>% 
  select(country, country_fr, year, date_start, date_end, section_title, 
         text_context, text_results, text_methods, survey_design, sample_size,
         bibtex_key, results_tables) %>% 
  unnest(results_tables)

gloss <- read_csv(here::here("data", "HHI_surveys_glossary.csv")) %>%
  rename(from = "Appears as", 
         to = "Should be") %>%
  arrange(str_to_lower(from)) %>% 
  distinct()
gl <- gloss$to
names(gl) <- gloss$from
rm(gloss) 

### survey data tables
map(db[["SurveysMeta"]]$results_tables, function(filename) { 
  ## for dev: filename = "Uganda_2005_Submission.xlsx" 
  surveytab <- readxl::read_xlsx(here::here("data", "downloads", filename))
  tooltips <- names(surveytab)
  tooltips[str_detect(tooltips, fixed("..."))] <- NA
  if(is.na(tooltips[1]) & surveytab[1, 1] == "Section") tooltips[1] <- "Section"
  if(is.na(tooltips[2]) & surveytab[1, 2] == "Question") tooltips[2] <- "Question"
  if(is.na(tooltips[3]) & surveytab[1, 3] == "Responses") tooltips[3] <- "Responses"
  tooltips <- fillr::fill_missing_previous(tooltips)
  tooltips[is.na(tooltips)] <- ""
  if(is.na(surveytab[1, 1]) & tooltips[1] == "Section") surveytab[1, 1] <- "Section"
  if(is.na(surveytab[1, 2]) & tooltips[2] == "Question") surveytab[1, 2] <- "Question"
  if(is.na(surveytab[1, 3]) & tooltips[3] == "Responses") surveytab[1, 3] <- "Responses"
  names(tooltips) <- names(surveytab) <- surveytab[1, ] %>% 
    as.character() %>% 
    str_trim() %>% 
    str_replace(fixed("Man"), "Men") %>% 
    str_replace(fixed("Woman"), "Women") %>% 
    str_replace(fixed("Male"), "Men") %>% 
    str_replace(fixed("Female"), "Women")
  surveytab[1, ] <- as.list(tooltips)
  surveytab <- surveytab %>% 
    mutate(Question = str_replace_all(Question, gl), 
           Responses = str_replace_all(Responses, gl))
  db[[str_replace(filename, ".xlsx", "")]] <<- surveytab %>%
    fill(Section, Question, Responses, .direction = "down")
})

### cleaning up workspace environment
rm(countrylist, translist, confllist, gl)

### database version 
db[["db_timestamp"]] <- tjet[["db_timestamp"]] %>% tibble(tjet_timestamp = .)
timestamp <- db[["db_timestamp"]] %>% 
  mutate(tjet_timestamp = as.character(date(tjet_timestamp))) %>% 
  unlist(use.names = FALSE)

### dealing with multi-select fields
tabs <- c("Amnesties" = "amnestyID", 
          "Reparations" = "reparationID", 
          "Trials" = "trialID", 
          "TruthCommissions" = "truthcommissionID", 
          "Vettings" = "vettingID")
multies <- map(names(tabs), function(tabname) {
  multitabs <- names(db)[str_detect(names(db), paste(tabname, "_", sep = ""))]
  temp <- map(multitabs, function(multitab) { 
    if("labelID" %in% names(db[[multitab]])) {
      db[[multitab]] %>% 
        left_join(db$labels, by = "labelID") %>% 
        group_by(across(all_of(tabs[[tabname]]))) %>%
        mutate(!!str_replace(multitab, paste(tabname, "_", sep = ""), "") := 
                 str_flatten(label, collapse ="; ")) %>% 
        ungroup() %>% 
        select(-labelID, -label) %>% 
        distinct()
    } else {
      var <- names(db[[multitab]])[!names(db[[multitab]]) %in% tabs[[tabname]]]
      db[[multitab]] %>% 
        group_by(across(all_of(tabs[[tabname]]))) %>% 
        mutate(across(all_of(var), function(x) str_flatten(x, collapse ="; "))) %>%
        ungroup() %>% 
        distinct()
    }
  }) %>% 
    reduce(full_join, by = tabs[[tabname]])
  return(temp)
}) 
names(multies) <- names(tabs)

db[["Amnesties"]] <- db[["Amnesties"]] %>% 
  left_join(multies[["Amnesties"]], by = "amnestyID") 
db[["Reparations"]] <- db[["Reparations"]] %>% 
  left_join(multies[["Reparations"]], by = "reparationID") 
db[["Trials"]] <- db[["Trials"]] %>% 
  left_join(multies[["Trials"]], by = "trialID") 
db[["TruthCommissions"]] <- db[["TruthCommissions"]] %>% 
  left_join(multies[["TruthCommissions"]], by = "truthcommissionID") 
db[["Vettings"]] <- db[["Vettings"]] %>% 
  left_join(multies[["Vettings"]], by = "vettingID") 
rm(tabs, multies)

### vettings 

db[["Vettings"]] <- db[["Vettings"]] |>
  mutate(policy_type = type_dismissal + type_ban + type_declassification, 
         inst_exe = case_when(
           str_detect(targetingPositionSought, "executive") ~ 1, 
           TRUE ~ 0),
         inst_legis = case_when(
           str_detect(targetingPositionSought, "legislature") ~ 1, 
           TRUE ~ 0),
         inst_judiciary = case_when(
           str_detect(targetingPositionSought, "judiciary") ~ 1, 
           TRUE ~ 0),
         inst_parties = case_when(
           str_detect(targetingPositionSought, "political parties") ~ 1, 
           TRUE ~ 0),
         inst_public = case_when(
           str_detect(targetingPositionSought, "public sector employee") ~ 1, 
           TRUE ~ 0),
         inst_police = case_when(
           str_detect(targetingPositionSought, "police") ~ 1, 
           TRUE ~ 0),
         inst_military = case_when(
           str_detect(targetingPositionSought, "armed forces") ~ 1, 
           TRUE ~ 0),
         inst_other = case_when(
           str_detect(targetingPositionSought, "other") ~ 1, 
           TRUE ~ 0), 
         sum_inst = inst_exe + inst_legis + inst_judiciary + inst_parties + inst_public + inst_police + inst_military + inst_other,
         inst_targeted = case_when(
           sum_inst == 0 ~ 0, 
           sum_inst == 1 ~ 1,
           sum_inst >= 2 & sum_inst <=5 ~ 2, 
           sum_inst > 5 ~ 3), 
         ban_from_elected = ifelse(inst_legis == 1 & type_ban == 1, 1, 0), 
         fairness = ifelse(str_detect(targetingWhy, "specific individual conduct") | 
                        appealJudgment == "yes", 1, 0) + 
           ifelse(hearingsPublic == "yes", 1, 0) + 
           ifelse(courtChallenge != "no court challenge" & courtChallenge != "don't know" & !is.na(courtChallenge), 1, 0) 
         ) 

### ICC data 
db[["ICC"]] <- db[["ICC"]] |> 
  reframe(.by = ccode_cow, 
    ICC_referral = min(ICC_referral, na.rm = TRUE), 
    ICC_prelim_exam = min(ICC_prelim_exam, na.rm = TRUE), 
    ICC_prelimEnd = min(ICC_prelimEnd, na.rm = TRUE), 
    ICC_investigation = min(ICC_investigation, na.rm = TRUE)) |> 
  mutate(across(!ccode_cow, ~ ifelse(is.infinite(.x), NA, .x ) ) ) |> 
  arrange(ccode_cow) 

db[["ICCaccused"]] <- db[["Accused"]] %>% 
  filter(!is.na(ICC_investigation)) %>% 
  left_join(db[["Trials"]] %>% 
              select(trialID, ccode_Accused), 
            by = "trialID") %>% 
  left_join(db[["Countries"]] %>% 
              filter(end == 2024) %>% 
              select(country, ccode),
            by = c(ccode_Accused = "ccode")) %>%
  select(trialID, accusedID, country, ccode_Accused, name, 
         position_desc, position_desc_fr, 
         ICC_arrest_warrant, ICC_arrestAppear, ICC_confirm_charges, 
         ICC_proceedings, ICC_withdrawnDismissed) %>% 
  arrange(ccode_Accused, ICC_arrest_warrant)

### helpers for CY measures
source("pipeline/fx/AmnestyMeasure.R", echo = TRUE)
source("pipeline/fx/ReparationMeasures.R", echo = TRUE)
source("pipeline/fx/TCgoals.R", echo = TRUE)
source("pipeline/fx/TCmeasure.R", echo = TRUE)
source("pipeline/fx/TrialsMeasure.R", echo = TRUE)
source("pipeline/fx/VettingMeasures.R", echo = TRUE)

### dtr = democratic transition, binary, from first transition year
### aco = all conflicts, binary, from first conflict year
### dco = during conflict, binary, when conflict active
### pco = post-conflict, binary, after active conflict ended

### merging it all together

### if country-year-dataset repo is made public, could get directly from there
### this currently does not work, but here is sample code to do so; if public, 
### would have to reset token because had accidentally included in repo before
# download.file(
#   url = "https://github.com/timothoms/country-year-dataset/raw/main/cy_covariates.RData?raw=True",
#   destfile = here::here("data", "cy_covariates.RData"), 
#   method = "auto", cacheOK = FALSE,
#   headers = c(
#     Authorization = "Authorization: token INSERT TOKEN HERE",
#     Accept = "Accept: application/vnd.github.v3.raw"))

### cy dataset assembled in other repo
df <- readRDS(here::here("data", "cy_covariates.rds")) %>% 
  select(-beg, -end) 
not <- c("histname", "cid_who", "ldc", "lldc", "sids", "income_wb", 
         "iso3c_wb", "region_wb2")
first <- c("country", "country_case", "year", "ccode_cow", "ccode_ksg", 
           "m49", "isoa3", "country_id_vdem", "region", "subregion", 
           "intregion", "region_wb", "micro_ksg")
then <- names(df)[!names(df) %in% c(first, not)]

df <- df %>%
  select(all_of(first), all_of(then)) %>%
  mutate(country = ifelse(country == "Ethiopia, FDR", "Ethiopia", country),
         country_case = ifelse(country_case == "Ethiopia, FDR", "Ethiopia", country_case)) |> 
  left_join(db[["Countries"]] |> 
              select(country, country_fr) |> 
              rename(country_case_fr = country_fr), 
            by = c("country_case" = "country") ) |> 
  mutate(country_case_fr = case_when(
    country_case == "Czechia / Czechoslovakia" ~ "Tchéquie / Tchécoslovaquie", 
    country_case == "East Germany" ~ "Allemagne de l'Est", 
    country_case == "Russia" ~ "Russie", 
    country_case == "South Vietnam" ~ "Viêt Nam du Sud", 
    country_case == "South Yemen" ~ "Yémen du Sud", 
    TRUE ~ country_case_fr
  )) 

### special country cases 
df %>% 
  filter(country != country_case) %>%
  group_by(country) %>%
  mutate(beg = min(year),
         end = max(year)) %>%
  select(country, country_case, ccode_cow, ccode_ksg, beg, end) %>%
  distinct()

# unique(df$country)[!unique(df$country) %in% unique(db[["CountryYears"]]$country) ]
# unique(db[["CountryYears"]]$country)[!unique(db[["CountryYears"]]$country) %in% unique(df$country) ]

### assembling dataset for website and analyses

df <- df %>%
  left_join(db[["CountryYears"]] %>%
               select(!any_of(c(
                 "country", "cyID", "country_case", "ccode_case", "beg", "end",
                 "country_label", "country_label_fr", "region", "tjet_focus", 
                 "reg_democ", "reg_autoc", "reg_trans", "conflict"))) %>%
               rename(ccode_cow = ccode), 
             by = c("ccode_cow", "ccode_ksg", "country_id_vdem", "year")) %>% # losing Slovenia 1991?
  ### problem here with country years for micros states left in that TJET did not collect data on, filtering them out
  left_join(db[["CountryYears"]] %>%
              select(country, beg, end) %>%
              distinct(), 
            by = "country") %>% 
  filter(year >= beg | is.na(beg)) %>% 
  select(-beg, -end) %>% 
  arrange(country_case, year) %>%
  mutate(amnesties_sample = ifelse(amnesties > 0, 1, NA),
         reparations_sample = ifelse(reparations > 0, 1, NA),
         tcs_sample = ifelse(tcs > 0, 1, NA),
         trials_domestic_sample = ifelse(trials_domestic > 0, 1, NA),
         vettings_sample = ifelse(vettings > 0, 1, NA)) %>%
  group_by(country_case) %>% 
  fill(amnesties_sample,
       reparations_sample,
       tcs_sample,
       trials_domestic_sample, 
       vettings_sample,
       .direction = "down") %>%
  mutate(amnesties_sample = ifelse(is.na(amnesties_sample), 0, amnesties_sample),
         reparations_sample = ifelse(is.na(reparations_sample), 0, reparations_sample),
         tcs_sample = ifelse(is.na(tcs_sample), 0, tcs_sample),
         trials_domestic_sample = ifelse(is.na(trials_domestic_sample), 0, trials_domestic_sample),
         vettings_sample = ifelse(is.na(vettings_sample), 0, vettings_sample)) %>%
  ungroup() %>% 
  full_join(db[["ICC"]], by = "ccode_cow" ) %>%
  mutate(
    ICC_prelim_exam = case_when(year >= ICC_prelim_exam & 
                                  year <= ICC_prelimEnd ~ 1, 
                                year >= ICC_investigation ~ 0, 
                                TRUE ~ 0),
    ICC_referral = case_when(year >= ICC_referral ~ 1, 
                             TRUE ~ 0),
    ICC_investigation = case_when(year >= ICC_investigation ~ 1, 
                                  TRUE ~ 0)) %>% 
  select(-ICC_prelimEnd) %>%
  arrange(country_case, year) %>%
  group_by(country_case) %>% 
  mutate(ICC_prelim_exam_cumu = cumsum(ICC_prelim_exam), 
         ICC_investigation_cumu = cumsum(ICC_investigation)) %>% 
  ungroup() %>% 
  full_join(db[["ICCaccused"]] %>% 
              select(ccode_Accused, ICC_arrest_warrant) %>%
              filter(!is.na(ICC_arrest_warrant) & ICC_arrest_warrant <= 2024) %>% 
              mutate(icc_action = 1) %>%
              group_by(ccode_Accused, ICC_arrest_warrant) %>%  
              reframe(icc_action = sum(icc_action)),
            by = c("ccode_cow" = "ccode_Accused", "year" = "ICC_arrest_warrant")) %>% 
  rename(ICC_arrest_warrant = icc_action) %>% 
  full_join(db[["ICCaccused"]] %>% 
              select(ccode_Accused, ICC_arrestAppear) %>%
              filter(!is.na(ICC_arrestAppear) & ICC_arrestAppear <= 2024) %>%
              mutate(icc_action = 1) %>%
              group_by(ccode_Accused, ICC_arrestAppear) %>%  
              reframe(icc_action = sum(icc_action)),
            by = c("ccode_cow" = "ccode_Accused", "year" = "ICC_arrestAppear")) %>% 
  rename(ICC_arrestAppear = icc_action) %>% 
  full_join(db[["ICCaccused"]] %>% 
              select(ccode_Accused, ICC_confirm_charges) %>%
              filter(!is.na(ICC_confirm_charges) & ICC_confirm_charges <= 2024) %>%
              mutate(icc_action = 1) %>%
              group_by(ccode_Accused, ICC_confirm_charges) %>%  
              reframe(icc_action = sum(icc_action)),
            by = c("ccode_cow" = "ccode_Accused", "year" = "ICC_confirm_charges")) %>% 
  rename(ICC_confirm_charges = icc_action) %>% 
  full_join(db[["ICCaccused"]] %>% 
              select(ccode_Accused, ICC_proceedings) %>%
              filter(!is.na(ICC_proceedings) & ICC_proceedings <= 2024) %>%
              mutate(icc_action = 1) %>%
              group_by(ccode_Accused, ICC_proceedings) %>%  
              reframe(icc_action = sum(icc_action)),
            by = c("ccode_cow" = "ccode_Accused", "year" = "ICC_proceedings")) %>% 
  rename(ICC_proceedings = icc_action) %>% 
  full_join(db[["ICCaccused"]] %>% 
              select(ccode_Accused, ICC_withdrawnDismissed) %>%
              filter(!is.na(ICC_withdrawnDismissed) & ICC_withdrawnDismissed <= 2024) %>%
              mutate(icc_action = 1) %>%
              group_by(ccode_Accused, ICC_withdrawnDismissed) %>%  
              reframe(icc_action = sum(icc_action)),
            by = c("ccode_cow" = "ccode_Accused", "year" = "ICC_withdrawnDismissed")) %>% 
  rename(ICC_withdrawnDismissed = icc_action) %>% 
  mutate(ICC_arrest_warrant = ifelse(is.na(ICC_arrest_warrant), 0, ICC_arrest_warrant), 
         ICC_arrestAppear = ifelse(is.na(ICC_arrestAppear), 0, ICC_arrestAppear), 
         ICC_confirm_charges = ifelse(is.na(ICC_confirm_charges), 0, ICC_confirm_charges), 
         ICC_proceedings = ifelse(is.na(ICC_proceedings), 0, ICC_proceedings), 
         ICC_withdrawnDismissed = ifelse(is.na(ICC_withdrawnDismissed), 0, ICC_withdrawnDismissed)) %>% 
  arrange(country_case, year) %>%
  group_by(country_case) %>% 
  mutate(ICC_arrest_warrant_cumu = cumsum(ICC_arrest_warrant), 
         ICC_proceedings_cumu = cumsum(ICC_proceedings)) %>% 
  ungroup() %>% 
  left_join(read_csv("data/icc_statesparty.csv") %>%
              filter(ccode != 511) %>%
              mutate(ccode = ifelse(ccode == 260 & year == 1990, 255, ccode)) %>%
              select(-country),
            by = c("ccode_cow" = "ccode", "year" = "year") ) %>%
  mutate(
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Andorra" & year >= 2001, 1, icc_sp),
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Bahamas", 0, icc_sp), 
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Bhutan", 0, icc_sp), 
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Croatia" & year >= 2001, 1, icc_sp), 
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Dominica" & year >= 2001, 1, icc_sp), 
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Grenada" & year >= 2011, 1, icc_sp), 
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Kiribati" & year >= 2019, 1, icc_sp), 
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Liechtenstein" & year >= 2001, 1, icc_sp),
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Marshall Islands" & year >= 2000, 1, icc_sp),
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Micronesia", 0, icc_sp), 
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Monaco", 0, icc_sp), 
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Nauru" & year >= 2001, 1, icc_sp),
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Oman", 0, icc_sp), 
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Saint Lucia" & year >= 2010, 1, icc_sp),
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Samoa" & year >= 2002, 1, icc_sp),
    icc_sp = ifelse(is.na(icc_sp) & country ==  "San Marino" & year >= 1999, 1, icc_sp),
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Tonga", 0, icc_sp), 
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Tuvalu", 0, icc_sp),
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Vanuatu" & year >= 2011, 1, icc_sp),
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Burundi" & year >= 2017, 0, icc_sp),
    icc_sp = ifelse(is.na(icc_sp) & country ==  "Philippines" & year >= 2019, 0, icc_sp),
    icc_sp = ifelse(is.na(icc_sp) & year <= 2021, 0, icc_sp)
    ) %>% 
  group_by(region, year) %>% 
  mutate(icc_sp_region = sum(icc_sp), 
         ICC_prelim_exam_region = sum(ICC_prelim_exam_cumu),
         ICC_investigation_region = sum(ICC_investigation_cumu),
         ICC_arrest_warrant_region = sum(ICC_arrest_warrant_cumu),
         ICC_proceedings_region = sum(ICC_proceedings_cumu)
         ) %>% 
  ungroup() %>% 
  left_join(db[["Investigations"]], 
            by = c("ccode_cow", "year")) %>% 
  mutate(across(c("uninv_beg", "uninv", "uninv_dompros_beg", "uninv_dompros", "uninv_evcoll_beg", 
                  "uninv_evcoll", "uninv_intlpros_beg", "uninv_intlpros"), 
                ~ ifelse(is.na(.x), 0, .x))) %>% 
  mutate(sample_trans = ifelse(transition == 1, year, NA),
         sample_confl_25 = ifelse(confl_new_25 == 1 | confl_recur_25 == 1 | confl_cont_25 == 1, year, NA), 
         sample_confl_100 = ifelse(confl_new_100 == 1 | confl_recur_100 == 1 | confl_cont_100 == 1, year, NA), 
         sample_confl_1000 = ifelse(confl_new_1000 == 1 | confl_recur_1000 == 1 | confl_cont_1000 == 1, year, NA), 
         dco_25 = ifelse(!is.na(sample_confl_25) & year == sample_confl_25, 1, 0), ### binary, when conflict active
         dco_100 = ifelse(!is.na(sample_confl_100) & year == sample_confl_100, 1, 0),
         dco_1000 = ifelse(!is.na(sample_confl_1000) & year == sample_confl_1000, 1, 0),
         # dco = dco_25
  ) %>%
  group_by(country_case) %>% 
  mutate(sample_trans = min(sample_trans, na.rm = TRUE), 
         ## warnings here that are addressed in next mutate
         sample_confl_25 = min(sample_confl_25, na.rm = TRUE), 
         sample_confl_100 = min(sample_confl_100, na.rm = TRUE), 
         sample_confl_1000 = min(sample_confl_1000, na.rm = TRUE), 
         peace_spell_25 = ifelse(dco_25 == 0 & (lag(dco_25) == 1 | is.na(lag(dco_25))), paste(ccode_ksg, year, sep = "-"), NA), 
         peace_spell_100 = ifelse(dco_100 == 0 & (lag(dco_100) == 1 | is.na(lag(dco_100))), paste(ccode_ksg, year, sep = "-"), NA), 
         peace_spell_1000 = ifelse(dco_1000 == 0 & (lag(dco_1000) == 1 | is.na(lag(dco_1000))), paste(ccode_ksg, year, sep = "-"), NA) 
         ) %>% 
  fill(peace_spell_25, peace_spell_100, peace_spell_1000, .direction = "down") |> 
  ungroup() %>%
  mutate(peace_spell_25 = ifelse(confl_cont_25 > 0, NA, peace_spell_25), 
         peace_spell_100 = ifelse(confl_cont_100 > 0, NA, peace_spell_100), 
         peace_spell_1000 = ifelse(confl_cont_1000 > 0, NA, peace_spell_1000), 
         pco_25 = ifelse(year > sample_confl_25 & dco_25 == 0, 1, 0),
         pco_100 = ifelse(year > sample_confl_100 & dco_100 == 0, 1, 0),
         pco_1000 = ifelse(year > sample_confl_1000 & dco_1000 == 0, 1, 0),
         # pco = pco_25, 
         sample_confl_25 = ifelse(is.infinite(sample_confl_25), NA, sample_confl_25),
         sample_confl_100 = ifelse(is.infinite(sample_confl_100), NA, sample_confl_100),
         sample_confl_1000 = ifelse(is.infinite(sample_confl_1000), NA, sample_confl_1000),
         aco_25 = case_when(is.na(sample_confl_25) | year < sample_confl_25 ~ 0, 
                            year >= sample_confl_25 ~ 1), 
         aco_100 = case_when(is.na(sample_confl_100) | year < sample_confl_100 ~ 0, 
                            year >= sample_confl_100 ~ 1), 
         aco_1000 = case_when(is.na(sample_confl_1000) | year < sample_confl_1000 ~ 0, 
                             year >= sample_confl_1000 ~ 1), 
         # aco = aco_25, 
         sample_trans = ifelse(is.infinite(sample_trans), NA, sample_trans),
         dtr = case_when(is.na(sample_trans) | year < sample_trans ~ 0, 
                         year >= sample_trans ~ 1), 
  ) %>% 
  left_join(reversions %>% select(-country), 
            by = c("country_id_vdem", "year")) %>% 
  mutate(reg_chg = case_when(!is.na(dem_reversion) | transition == 1 ~ 1, 
                             TRUE ~ 0), 
         reg_anoc = case_when(v2x_regime_ert %in% c("Closed autocracy", "Liberal democracy") ~ 0, 
                              str_detect(v2x_regime_ert, "Electoral") ~ 1)
         ) |> 
  left_join(fair_trials, by = c("ccode_cow" = "ccode", "year" = "year")) %>%
  group_by(country_case) %>%
  fill(fair_postautocratic_trials, .direction = "down") %>%
  mutate(fair_postautocratic_trials = ifelse(is.na(fair_postautocratic_trials), 0, fair_postautocratic_trials)) %>% 
  ungroup() %>% 
  arrange(country_case, year) %>%
  group_by(country_case, isna = is.na(theta_mean_fariss) ) %>%
  mutate(cum_theta_mean_fariss = ifelse(isna, NA, cummean(theta_mean_fariss)),
         sample_combi = ifelse(sample_trans + sample_confl_25 > 0, 1, 0) ) %>%
  ungroup() %>%
  select(-isna) 

if(nrow(df) != 9871) stop("Incorrect number of country-year for 1970-2024") 

### these CYs are included in the analyses data but not in TJET CountryYears
### this is ok because CountryYears is for mapping purposes and 
### these are for the most part microstates for which TJET has later start years
### we could elect to delete these country years from the analyses dataset
### UPDATE: this has now been addressed upstream, so this no longer applies 

df %>% 
  select(country, ccode_cow, ccode_ksg, year) %>% 
  mutate(df = TRUE) %>% 
  full_join(db[["CountryYears"]] %>% 
              select(country, ccode, ccode_ksg, year) %>% 
              mutate(db = TRUE), 
            by = c("ccode_cow" = "ccode", 
                   "ccode_ksg" = "ccode_ksg", 
                   "year" = "year") ) %>% 
  filter(is.na(df) | is.na(db) | is.na(country.x) | is.na(country.y)) %>% 
  group_by(ccode_cow) %>% 
  mutate(beg = min(year), 
         end = max(year)) %>% 
  select(-year) %>% 
  distinct() %>% 
  print(n = Inf)

## for dev only
# df <- df %>%
#   select(country, country_case, ccode_cow, year)

source("pipeline/go/measures_amnesties.R", echo = TRUE)
source("pipeline/go/measures_prosecutions.R", echo = TRUE)
source("pipeline/go/measures_tcs.R", echo = TRUE) 
df <- ReparationMeasures(cy = df)
df <- VettingMeasures(cy = df)
rm(vet_spells)

### checking domestic trials sample indicator

# df %>%
#   filter(year < 2021) %>%
#   # filter(is.na(trials_domestic) |
#   #          is.na(all_trs_dom_ctj_dtj_dcj_pcj_all) |
#   #          is.na(tran_trs_dom_dtj_ctj_sta) |
#   #          is.na(tran_trs_dom_ctj_opp) |
#   #          is.na(regu_trs_dom_sta) |
#   #          is.na(oppo_trs_dom_sta_opp))  %>%
#   arrange(country, year) %>%
#   select(country, year, trials_domestic, tran_trs_dom_dtj_ctj_sta, 
#          tran_trs_dom_ctj_opp, regu_trs_dom_sta, oppo_trs_dom_sta_opp) %>%
#   mutate(new = tran_trs_dom_dtj_ctj_sta + regu_trs_dom_sta + tran_trs_dom_ctj_opp + oppo_trs_dom_sta_opp,
#          diff = new - trials_domestic) %>%
#   filter(diff != 0) %>%
#   print(n = Inf)

### integrate HRA indices

# hra <- read_csv("../tjet-hra/tjet-hra.csv")
# unique(hra$country)[!unique(hra$country) %in% unique(df$country_case)]
# unique(df$country_case)[!unique(df$country_case) %in% unique(hra$country)]

# df %>%
#   select(country, country_case, year, country_id_vdem) %>%
#   filter(year < 2021) %>%
#   mutate(df = TRUE) %>%
#   full_join(read_csv("../tjet-hra/tjet-hra.csv") %>%
#               select(country, cid_vdem, year) %>%
#               mutate(hra = TRUE),
#             by = c("country_case" = "country", "year" = "year")) %>%
#   filter(country_id_vdem != cid_vdem)

### HRA 

hra <- read_csv("../tjet-hra/tjet-hra.csv") %>%
  select(country, year, legacy_mean, legacy_low95, legacy_upp95, legacy_rank)
hra_lags <- c(1:10, 20) |>
  map(function(yrs) {
    hra |>
      select(country, year, legacy_mean) |>
      mutate(year = year + yrs) |>
      rename_with(.cols = legacy_mean, ~ paste(.x, "_lag", yrs, sep = ""))
  }) |>
  reduce(full_join, by = c("country", "year")) |>
  filter(year <= 2024)
hra <- full_join(hra, hra_lags, by = c("country", "year")) |> 
  filter(year %in% 1970:2024) 

df <- df %>% 
  left_join(hra, by = c("country_case" = "country", "year" = "year")) |> 
  mutate(legacy_decile = as.numeric(gtools::quantcut(legacy_mean, q = 10)))

### cleanup
# first <- c(first, "dtr", "aco", "dco", "pco")
not <- c(not, "regime_sample", "reg_democ", "reg_autoc", "reg_trans", 
         "transition", "conflict", "sample_trans", "sample_confl_25", 
         "sample_confl_100", "sample_confl_1000", "sample_combi") 
then <- names(df)[!names(df) %in% c(first, not)]
df <- df %>% 
  select(all_of(first), dtr, all_of(then))
rm(first, not, then)

### var order
codebook <- db[["codebook"]] %>% 
  filter(tables == "tjet_cy") %>% 
  filter(col_name != "lag_*") %>% 
  filter(!str_detect(col_name, "access_"))
## these are in the codebook but not in the dataset 
codebook$col_name[!codebook$col_name %in% names(df)]
## these are in the dataset but not in the codebook
if(length(names(df)[!names(df) %in% codebook$col_name]) > 0) {
  print(names(df)[!names(df) %in% codebook$col_name])
  stop("Not all dataset variable names are included in the codebook; add these to the codebook first!") 
}

df <- df %>% 
  select(all_of(codebook$col_name[codebook$col_name %in% names(df)]))

### create lags and saving the analyses dataset
### saving to Dropbox only works locally NEED TO DISABLE THIS FOR GITHUB ACTIONS 
dropbox_path <- "~/Dropbox/TJLab/TimoDataWork/analyses_datasets/"
exclude <- c("country_label", "country_name", "country_fr", "country_label_fr")
# lags <- df %>%
#   select(!any_of(c("country", "country_label", "country_name", "country_fr", 
#                    "country_label_fr", "ccode_cow", "ccode_ksg", "m49", "isoa3", 
#                    "country_id_vdem", "region", "subregion", "intregion", 
#                    "region_wb", "micro_ksg", "dtr", "aco", "dco", "pco"))) %>%
#   mutate(year = year + 1) %>%
#   rename_with(~ paste0("lag_", .x))
df %>% 
  # left_join(lags, by = c("country_case" = "lag_country_case",
  #                        "year" = "lag_year")) %>%
  mutate(tjet_version = timestamp) %>% 
  select(!any_of(exclude)) %>% 
  write_csv(here::here("tjet_datasets", "tjet_cy_analyses.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_cy_analyses.csv"), na = "")
rm(exclude)

### downloads datasets 
# - include country IDs, transitions and our data
# - everything in our filters, but not other outcomes
# - variables that should not be in public CY downloads file?
#   - regime_sample, reg_democ, reg_autoc, reg_trans, conflict, transition

included <- codebook$col_name[codebook$col_name %in% names(df)]

codebook %>% 
  filter(col_name %in% included) %>% 
  select(section, col_name, definition, source_abr) %>% 
  rename(source = source_abr) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_codebook_analyses.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_codebook_analyses.csv"), na = "")

db[["dl_tjet_codebook"]] <- codebook %>% 
  filter(col_name %in% included & !is.na(section)) %>% 
  select(section, col_name, definition, source_abr) %>% 
  rename(source = source_abr) %>% 
  left_join(read_csv(here::here("data", "sources.csv")), by = "source") %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_codebook.csv"), na = "")
rm(codebook, included) 

vars <- db[["dl_tjet_codebook"]]$col_name[db[["dl_tjet_codebook"]]$col_name %in% names(df)]

db[["dl_tjet_cy"]] <- df %>%
  select(all_of(vars)) %>% 
  filter(year %in% db_years_cy) %>% 
  filter(!(country == "Andorra" & year < 1994)) %>% 
  filter(!(country == "Antigua and Barbuda" & year == 1981)) %>%
  filter(!(country == "Brunei" & year == 1984)) %>% 
  filter(!(country == "Kiribati" & year < 1997)) %>%
  filter(!(country == "Liechtenstein" & year < 1991)) %>% 
  filter(!(country == "Marshall Islands" & year < 1992)) %>%
  filter(!(country == "Micronesia" & year < 1992)) %>%
  filter(!(country == "Monaco" & year < 1994)) %>%
  filter(!(country == "Palau" & year < 1995)) %>%
  filter(!(country == "Saint Kitts and Nevis" & year == 1983)) %>%
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_cy.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_cy.csv"), na = "")
rm(vars)

### translations checks 
### INTEGRATE ELSEWHERE?

db[["Amnesties"]] <- db[["Amnesties"]] %>% 
  mutate(mechanismDescription_fr = str_replace_all(mechanismDescription_fr, "droits de l'homme", "droits de la personne")) 
db[["Amnesties"]] %>% 
  filter(amnestyYear %in% db_years_mech) %>%
  select(amnestyID, invalid, amnestyYear, mechanismDescription_fr) %>% 
  filter(str_detect(mechanismDescription_fr, "droits de l'homme") | is.na(mechanismDescription_fr)) 

db[["Reparations"]] <- db[["Reparations"]] %>% 
  mutate(officialName_en_fr = str_replace_all(officialName_en_fr, "droits de l'homme", "droits de la personne")) 
db[["Reparations"]] %>% 
  filter(yearCreated %in% db_years_mech) %>%
  select(reparationID, invalid, yearCreated, officialName_en_fr) %>% 
  filter(str_detect(officialName_en_fr, "droits de l'homme") | is.na(officialName_en_fr)) 

db[["Trials"]] <- db[["Trials"]] %>% 
  mutate(caseDescription_fr = str_replace_all(caseDescription_fr, "droits de l'homme", "droits de la personne")) 
db[["Trials"]] %>% 
  filter(yearStart %in% db_years_trials) %>%
  select(trialID, invalid, yearStart, caseDescription_fr) %>% 
  filter(str_detect(caseDescription_fr, "droits de l'homme") | is.na(caseDescription_fr)) %>% 
  arrange(trialID) |> 
  print(n = Inf)

db[["Accused"]] <- db[["Accused"]] %>% 
  mutate(position_desc_fr = str_replace_all(position_desc_fr, "droits de l'homme", "droits de la personne"))
db[["Accused"]] %>% 
  select(accusedID, invalid, position_desc, position_desc_fr) %>% 
  filter(!is.na(position_desc) & (str_detect(position_desc_fr, "droits de l'homme") | is.na(position_desc_fr))) %>% 
  arrange(accusedID) |> 
  print(n = Inf)
 
db[["TruthCommissions"]] <- db[["TruthCommissions"]] %>% 
  mutate(officialName_en_fr = str_replace_all(officialName_en_fr, "droits de l'homme", "droits de la personne"))
db[["TruthCommissions"]] %>% 
  filter(yearPassed %in% db_years_mech) %>%
  select(truthcommissionID, yearPassed, officialName_en_fr) %>% 
  filter(str_detect(officialName_en_fr, "droits de l'homme") | is.na(officialName_en_fr)) 

db[["Vettings"]] <- db[["Vettings"]] %>%
  mutate(policyName_fr = str_replace_all(policyName_fr,"droits de l'homme", "droits de la personne"))
db[["Vettings"]] %>% 
  select(vettingID, invalid, yearStart, policyName, policyName_fr) %>% 
  filter(str_detect(policyName_fr, "droits de l'homme") | is.na(policyName_fr)) 

db[["Countries"]] <- db[["Countries"]] %>%
  mutate(across(all_of(c("auto_conflict_fr", "auto_regime_fr", "txt_amnesties_fr", 
                         "txt_conflict_fr", "txt_domestic_fr", "txt_foreign_fr", 
                         "txt_intl_fr", "txt_intro_fr", "txt_regime_fr", 
                         "txt_reparations_fr", "txt_summary_fr", "txt_tcs_fr", 
                         "txt_TJ", "txt_TJ_fr", "txt_un_fr", "txt_vetting_fr")), 
                ~ str_replace_all(.x, "droits de l'homme", "droits de la personne")))

### saving individual mechanism tables for local analyses & repo
### these will also be written to the database for downloads
### THIS COULD BE MOVED UP TO BEFORE DF PREP

db[["Transitions"]] <- db[["Transitions"]] %>% 
  rename(ccode_cow = ccode) %>%
  mutate(tjet_version = timestamp) %>%
  write_csv(here::here("tjet_datasets", "tjet_transitions.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_transitions.csv"), na = "")

db[["AmnestiesCodebook"]] <- db[["codebook"]] %>% 
  filter(tables == "tjet_amnesties.csv" & !is.na(order)) %>%
  arrange(order) |> 
  # filter(col_name %in% names(db[["Amnesties"]])) %>% 
  select(col_name, definition) %>% 
  # arrange(str_to_lower(col_name)) %>% 
  write_csv(here::here("tjet_datasets", "tjet_amnesties_codebook.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_amnesties_codebook.csv"), na = "")
db[["Amnesties"]] <- db[["Amnesties"]] %>% 
  left_join(db[["Countries"]] %>%
              select(country_case, ccode) %>% 
              rename(country = country_case) %>% 
              distinct(),
            by = "ccode") %>% 
  filter(amnestyYear %in% db_years_mech) %>%
  rename(ccode_cow = ccode) %>% 
  arrange(country, amnestyYear) |>  
  mutate(tjet_version = timestamp) %>% 
  select(all_of(db[["AmnestiesCodebook"]] |> 
                  select(col_name) |> 
                  unlist(use.names = FALSE))) %>% 
  write_csv(here::here("tjet_datasets", "tjet_amnesties.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_amnesties.csv"), na = "")

db[["ReparationsCodebook"]] <- db[["codebook"]] %>% 
  filter(tables == "tjet_reparations.csv" & !is.na(order)) %>% 
  arrange(order) |> 
  # filter(col_name %in% names(db[["Reparations"]])) %>% 
  select(col_name, definition) %>% 
  # arrange(str_to_lower(col_name)) %>% 
  write_csv(here::here("tjet_datasets", "tjet_reparations_codebook.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_reparations_codebook.csv"), na = "")
db[["Reparations"]] <- db[["Reparations"]] %>% 
  left_join(db[["Countries"]] %>%
              select(country_case, ccode) %>% 
              rename(country = country_case) %>% 
              distinct(),
            by = "ccode") %>% 
  filter(yearCreated %in% db_years_mech) %>%
  rename(ccode_cow = ccode) %>% 
  arrange(country, yearCreated) |> 
  mutate(tjet_version = timestamp) %>% 
  select(all_of(db[["ReparationsCodebook"]] |> 
                  select(col_name) |> 
                  unlist(use.names = FALSE))) %>% 
  write_csv(here::here("tjet_datasets", "tjet_reparations.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_reparations.csv"), na = "")

db[["TrialsCodebook"]] <- db[["codebook"]] %>% 
  filter(tables == "tjet_trials.csv" & !is.na(order)) %>% 
  arrange(order) |> 
  # filter(col_name %in% names(db[["Trials"]])) %>% 
  select(col_name, definition) %>% 
  # arrange(str_to_lower(col_name)) %>% 
  write_csv(here::here("tjet_datasets", "tjet_trials_codebook.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_trials_codebook.csv"), na = "")
db[["Trials"]] <- db[["Trials"]] %>%
  left_join(db[["Countries"]] %>%
              select(country_case, ccode) %>% 
              rename(country = country_case) %>% 
              distinct(),
            by = c("ccode_Accused" = "ccode")) %>% 
  rename(country_Accused = country) %>% 
  left_join(db[["Countries"]] %>%
              select(country_case, ccode) %>% 
              rename(country = country_case) %>% 
              distinct(),
            by = c("ccode_Trial" = "ccode")) %>% 
  rename(country_Trial = country) %>% 
  filter(yearStart %in% db_years_trials) %>% 
  arrange(country_Accused, yearStart, yearEnd) |> 
  mutate(tjet_version = timestamp) %>% 
  select(all_of(db[["TrialsCodebook"]] |> 
                  select(col_name) |> 
                  unlist(use.names = FALSE))) %>% 
  write_csv(here::here("tjet_datasets", "tjet_trials.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_trials.csv"), na = "")

db[["AccusedCodebook"]] <- db[["codebook"]] %>% 
  filter(tables == "tjet_accused.csv" & !is.na(order)) %>% 
  arrange(order) |> 
  # filter(col_name %in% names(db[["Accused"]])) %>% 
  select(col_name, definition) %>% 
  # arrange(str_to_lower(col_name)) %>% 
  write_csv(here::here("tjet_datasets", "tjet_accused_codebook.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_accused_codebook.csv"), na = "")
db[["Accused"]] <- db[["Accused"]] %>%
  arrange(trialID, accusedID) |> 
  mutate(tjet_version = timestamp) %>% 
  select(all_of(db[["AccusedCodebook"]] |> 
                  select(col_name) |> 
                  unlist(use.names = FALSE))) %>% 
  write_csv(here::here("tjet_datasets", "tjet_accused.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_accused.csv"), na = "")

db[["CourtLevelsCodebook"]] <- db[["codebook"]] %>% 
  filter(tables == "tjet_courtlevels.csv" & !is.na(order)) %>% 
  arrange(order) |> 
  # filter(col_name %in% names(db[["CourtLevels"]])) %>% 
  select(col_name, definition) %>% 
  # arrange(str_to_lower(col_name)) %>% 
  write_csv(here::here("tjet_datasets", "tjet_courtlevels_codebook.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_courtlevels_codebook.csv"), na = "")
db[["CourtLevels"]] <- db[["CourtLevels"]] %>%
  # select(CLID, accusedID, courtLevel, courtName, day, month, year, date, 
  #        last_fx, verdict, guilty, sentence, sentencingTime, 
  #        sentencingArrangement, sentenceNotes) %>% 
  arrange(accusedID, date) |> 
  mutate(tjet_version = timestamp) %>% 
  select(all_of(db[["CourtLevelsCodebook"]] |> 
                  select(col_name) |> 
                  unlist(use.names = FALSE))) %>% 
  write_csv(here::here("tjet_datasets", "tjet_courtlevels.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_courtlevels.csv"), na = "")

db[["TruthCommissionsCodebook"]] <- db[["codebook"]] %>% 
  filter(tables == "tjet_tcs.csv" & !is.na(order)) %>% 
  arrange(order) |> 
  # filter(col_name %in% names(db[["TruthCommissions"]])) %>% 
  select(col_name, definition) %>% 
  # arrange(str_to_lower(col_name)) %>% 
  write_csv(here::here("tjet_datasets", "tjet_tcs_codebook.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_tcs_codebook.csv"), na = "")
db[["TruthCommissions"]] <- db[["TruthCommissions"]] %>%
  left_join(db[["Countries"]] %>%
              select(country_case, ccode) %>% 
              rename(country = country_case) %>% 
              distinct(),
            by = "ccode") %>% 
  filter(yearPassed %in% db_years_mech) %>%
  rename(ccode_cow = ccode) %>% 
  arrange(country, yearPassed) |> 
  mutate(tjet_version = timestamp) %>% 
  select(all_of(db[["TruthCommissionsCodebook"]] |> 
                  select(col_name) |> 
                  unlist(use.names = FALSE))) %>% 
  write_csv(here::here("tjet_datasets", "tjet_tcs.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_tcs.csv"), na = "")

db[["VettingsCodebook"]] <- db[["codebook"]] %>% 
  filter(tables == "tjet_vettings.csv" & !is.na(order)) %>% 
  arrange(order) |> 
  # filter(col_name %in% names(db[["Vettings"]])) %>% 
  select(col_name, definition) %>% 
  # arrange(str_to_lower(col_name)) %>% 
  write_csv(here::here("tjet_datasets", "tjet_vettings_codebook.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_vettings_codebook.csv"), na = "")
db[["Vettings"]] <- db[["Vettings"]] %>% 
  left_join(db[["Countries"]] %>%
              select(country_case, ccode) %>% 
              rename(country = country_case) %>% 
              distinct(),
            by = "ccode") %>% 
  filter(yearStart %in% db_years_mech) %>%
  rename(ccode_cow = ccode) %>% 
  arrange(country, yearStart) |> 
  mutate(tjet_version = timestamp) %>% 
  select(all_of(db[["VettingsCodebook"]] |> 
                  select(col_name) |> 
                  unlist(use.names = FALSE))) %>% 
  write_csv(here::here("tjet_datasets", "tjet_vettings.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_vettings.csv"), na = "")

db[["InvestigationsCodebook"]] <- db[["codebook"]] %>% 
  filter(tables == "tjet_un_investigations.csv" & !is.na(order)) %>% 
  arrange(order) |> 
  # filter(col_name %in% names(db[["Investigations"]])) %>% 
  select(col_name, definition) %>% 
  # arrange(str_to_lower(col_name)) %>% 
  write_csv(here::here("tjet_datasets", "tjet_un_investigations_codebook.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_un_investigations_codebook.csv"), na = "")
db[["Investigations"]] <- dl_invest %>% 
  arrange(country, beg, end) |> 
  mutate(tjet_version = timestamp) %>% 
  select(all_of(db[["InvestigationsCodebook"]] |> 
                  select(col_name) |> 
                  unlist(use.names = FALSE))) %>% 
  write_csv(here::here("tjet_datasets", "tjet_un_investigations.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_un_investigations.csv"), na = "")
rm(dl_invest)

db[["ICCcodebook"]] <- db[["codebook"]] %>% 
  filter(tables == "tjet_icc_interventions.csv" & !is.na(order)) %>% 
  arrange(order) |> 
  # filter(col_name %in% names(db[["ICC"]])) %>% 
  select(col_name, definition) %>% 
  # arrange(str_to_lower(col_name)) %>% 
  write_csv(here::here("tjet_datasets", "tjet_icc_interventions_codebook.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_icc_interventions_codebook.csv"), na = "")
db[["ICC"]] <- db[["ICC"]] %>% 
  arrange(ccode_cow, ICC_prelim_exam) |> 
  mutate(tjet_version = timestamp) %>% 
  select(all_of(db[["ICCcodebook"]] |> 
                  select(col_name) |> 
                  unlist(use.names = FALSE))) %>% 
  write_csv(here::here("tjet_datasets", "tjet_icc_interventions.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_icc_interventions.csv"), na = "")

db[["ICCaccusedCodebook"]] <- db[["codebook"]] %>% 
  filter(tables == "tjet_icc_accused.csv" & !is.na(order)) %>% 
  arrange(order) |> 
  # filter(col_name %in% names(db[["ICCaccused"]])) %>% 
  select(col_name, definition) %>% 
  # arrange(str_to_lower(col_name)) %>% 
  write_csv(here::here("tjet_datasets", "tjet_icc_accused_codebook.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_icc_accused_codebook.csv"), na = "")
db[["ICCaccused"]] <- db[["ICCaccused"]] %>% 
  arrange(ccode_Accused, trialID, accusedID) |> 
  mutate(tjet_version = timestamp) %>% 
  select(all_of(db[["ICCaccusedCodebook"]] |> 
                  select(col_name) |> 
                  unlist(use.names = FALSE))) %>% 
  write_csv(here::here("tjet_datasets", "tjet_icc_accused.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_icc_accused.csv"), na = "")

rm(timestamp)
save(db, file = here::here("data", "tjetdb.RData"))

source("pipeline/go/auto_texts.R", echo = TRUE)
