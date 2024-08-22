### packages
require(tidyverse)
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
             "AdHocHybrid", "Ethiopia", "ArgCausas", "ArgAccused", "ArgCLs")

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
checkbox_to_binary <- function(col) {
  ifelse(is.na(col), 0, 1)
}
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
      map(db[[basename]][[tab_name]][, fields], checkbox_to_binary)
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
                  select(all_of(c(
                    "airtable_record_id", pkeys[[tab_name]]
                  ))),
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

### dummies for multi-select fields for data downloads?
### sample code, would have to be expanded and generalized
### would need a consistent naming scheme
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
  ## filter(!(accusedID == 20968 & lastVerdictYear == 2015 & lastVerdict == "Guilty")) 
  ## taking out duplicate that were missed in Airtable -- resolved now

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
  mutate(yearEnd = ifelse(is.na(yearEnd) & ongoing == 1, 2023, yearEnd), 
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

### formatting transitions table for website 
db[["MegaBase"]][["Transitions"]] <-
  db[["MegaBase"]][["Transitions"]] %>%
  filter(trans == 1) %>% 
  mutate(p5 = case_when(is.na(p5_year) ~ 0, 
                        trans_year_begin < p5_year ~ 0,
                        trans_year_begin >= p5_year ~ 1),
         bmr = case_when(is.na(bmr_year) ~ 0, 
                         trans_year_begin < bmr_year ~ 0,
                         trans_year_begin >= bmr_year ~ 1),
         ert = case_when(is.na(ert_year) ~ 0, 
                         trans_year_begin < ert_year ~ 0,
                         trans_year_begin >= ert_year ~ 1), 
         nsupport = p5 + bmr + ert) %>% 
  rowwise() %>% 
  mutate(sources = str_flatten(c(
           case_when(!is.na(p5_year) ~ 
                       paste("Polity5 (", p5_year, ")", sep = "")),
           case_when(!is.na(bmr_year) ~ 
                       paste("BMR (", bmr_year, ")", sep = "")),
           case_when(!is.na(ert_year) ~ 
                       paste("VDem-ERT (", ert_year, ")", sep = ""))),
           collapse = " & ", na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(trans_year_begin >= 1970 & trans_year_begin <= 2020) %>%
  filter(!(ccode == 265 & trans_year_begin == 1990)) %>%
  select(transitionID, ccode, trans_year_begin, 
         nsupport, sources, p5_year, ert_year, bmr_year) %>%
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

### checking amnesties
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

# db[["MegaBase"]][["Reparations"]] %>% 
#   filter(harmsChildRecruitment == 1 | harmsTorture == 1 | 
#            harmsSexualViolence == 1 | harmsMurder == 1 | 
#            harmsDisappearance == 1 | harmsMurder == 1 | 
#            harmsDisplacement == 1 | harmsDetention == 1)

# db[["MegaBase"]][["TruthCommissions"]] %>%
#   filter(torture == 1 | SGBV == 1 | forcedDisplacement == 1 | 
#            torture == 1 | disappearance == 1 | 
#            focusedPast == 1 | investigatePatternAbuse == 1)

### UN investigations

dl_invest <- db[["MegaBase"]][["Investigations"]] %>%
  rename(beg = year_beg, 
         end = year_end) %>% 
  select(ccode_cow, beg, end, name, mandate_en, mandate_fr, 
         uninv_dompros, uninv_intlpros, uninv_evcoll, 
         secgen, unsc, cohr, unga, ohchr, hrc, source) %>% 
  filter(beg >= 1970 & beg <= 2020) %>%
  mutate(
    ccode_cow = ifelse(ccode_cow == 860 & beg == 1999, 850, ccode_cow),
    ccode_cow = ifelse(ccode_cow == 541 & beg == 1973, 235, ccode_cow)
  ) %>% 
  arrange(name, beg) %>%
  filter(!(ccode_cow == 490 & str_detect(name, " & "))) %>% 
  # mutate(name = str_split(name, " & ") ) %>% 
  # unnest(name, keep_empty = TRUE) %>% 
  distinct() %>%
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
  # mutate(n = n() ) %>%
  # filter(n > 1) %>% 
  # print(n = Inf)

### investigations where mandate differs
# dl_invest %>% 
#   filter(mandate != mandate_en | is.na(mandate_en))


dl_invest %>%
  select(ccode_cow, beg, uninv_dompros, uninv_evcoll, uninv_intlpros) %>%
  group_by(ccode_cow, beg) %>%
  reframe(uninv_dompros_created = max(uninv_dompros),
         uninv_evcoll_created = max(uninv_evcoll),
         uninv_intlpros_created = max(uninv_intlpros))

db[["MegaBase"]][["Investigations"]] <- dl_invest %>% 
  rowwise() %>% 
  mutate(uninv = 1, 
         year = list(beg:end)) %>% 
  unnest(year) %>% 
  select(ccode_cow, year, uninv, uninv_dompros, uninv_evcoll, uninv_intlpros) %>%
  group_by(ccode_cow, year) %>% 
  reframe(across(all_of(c("uninv", "uninv_dompros", 
                          "uninv_evcoll", "uninv_intlpros")), max)) %>% 
  left_join(dl_invest %>%
              select(ccode_cow, beg, uninv_dompros, uninv_evcoll, uninv_intlpros) %>%
              group_by(ccode_cow, beg) %>%
              reframe(uninv_dompros_created = max(uninv_dompros),
                      uninv_evcoll_created = max(uninv_evcoll),
                      uninv_intlpros_created = max(uninv_intlpros)), 
            by = c("ccode_cow" = "ccode_cow", "year" = "beg") ) 

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
rm(exclude, select, checkbox_to_binary, dim_drop, dim_last, dim_now, dim_orig, 
   drop_invalids, crimes, victims, multi_selects, pkeys)

### creating country list as basis for country-year dataset
countrylist <- db$Countries %>% 
  mutate(beg = as.integer(str_sub(begin_date, 1, 4)), 
         end = as.integer(str_sub(end_date, 1, 4)), 
         beg = ifelse(beg <= 1970, 1970, beg),
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
countrylist %>%
  select(country, beg, end, ccode, ccode_case) %>%
  rename("country_case" = "country") %>%
  group_by(ccode_case) %>%
  mutate(n = n()) %>%
  filter(n > 1 & ccode == ccode_case & end < 2020)

countrylist <- countrylist %>%
  left_join(countrylist %>% 
              select(country, country_fr, ccode) %>%
              filter(!country %in% 
                       c("Soviet Union", "Yugoslavia", "Serbia and Montenegro") ) %>%
              rename("country_case" = "country", 
                     "country_case_fr" = "country_fr"),
          by = c("ccode_case" = "ccode")) %>% 
  select(country, country_case, include, country_fr, country_case_fr, ccode, ccode_case, ccode_ksg, m49, isoa3, 
         country_id_vdem, beg, end, micro_ksg, region, region_sub_un, region_wb, 
         tjet_focus, factsheet, starts_with("txt_"), starts_with("auto_")) 

### ccode_case / country_case for the countries on the 2020 map that data are matched to
countrylist %>%
  filter(country != country_case | ccode != ccode_case |
           ccode %in% c(255, 260, 265, 315, 316, 345, 365, 678, 679, 680, 816, 817)) %>%
  select(country_case, ccode_case, country, ccode, beg, end) %>%
  arrange(country_case, end)

### clean up text fields 
tabs <- c("Amnesties", "Reparations", "TruthCommissions", 
          "Vettings", "Trials", "Accused") 
db[tabs] <- map(tabs, function(tab) {
  text_fields <- names(db[[tab]])[map(db[[tab]], class) == "character"]
  db[[tab]][text_fields] <- map(db[[tab]][text_fields], str_squish)
  return(db[[tab]])
})

### the next few code blocks could be simplified with functions

### preparing amnesties list for merging into country-year dataset
amnesties <- db[["Amnesties"]] %>% 
  arrange(ccode, amnestyYear) %>%  
  group_by(ccode, amnestyYear) %>%
  reframe(amnesties = n(),
          SGBV = max(SGBV)) %>%
  rename("year" = "amnestyYear", 
         "amnesties_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

### preparing reparations list for merging into country-year dataset
reparations <- db[["Reparations"]] %>%
  arrange(ccode, yearCreated) %>%  
  mutate(SGBV = ifelse(harmsSexualViolence == 1 | 
                         genderCrimes == "yes" | 
                         lgbtqCrimes == "yes", 1, 0) ) %>%    
  group_by(ccode, yearCreated) %>%
  reframe(reparations = n(),
          # genderAttentive = max(genderAttentive), 
          SGBV = max(SGBV)) %>% 
  rename("year" = "yearCreated",
         # "reparations_gaTJ" = "genderAttentive", 
         "reparations_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

### preparing TCs list for merging into country-year dataset
tcs <- db[["TruthCommissions"]] %>%
  arrange(ccode, yearPassed) %>%  
  group_by(ccode, yearPassed) %>%
  reframe(tcs = n(),
          SGBV = max(SGBV)) %>% 
  rename("year" = "yearPassed",
         "tcs_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

### preparing vettings list for merging into country-year dataset
vettings <- db[["Vettings"]] %>%
  filter(is.na(alterationOf)) %>% 
  arrange(ccode, yearStart) %>%  
  group_by(ccode, yearStart) %>%  
  reframe(vettings = n()) %>% 
  rename("year" = "yearStart") %>% 
  filter(year >= 1970 & year <= 2020)

### preparing trials list for merging into country-year dataset
trials <- db[["Trials"]] %>%
  rename("ccode" = "ccode_Accused") %>% 
  arrange(ccode, yearStart) %>%
  mutate(SGBV = ifelse(rape_Accused == 1 | 
                         sexualViolence_Accused == 1 | 
                         otherSGBV_Accused == 1, 1, 0) ) %>% 
  select(ccode, trialType, yearStart, SGBV) %>% 
  filter(yearStart >= 1970 & yearStart <= 2020) %>%
  rename("year" = "yearStart")

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
  filter(year >= 1970 & year <= 2020)

### transitions dataset in country-year format
translist <- read_csv("transitions/transitions_new_revised.csv",
                      show_col_types = FALSE) %>% 
  mutate(country = ifelse(country == "Congo (Brazzaville)", "Congo", country),
         country = ifelse(country == "DR Congo", "Democratic Republic of the Congo", country),
         country = ifelse(country == "Republic of Vietnam", "Vietnam (Republic of / South)", country),
         country = ifelse(country == "St. Kitts & Nevis", "Saint Kitts and Nevis", country),
         country = ifelse(country == "St. Lucia", "Saint Lucia", country),
         country = ifelse(country == "St. Vincent", "Saint Vincent and the Grenadines", country),
         country = ifelse(country == "Antigua & Barbuda", "Antigua and Barbuda", country),
         country = ifelse(country == "Bosnia & Herzegovina", "Bosnia and Herzegovina", country),
         country = ifelse(country == "Sao Tome & Principe", "Sao Tome and Principe", country),
         country = ifelse(country == "Trinidad & Tobago", "Trinidad and Tobago", country),
         country = ifelse(country == "Cape Verde", "Cabo Verde", country),
         country = ifelse(country == "Czech Republic", "Czechia", country),
         country = ifelse(country == "South Yemen", "Yemen People's Republic (South)", country),
         country = ifelse(country == "North Yemen", "Yemen Arab Republic (North)", country),
         country = ifelse(country == "Yemen" & year < 1990, "Yemen Arab Republic (North)", country),
         country = ifelse(country == "Germany" & year < 1990, "German Federal Republic (West)", country),
         country = ifelse(country == "Russia" & year < 1992, "Soviet Union", country),
         country = ifelse(country == "Russia", "Russian Federation", country),
         country = ifelse(country == "Serbia" & year < 1992, "Yugoslavia", country),
         country = ifelse(country == "Serbia" & year %in% 1992:2005, "Serbia and Montenegro", country)
  ) %>% 
  full_join(countrylist %>% 
              select(country, beg, end), 
            by = "country") %>%
  arrange(country, year) %>% 
  filter(year >= beg & year <= end) %>% 
  mutate(transition = ifelse(is.na(trans_year), 0, 1), 
         dem_polity = ifelse(polity_p5 >= 6, 1, 0),
         dem_vdem = ifelse(str_detect(str_to_lower(v2x_regime_amb), 
                                      "democracy"), 1, 0), 
         dem_all = rowSums(across(all_of(c("dem_bmr", "dem_polity", 
                                           "dem_vdem"))), na.rm = TRUE)/3) %>% 
  select(country, ccode, year, transition, dem_bmr, dem_polity, dem_vdem, dem_all) %>%
  group_by(ccode) %>% 
  mutate(finite_check = sum(!is.na(dem_polity)), 
         dem_polity_min = ifelse(finite_check > 0, 
                                 min(dem_polity, na.rm = TRUE), NA), 
         dem_polity_max = ifelse(finite_check > 0, 
                                 max(dem_polity, na.rm = TRUE), NA), 
         finite_check = sum(!is.na(dem_bmr)), 
         dem_bmr_min = ifelse(finite_check > 0, 
                              min(dem_bmr, na.rm = TRUE), NA), 
         dem_bmr_max = ifelse(finite_check > 0, 
                              max(dem_bmr, na.rm = TRUE), NA), 
         finite_check = sum(!is.na(dem_vdem)), 
         dem_vdem_min = ifelse(finite_check > 0, 
                               min(dem_vdem, na.rm = TRUE), NA), 
         dem_vdem_max = ifelse(finite_check > 0, 
                               max(dem_vdem, na.rm = TRUE), NA), 
         sources = 3 - (is.na(dem_polity_max) + 
                          is.na(dem_bmr_max) + is.na(dem_vdem_max)),
         regime = max(transition, na.rm = TRUE), 
         dem_prop = sum(dem_all, na.rm = TRUE) / n()) %>% 
  ungroup() %>% 
  mutate(context_bmr = case_when(dem_bmr_min == 0 & dem_bmr_max == 0 ~ 0,
                                 dem_bmr_min == 1 & dem_bmr_max == 1 ~ 1),
         context_polity = case_when(dem_polity_min == 0 & 
                                      dem_polity_max == 0 ~ 0,
                                    dem_polity_min == 1 & 
                                      dem_polity_max == 1 ~ 1),
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
         regime_sample, reg_democ, reg_autoc, reg_trans)

### democratic reversions data for merging 
reversions <- read_csv("transitions/transitions_new_revised.csv",
         show_col_types = FALSE) %>% 
  select(country, country_id_vdem, year, reg_type, reg_age, dem_spell_id, dem_reversion) %>% 
  rename(reg_type_vdem = reg_type,
         reg_age_vdem = reg_age,
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
  mutate(conflict = ifelse(is.na(conflict), 0, 1), 
         conflict_active = conflict) %>% 
  group_by(ccode_case) %>%
  mutate(conflict = max(conflict) ) %>%
  ungroup() %>% 
  mutate(country_label = ifelse(end < 2020, 
                                paste(country, " [-", end, ", included on ", country_case, " page]", sep = ""), 
                                country), 
         country_label_fr = ifelse(end < 2020,
                                   paste(country_fr, " [-", end, ", inclus dans la page ", country_case_fr, "]", sep = ""),
                                   country_fr)
         ) %>% 
  select(cyID, country, country_fr, country_label, country_label_fr, year, 
         beg, end, ccode, ccode_case, country_case, ccode_ksg, country_id_vdem, 
         tjet_focus, region, regime_sample, reg_democ, reg_autoc, reg_trans, 
         conflict, transition, conflict_active) %>% 
  left_join(amnesties, by = c("ccode", "year") ) %>% 
  mutate(amnesties = ifelse(is.na(amnesties), 0, amnesties),
         amnesties_SGBV = ifelse(is.na(amnesties_SGBV), 
                                 0, amnesties_SGBV)) %>% 
  left_join(reparations, by = c("ccode", "year") ) %>% 
  mutate(reparations = ifelse(is.na(reparations), 
                              0, reparations),
         # reparations_gaTJ = ifelse(is.na(reparations_gaTJ), 
         #                           0, reparations_gaTJ),
         reparations_SGBV = ifelse(is.na(reparations_SGBV), 
                                   0, reparations_SGBV)
         ) %>% 
  left_join(tcs, by = c("ccode", "year") ) %>% 
  mutate(tcs = ifelse(is.na(tcs), 0, tcs),
         tcs_SGBV = ifelse(is.na(tcs_SGBV), 
                           0, tcs_SGBV)) %>% 
  left_join(vettings, by = c("ccode", "year") ) %>% 
  mutate(vettings = ifelse(is.na(vettings), 
                           0, vettings)) %>% 
  left_join(domestic, by = c("ccode", "year") ) %>% 
  mutate(trials_domestic = ifelse(is.na(trials_domestic), 
                                  0, trials_domestic),
         trials_domestic_SGBV = ifelse(is.na(trials_domestic_SGBV), 
                                       0, trials_domestic_SGBV)) %>% 
  left_join(intl, by = c("ccode", "year") ) %>% 
  mutate(trials_intl = ifelse(is.na(trials_intl), 
                              0, trials_intl),
         trials_intl_SGBV = ifelse(is.na(trials_intl_SGBV), 
                                   0, trials_intl_SGBV)) %>% 
  left_join(foreign, by = c("ccode", "year") ) %>% 
  mutate(trials_foreign = ifelse(is.na(trials_foreign), 
                                 0, trials_foreign),
         trials_foreign_SGBV = ifelse(is.na(trials_foreign_SGBV), 
                                      0, trials_foreign_SGBV)) 

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
db$codebook <- read_csv(here::here("data", "tjet_codebook.csv"), 
                          show_col_types = FALSE) %>% tibble()
attr(db$codebook, "spec") <- NULL
attr(db$codebook, "problems") <- NULL
# attributes(db$codebook) 

### field meta data for website tables
db[["fields_meta"]] <- read_csv(here::here("data", "tjet_fields_meta.csv"), 
                                show_col_types = FALSE) %>% tibble()
attr(db$fields_meta, "spec") <- NULL
attr(db$fields_meta, "problems") <- NULL
# attributes(db$fields_meta) 

### translations table for website 
db[["translations"]] <- read_csv(here::here("data", "tjet_translations.csv"), 
                                show_col_types = FALSE) %>% tibble()
attr(db$translations, "spec") <- NULL
attr(db$translations, "problems") <- NULL
# attributes(db$translations) 

### conflict dyads lookup table for database
db$ConflictDyads <- read_csv(here::here("conflicts", "confl_dyads.csv"), 
                             show_col_types = FALSE) %>% 
  tibble() %>%
  filter(ep_start_year <= 2020 & ep_end_year >= 1970)

attr(db$ConflictDyads, "spec") <- NULL
attr(db$ConflictDyads, "problems") <- NULL
# attributes(db$ConflictDyads) 

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
map(db[["SurveysMeta"]]$results_tables, function(filename) { # for dev: filename = "Uganda_2005_Submission.xlsx" 
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
# rm(countrylist, translist, confllist, amnesties, reparations, tcs, vettings, 
#    trials, domestic, intl, foreign) 
rm(countrylist, translist, confllist, gl) 

# db[["TJETmembers"]] <- db[["TJETmembers"]] %>% 
#   filter(TJET_website_add == 1 & !is.na(bio_text) ) %>% 
#   select(last_name, given_name, institution, position, TJET_role, 
#          email, email_public, url, url_public, bio_text)

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
        mutate(across(all_of(var), 
                      function(x) str_flatten(x, collapse ="; "))) %>%
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

### ICC data 
db[["ICC"]] <- db[["ICC"]] %>%  
  select(country, ccode_cow, 
         ICC_referral, ICC_prelim_exam, ICC_prelimEnd, ICC_investigation)

db[["ICCaccused"]] <- db[["Accused"]] %>% 
  filter(!is.na(ICC_investigation)) %>% 
  left_join(db[["Trials"]] %>% 
              select(trialID, ccode_Accused), 
            by = "trialID") %>% 
  left_join(db[["Countries"]] %>% 
              filter(end == 2020) %>% 
              select(country, ccode),
            by = c(ccode_Accused = "ccode")) %>%
  select(trialID, accusedID, country, ccode_Accused, nameOrDesc, ICC_arrest_warrant, ICC_arrestAppear, 
         ICC_confirm_charges, ICC_proceedings, ICC_withdrawnDismissed) %>% 
  arrange(ccode_Accused, ICC_arrest_warrant)

### helpers for CY measures
source("pipeline/AmnestyMeasure.R", echo = TRUE)
source("pipeline/ReparationMeasures.R", echo = TRUE)
source("pipeline/TCgoals.R", echo = TRUE)
source("pipeline/TCmeasure.R", echo = TRUE)
source("pipeline/TrialsMeasure.R", echo = TRUE)
source("pipeline/VettingMeasures.R", echo = TRUE)

# sample_cy <- c(
#   glo = "global", ### all, all the time, i.e. full dataset
#   dtr = "democratic transition", ### binary, from first transition year
#   aco = "all conflicts", ### binary, from first conflict year
#   dco = "during conflict", ### binary, when conflict active
#   pco = "post-conflict") ### binary, after active conflict ended

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
df <- readRDS(here::here("data", "cy_covariates.rds"))
not <- c("histname", "cid_who", "ldc", "lldc", "sids", "income_wb", 
         "iso3c_wb", "region_wb2")
first <- c("country", "country_case", "year", "ccode_cow", "ccode_ksg", 
           "m49", "isoa3", "country_id_vdem", "region", "subregion", 
           "intregion", "region_wb", "micro_ksg")
then <- names(df)[!names(df) %in% c(first, not)]
df <- df %>%
  select(all_of(first), all_of(then)) %>%
  mutate(country = ifelse(country == "Ethiopia, FDR", "Ethiopia", country),
         country_case = ifelse(country_case == "Ethiopia, FDR", "Ethiopia", country_case)) 

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
  mutate(ruler_exit = ifelse(year(lead_enddate_beg) == year, 1, 0), 
         ruler_days = case_when(
           year(lead_enddate_beg) != year ~ 
             as.integer(as_date(paste(year, 12, 31, sep = "-")) - lead_startdate_beg),
           year(lead_enddate_beg) == year ~ 
             as.integer(lead_enddate_beg - lead_startdate_beg)) 
         ) %>%
  ### the creation of Archigos variables could be moved to cy-data
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
  full_join(db[["ICC"]] %>% select(-country),
            by = "ccode_cow" ) %>%
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
              filter(!is.na(ICC_arrest_warrant)) %>% 
              mutate(icc_action = 1) %>%
              group_by(ccode_Accused, ICC_arrest_warrant) %>%  
              reframe(icc_action = sum(icc_action)),
            by = c("ccode_cow" = "ccode_Accused", "year" = "ICC_arrest_warrant")) %>% 
  rename(ICC_arrest_warrant = icc_action) %>% 
  full_join(db[["ICCaccused"]] %>% 
              select(ccode_Accused, ICC_arrestAppear) %>%
              filter(!is.na(ICC_arrestAppear)) %>%
              mutate(icc_action = 1) %>%
              group_by(ccode_Accused, ICC_arrestAppear) %>%  
              reframe(icc_action = sum(icc_action)),
            by = c("ccode_cow" = "ccode_Accused", "year" = "ICC_arrestAppear")) %>% 
  rename(ICC_arrestAppear = icc_action) %>% 
  full_join(db[["ICCaccused"]] %>% 
              select(ccode_Accused, ICC_confirm_charges) %>%
              filter(!is.na(ICC_confirm_charges)) %>%
              mutate(icc_action = 1) %>%
              group_by(ccode_Accused, ICC_confirm_charges) %>%  
              reframe(icc_action = sum(icc_action)),
            by = c("ccode_cow" = "ccode_Accused", "year" = "ICC_confirm_charges")) %>% 
  rename(ICC_confirm_charges = icc_action) %>% 
  full_join(db[["ICCaccused"]] %>% 
              select(ccode_Accused, ICC_proceedings) %>%
              filter(!is.na(ICC_proceedings)) %>%
              mutate(icc_action = 1) %>%
              group_by(ccode_Accused, ICC_proceedings) %>%  
              reframe(icc_action = sum(icc_action)),
            by = c("ccode_cow" = "ccode_Accused", "year" = "ICC_proceedings")) %>% 
  rename(ICC_proceedings = icc_action) %>% 
  full_join(db[["ICCaccused"]] %>% 
              select(ccode_Accused, ICC_withdrawnDismissed) %>%
              filter(!is.na(ICC_withdrawnDismissed)) %>%
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
    # icc_sp = ifelse(year < 1998, NA, icc_sp)
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
  mutate(across(c("uninv", "uninv_dompros_created", "uninv_dompros", 
                  "uninv_evcoll_created", "uninv_evcoll", 
                  "uninv_intlpros_created", "uninv_intlpros"), 
                ~ ifelse(is.na(.x), 0, .x))) %>% 
  mutate(sample_trans = ifelse(transition == 1, year, NA),
         sample_confl = ifelse(conflict_active == 1, year, NA), 
         dco = ifelse(!is.na(sample_confl) & year == sample_confl, 1, 0) # dco = "during conflict", ### binary, when conflict active
  ) %>% 
  group_by(country_case) %>% 
  mutate(sample_trans = min(sample_trans, na.rm = TRUE), 
         sample_confl = min(sample_confl, na.rm = TRUE) ) %>% 
  ## warning here that is addressed in next mutate
  ungroup() %>%
  mutate(pco = ifelse(year > sample_confl & dco == 0, 1, 0), ## "post-conflict"
         sample_trans = ifelse(is.infinite(sample_trans), NA, sample_trans),
         sample_trans = case_when(is.na(sample_trans) ~ 0, 
                                  year < sample_trans ~ 0, 
                                  year >= sample_trans ~ 1), 
         dtr = sample_trans, ## "democratic transition"
         sample_confl = ifelse(is.infinite(sample_confl), NA, sample_confl),
         sample_confl = case_when(is.na(sample_confl) ~ 0, 
                                  year < sample_confl ~ 0, 
                                  year >= sample_confl ~ 1), 
         aco = sample_confl ## "all conflicts"
  ) %>% 
  left_join(reversions %>% select(-country), 
            by = c("country_id_vdem", "year")) %>% 
  left_join(fair_trials, by = c("ccode_cow" = "ccode", "year" = "year")) %>%
  group_by(country_case) %>%
  fill(fair_postautocratic_trials, .direction = "down") %>%
  mutate(fair_postautocratic_trials = ifelse(is.na(fair_postautocratic_trials), 0, fair_postautocratic_trials)) %>% 
  ungroup() %>% 
  arrange(country_case, year) %>%
  group_by(country_case, isna = is.na(theta_mean_fariss) ) %>%
  mutate(cum_theta_mean_fariss = ifelse(isna, NA, cummean(theta_mean_fariss)),
         sample_combi = ifelse(sample_trans + sample_confl > 0, 1, 0) ) %>%
  ungroup() %>%
  select(-isna)

# df %>%
#   filter(uninv == 1) %>%
#   select(country, year, uninv_dompros_created, uninv_dompros, uninv_evcoll_created, uninv_evcoll, uninv_intlpros_created, uninv_intlpros) %>%
#   print(n = Inf)

### these CYs are included in analyses data but not in TJET CountryYears
### this is ok because CountryYears is for mapping purposes and 
### these are for the most part microstates for which TJET has later start years
### we could elect to delete these country years from the analyses dataset
### moreover the analyses data include years 2021-2022, which we should drop below
df %>% 
  select(country, ccode_cow, ccode_ksg, year) %>% 
  mutate(df = TRUE) %>% 
  filter(year < 2021) %>% ### need to limit years in datasets
  full_join(db[["CountryYears"]] %>% 
              select(country, ccode, ccode_ksg, year) %>% 
              mutate(db = TRUE), 
            by = c("ccode_cow" = "ccode", 
                   "ccode_ksg" = "ccode_ksg", 
                   "year" = "year") ) %>% 
  filter(is.na(df) | is.na(db) | is.na(country.x) | is.na(country.y)) %>% 
  group_by(ccode_cow) %>% 
  mutate(beg = min(year), 
         end = max(year) ) %>% 
  select(-year) %>% 
  distinct() %>% 
  print(n = Inf)

## for dev only
# df <- df %>%
#   select(country, country_case, ccode_cow, year, trials_domestic)

source("pipeline/measures_amnesties.R", echo = TRUE) 
source("pipeline/measures_prosecutions.R", echo = TRUE) 
source("pipeline/measures_tcs.R", echo = TRUE) 
df <- ReparationMeasures(cy = df)
df <- VettingMeasures(cy = df)

rm(vet_ctry_incl, vet_spells)

### checking domestic trials sample indicator

# df %>% 
#   filter(year < 2021) %>%
#   # filter(is.na(trials_domestic) | 
#   #          is.na(all_trs_dom_ctj_dtj_dcj_pcj_all) | 
#   #          is.na(tran_trs_dom_dtj_ctj_sta) |
#   #          is.na(tran_trs_dom_ctj_opp) |
#   #          is.na(regu_trs_dom_sta) |
#   #          is.na(lcon_trs_dom_sta_opp))  %>% 
#   arrange(country, year) %>%
#   select(country, year, trials_domestic, tran_trs_dom_dtj_ctj_sta, tran_trs_dom_ctj_opp, regu_trs_dom_sta, lcon_trs_dom_sta_opp) %>%
#   mutate(new = tran_trs_dom_dtj_ctj_sta + regu_trs_dom_sta + tran_trs_dom_ctj_opp + lcon_trs_dom_sta_opp, 
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

df <- df %>% 
  left_join(read_csv("../tjet-hra/tjet-hra.csv") %>% 
              select(country, year, legacy_mean, legacy_low95, legacy_upp95, 
                     legacy_rank), 
            by = c("country_case" = "country", "year" = "year"))

### cleanup

first <- c(first, "dtr", "aco", "dco", "pco")
not <- c(not, "regime_sample", "reg_democ", "reg_autoc", "reg_trans", "transition", 
         "conflict", "conflict_active", "sample_trans", "sample_confl", "sample_combi") 
then <- names(df)[!names(df) %in% c(first, not)]

df <- df %>% 
  select(all_of(first), all_of(then))

rm(first, not, then)

### var order
codebook <- db[["codebook"]] %>% 
  filter(tables == "tjet_cy") %>% 
  filter(colname != "lag_*") %>% 
  filter(!str_detect(colname, "access_"))

## these are in the codebook but not in the dataset 
codebook$colname[!codebook$colname %in% names(df)]
## these are in the dataset but not in the codebook
if(length(names(df)[!names(df) %in% codebook$colname]) > 0) {
  print(names(df)[!names(df) %in% codebook$colname])
  stop("Not all dataset variable names are included in the codebook; add these to the codebook first!") 
}

df <- df %>% 
  select(all_of(codebook$colname[codebook$colname %in% names(df)]))

### create lags and saving the analyses dataset
### saving to Dropbox only works locally 
### NEED TO DISABLE THIS FOR GITHUB ACTIONS 

lags <- df %>%
  select(!any_of(c("country", "country_label", "country_name", "country_fr", 
                   "country_label_fr", "ccode_cow", "ccode_ksg", "m49", "isoa3", 
                   "country_id_vdem", "region", "subregion", "intregion", 
                   "region_wb", "micro_ksg", "dtr", "aco", "dco", "pco"))) %>%
  mutate(year = year + 1) %>%
  rename_with(~ paste0("lag_", .x))

dropbox_path <- "~/Dropbox/TJLab/TimoDataWork/analyses_datasets/"

exclude <- c("country_label", "country_name", "country_fr", "country_label_fr")
  
df %>% 
  left_join(lags, by = c("country_case" = "lag_country_case",
                         "year" = "lag_year")) %>%
  mutate(tjet_version = timestamp) %>% 
  select(!any_of(exclude)) %>% 
  write_csv(here::here("tjet_datasets", "tjet_cy_analyses.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_cy_analyses.csv"), na = "")

rm(lags, exclude)

### downloads datasets 
# - include country IDs, transitions and our data
# - everything in our filters, but not other outcomes
# - variables that should not be in public CY downloads file?
#   - regime_sample, reg_democ, reg_autoc, reg_trans, conflict, transition

included <- codebook$colname[codebook$colname %in% names(df)]

codebook %>% 
  filter(colname %in% included) %>% 
  select(colname, definition, source) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_codebook_analyses.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_codebook_analyses.csv"), na = "")

db[["dl_tjet_codebook"]] <- codebook %>% 
  filter(colname %in% included) %>% 
  filter(is.na(source) | 
           source %in% c("TJET", "COW", "Kristian S. Gleditsch", 
                         "UN Statistics Division", "World Bank") | 
           colname %in% c("country_id_vdem") ) %>%
  filter(!colname %in% c("histname", "cid_wb", "iso3c_wb", 
                         "sample_trans", "sample_confl", "sample_combi") ) %>%
  select(colname, definition, source) %>% 
  left_join(read_csv(here::here("data", "sources.csv")), by = "source") %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_codebook.csv"), na = "")

rm(codebook) 

vars <- db[["dl_tjet_codebook"]]$colname[db[["dl_tjet_codebook"]]$colname %in% names(df)]

db[["dl_tjet_cy"]] <- df %>%
  select(all_of(vars)) %>% 
  filter(year >= 1970 & year <= 2023) %>% 
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

### translations checks 

db[["Amnesties"]] <- db[["Amnesties"]] %>% 
  mutate(mechanismDescription_fr = str_replace_all(mechanismDescription_fr, 
                                                   "droits de l'homme", 
                                                   "droits de la personne")) 
db[["Amnesties"]] %>% 
  filter(amnestyYear <= 2020 & amnestyYear >= 1970) %>%  
  select(amnestyID, invalid, amnestyYear, mechanismDescription_fr) %>% 
  filter(str_detect(mechanismDescription_fr, "droits de l'homme") | 
           is.na(mechanismDescription_fr)) 

db[["Reparations"]] <- db[["Reparations"]] %>% 
  mutate(officialName_en_fr = str_replace_all(officialName_en_fr, 
                                                   "droits de l'homme", 
                                                   "droits de la personne")) 
db[["Reparations"]] %>% 
  filter(yearCreated <= 2020 & yearCreated >= 1970) %>%  
  select(reparationID, invalid, yearCreated, officialName_en_fr) %>% 
  filter(str_detect(officialName_en_fr, "droits de l'homme") | 
           is.na(officialName_en_fr)) 

db[["Trials"]] <- db[["Trials"]] %>% 
  mutate(caseDescription_fr = str_replace_all(caseDescription_fr, 
                                              "droits de l'homme", 
                                              "droits de la personne")) 
db[["Trials"]] %>% 
  filter(yearStart <= 2020 & yearStart >= 1970) %>%  
  select(trialID, invalid, yearStart, caseDescription_fr) %>% 
  filter(str_detect(caseDescription_fr, "droits de l'homme") | 
           is.na(caseDescription_fr)) 

db[["Accused"]] <- db[["Accused"]] %>% 
  mutate(nameOrDesc_fr = str_replace_all(nameOrDesc_fr, 
                                              "droits de l'homme", 
                                              "droits de la personne"))
db[["Accused"]] %>% 
  select(accusedID, invalid, nameOrDesc_fr) %>% 
  filter(str_detect(nameOrDesc_fr, "droits de l'homme") | 
           is.na(nameOrDesc_fr)) 

db[["TruthCommissions"]] <- db[["TruthCommissions"]] %>% 
  mutate(officialName_en_fr = str_replace_all(officialName_en_fr, 
                                         "droits de l'homme", 
                                         "droits de la personne"))
db[["TruthCommissions"]] %>% 
  filter(yearPassed <= 2020 & yearPassed >= 1970) %>%  
  select(truthcommissionID, yearPassed, officialName_en_fr) %>% 
  filter(str_detect(officialName_en_fr, "droits de l'homme") | 
           is.na(officialName_en_fr)) 

db[["Vettings"]] %>% 
  select(vettingID, invalid, yearStart, policyName, policyName_fr) %>% 
  filter(str_detect(policyName_fr, "droits de l'homme") | 
           is.na(policyName_fr)) 

txt_fields <- c("auto_conflict_fr", "auto_regime_fr", "txt_amnesties_fr", 
                "txt_conflict_fr", "txt_domestic_fr", "txt_foreign_fr", 
                "txt_intl_fr", "txt_intro_fr", "txt_regime_fr", 
                "txt_reparations_fr", "txt_summary_fr", "txt_tcs_fr", 
                "txt_TJ", "txt_TJ_fr", "txt_un_fr", "txt_vetting_fr") 

db[["Countries"]] <- db[["Countries"]] %>%
  mutate(across(all_of(txt_fields), 
                ~ str_replace_all(.x, "droits de l'homme", 
                                  "droits de la personne")))

### saving individual mechanism tables for local analyses & repo
### these will also be written to the database for downloads

db[["Transitions"]] <- db[["Transitions"]] %>% 
  rename(ccode_cow = ccode) %>%
  mutate(tjet_version = timestamp) %>%
  write_csv(here::here("tjet_datasets", "tjet_transitions.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_transitions.csv"), na = "")
db[["Amnesties"]] <- db[["Amnesties"]] %>% 
  left_join(db[["Countries"]] %>%
              select(country_case, ccode) %>% 
              rename(country = country_case) %>% 
              distinct(),
            by = "ccode") %>% 
  filter(amnestyYear >= 1970 & amnestyYear <= 2020) %>%
  rename(ccode_cow = ccode) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_amnesties.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_amnesties.csv"), na = "")
db[["Reparations"]] <- db[["Reparations"]] %>% 
  left_join(db[["Countries"]] %>%
              select(country_case, ccode) %>% 
              rename(country = country_case) %>% 
              distinct(),
            by = "ccode") %>% 
  filter(yearCreated >= 1970 & yearCreated <= 2020) %>%
  rename(ccode_cow = ccode) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_reparations.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_reparations.csv"), na = "")
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
  filter(yearStart >= 1970 & yearStart <= 2020) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_trials.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_trials.csv"), na = "")
db[["Accused"]] <- db[["Accused"]] %>%
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_accused.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_accused.csv"), na = "")
db[["CourtLevels"]] <- db[["CourtLevels"]] %>%
  select(CLID, accusedID, courtLevel, courtName, day, month, year, date, 
         last_fx, verdict, guilty, sentence, sentencingTime, 
         sentencingArrangement, sentenceNotes) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_courtlevels.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_courtlevels.csv"), na = "")
db[["TruthCommissions"]] <- db[["TruthCommissions"]] %>%
  left_join(db[["Countries"]] %>%
              select(country_case, ccode) %>% 
              rename(country = country_case) %>% 
              distinct(),
            by = "ccode") %>% 
  filter(yearPassed >= 1970 & yearPassed <= 2020) %>%
  rename(ccode_cow = ccode) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_tcs.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_tcs.csv"), na = "")
db[["Vettings"]] <- db[["Vettings"]] %>% 
  left_join(db[["Countries"]] %>%
              select(country_case, ccode) %>% 
              rename(country = country_case) %>% 
              distinct(),
            by = "ccode") %>% 
  filter(yearStart >= 1970 & yearStart <= 2020) %>%
  rename(ccode_cow = ccode) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_vettings.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_vettings.csv"), na = "")
db[["Investigations"]] <- dl_invest %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_un_investigations.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_un_investigations.csv"), na = "")
db[["ICC"]] <- db[["ICC"]] %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_icc_interventions.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_icc_interventions.csv"), na = "")
db[["ICCaccused"]] <- db[["ICCaccused"]] %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_icc_accused.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_icc_accused.csv"), na = "")

save(db, file = here::here("data", "tjetdb.RData"))

rm(dl_invest, timestamp)

### auto-texts for country profiles

options(scipen = 999) 

countries <- db[["Countries"]] %>%
  filter(!country %in% c("Serbia and Montenegro",
                         "Soviet Union",
                         "Yugoslavia") ) %>%
  select(country, country_case, ccode, ccode_case, ccode_ksg)

# countries %>% 
#   select(country_case, ccode_case) %>% 
#   distinct() %>% 
#   group_by(ccode_case) %>% 
#   mutate(n = n()) %>% 
#   filter(n > 1) 

autotxt <- autoprep <- data <- list()

### summaries autoprep

autoprep$amnesties <-  amnesties %>% 
  left_join(countries %>% select(ccode, ccode_case), 
            by = "ccode") %>% 
  group_by(ccode_case) %>%
  reframe(amnesties = sum(amnesties), 
          amnesties_yrs = list(sort(unique(year))))

autoprep$domestic <- domestic %>% 
  left_join(countries %>% select(ccode, ccode_case), 
            by = "ccode") %>% 
  group_by(ccode_case) %>%
  reframe(domestic = sum(trials_domestic), 
          domestic_yrs = list(sort(unique(year))))

autoprep$foreign <- foreign %>% 
  left_join(countries %>% select(ccode, ccode_case), 
            by = "ccode") %>% 
  group_by(ccode_case) %>%
  reframe(foreign = sum(trials_foreign), 
          foreign_yrs = list(sort(unique(year))))

autoprep$intl <- intl %>% 
  left_join(countries %>% select(ccode, ccode_case), 
            by = "ccode") %>% 
  group_by(ccode_case) %>%
  reframe(intl = sum(trials_intl), 
          intl_yrs = list(sort(unique(year))))

autoprep$reparations <-  reparations %>% 
  left_join(countries %>% select(ccode, ccode_case), 
            by = "ccode") %>% 
  group_by(ccode_case) %>%
  reframe(reparations = sum(reparations), 
          reparations_yrs = list(sort(unique(year))))

autoprep$tcs <-  tcs %>% 
  left_join(countries %>% select(ccode, ccode_case), 
            by = "ccode") %>% 
  group_by(ccode_case) %>%
  reframe(tcs = sum(tcs), 
          tcs_yrs = list(sort(unique(year))))

autoprep$vettings <-  vettings %>% 
  left_join(countries %>% select(ccode, ccode_case), 
            by = "ccode") %>% 
  group_by(ccode_case) %>%
  reframe(vettings = sum(vettings), 
          vettings_yrs = list(sort(unique(year))))

autoprep$summary <- reduce(autoprep, 
                       function(x, y) full_join(x, y, by = "ccode_case")) %>% 
  mutate(amnesties = ifelse(is.na(amnesties), 0, amnesties), 
         domestic = ifelse(is.na(domestic), 0, domestic), 
         foreign = ifelse(is.na(foreign), 0, foreign), 
         intl = ifelse(is.na(intl), 0, intl), 
         reparations = ifelse(is.na(reparations), 0, reparations), 
         tcs = ifelse(is.na(tcs), 0, tcs), 
         vettings = ifelse(is.na(vettings), 0, vettings)) %>%
  left_join(countries %>% 
              select(country_case, ccode_case) %>% 
              distinct(), 
            by = "ccode_case") %>%
  arrange(country_case) 

autoprep$transitions <- db[["Transitions"]] %>% 
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>%
  select(country_case, ccode_case, trans_year_begin) %>%
  arrange(country_case, trans_year_begin) %>%
  group_by(country_case) %>% 
  reframe(ccode_case = unique(ccode_case), 
          trans_year_begin = list(unique(trans_year_begin)))

autoprep$conflicts <- db[["ConflictDyads"]] %>% 
  mutate(internationalized = ifelse(str_detect(intensity, "Internationalized"), 1, 0)) %>% 
  rowwise() %>% 
  mutate(years = list(ep_start_year:ep_end_year) ) %>% 
  ungroup() %>% 
  select(dyad_id, conflict_id, gwno_loc, ep_start_year, ep_end_year, years, internationalized) %>%
  left_join(countries %>% 
              select(country_case, ccode_ksg, ccode_case) %>% 
              distinct(), 
            by = c("gwno_loc" = "ccode_ksg")) %>%
  select(country_case, ccode_case, dyad_id, conflict_id, years, internationalized) %>%
  group_by(country_case) %>%
  reframe(ccode_case = unique(ccode_case), 
          conflicts = length(unique(conflict_id)), 
          dyads = length(unique(dyad_id)), 
          episodes = n(),
          years = list(sort(unique(unlist(years)))),
          years = list(unlist(years)[unlist(years) %in% 1970:2020]),
          int_ep = sum(internationalized))

autoprep[["rankings"]] <- db[["dl_tjet_cy"]] %>%
  select(-country_case) %>%
  left_join(countries %>%
              select(country_case, ccode, ccode_case),
            by = c(ccode_cow = "ccode")) %>%
  filter(year == 2020) %>%
  # filter(!is.na(access_rank) | !is.na(legacy_rank)) %>%
  filter(!is.na(legacy_rank)) %>%
  arrange(legacy_rank) %>%
  select(country_case, ccode_case, country_fr, legacy_rank) %>% 
  mutate(n = max(legacy_rank)) 

### data for summary spreadsheet & auto texts 

data[["Amnesties"]] <- db[["Amnesties"]] %>%
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  mutate(what_hrv = ifelse(str_detect(whatCrimes, 
                                      "human rights violations"), 1, 0), 
         who_pol = ifelse(str_detect(whoWasAmnestied, 
                                     "protesters / political prisoners"), 1, 0)) %>% 
  group_by(country_case) %>%
  mutate(beg = min(amnestyYear), 
         end = max(amnestyYear), 
         count_all = n(), 
         count_demtrans = sum(fitsPostAutocraticTJ),
         count_conflict = sum(fitsConflictTJ),
         count_dcj = sum(dcj),
         count_pcj = sum(pcj),
         count_peaceagree = sum(peaceSettlement), 
         count_prisoners = sum(who_pol),
         count_hrv = sum(what_hrv),
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, beg, end, count_all, count_demtrans, count_conflict, count_dcj, 
         count_pcj, count_peaceagree, count_prisoners, count_hrv) %>%
  distinct() %>%
  arrange(country_case)

vars_dom <- c("trials_domestic", "tran_trs_dom_dtj_sta", "tran_trs_dom_ctj_sta", 
              "tran_trs_dom_dtj_ctj_sta", "regu_trs_dom_sta", 
              "tran_cce_dom_dtj_ctj_sta", "regu_cce_dom_sta", 
              "tran_trs_dom_dtj_ctj_sta_hi", "tran_cce_dom_dtj_ctj_sta_hi", 
              "tran_trs_dom_ctj_opp", "tran_cce_dom_ctj_opp", 
              "lcon_trs_dom_sta_opp", "lcon_cce_dom_sta_opp")
vars_int <- c("trials_intl", "trs_int_sta", "trs_int_opp", 
              "cce_int_sta", "cce_int_opp") 
vars_for <- c("trials_foreign", "trs_for_sta", "trs_for_opp", 
              "cce_for_sta", "cce_for_opp") 

data[["Domestic_cy"]] <- db[["dl_tjet_cy"]] %>% 
  select(-country_case) %>% 
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  ### why this craziness? because what is outwardly the country-case does not line up perfectly with how we need to assign TJ events internally for consistency
  arrange(country_case, year) %>% 
  filter(if_any(all_of(vars_dom), ~ . > 0)) %>%
  group_by(country_case) %>% 
  reframe(ccode_case = unique(ccode_case), 
          beg = min(year), 
          end = max(year),
          total = sum(trials_domestic, na.rm = TRUE), 
          tran_trs_dom_dtj_sta = sum(tran_trs_dom_dtj_sta, na.rm = TRUE), 
          tran_cce_dom_dtj_sta = sum(tran_cce_dom_dtj_sta, na.rm = TRUE),
          tran_trs_dom_ctj_sta = sum(tran_trs_dom_ctj_sta, na.rm = TRUE), 
          tran_cce_dom_ctj_sta = sum(tran_cce_dom_ctj_sta, na.rm = TRUE),
          tran_trs_dom_dtj_ctj_sta = sum(tran_trs_dom_dtj_ctj_sta, na.rm = TRUE), 
          tran_cce_dom_dtj_ctj_sta = sum(tran_cce_dom_dtj_ctj_sta, na.rm = TRUE), 
          tran_trs_dom_dtj_ctj_sta_hi = sum(tran_trs_dom_dtj_ctj_sta_hi, na.rm = TRUE), 
          tran_cce_dom_dtj_ctj_sta_hi = sum(tran_cce_dom_dtj_ctj_sta_hi, na.rm = TRUE), 
          regu_trs_dom_sta = sum(regu_trs_dom_sta, na.rm = TRUE), 
          regu_cce_dom_sta = sum(regu_cce_dom_sta, na.rm = TRUE), 
          tran_trs_dom_ctj_opp = sum(tran_trs_dom_ctj_opp, na.rm = TRUE), 
          tran_cce_dom_ctj_opp = sum(tran_cce_dom_ctj_opp, na.rm = TRUE), 
          lcon_trs_dom_sta_opp = sum(lcon_trs_dom_sta_opp, na.rm = TRUE), 
          lcon_cce_dom_sta_opp = sum(lcon_cce_dom_sta_opp, na.rm = TRUE))

data[["Intl_cy"]] <- db[["dl_tjet_cy"]] %>% 
  select(-country_case) %>% 
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  arrange(country_case, year) %>% 
  filter(if_any(all_of(vars_int), ~ . > 0)) %>% 
  group_by(country_case) %>% 
  reframe(ccode_case = unique(ccode_case), 
          beg = min(year), 
          end = max(year),
          trials_intl = sum(trials_intl, na.rm = TRUE),
          trs_int_hrs_con_all = sum(trs_int_sta + trs_int_opp, na.rm = TRUE), 
          cce_int_hrs_con_all = sum(cce_int_sta + cce_int_opp, na.rm = TRUE))

data[["Foreign_cy"]] <- db[["dl_tjet_cy"]] %>% 
  select(-country_case) %>% 
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  arrange(country_case, year) %>% 
  filter(if_any(all_of(vars_for), ~ . > 0)) %>%
  group_by(country_case) %>% 
  reframe(ccode_case = unique(ccode_case), 
          beg = min(year), 
          end = max(year), 
          trials_foreign = sum(trials_foreign, na.rm = TRUE),
          trs_for_hrs_con_all = sum(trs_for_sta + trs_for_opp, na.rm = TRUE), 
          cce_for_hrs_con_all = sum(cce_for_sta + cce_for_opp, na.rm = TRUE)) 

data[["Foreign"]] <- db[["Trials"]] %>%
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_Accused = "ccode")) %>%
  select(-ccode_Accused) %>%  
  rename("countryAccused" = "country_case", 
         "ccode_Accused" = "ccode_case") %>%
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_Trial = "ccode")) %>%
  select(-ccode_Trial) %>%  
  rename("countryTrial" = "country_case", 
         "ccode_Trial" = "ccode_case") %>%
  filter(trialType == "foreign") %>% 
  select(trialID, countryAccused, ccode_Accused, countryTrial, ccode_Trial, yearStart, yearEnd, caseDescription) %>% 
  arrange(countryAccused, yearStart) 

data[["Reparations"]] <- db[["Reparations"]] %>%
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  group_by(country_case) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  select(country_case, ccode_case, count, reparationID, yearCreated, yearBegin, yearEnd, 
         individualReparations, collectiveReparations, beneficiariesCount) %>%
  arrange(country_case, yearCreated)

data[["TruthCommissions"]] <- db[["TruthCommissions"]] %>%
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  group_by(country_case) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  rename("rec_prosecutions" = "recommendProsecutions", 
         "rec_reparations" = "recommendReparations", 
         "rec_reforms" = "reportRecommendInstitutionalReform", 
         "reform_HRs" = "humanRightsReforms", 
         "reform_legal" = "legalReform", 
         "reform_judicial" = "judicialReforms", 
         "reform_gender" = "genderReform", 
         "reform_corruption" = "corruptionReforms", 
         "reform_SSR" = "SecuritySectorReforms", 
         "reform_vetting" = "vetting") %>% 
  select(country_case, ccode_case, count, truthcommissionID, yearPassed, 
         yearBeginOperation, yearCompleteOperation, finalReportIssued, 
         reportPubliclyAvailable, rec_prosecutions, rec_reparations, 
         rec_reforms, reform_HRs, reform_legal, reform_judicial, reform_gender, 
         reform_corruption, reform_SSR, reform_vetting, consultedVictims, 
         commissionersVictimGroups, encourageVictimTestimony
  ) %>%
  arrange(country_case, yearPassed)

data[["Vettings"]] <- db[["Vettings"]] %>%
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  mutate(
    individual_conduct = case_when(
      str_detect(targetingWhy, "specific individual conduct") ~ 1,
      TRUE ~ 0)) %>%
  select(country_case, ccode_case, vettingID, alterationOf, yearStart, yearEnd, 
         individual_conduct, type_dismissal, type_ban, type_declassification, 
         type_perjury, numberInvestigated, dateLaw) %>%
  arrange(country_case, ccode_case, yearStart)

data[["ICC-interventions"]] <- db[["ICC"]] %>% 
  select(-country) %>%  
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  select(country_case, ccode_case, ICC_referral, ICC_prelim_exam, ICC_prelimEnd, ICC_investigation) %>%
  arrange(country_case, ICC_prelim_exam) 

data[["ICC-accused"]] <- db[["ICCaccused"]] %>% 
  select(-country) %>%  
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_Accused = "ccode")) %>% 
  select(country_case, ccode_case, nameOrDesc, ICC_arrest_warrant, 
         ICC_arrestAppear, ICC_confirm_charges, ICC_proceedings, 
         ICC_withdrawnDismissed, trialID, accusedID) %>%
  arrange(country_case, ICC_arrest_warrant) 

data[["Investigations"]] <- db[["Investigations"]] %>% 
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  select(country, country_case, ccode_case, beg, end, mandate, 
         uninv_dompros, uninv_evcoll, uninv_intlpros, goals) %>%
  arrange(country_case, beg)

### peace agreements: "https://peaceaccords.nd.edu/wp-content/uploads/2019/08/PAM_ID-V.1.5-Updated-29JULY2015.xlsx"
data[["peace-agreements"]] <- readxl::read_xlsx("data/PAM_ID-V.1.5-Updated-29JULY2015.xlsx") %>% 
  rename("accord_name" = "accord name") %>%
  group_by(pam_caseid) %>% 
  mutate(beg = min(year), 
         end = max(year)) %>% 
  ungroup() %>%
  mutate(war_start = as_date(war_start), 
         cease_date = as_date(cease_date)) %>%
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(cowcode = "ccode")) %>% 
  
  select(pam_caseid, country_case, ccode_case, accord_name, war_start, 
         cease_date, beg, end, amnest_prov, humrts_prov, prisr_prov, repar_prov, 
         truth_prov) %>%
  distinct() %>%
  arrange(country_case, cease_date)

### PAX: https://www.peaceagreements.org/search
tj_vars <- c("TjGen", "TjAm", "TjAmPro", "TjSan", "TjPower", "TjCou", "TjJaNc", 
             "TjJaIc", "TjMech", "TjPrire", "TjVet", "TjVic", "TjMis", "TjRep", 
             "TjRSym", "TjRMa", "TjNR")
## have not adjusted this to ccode_case
data[["PAX"]] <- read_csv("data/pax_all_agreements_data_v6.csv") %>% 
  arrange(Con, PPName, Dat) %>% 
  filter(Stage %in% c("SubComp", "SubPar")) %>% 
  filter(if_any(all_of(tj_vars), ~ . == 1)) %>%
  select(Con, PP, PPName, AgtId, Agt, Dat, Status, Agtp, Stage, Loc1GWNO, 
         Loc2GWNO, UcdpCon, UcdpAgr, PamAgr, all_of(tj_vars)) 

### write to file 
### WE DON'T NEED THIS ANYMORE
# write_xlsx(data, path = "~/Dropbox/TJLab/TimoDataWork/country_profiles/summary_data.xlsx")

rm(tj_vars, vars_dom, vars_for, vars_int) 

source("pipeline/auto_texts.R", echo = TRUE) 

### auto-text fields check 
### 'auto_' fields are generated here and need to be transfered to Airtable
### 'txt_' fields are manually adjusted in Airtable 
check_vars <- paste("chk_", c("summary", "regime", "conflict", "amnesties", "domestic", "foreign", "intl", "reparations", "tcs", "vetting", "un"), sep = "")
chk <- db[["Countries"]] %>% 
  filter(include) %>% 
  full_join(autotxt, 
            by = c("country_case", "ccode_case")) %>% 
  mutate(chk_summary = ifelse(
    auto_summary != summary | (!is.na(summary) & is.na(auto_summary)), 1, 0), 
         chk_regime = ifelse(
           auto_regime != regime | (!is.na(regime) & is.na(auto_regime)), 1, 0), 
         chk_conflict = ifelse(
           auto_conflict != conflict | (!is.na(conflict) & is.na(auto_conflict)), 1, 0), 
         chk_amnesties = ifelse(
           auto_amnesties != amnesties | (!is.na(amnesties) & is.na(auto_amnesties)), 1, 0), 
         chk_domestic = ifelse(
           auto_domestic != domestic | (!is.na(domestic) & is.na(auto_domestic)), 1, 0), 
         chk_foreign = ifelse(
           auto_foreign != foreign | (!is.na(foreign) & is.na(auto_foreign)), 1, 0), 
         chk_intl = ifelse(
           auto_intl != intl | (!is.na(intl) & is.na(auto_intl)), 1, 0), 
         chk_reparations = ifelse(
           auto_reparations != reparations | (!is.na(reparations) & is.na(auto_reparations)), 1, 0), 
         chk_tcs = ifelse(
           auto_tcs != tcs | (!is.na(tcs) & is.na(auto_tcs)), 1, 0), 
         chk_vetting = ifelse(
           auto_vetting != vetting | (!is.na(vetting) & is.na(auto_vetting)), 1, 0), 
         chk_un = ifelse(
           auto_un != un | (!is.na(un) & is.na(auto_un)), 1, 0), ) %>%
  select(country_case, all_of(check_vars), summary, regime, conflict, amnesties, 
         domestic, intl, foreign, reparations, tcs, vetting, un) %>% 
  filter(if_any(all_of(check_vars), ~ . == 1)) %>% 
  write_csv("~/Desktop/temp.csv", na = "") %>%
  print(n = Inf)

if(nrow(chk) > 0) warning("Some auto-texts in Airtable do not match the current data!")
