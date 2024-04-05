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
### but these should be the same
### ideally the two bases should be combined and using only one version each

### exclude these tables from the production database
exclude <- c("metadata", "select_options", "Experts", "NGOs", "Legal", 
             "ConflictDyadSpells", "UCDPcountries", "Mallinder", "Rozic", 
             "Challenges", "VettingComparison", "ICDB", "BIcomparison", 
             "AdHocHybrid", "Ethiopia")

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
### for later checking
dim_orig <- map(db, function(dat) {
  map_vec(dat, nrow)
})

### filtering out invalid records (more removal below)
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
### SHOULD simplify code below with functions

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
### (it should be many-to-many but the DB was not built to accomodate this)

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
  distinct() %>% 
  filter(!(accusedID == 20968 & lastVerdictYear == 2015 & lastVerdict == "Guilty")) 
  ## taking out duplicates that were missed in Airtable -- look at this again

### need to figure out a better way to deal with the multi-select field 
### because this now defunct code below created duplicates 

# db[["Prosecutions"]][["Trials"]] <- db[["Prosecutions"]][["Trials"]] %>%
#   unnest_longer(all_of(c("oppositionType")), keep_empty = TRUE )
# db[["Prosecutions"]][["Trials"]] %>%
#   unnest_longer(all_of(c("oppositionType")), keep_empty = TRUE ) %>%
#   group_by(trialID) %>%
#   mutate(n = n()) %>%
#   filter(n > 1) %>% select(trialID, oppositionType)
# db[["Prosecutions"]][["Trials"]] %>% 
#   select(trialID, oppositionType) %>% 
#   rowwise() %>% 
#   mutate(n = length(oppositionType), 
#          check = sum(is.na(oppositionType)), 
#          new = list(oppositionType[!is.na(oppositionType)]) ) %>% 
#   filter(n > 1) %>% 
#   unnest_longer(all_of(c("new")), keep_empty = TRUE ) %>% 
#   print(n = Inf )
# db[["Prosecutions"]][["Trials"]] <- db[["Prosecutions"]][["Trials"]] %>%
#   rowwise() %>%
#   mutate(oppositionType = ifelse(length(oppositionType) > 0, str_c(), ""))

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

### need to filter out non-HRs policies & events, 
### state agents and opposition members only for domestic trials
### check again: amnesties, TCs, reparations, vettings

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

### any where mandate differs?
dl_invest %>% 
  filter(mandate != mandate_en | is.na(mandate_en))
  
db[["MegaBase"]][["Investigations"]] <- dl_invest %>% 
  rowwise() %>% 
  mutate(uninv = 1, 
         year = list(beg:end)) %>% 
  unnest(year) %>% 
  select(ccode_cow, year, uninv, uninv_dompros, uninv_evcoll, uninv_intlpros) %>%
  group_by(ccode_cow, year) %>% 
  reframe(across(all_of(c("uninv", "uninv_dompros", 
                          "uninv_evcoll", "uninv_intlpros")), max))

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

### combining the relevant tables from MegaBase and Prosecutions into one DB 
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

## ccode_case / country_case for the countries on the 2020 map that data are matched to
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
          SGBV = max(SGBV)) %>% 
  rename("year" = "yearCreated",
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
  select(country, ccode, year, transition, 
         dem_bmr, dem_polity, dem_vdem, dem_all) %>% 
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
         reparations_SGBV = ifelse(is.na(reparations_SGBV), 
                                   0, reparations_SGBV)) %>% 
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

### data definition codebook
db$codebook <- read_csv(here::here("data", "tjet_codebook.csv"), 
                          show_col_types = FALSE) %>% tibble()
attr(db$codebook, "spec") <- NULL
attr(db$codebook, "problems") <- NULL
# attributes(db$codebook) 

db[["fields_meta"]] <- read_csv(here::here("data", "tjet_fields_meta.csv"), 
                                show_col_types = FALSE) %>% tibble()
attr(db$fields_meta, "spec") <- NULL
attr(db$fields_meta, "problems") <- NULL
# attributes(db$fields_meta) 

db[["translations"]] <- read_csv(here::here("data", "tjet_translations.csv"), 
                                show_col_types = FALSE) %>% tibble()
attr(db$tjet_translations, "spec") <- NULL
attr(db$tjet_translations, "problems") <- NULL
# attributes(db$tjet_translations) 

### conflict dyads lookup table for database
db$ConflictDyads <- read_csv(here::here("conflicts", "confl_dyads.csv"), 
                             show_col_types = FALSE) %>% 
  tibble() %>%
  filter(ep_start_year <= 2020 & ep_end_year >= 1970)

attr(db$ConflictDyads, "spec") <- NULL
attr(db$ConflictDyads, "problems") <- NULL
# attributes(db$ConflictDyads) 

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

db[["TJETmembers"]] <- db[["TJETmembers"]] %>% 
  filter(TJET_website_add == 1 & !is.na(bio_text) ) %>% 
  select(last_name, given_name, institution, position, TJET_role, 
         email, email_public, url, url_public, bio_text)

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
source("functions/AmnestyMeasure.R")
source("functions/ReparationMeasures.R")
source("functions/TCgoals.R")
source("functions/TCmeasure.R")
source("functions/TrialsMeasure.R")
source("functions/VettingMeasures.R")

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

df %>% 
  filter(country != country_case) %>%
  group_by(country) %>%
  mutate(beg = min(year),
         end = max(year)) %>%
  select(country, country_case, ccode_cow, ccode_ksg, beg, end) %>%
  distinct()

# unique(df$country)[!unique(df$country) %in% unique(db[["CountryYears"]]$country) ]
# unique(db[["CountryYears"]]$country)[!unique(db[["CountryYears"]]$country) %in% unique(df$country) ]

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
  mutate(uninv = ifelse(is.na(uninv), 0, uninv), 
         uninv_dompros = ifelse(is.na(uninv_dompros), 0, uninv_dompros),
         uninv_evcoll = ifelse(is.na(uninv_evcoll), 0, uninv_evcoll),
         uninv_intlpros = ifelse(is.na(uninv_intlpros), 0, uninv_intlpros)) %>% 
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
  arrange(country_case, year) %>%
  group_by(country_case, isna = is.na(theta_mean_fariss) ) %>%
  mutate(cum_theta_mean_fariss = ifelse(isna, NA, cummean(theta_mean_fariss)),
         sample_combi = ifelse(sample_trans + sample_confl > 0, 1, 0) ) %>%
  ungroup() %>%
  select(-isna)

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

### trials 

### tested with confirmation that all_trs_dom_ctj_dtj_dcj_pcj_all == trials_domestic == all_trs_dom
# df <- TrialsMeasure(cy = df, prefix = "all", measure = "trs", type_opts = "dom", nexus_vars = c("hrs", "con", "ctj", "dtj", "dcj", "pcj"), memb_opts = "all") %>% 
#   rename(all_trs_dom = all_trs_dom_ctj_dtj_dcj_pcj)

#### transitional prosecutions (prefix: tran)

## dtj trials of state agents: _dtj_sta
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta") %>% 
  mutate(tran_trs_dom_dtj_sta_binary = ifelse(tran_trs_dom_dtj_sta > 0, 1, 0), 
         tran_trs_dom_dtj_sta_scale = case_when(tran_trs_dom_dtj_sta == 0 ~ 0, 
                                                tran_trs_dom_dtj_sta %in% 1:2 ~ 1, 
                                                tran_trs_dom_dtj_sta > 2 ~ 2) )
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta") %>% 
  arrange(country_case, year) %>% 
  group_by(country_case) %>%  
  mutate(tran_tfc_dom_dtj_sta_cumu = cumsum(tran_tfc_dom_dtj_sta))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta", rank_opts = "hi") %>% 
  arrange(country_case, year) %>% 
  group_by(country_case) %>%  
  mutate(tran_tfc_dom_dtj_sta_hi_cumu = cumsum(tran_tfc_dom_dtj_sta_hi))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta", rank_opts = "hi") 

## ctj trials of state agents: _ctj_sta
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta") %>% 
  mutate(tran_trs_dom_ctj_sta_binary = ifelse(tran_trs_dom_ctj_sta > 0, 1, 0), 
         tran_trs_dom_ctj_sta_scale = case_when(tran_trs_dom_ctj_sta == 0 ~ 0, 
                                                tran_trs_dom_ctj_sta %in% 1:2 ~ 1, 
                                                tran_trs_dom_ctj_sta > 2 ~ 2) )
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta") %>% 
  arrange(country_case, year) %>% 
  group_by(country_case) %>%  
  mutate(tran_tfc_dom_ctj_sta_cumu = cumsum(tran_tfc_dom_ctj_sta))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta", rank_opts = "hi") %>% 
  arrange(country_case, year) %>% 
  group_by(country_case) %>%  
  mutate(tran_tfc_dom_ctj_sta_hi_cumu = cumsum(tran_tfc_dom_ctj_sta_hi))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta", rank_opts = "hi") 

## combined dtj and ctj trials of state agents: _dtj_ctj_sta
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta") %>% 
  mutate(tran_trs_dom_dtj_ctj_sta_binary = ifelse(tran_trs_dom_dtj_ctj_sta > 0, 1, 0), 
         tran_trs_dom_dtj_ctj_sta_scale = case_when(tran_trs_dom_dtj_ctj_sta == 0 ~ 0, 
                                                tran_trs_dom_dtj_ctj_sta %in% 1:2 ~ 1, 
                                                tran_trs_dom_dtj_ctj_sta > 2 ~ 2) ) 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta") %>% 
  arrange(country_case, year) %>% 
  group_by(country_case) %>%  
  mutate(tran_tfc_dom_dtj_ctj_sta_cumu = cumsum(tran_tfc_dom_dtj_ctj_sta))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi") %>% 
  arrange(country_case, year) %>% 
  group_by(country_case) %>%  
  mutate(tran_tfc_dom_dtj_ctj_sta_hi_cumu = cumsum(tran_tfc_dom_dtj_ctj_sta_hi))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi") 

## ctj trials of opposition members: _ctj_opp 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp") %>% 
  mutate(tran_trs_dom_ctj_opp_binary = ifelse(tran_trs_dom_ctj_opp > 0, 1, 0), 
         tran_trs_dom_ctj_opp_scale = case_when(tran_trs_dom_ctj_opp == 0 ~ 0, 
                                                tran_trs_dom_ctj_opp %in% 1:2 ~ 1, 
                                                tran_trs_dom_ctj_opp > 2 ~ 2)) 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp") %>% 
  arrange(country_case, year) %>% 
  group_by(country_case) %>%  
  mutate(tran_tfc_dom_ctj_opp_cumu = cumsum(tran_tfc_dom_ctj_opp))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tro", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "tfc", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp", rank_opts = "hi") %>% 
  arrange(country_case, year) %>% 
  group_by(country_case) %>%  
  mutate(tran_tfc_dom_ctj_opp_hi_cumu = cumsum(tran_tfc_dom_ctj_opp_hi))
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "cct", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "crt", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, prefix = "tran", measure = "sen", type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp", rank_opts = "hi") 

# TrialsMeasure(cy = df, prefix = "tran", measure = "trs", type_opts = "dom", nexus_vars = "dtj", memb_opts = "opp") %>% 
#   select(tran_trs_dom_dtj_opp) %>% 
#   filter(tran_trs_dom_dtj_opp > 0) 

#### regular HRs prosecutions (prefix: regu)

## trials of state agents that are not dtj or ctj: [_hrs_con]_Xdtj_Xctj_sta
df <- TrialsMeasure(cy = df, prefix = "regu", measure = "trs", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta") %>% 
  mutate(regu_trs_dom_sta_binary = ifelse(regu_trs_dom_sta > 0, 1, 0), 
         regu_trs_dom_sta_scale = case_when(regu_trs_dom_sta == 0 ~ 0, 
                                            regu_trs_dom_sta %in% 1:2 ~ 1, 
                                            regu_trs_dom_sta > 2 ~ 2)) 
df <- TrialsMeasure(cy = df, prefix = "regu", measure = "tro", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, prefix = "regu", measure = "tfc", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta") %>% 
  arrange(country_case, year) %>% 
  group_by(country_case) %>%  
  mutate(regu_tfc_dom_sta_cumu = cumsum(regu_tfc_dom_sta))
df <- TrialsMeasure(cy = df, prefix = "regu", measure = "cct", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, prefix = "regu", measure = "crt", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, prefix = "regu", measure = "sen", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 

# TrialsMeasure(cy = df, prefix = "regu", measure = "trs", type_opts = "dom", nexus_vars = c("hrs", "con"), excl_nexus_vars = c("dtj", "ctj"), memb_opts = "opp") %>% 
#   select(regu_trs_dom_opp) %>% summary 

#### low level conflict-related prosecutions: (prefix: lcon)

## conflict trials which are not ctj (i.e. not matched to UCDP conflict codes) of state agents and opposition: [_con]_Xctj_sta_opp
df <- TrialsMeasure(cy = df, prefix = "lcon", measure = "trs", type_opts = "dom", nexus_vars = "con", excl_nexus_vars = "ctj", memb_opts = c("sta", "opp")) 
df <- TrialsMeasure(cy = df, prefix = "lcon", measure = "tro", type_opts = "dom", nexus_vars = "con", excl_nexus_vars = "ctj", memb_opts = c("sta", "opp")) 
df <- TrialsMeasure(cy = df, prefix = "lcon", measure = "tfc", type_opts = "dom", nexus_vars = "con", excl_nexus_vars = "ctj", memb_opts = c("sta", "opp")) %>% 
  arrange(country_case, year) %>% 
  group_by(country_case) %>%  
  mutate(lcon_tfc_dom_sta_opp_cumu = cumsum(lcon_tfc_dom_sta_opp))
df <- TrialsMeasure(cy = df, prefix = "lcon", measure = "cct", type_opts = "dom", nexus_vars = "con", excl_nexus_vars = "ctj", memb_opts = c("sta", "opp")) 
df <- TrialsMeasure(cy = df, prefix = "lcon", measure = "crt", type_opts = "dom", nexus_vars = "con", excl_nexus_vars = "ctj", memb_opts = c("sta", "opp")) 
df <- TrialsMeasure(cy = df, prefix = "lcon", measure = "sen", type_opts = "dom", nexus_vars = "con", excl_nexus_vars = "ctj", memb_opts = c("sta", "opp")) 

#### intl & foreign 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi")
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta", rank_opts = "hi") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp", rank_opts = "hi")

### TCs

df <- TCmeasure(cy = df, new_col_name = "tcs_ctj", 
                start_year_var = "yearBeginOperation", 
                filter_nexus_vars = "fitsConflictTJ", 
                filter_crimes_vars = "all", 
                independence_opts = NULL, aims_opts = NULL, consult_vars = NULL, 
                powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL, 
                recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_ctj)

df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_victim_process", 
                start_year_var = "yearBeginOperation", 
                filter_nexus_vars = "fitsConflictTJ", 
                filter_crimes_vars = "all",
                aims_opts = c("truth for victims", "memorialization", "apology",
                              "recognition of victims", "reparation"), 
                independence_opts = NULL, 
                consult_vars = "consultedVictims", 
                powers_vars = "allocateReparations", 
                testimony_vars = "encourageVictimTestimony", 
                reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_ctj_victim_process_binary)
df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_victim_outcome", 
                start_year_var = "yearCompleteOperation",
                filter_nexus_vars = "fitsConflictTJ", 
                filter_crimes_vars = "all",
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
                powers_vars = NULL, testimony_vars = NULL, 
                reports_vars = "reportPubliclyAvailable",
                recommend_vars = "recommendReparations",
                monitor_vars = "mandatePeriodicMonitoringImplementation") %>% 
  select(-tcs_ctj_victim_outcome_binary)

df <- TCmeasure(cy = df, new_col_name = "tcs_victim_process", 
                start_year_var = "yearBeginOperation", 
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all",
                aims_opts = c("truth for victims", "memorialization", "apology",
                              "recognition of victims", "reparation"), 
                independence_opts = NULL, 
                consult_vars = "consultedVictims", 
                powers_vars = "allocateReparations", 
                testimony_vars = "encourageVictimTestimony", 
                reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_victim_process_binary)
df <- TCmeasure(cy = df, new_col_name = "tcs_victim_outcome", 
                start_year_var = "yearCompleteOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all",
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
                powers_vars = NULL, testimony_vars = NULL, 
                reports_vars = "reportPubliclyAvailable",
                recommend_vars = "recommendReparations",
                monitor_vars = "mandatePeriodicMonitoringImplementation") %>% 
  select(-tcs_victim_outcome_binary)

df <- TCmeasure(cy = df, new_col_name = "tcs_recreparations", 
                start_year_var = "yearCompleteOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all",
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
                powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
                recommend_vars = "recommendReparations",
                monitor_vars = NULL) %>% 
  select(-tcs_recreparations_binary)

df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_account_process", 
                start_year_var = "yearBeginOperation", 
                filter_nexus_vars = "fitsConflictTJ", 
                filter_crimes_vars = "all",
                aims_opts = c("accountability", "responsibility",
                              "prevention of human rights violations"),
                independence_opts = c("partially independent", 
                                      "fully independent"), 
                consult_vars = NULL, 
                powers_vars = c("compelTestimony", "supportProsecutions", 
                                "namePerpetrators"),
                testimony_vars = "perpetratorTestimony",
                reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) %>%
  select(-tcs_ctj_account_process_binary)
df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_account_outcome", 
                start_year_var = "yearCompleteOperation",
                filter_nexus_vars = "fitsConflictTJ", 
                filter_crimes_vars = "all",
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
                powers_vars = NULL, testimony_vars = NULL, 
                reports_vars = "reportPubliclyAvailable",
                recommend_vars = "recommendProsecutions",
                monitor_vars = "mandatePeriodicMonitoringImplementation") %>% 
  select(-tcs_ctj_account_outcome_binary)

df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_peace_process", 
                start_year_var = "yearBeginOperation", 
                filter_nexus_vars = "fitsConflictTJ", 
                filter_crimes_vars = "all",
                aims_opts = c("reconciliation", "coexistence", "dialogue", 
                              "non-recurrence"),
                independence_opts = NULL, consult_vars = NULL,
                powers_vars = "grantAmnesty",
                testimony_vars = "heldPublicHearings",
                reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_ctj_peace_process_binary)
df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_peace_outcome", 
                start_year_var = "yearCompleteOperation",
                filter_nexus_vars = "fitsConflictTJ", 
                filter_crimes_vars = "all",
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
                powers_vars = NULL, testimony_vars = NULL, 
                reports_vars = "reportPubliclyAvailable",
                recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_ctj_peace_outcome_binary)

df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_reform_process", 
                start_year_var = "yearBeginOperation", 
                filter_nexus_vars = "fitsConflictTJ", 
                filter_crimes_vars = "all",
                aims_opts = c("historial truth", "institutional reform", 
                              "addressing corruption"),
                independence_opts = c("partially independent", "fully independent"), 
                consult_vars = NULL,
                powers_vars = "recommendInstitutionalReforms",
                testimony_vars = "heldPublicHearings",
                reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_ctj_reform_process_binary)
df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_reform_outcome", 
                start_year_var = "yearCompleteOperation",
                filter_nexus_vars = "fitsConflictTJ", 
                filter_crimes_vars = "all",
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
                powers_vars = NULL, testimony_vars = NULL, 
                reports_vars = "reportPubliclyAvailable",
                recommend_vars = "reportRecommendInstitutionalReform", 
                monitor_vars = "mandatePeriodicMonitoringImplementation") %>% 
  select(-tcs_ctj_reform_outcome_binary)

df <- TCmeasure(cy = df, new_col_name = "tcs_metcriteria", operated = FALSE,
                start_year_var = "yearPassed",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all", 
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
                powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
                recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_metcriteria) %>% 
  rename("tcs_metcriteria" = "tcs_metcriteria_binary")

df <- TCmeasure(cy = df, new_col_name = "tcs_operated",
                start_year_var = "yearBeginOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all", 
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
                powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
                recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_operated) %>% 
  rename("tcs_operated" = "tcs_operated_binary")

df <- TCmeasure(cy = df, new_col_name = "tcs_goalstruth",
                start_year_var = "yearBeginOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all", 
                aims_opts = c("historial truth", "truth for victims"),
                independence_opts = NULL, consult_vars = NULL,
                powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
                recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_goalstruth_binary)

df <- TCmeasure(cy = df, new_col_name = "tcs_independent",
                start_year_var = "yearBeginOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all", 
                aims_opts = NULL,
                independence_opts = c("partially independent", "fully independent"), 
                consult_vars = NULL, powers_vars = NULL, testimony_vars = NULL, 
                reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_independent_binary)
df <- TCmeasure(cy = df, new_col_name = "tcs_ind_no",
          start_year_var = "yearBeginOperation",
          filter_nexus_vars = NULL, 
          filter_crimes_vars = "all", 
          aims_opts = NULL,
          independence_opts = c("don't know", "not independent"), 
          consult_vars = NULL, powers_vars = NULL, testimony_vars = NULL, 
          reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_ind_no_binary)
df <- TCmeasure(cy = df, new_col_name = "tcs_ind_part",
          start_year_var = "yearBeginOperation",
          filter_nexus_vars = NULL, 
          filter_crimes_vars = "all", 
          aims_opts = NULL,
          independence_opts = c("partially independent"), 
          consult_vars = NULL, powers_vars = NULL, testimony_vars = NULL, 
          reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_ind_part_binary)
df <- TCmeasure(cy = df, new_col_name = "tcs_ind_full",
          start_year_var = "yearBeginOperation",
          filter_nexus_vars = NULL, 
          filter_crimes_vars = "all", 
          aims_opts = NULL,
          independence_opts = c("fully independent"), 
          consult_vars = NULL, powers_vars = NULL, testimony_vars = NULL, 
          reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_ind_full_binary)
df <- df %>% 
  mutate(tcs_independent_scale = case_when(tcs_ind_no == 1 ~ 0,
                                           tcs_ind_part == 1 ~ 1, 
                                           tcs_ind_full == 1 ~ 2,
                                           TRUE ~ 0)) %>% 
  select(-tcs_ind_no,-tcs_ind_part, -tcs_ind_full) 

df <- TCmeasure(cy = df, new_col_name = "tcs_harms", 
                start_year_var = "yearBeginOperation",
                filter_nexus_vars = NULL,
                filter_crimes_vars = "all", 
                harms_vars = c("torture", "death", "disappearance", "SGBV", 
                               "forcedDisplacement", "corruption", "crimesOther"),
                aims_opts = NULL,
                independence_opts = NULL,
                consult_vars = NULL, powers_vars = NULL, testimony_vars = NULL,
                reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_harms_binary) %>% 
  mutate(tcs_harms = case_when(tcs_harms == 0 ~ 0, 
                               tcs_harms %in% 1:2 ~ 1, 
                               tcs_harms %in% 3:5 ~ 2, 
                               tcs_harms %in% 6:7 ~ 3)) 

df <- TCmeasure(cy = df, new_col_name = "tcs_powers",
                start_year_var = "yearBeginOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all", 
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
                powers_vars = c("investigateStateMembers", "compelTestimony", 
                                "accessOfficialDocuments", "namePerpetrators", 
                                "recommendInstitutionalReforms"), 
                testimony_vars = NULL, reports_vars = NULL,
                recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_powers_binary)

df <- TCmeasure(cy = df, new_col_name = "tcs_power_investigate",
                start_year_var = "yearBeginOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all", 
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
                powers_vars = c("investigateStateMembers"), 
                testimony_vars = NULL, reports_vars = NULL,
                recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_power_investigate_binary)
df <- TCmeasure(cy = df, new_col_name = "tcs_power_compel",
                start_year_var = "yearBeginOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all", 
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
                powers_vars = c("compelTestimony"), 
                testimony_vars = NULL, reports_vars = NULL,
                recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_power_compel_binary)
df <- TCmeasure(cy = df, new_col_name = "tcs_power_docs",
                start_year_var = "yearBeginOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all", 
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
                powers_vars = c("accessOfficialDocuments"), 
                testimony_vars = NULL, reports_vars = NULL,
                recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_power_docs_binary)
df <- TCmeasure(cy = df, new_col_name = "tcs_power_name",
                start_year_var = "yearBeginOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all", 
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
                powers_vars = c("namePerpetrators"), 
                testimony_vars = NULL, reports_vars = NULL,
                recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_power_name_binary)
df <- TCmeasure(cy = df, new_col_name = "tcs_power_reform",
                start_year_var = "yearBeginOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all", 
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
                powers_vars = c("recommendInstitutionalReforms"), 
                testimony_vars = NULL, reports_vars = NULL,
                recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_power_reform_binary)

df <- TCmeasure(cy = df, new_col_name = "tcs_report",
                start_year_var = "yearCompleteOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all", 
                aims_opts = NULL,
                independence_opts = NULL, consult_vars = NULL,
                powers_vars = NULL, testimony_vars = NULL, 
                reports_vars = "finalReportIssued",
                recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_report_binary)

df <- TCmeasure(cy = df, new_col_name = "tcs_public_process",
                start_year_var = "yearBeginOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all", 
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
                powers_vars = NULL, 
                testimony_vars = "heldPublicHearings", 
                reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_public_process_binary)
df <- TCmeasure(cy = df, new_col_name = "tcs_public_outcome",
                start_year_var = "yearCompleteOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all", 
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
                powers_vars = NULL, testimony_vars = NULL, 
                reports_vars = "reportPubliclyAvailable",
                recommend_vars = NULL, monitor_vars = NULL) %>% 
  select(-tcs_public_outcome_binary)

df <- TCmeasure(cy = df, new_col_name = "tcs_recommendations",
                start_year_var = "yearCompleteOperation",
                filter_nexus_vars = NULL,
                filter_crimes_vars = "all",
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
                powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
                recommend_vars = c("vetting", "SecuritySectorReforms", 
                                   "legalReform", "judicialReforms", 
                                   "humanRightsReforms", "corruptionReforms", 
                                   "genderReform") , 
                monitor_vars = NULL) %>% 
  select(-tcs_recommendations_binary)

df <- TCmeasure(cy = df, new_col_name = "tcs_monitoring",
                start_year_var = "yearCompleteOperation",
                filter_nexus_vars = NULL, 
                filter_crimes_vars = "all", 
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
                powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
                recommend_vars = NULL, 
                monitor_vars = "mandatePeriodicMonitoringImplementation") %>%
  select(-tcs_monitoring_binary)

### reparations

df <- ReparationMeasures(cy = df)

### amnesties

df <- AmnestyMeasure(cy = df, nexus_vars = c("dtj", "ctj"), who_opts = "sta")
df <- AmnestyMeasure(cy = df, nexus_vars = c("dtj", "ctj"), who_opts = "opp") 
df <- AmnestyMeasure(cy = df, nexus_vars = "ctj", who_opts = c("sta", "opp")) 
df <- AmnestyMeasure(cy = df, nexus_vars = "ctj", who_opts = "all") 
df <- AmnestyMeasure(cy = df, who_opts = "sta")
df <- AmnestyMeasure(cy = df, who_opts = "sta", what_opts = "hrv")
df <- AmnestyMeasure(cy = df, who_opts = "opp") 
df <- AmnestyMeasure(cy = df, who_opts = "opp", what_opts = "hrv")
df <- AmnestyMeasure(cy = df, who_opts = "pol")
df <- AmnestyMeasure(cy = df, peace_vars = "peaceSettlement") 

### vetting

df <- VettingMeasures(cy = df)

rm(vet_ctry_incl, vet_spells)

### domestic trials sample indicator

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
              select(country, year, access_mean, access_low95, access_upp95, 
                     access_rank, legacy_mean, legacy_low95, legacy_upp95, 
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
  filter(colname != "lag_*")

## these are in the codebook but not in the dataset 
codebook$colname[!codebook$colname %in% names(df)]
## these are in the dataset but not in the codebook
names(df)[!names(df) %in% codebook$colname]

df <- df %>% 
  select(all_of(codebook$colname[codebook$colname %in% names(df)]))

### last step, created lags and saving the analyses dataset

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

codebook %>% 
  select(colname, definition, source) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_codebook_analyses.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_codebook_analyses.csv"), na = "")

db[["dl_tjet_codebook"]] <- codebook %>% 
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

db[["dl_tjet_cy"]] <- df %>%
  select(all_of(db[["dl_tjet_codebook"]]$colname)) %>% 
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

### saving individual mechanism tables for local analyses & repo
### these will also be written to the database for downloads

db[["Transitions"]] <- db[["Transitions"]] %>% 
  rename(ccode_cow = ccode) %>%
  mutate(tjet_version = timestamp) %>%
  write_csv(here::here("tjet_datasets", "tjet_transitions.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_transitions.csv"), na = "")
db[["Amnesties"]] <- db[["Amnesties"]] %>% 
  filter(amnestyYear >= 1970 & amnestyYear <= 2020) %>%
  rename(ccode_cow = ccode) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_amnesties.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_amnesties.csv"), na = "")
db[["Reparations"]] <- db[["Reparations"]] %>% 
  filter(yearCreated >= 1970 & yearCreated <= 2020) %>%
  rename(ccode_cow = ccode) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_reparations.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_reparations.csv"), na = "")
db[["Trials"]] <- db[["Trials"]] %>%
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
  filter(yearPassed >= 1970 & yearPassed <= 2020) %>%
  rename(ccode_cow = ccode) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_tcs.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_tcs.csv"), na = "")
db[["Vettings"]] <- db[["Vettings"]] %>% 
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

### country profiles

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
  filter(!is.na(access_rank) | !is.na(legacy_rank)) %>%
  arrange(desc(legacy_rank)) %>%  
  select(country_case, ccode_case, country_fr, access_mean, access_rank, legacy_mean, legacy_rank) 

### data for summary spreadsheet

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
              "tran_tfc_dom_dtj_ctj_sta", "regu_tfc_dom_sta", 
              "tran_trs_dom_dtj_ctj_sta_hi", "tran_tfc_dom_dtj_ctj_sta_hi", 
              "tran_trs_dom_ctj_opp", "tran_tfc_dom_ctj_opp", 
              "lcon_trs_dom_sta_opp", "lcon_tfc_dom_sta_opp")
vars_int <- c("trials_intl", "trs_int_sta", "trs_int_opp", 
              "tfc_int_sta", "tfc_int_opp") 
vars_for <- c("trials_foreign", "trs_for_sta", "trs_for_opp", 
              "tfc_for_sta", "tfc_for_opp") 

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
          tran_tfc_dom_dtj_sta = sum(tran_tfc_dom_dtj_sta, na.rm = TRUE),
          tran_trs_dom_ctj_sta = sum(tran_trs_dom_ctj_sta, na.rm = TRUE), 
          tran_tfc_dom_ctj_sta = sum(tran_tfc_dom_ctj_sta, na.rm = TRUE),
          tran_trs_dom_dtj_ctj_sta = sum(tran_trs_dom_dtj_ctj_sta, na.rm = TRUE), 
          tran_tfc_dom_dtj_ctj_sta = sum(tran_tfc_dom_dtj_ctj_sta, na.rm = TRUE), 
          tran_trs_dom_dtj_ctj_sta_hi = sum(tran_trs_dom_dtj_ctj_sta_hi, na.rm = TRUE), 
          tran_tfc_dom_dtj_ctj_sta_hi = sum(tran_tfc_dom_dtj_ctj_sta_hi, na.rm = TRUE), 
          regu_trs_dom_sta = sum(regu_trs_dom_sta, na.rm = TRUE), 
          regu_tfc_dom_sta = sum(regu_tfc_dom_sta, na.rm = TRUE), 
          tran_trs_dom_ctj_opp = sum(tran_trs_dom_ctj_opp, na.rm = TRUE), 
          tran_tfc_dom_ctj_opp = sum(tran_tfc_dom_ctj_opp, na.rm = TRUE), 
          lcon_trs_dom_sta_opp = sum(lcon_trs_dom_sta_opp, na.rm = TRUE), 
          lcon_tfc_dom_sta_opp = sum(lcon_tfc_dom_sta_opp, na.rm = TRUE))

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
          tfc_int_hrs_con_all = sum(tfc_int_sta + tfc_int_opp, na.rm = TRUE))

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
          tfc_for_hrs_con_all = sum(tfc_for_sta + tfc_for_opp, na.rm = TRUE)) 

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
write_xlsx(data, path = "~/Dropbox/TJLab/TimoDataWork/country_profiles/summary_data.xlsx")

rm(tj_vars, vars_dom, vars_for, vars_int) 

### automating written summaries 

n_transform <- function(x) {
  x %>% 
    paste(" ", ., sep = "") %>%  
    str_replace_all(" 0 ", " none ") %>% 
    str_replace_all(" 1 ", " one ") %>% 
    str_replace_all(" 2 ", " two ") %>% 
    str_replace_all(" 3 ", " three ") %>% 
    str_replace_all(" 4 ", " four ") %>% 
    str_replace_all(" 5 ", " five ") %>% 
    str_replace_all(" 6 ", " six ") %>% 
    str_replace_all(" 7 ", " seven ") %>% 
    str_replace_all(" 8 ", " eight ") %>% 
    str_replace_all(" 9 ", " nine ") %>% 
    str_replace_all(" 10 ", " ten ") %>%
    str_replace_all(" 11 ", " eleven ") %>% 
    str_replace_all(" 12 ", " twelve ") %>% 
    str_trim() %>%
    return() 
}

n_transform_nth <- function(x) {
  x %>% 
    paste(" ", ., sep = "") %>%  
    str_replace_all("1 ", "1st ") %>% 
    str_replace_all("2 ", "2nd ") %>% 
    str_replace_all("3 ", "3rd ") %>% 
    str_replace_all("4 ", "4th ") %>% 
    str_replace_all("5 ", "5th ") %>% 
    str_replace_all("6 ", "6th ") %>% 
    str_replace_all("7 ", "7th ") %>% 
    str_replace_all("8 ", "8th ") %>% 
    str_replace_all("9 ", "9th ") %>% 
    str_replace_all("0 ", "0th ") %>%
    str_trim() %>%
    return() 
}

### summary 

autotxt[["summary"]] <- autoprep[["summary"]] %>%
  rowwise() %>%
  mutate(
    text = paste("For ", 
                 country_case, 
                 ", TJET has collected information on: ",  
                 str_flatten(
                   c(
                     if(amnesties > 0) paste(amnesties, 
                                             ifelse(amnesties == 1,
                                                    "amnesty", "amnesties"), 
                                             ifelse(min(amnesties_yrs) == max(amnesties_yrs), 
                                                    paste("in", unique(amnesties_yrs)), 
                                                    paste("between", 
                                                          min(amnesties_yrs), 
                                                          "and", 
                                                          max(amnesties_yrs) )) ), 
                     if(domestic > 0) paste(domestic, "domestic", 
                                            ifelse(domestic == 1,
                                                   "trial", "trials"), 
                                            "starting", 
                                            ifelse(min(domestic_yrs) == max(domestic_yrs), 
                                                   paste("in", unique(domestic_yrs)), 
                                                   paste("between", 
                                                         min(domestic_yrs), 
                                                         "and", 
                                                         max(domestic_yrs) ))), 
                     if(foreign > 0) paste(foreign, "foreign", 
                                           ifelse(foreign == 1,
                                                  "trial", "trials"), 
                                           "starting", 
                                           ifelse(min(foreign_yrs) == max(foreign_yrs), 
                                                  paste("in", unique(foreign_yrs)), 
                                                  paste("between", 
                                                        min(foreign_yrs), 
                                                        "and", 
                                                        max(foreign_yrs) ))),
                     if(intl > 0) paste(intl, "international", 
                                        ifelse(intl == 1,
                                               "trial", "trials"), 
                                        "starting", 
                                        ifelse(min(intl_yrs) == max(intl_yrs), 
                                               paste("in", unique(intl_yrs)), 
                                               paste("between", 
                                                     min(intl_yrs), 
                                                     "and", 
                                                     max(intl_yrs) ))),
                     if(reparations > 0) paste(reparations, "reparations", 
                                               ifelse(reparations == 1,
                                                      "policy", "policies"), 
                                               "created", 
                                               ifelse(min(reparations_yrs) == max(reparations_yrs), 
                                                      paste("in", unique(reparations_yrs)), 
                                                      paste("between", 
                                                            min(reparations_yrs), 
                                                            "and", 
                                                            max(reparations_yrs) )) ), 
                     if(tcs > 0) paste(tcs, "truth", 
                                       ifelse(tcs == 1,
                                              "commission", "commissions"), 
                                       "mandated", 
                                       ifelse(min(tcs_yrs) == max(tcs_yrs), 
                                              paste("in", unique(tcs_yrs)), 
                                              paste("between", 
                                                    min(tcs_yrs), 
                                                    "and", 
                                                    max(tcs_yrs) )) ),
                     if(vettings > 0) paste(vettings, "vetting", 
                                            ifelse(vettings == 1,
                                                   "policy", "policies"), 
                                            "starting", 
                                            ifelse(min(vettings_yrs) == max(vettings_yrs), 
                                                   paste("in", unique(vettings_yrs)), 
                                                   paste("between", 
                                                         min(vettings_yrs), 
                                                         "and", 
                                                         max(vettings_yrs) )) )
                   ), 
                   collapse = "; ", last = "; and "), 
                 ".", sep = "") %>% 
      n_transform(), 
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>% 
  select(country_case, ccode_case, text) 

### transitions 

autotxt[["Transitions"]] <- autoprep[["transitions"]] %>%
  rowwise() %>%
  mutate(
    text = paste("Based on well-known democracy data, TJET records ", 
                 length(trans_year_begin), 
                 " democratic ", 
                 ifelse(length(trans_year_begin) == 1,
                        "transition", "transitions"), 
                 " starting in ", 
                 str_flatten_comma(trans_year_begin, ", and "), 
                 ".", sep = "") %>% 
      n_transform(), 
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text) 

### violent intrastate conflict episodes

autotxt[["Conflicts"]] <- autoprep[["conflicts"]] %>%
  rowwise() %>%
  mutate(text = "", 
         text = paste("Based on the Uppsala Conflict Data Program, TJET records ",
                      episodes,
                      " violent intrastate conflict ",
                      ifelse(episodes == 1, "episode ", "episodes "),
                      ifelse(min(years) == max(years),
                             paste("in", unique(years)),
                             paste("between",
                                   min(years),
                                   "and",
                                   max(years) )),
                      ifelse(length(years) > 1 & length(years) < max(years) - min(years) + 1, 
                             paste(" (during ", 
                                   length(years), 
                                   " calendar years)", 
                                   sep = ""), 
                             ""), 
                      ", involving ",
                      dyads,
                      ifelse(dyads == 1,
                             " armed opposition group",
                             " distinct armed opposition groups"),
                      " fighting against the government.",
                      sep = "") %>%
           n_transform(),
         text = list(c(text, 
                       if(int_ep > 0) 
                         paste(int_ep, 
                               "conflict", 
                               ifelse(int_ep == 1, "episode was", "episodes were"), 
                               "internationalized by involvement of external state actors.") %>% 
                         n_transform() %>% 
                         str_to_sentence() )), 
         text = str_flatten(text, " ") %>%
           str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text)

### indices 

pluriel <- c("Philippines", "United States of America", "Maldives", "Fiji", 
             "Comoros", "Netherlands", "Solomon Islands", "Seychelles", "United Arab Emirates") 
  
autoprep[["rankings"]] %>% 
  rowwise() %>%
  mutate(
    legacy = paste(country_case,
                 "ranks",
                 n_transform_nth(paste(legacy_rank, " ", sep = "")),
                 "on our legacy of violence index in 2020."),
    legacy = str_flatten(legacy, " ") %>%
      str_trim(),
    legacy_fr = paste(country_fr, 
                   "se classe au",
                   paste(legacy_rank, "e", sep = ""), 
                   "rang de notre indice d'héritage de la violence en 2020."),
    legacy_fr = str_flatten(legacy_fr, " ") %>%
      str_trim(), 
    legacy_fr = ifelse(country_case %in% pluriel, 
                       str_replace(legacy_fr, "se classe", "se classent"), 
                       legacy_fr), 
    access = paste(country_case,
                   "ranks",
                   n_transform_nth(paste(access_rank, " ", sep = "")),
                   "on our access to regular justice index in 2020."),
    access_fr = str_flatten(access, " ") %>%
      str_trim(),
    access_fr = paste(country_fr, 
                   "se classe au",
                   n_transform_nth(paste(access_rank, " ", sep = "")), 
                   "rang de notre indice d'accès à la justice régulière en 2020."),
    access_fr = ifelse(country_case %in% pluriel, 
                       str_replace(access_fr, "se classe", "se classent"), 
                       access_fr), 
    access_fr = str_flatten(access_fr, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, country_fr, ccode_case, legacy_fr, access_fr, 
         access_mean, access_rank, legacy_mean, legacy_rank) %>% 
  saveRDS(here::here("data", "rankings.rds")) 

### TJ mechanisms

autotxt[["Amnesties"]] <- data[["Amnesties"]] %>% 
  rowwise() %>%
  mutate(
    text = list(paste(country_case, 
                      " had ", 
                      count_all, 
                      ifelse(count_all == 1, " amnesty ", " amnesties "), 
                      ifelse(beg == end, 
                             paste("in", beg), 
                             paste("between", beg, "and", end)), 
                      ".", sep = "") %>% 
                  n_transform() ), 
    text = list(c(text, 
                  if(count_demtrans > 0) 
                    paste(count_demtrans, 
                          "occurred in the context of democratic transition.") %>% 
                    n_transform() %>% 
                    str_to_sentence() )), 
    text = list(c(text, 
                  if(count_dcj > 0) 
                    paste(count_dcj, 
                          ifelse(count_dcj == 1, "was", "were"), 
                          "passed during ongoing internal armed conflict.") %>% 
                    n_transform() %>% 
                    str_to_sentence() )), 
    text = list(c(text, 
                  if(count_pcj > 0) 
                    paste(count_pcj, 
                          ifelse(count_pcj == 1, "was", "were"), 
                          "passed after internal armed conflict.") %>% 
                    n_transform() %>% 
                    str_to_sentence() )), 
    text = list(c(text, 
                  if(count_peaceagree > 0) 
                    paste(count_peaceagree, 
                          ifelse(count_peaceagree == 1, "was", "were"), 
                          "part of a peace agreement.") %>% 
                    n_transform() %>% 
                    str_to_sentence() )), 
    text = list(c(text, 
                  if(count_prisoners > 0) 
                    paste(count_prisoners, 
                          ifelse(count_prisoners == 1, "amnesty", "amnesties"), 
                          "released political prisoners.") %>% 
                    n_transform() %>% 
                    str_to_sentence() )),
    text = list(c(text, 
                  if(count_hrv > 0) 
                    paste(count_hrv, 
                          ifelse(count_hrv == 1, "amnesty", "amnesties"), 
                          "forgave human rights violations.") %>% 
                    n_transform() %>% 
                    str_to_sentence() )), 
    text = str_flatten(text, " ") %>% 
      str_trim()
  ) %>%
  ungroup() %>% 
  select(country_case, ccode_case, text)

autotxt[["Domestic_cy"]] <- data[["Domestic_cy"]] %>%
  rowwise() %>%
  mutate(text = list(c("", 
                       if(total > 0)
                         paste("TJET has compiled data on",
                               total,
                               "domestic",
                               ifelse(total == 1, "prosecution", "prosecutions"),
                               ifelse(beg == end,
                                      paste("in ", beg, ".", sep = ""),
                                      paste("between ", beg, " and ", end, ".", sep = ""))
         ) %>%
           n_transform())),
         text = list(c(text, 
                       if(tran_trs_dom_dtj_sta > 0 | regu_trs_dom_sta > 0 | 
                          tran_trs_dom_ctj_sta > 0 | tran_trs_dom_ctj_opp > 0 | 
                          lcon_trs_dom_sta_opp > 0)
                         paste("These include ",
                               str_flatten(c(
                                 if(tran_trs_dom_dtj_sta > 0)
                                   paste(tran_trs_dom_dtj_sta,
                                         "transitional human rights",
                                         ifelse(tran_trs_dom_dtj_sta == 1,
                                                "prosecution", "prosecutions"),
                                         "of state agents, in which",
                                         tran_tfc_dom_dtj_sta,
                                         ifelse(tran_tfc_dom_dtj_sta == 1,
                                                "person was", "persons were"),
                                         "convicted"),
                                 if(regu_trs_dom_sta > 0)
                                   paste(regu_trs_dom_sta,
                                         "regular human rights",
                                         ifelse(regu_trs_dom_sta == 1,
                                                "prosecution", "prosecutions"),
                                         "of state agents, in which",
                                         regu_tfc_dom_sta,
                                         ifelse(regu_tfc_dom_sta == 1,
                                                "person was", "persons were"),
                                         "convicted"),
                                 if(tran_trs_dom_ctj_sta > 0)
                                   paste(tran_trs_dom_ctj_sta,
                                         "intrastate conflict",
                                         ifelse(tran_trs_dom_ctj_sta == 1,
                                                "prosecution", "prosecutions"),
                                         "of state agents, in which",
                                         tran_tfc_dom_ctj_sta,
                                         ifelse(tran_tfc_dom_ctj_sta == 1,
                                                "person was", "persons were"),
                                         "convicted"),
                                 if(tran_trs_dom_ctj_opp > 0)
                                   paste(tran_trs_dom_ctj_opp,
                                         "intrastate conflict",
                                         ifelse(tran_trs_dom_ctj_opp == 1,
                                                "prosecution", "prosecutions"),
                                         "of opposition members, in which",
                                         tran_tfc_dom_ctj_opp,
                                         ifelse(tran_tfc_dom_ctj_opp == 1,
                                                "person was", "persons were"),
                                         "convicted"),
                                 if(lcon_trs_dom_sta_opp > 0)
                                   paste(lcon_trs_dom_sta_opp,
                                         "low-level conflict",
                                         ifelse(lcon_trs_dom_sta_opp == 1,
                                                "prosecution", "prosecutions"),
                                         "of state agents or opposition members, in which",
                                         lcon_tfc_dom_sta_opp,
                                         ifelse(lcon_tfc_dom_sta_opp == 1,
                                                "person was", "persons were"),
                                         "convicted")), 
                                 collapse = "; ", last = "; and ", na.rm = TRUE), 
                               ".", sep = ""
                           ) %>%
                         n_transform() %>%
                         str_replace_all("none persons were", "noone was") 
                       )),
         # "Of XX trials that involved high-ranking state agents, XX were convicted."
         text = list(c(text,
                       if(tran_trs_dom_dtj_ctj_sta_hi > 0)
                         paste("In",
                               tran_trs_dom_dtj_ctj_sta_hi,
                               ifelse(tran_trs_dom_dtj_ctj_sta_hi == 1,
                                      "trial", "trials"),
                               "that involved high-ranking state agents,", 
                               tran_tfc_dom_dtj_ctj_sta_hi,
                               ifelse(tran_tfc_dom_dtj_ctj_sta_hi == 1,
                                      "person was", "persons were"),
                               "convicted."
                         ) %>%
                         n_transform() %>%
                         str_replace("none persons were", "noone was")
                       )),
         text = str_flatten(text, " ") %>% 
           str_trim()
  ) %>%
  ungroup() %>% 
  select(country_case, ccode_case, 
         # beg, end, total, # total domestic trials between beg and end
         # tran_trs_dom_dtj_sta, # transitional prosecutions of state agents in democratic transition context
         # tran_trs_dom_ctj_sta, # transitional prosecutions of state agents in conflict or post-conflict context
         # tran_trs_dom_dtj_ctj_sta, # transitional prosecutions of state agents
         # tran_tfc_dom_dtj_ctj_sta, # total final convictions in transitional prosecutions of state agents
         # tran_trs_dom_dtj_ctj_sta_hi, # transitional prosecutions of high-ranking state agents
         # tran_tfc_dom_dtj_ctj_sta_hi, # total final convictions in transitional prosecutions of high-ranking state agents
         # regu_trs_dom_sta, # regular human rights prosecutions of state agents
         # regu_tfc_dom_sta, # total final convictions in regular human rights prosecutions of state agents
         # tran_trs_dom_ctj_opp, # transitional prosecutions of opposition members in conflict or post-conflict context
         # tran_tfc_dom_ctj_opp, # total final convictions in transitional prosecutions of opposition members in conflict or post-conflict context
         # lcon_trs_dom_sta_opp, # prosecutions of state agents or opposition members in low-level conflict context
         # lcon_tfc_dom_sta_opp, # total final convictions in prosecutions of state agents or opposition members in low-level conflict context
         text)

autotxt[["Foreign"]] <- data[["Foreign"]] %>% 
  group_by(countryAccused) %>% 
  mutate(count = n(), 
         countryTrial = list(sort(unique(unlist(countryTrial)))),
         yearStart = list(unlist(yearStart)),
         yearEnd = list(unlist(yearEnd))) %>% 
  select(countryAccused, ccode_Accused, count, countryTrial, yearStart, yearEnd) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(countryTrial = str_flatten_comma(countryTrial, last = ", and ") %>% 
           str_replace(" Netherlands", " the Netherlands") %>% 
           str_replace(" United Kingdom", " the United Kingdom") %>% 
           str_replace(" United States of America", " the United States of America") , 
         text = paste("Nationals of ", 
                      countryAccused, 
                      " were defendants in ", 
                      count, 
                      ifelse(count == 1, " foreign prosecution", " foreign prosecutions"), 
                      " in ",
                      countryTrial, 
                      " beginning ", 
                      ifelse(length(unique(yearStart)) == 1, 
                             paste("in", unique(yearStart)), 
                             paste("between", 
                                   min(yearStart), 
                                   "and", 
                                   max(yearStart))), 
                      ".", sep = "") %>% 
           n_transform()
  ) %>% 
  ungroup() %>% 
  select(countryAccused, ccode_Accused, text) %>% 
  rename("country_case" = "countryAccused",
         "ccode_case" = "ccode_Accused")

autotxt[["Intl"]] <- data[["Intl_cy"]] %>%
  rowwise() %>% 
  mutate(text = paste("Nationals of ", 
                      country_case, 
                      " were subject to ", 
                      trs_int_hrs_con_all, 
                      " international ", 
                      ifelse(trs_int_hrs_con_all == 1, 
                             "prosecution ", "prosecutions "), 
                      # " starting ",
                      ifelse(beg == end, 
                             paste("in", beg), 
                             paste("between", 
                                   beg, 
                                   "and", 
                                   end) 
                      ), 
                      ", ", 
                      tfc_int_hrs_con_all, 
                      " of which led to a final conviction.", 
                      sep = "") %>% 
           n_transform()
  ) %>% 
  ungroup() %>% 
  select(country_case, ccode_case, text) 

autotxt[["ICC"]] <- data[["ICC-interventions"]] %>% 
  rowwise() %>%
  mutate(
    text = ifelse(is.na(ICC_prelimEnd), 
                  paste("The ICC's Office of the Prosecutor opened a preliminary examination of the situation in ", 
                        country_case, " in ", ICC_prelim_exam, ".", sep = ""), 
                  paste("The ICC's Office of the Prosecutor carried out a preliminary examination of the situation in ", 
                        country_case, " from ", ICC_prelim_exam, " until ", ICC_prelimEnd, ".", sep = "")), 
    text = list(c(if(!is.na(ICC_referral))
      paste(country_case, " was referred to the ICC in ", ICC_referral, ".", sep = ""), 
      text)), 
    text = list(c(text,
                  if(!is.na(ICC_investigation))
                    paste("The first investigation of a specific case began in ", ICC_investigation, ".", sep = ""))),
    text = str_flatten(text, " ") %>% 
      str_trim()
  ) %>%
  ungroup() %>% 
  select(country_case, ccode_case, text)

autotxt[["ICCaccused"]] <- data[["ICC-accused"]] %>% 
  group_by(country_case) %>%
  mutate(count = n(),
         count_appear = sum(!is.na(ICC_arrestAppear)),
         count_proceed = sum(!is.na(ICC_proceedings)),
         ICC_arrest_warrant = list(unlist(ICC_arrest_warrant)),
         ICC_arrestAppear = list(unlist(ICC_arrestAppear)),
         ICC_proceedings = list(unlist(ICC_proceedings))) %>%
  select(country_case, ccode_case, count, ICC_arrest_warrant, count_appear, ICC_arrestAppear, 
         count_proceed, ICC_proceedings) %>% 
  ungroup() %>% 
  distinct() %>% 
  rowwise() %>%
  mutate(ICC_arrestAppear = list(ICC_arrestAppear[!is.na(ICC_arrestAppear)]),
         ICC_proceedings = list(ICC_proceedings[!is.na(ICC_proceedings)]),
         text = paste("Starting in ", 
                      min(ICC_arrest_warrant), 
                      ", the ICC issued ", 
                      count, 
                      " arrest warrants, ",
                      count_appear, 
                      " of which resulted in court appearances.", 
                      sep = "") %>% 
           n_transform(), 
         text = list(c(text,
                       if(count_proceed > 0)
                         paste("Proceedings began in ",
                               count_proceed, 
                               ifelse(count_proceed == 1, " case ", " cases "), 
                               ifelse(length(unique(ICC_proceedings)) == 1, 
                                      paste("in", unique(ICC_proceedings)), 
                                      paste("between", 
                                            min(ICC_proceedings, na.rm = TRUE), 
                                            "and", 
                                            max(ICC_proceedings, na.rm = TRUE))), 
                               ".", sep = "") %>% 
                         n_transform()
         )),
         text = str_flatten(text, " ") %>% 
           str_trim()
  ) %>% 
  ungroup() %>% 
  select(country_case, ccode_case, text)

autotxt[["UNinvestigations"]] <- data[["Investigations"]] %>%
  mutate(country = ifelse(country == "Iran/Iraq" & ccode_case == 630, "Iran", country), 
         country = ifelse(country == "Iran/Iraq" & ccode_case == 645, "Iraq", country)) %>%
  select(country, country_case, ccode_case, mandate, beg, end, 
         uninv_dompros, uninv_evcoll, uninv_intlpros) %>% 
  arrange(country_case, mandate) %>% 
  group_by(ccode_case, mandate) %>%
  mutate(beg = ifelse(ccode_case == 437 & mandate == "Office of the UN High Commissioner for Human Rights", beg, min(beg)), 
         end = ifelse(ccode_case == 437 & mandate == "Office of the UN High Commissioner for Human Rights", end, max(end)), 
         uninv_dompros = max(uninv_dompros),
         uninv_evcoll = max(uninv_evcoll),
         uninv_intlpros = max(uninv_intlpros)) %>% 
  distinct() %>% 
  arrange(ccode_case, beg) %>% 
  group_by(ccode_case) %>%
  mutate(count = n(), 
         beg = list(unlist(beg)),
         end = list(unlist(end)),
         uninv_dompros = sum(uninv_dompros),
         uninv_evcoll = sum(uninv_evcoll),
         uninv_intlpros = sum(uninv_intlpros)
  ) %>% 
  select(country, country_case, ccode_case, count, beg, end, uninv_dompros, uninv_evcoll, uninv_intlpros) %>%
  distinct() %>% 
  mutate(country = ifelse(country == "Yugoslavia", "Serbia and Montenegro", country)) %>% 
  rowwise() %>%
  mutate(text = paste(country, 
                      " was subject to ", 
                      count, 
                      ifelse(count == 1, " UN investigation ", " UN investigations "), 
                      ifelse(min(beg) == max(end), 
                             paste("in", min(beg)),
                             paste("between", min(beg), "and", max(end))), 
                      ".", sep = "") %>% 
           n_transform(), 
         text = list(c(text,
                       if(uninv_dompros > 0)
                         paste(uninv_dompros, 
                               ifelse(uninv_dompros == 1, "investigation", "investigations"), 
                               "aimed to encourage domestic prosecutions.") %>% 
                         n_transform() %>% 
                         str_to_sentence() %>% 
                         str_trim()
         )),
         text = list(c(text,
                       if(uninv_intlpros > 0)
                         paste(uninv_intlpros, 
                               ifelse(uninv_intlpros == 1, "investigation", "investigations"), 
                               "aimed to support international prosecutions.") %>% 
                         n_transform() %>% 
                         str_to_sentence()
         )),
         text = list(c(text,
                       if(uninv_evcoll > 0)
                         paste(uninv_evcoll, 
                               ifelse(uninv_evcoll == 1, "investigation", "investigations"), 
                               "aimed to collect evidence for prosecutions.") %>% 
                         n_transform() %>% 
                         str_to_sentence()
         )),
         text = str_flatten(text, " ") %>% 
           str_trim()
  ) %>%
  ungroup() %>% 
  select(country_case, ccode_case, text)

autotxt[["Reparations"]] <- data[["Reparations"]] %>%
  mutate(implemented = ifelse(!is.na(yearBegin) | !is.na(yearEnd) | !is.na(beneficiariesCount), 1, 0), 
         individual = case_when(individualReparations == "yes" ~ 1, 
                                TRUE ~ 0), 
         collective = case_when(collectiveReparations == "yes" ~ 1, 
                                TRUE ~ 0), 
         beneficiariesCount = ifelse(is.na(beneficiariesCount), 0, beneficiariesCount) 
  ) %>% 
  group_by(country_case) %>% 
  mutate(yearCreated = list(unlist(yearCreated)),
         yearBegin = list(unlist(yearBegin)),
         yearEnd = list(unlist(yearEnd)),
         individual = sum(individual),
         collective = sum(collective),
         implemented = sum(implemented), 
         beneficiariesCount = sum(beneficiariesCount) 
  ) %>% 
  select(country_case, ccode_case, count, yearCreated, yearBegin, yearEnd, 
         individual, collective, beneficiariesCount, implemented) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(yearBegin = list(yearBegin[!is.na(yearBegin)]),
         yearEnd = list(yearEnd[!is.na(yearEnd)]),
         text = "", 
         text = list(c(text, 
                       if(length(yearBegin) == 0 & length(yearEnd) == 0)
                         paste(country_case, 
                               " mandated ", 
                               count, 
                               ifelse(count == 1, " reparations policy in ", " reparations policies in "), 
                               str_flatten_comma(unique(yearCreated), last = ", and "),
                               # ifelse(length(yearCreated) == 1, 
                               #        paste("in", unique(yearCreated)), 
                               #        paste("between", min(yearCreated), "and", max(yearCreated))
                               # ), 
                               ".", sep = "") %>% 
                         n_transform()
         )), 
         text = list(c(text, 
                       if(length(yearBegin) == 0 & length(yearEnd) == 1)
                         paste(country_case, 
                               " mandated ", 
                               count, 
                               ifelse(count == 1, " reparations policy ", " reparations policies "), 
                               ifelse(length(unique(yearCreated)) == 1, 
                                      paste("in", unique(yearCreated)), 
                                      paste("between", min(yearCreated), "and", max(yearCreated))
                               ), 
                               ", which ended by ", 
                               max(yearEnd),
                               ".", sep = ""
                         ) %>% 
                         n_transform()
         )), 
         text = list(c(text, 
                       if(length(yearBegin) > 0 & length(yearBegin) == length(yearEnd))
                         paste(country_case, 
                               " implemented ", 
                               count, 
                               ifelse(count == 1, " reparations policy ", " reparations policies "), 
                               ifelse(min(yearBegin) == max(yearEnd), 
                                      paste("in", min(yearBegin)), 
                                      paste("between", min(yearBegin), "and", max(yearEnd))
                               ), 
                               ".", sep = ""
                         ) %>% 
                         n_transform()
         )), 
         text = list(c(text, 
                       if(length(yearBegin) > 0 & length(yearBegin) > length(yearEnd))
                         paste(country_case, 
                               " implemented ", 
                               count, 
                               ifelse(count == 1, " reparations policy, ", " reparations policies, "), 
                               "starting in ", 
                               min(yearBegin),
                               ".", sep = ""
                         ) %>% 
                         n_transform()
         )), 
         text = list(c(text, 
                       if(beneficiariesCount > 0)
                         paste("According to available information, there was a total of", 
                               beneficiariesCount, 
                               "individual beneficiaries."
                         ) %>% 
                         n_transform() %>% 
                         str_to_sentence()
         )),
         text = list(c(text, 
                       if(beneficiariesCount == 0)
                         paste("TJET found no information on the total number of beneficiaries.")
         )),
         text = list(c(text, 
                       if(collective > 0)
                         paste(collective, 
                               ifelse(collective == 1, " reparations policy ", " reparations policies "), 
                               "provided collective benefits.", 
                               sep = ""
                         ) %>% 
                         n_transform() %>% 
                         str_to_sentence() %>% 
                         str_trim()
         )),
         text = list(c(text, 
                       if(implemented > 0 & implemented < count)
                         paste("TJET found evidence on implementation only for", 
                               implemented, 
                               ifelse(implemented == 1, " reparations policy.", " reparations policies.")) %>% 
                         n_transform()
         )), 
         text = list(c(text, 
                       if(implemented == 0)
                         paste("TJET found no evidence on implementation.")
         )), 
         text = str_flatten(text, " ") %>% 
           str_trim()
  ) %>%
  ungroup() %>% 
  select(country_case, ccode_case, text)

autotxt[["TruthCommissions"]] <- data[["TruthCommissions"]] %>%
  mutate(finalReportIssued = case_when(finalReportIssued == "Yes" ~ 1,
                                       TRUE ~ 0),
         reportPubliclyAvailable = case_when(reportPubliclyAvailable == "Yes" ~ 1,
                                             TRUE ~ 0),
         victims = case_when(consultedVictims == 1 | 
                               commissionersVictimGroups == "Yes" | 
                               encourageVictimTestimony == "Yes" ~ 1,
                             TRUE ~ 0), 
         yearBeginOperation = ifelse(is.na(yearBeginOperation) & !is.na(yearCompleteOperation), 
                                     yearPassed, yearBeginOperation) 
  ) %>% 
  group_by(country_case) %>% 
  mutate(yearPassed = list(unlist(yearPassed)),
         yearBeginOperation = list(unlist(yearBeginOperation)),
         yearCompleteOperation = list(unlist(yearCompleteOperation)),
         finalReportIssued = sum(finalReportIssued),
         reportPubliclyAvailable = sum(reportPubliclyAvailable),
         rec_prosecutions = sum(rec_prosecutions),
         rec_reparations = sum(rec_reparations),
         rec_reforms = sum(rec_reforms),
         victims = sum(victims)
  ) %>%
  select(country_case, ccode_case, count, yearPassed, yearBeginOperation, yearCompleteOperation, 
         finalReportIssued, reportPubliclyAvailable, rec_prosecutions,
         rec_reparations, rec_reforms, victims) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(yearBeginOperation = list(yearBeginOperation[!is.na(yearBeginOperation)]),
         yearCompleteOperation = list(yearCompleteOperation[!is.na(yearCompleteOperation)]),
         text = paste(country_case,
                      " mandated ",
                      count,
                      " truth ",
                      ifelse(count == 1, "commission", "commissions"),
                      " in ",
                      str_flatten_comma(unique(yearPassed), last = ", and "),
                      ".", sep = "") %>%
           n_transform(),
         ### one but not completed
         text = list(c(text,
                       if(length(yearBeginOperation) == count & length(yearCompleteOperation) == 0 & count == 1)
                         paste("The commission began its operations in ",
                               min(yearBeginOperation),
                               "; TJET has found no evidence of completion.", sep = "")
         )),
         ### all started but not all completed
         text = list(c(text,
                       if(length(yearBeginOperation) == count & length(yearCompleteOperation) != 0 & length(yearCompleteOperation) < count)
                         paste(length(yearCompleteOperation), 
                               " completed ",
                               ifelse(length(yearCompleteOperation) == 1, "its", "their"), 
                               " operations by ", 
                               max(yearCompleteOperation), 
                               ".", sep = "") %>% 
                         n_transform() %>%
                         str_to_sentence()
         )),
         ### not all started, but all that started also completed
         text = list(c(text,
                       if(length(yearBeginOperation) < count & length(yearCompleteOperation) == length(yearBeginOperation))
                         paste(length(yearBeginOperation),
                               " of these operated between ",
                               min(yearBeginOperation),
                               " and ",
                               max(yearCompleteOperation),
                               ".", sep = "") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         ### not all started, and none completed
         text = list(c(text,
                       if(length(yearBeginOperation) < count & length(yearCompleteOperation) == 0)
                         paste(length(yearBeginOperation), 
                               "began operations but TJET has found no evidence of completion.") %>% 
                         n_transform() %>%
                         str_to_sentence() %>%
                         str_replace("tjet", "TJET")
         )),
         ### all completed
         text = list(c(text,
                       if(length(yearCompleteOperation) == count)
                         paste(ifelse(count == 1, 
                                      "The commission completed its", 
                                      "The commissions completed their"),
                               " operations in ",
                               str_flatten_comma(unique(yearCompleteOperation), last = ", and "),
                               ".", sep = "") %>%
                         n_transform()
         )),
         text = list(c(text,
                       if(finalReportIssued > 0)
                         paste(ifelse(finalReportIssued == count,
                                      ifelse(count == 1,
                                             "The commission issued",
                                             "The commissions issued"),
                                      paste(finalReportIssued, "of the commissions issued")),
                               ifelse(finalReportIssued == 1, " a ", " "),
                               ifelse(finalReportIssued == 1, "final report", "final reports"),
                               ifelse(reportPubliclyAvailable > 0 & reportPubliclyAvailable < finalReportIssued,
                                      paste(",",
                                            reportPubliclyAvailable,
                                            "of which"),
                                      ", which"),
                               ifelse(reportPubliclyAvailable == 1,
                                      " is", " are"),
                               " publicly available.",
                               sep = ""
                         ) %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         text = list(c(text,
                       if(finalReportIssued > 0 &
                          (rec_prosecutions > 0 | rec_reparations > 0 | rec_reforms > 0))
                         paste("The ",
                               ifelse(finalReportIssued == 1, "report", "reports"),
                               " included recommendations for ",
                               str_flatten_comma(
                                 c(if(rec_prosecutions > 0) "prosecutions",
                                   if(rec_reparations > 0) "reparations",
                                   if(rec_reforms > 0) "institutional reforms"
                                 ),
                                 last = ", and ", na.rm = TRUE
                               ),
                               ".", sep = ""
                         ) %>%
                         n_transform() %>%
                         str_to_sentence() %>%
                         str_trim()
         )),
         text = str_flatten(text, " ") %>% 
           str_trim()
  ) %>%
  ungroup() %>% 
  select(country_case, ccode_case, text)

autotxt[["Vettings"]] <- data[["Vettings"]] %>% 
  mutate(alterationOf = ifelse(is.na(alterationOf), vettingID, alterationOf), 
         alterationOf = str_replace(alterationOf, fixed("?"), "")) %>% 
  select(-vettingID) %>%  
  rename("vettingID" = "alterationOf") %>%
  group_by(vettingID) %>% 
  mutate(yearStart = min(yearStart), 
         yearEnd = max(yearEnd),
         individual_conduct = max(individual_conduct), 
         type_dismissal = max(type_dismissal), 
         type_ban = max(type_ban), 
         type_declassification = max(type_declassification), 
         type_perjury = max(type_perjury)) %>% 
  select(country_case, ccode_case, vettingID, yearStart, yearEnd, individual_conduct, 
         type_dismissal, type_ban, type_declassification, type_perjury) %>% 
  distinct() %>% 
  group_by(country_case) %>% 
  mutate(count = n(), 
         yearStart = list(unlist(yearStart)),
         yearEnd = list(unlist(yearEnd)),
         individual_conduct = sum(individual_conduct), 
         type_dismissal = sum(type_dismissal), 
         type_ban = sum(type_ban), 
         type_declassification = sum(type_declassification), 
         type_perjury = sum(type_perjury)
  ) %>% 
  ungroup() %>% 
  select(country_case, ccode_case, count, yearStart, yearEnd, individual_conduct, 
         type_dismissal, type_ban, type_declassification, type_perjury) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(yearStart = list(yearStart[!is.na(yearStart)]),
         yearEnd = list(yearEnd[!is.na(yearEnd)]),
         text = "",
         text = list(c(text,
                       if(length(yearStart) == count & 
                          length(yearEnd) == count)
                         paste(country_case, 
                               " had ", 
                               count, 
                               " vetting ", 
                               ifelse(count == 1, "policy ", "policies "), 
                               ifelse(min(yearStart) == max(yearEnd),
                                      paste("in", max(yearEnd)),
                                      paste("between", min(yearStart), "and", max(yearEnd))
                               ),
                               ".", sep = "") %>%
                         n_transform()
         )),
         text = list(c(text,
                       if(length(yearStart) == count & 
                          length(yearEnd) > 0 & 
                          length(yearEnd) < count)
                         paste(country_case, 
                               " had ", 
                               count, 
                               " vetting ", 
                               ifelse(count == 1, "policy,", "policies,"), 
                               " starting in ", 
                               min(yearStart), 
                               "; ", 
                               length(yearEnd), 
                               " of these ended by ", 
                               max(yearEnd), 
                               ".", sep = "") %>%
                         n_transform()
         )),
         text = list(c(text,
                       if(length(yearStart) == count & 
                          length(yearEnd) == 0)
                         paste(country_case, 
                               " had ", 
                               count, 
                               " vetting ", 
                               ifelse(count == 1, "policy,", "policies,"), 
                               " starting in ", 
                               min(yearStart), 
                               "; TJET found no information on whether or when the ",
                               ifelse(count == 1, "policy", "policies"), 
                               " ended.",
                               sep = "") %>%
                         n_transform()
         )),
         text = list(c(text,
                       if(individual_conduct == count)
                         paste(ifelse(individual_conduct == 1, "This policy", "These policies"),
                               "provided sanctions based on past individual conduct.") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         text = list(c(text,
                       if(individual_conduct > 0 & individual_conduct < count)
                         paste(individual_conduct,
                               ifelse(individual_conduct == 1, "policy", "policies"),
                               "provided sanctions based on past individual conduct.") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         # don't use next
         # text = list(c(text,
         #               if(type_dismissal == count &
         #                  type_dismissal == type_ban)
         #                 paste(ifelse(type_dismissal == 1, "This policy", "These policies"),
         #                       "prescribed both dismissals from current employment and bans from holding future office.") %>%
         #                 n_transform() %>%
         #                 str_to_sentence()
         # )),
         text = list(c(text,
                       if(type_dismissal > 0 &
                          # type_dismissal != count &
                          type_dismissal == type_ban)
                         paste(type_dismissal,
                               ifelse(type_dismissal == 1, "policy", "policies"),
                               "prescribed both dismissals from current employment and bans from holding future office.") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         text = list(c(text,
                       if(type_dismissal > 0 &
                          type_dismissal != type_ban)
                         paste(type_dismissal,
                               ifelse(type_dismissal == 1, "policy", "policies"),
                               "prescribed dismissals from current employment.") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         text = list(c(text,
                       if(type_ban > 0 &
                          type_dismissal != type_ban)
                         paste(type_ban,
                               ifelse(type_ban == 1, "policy", "policies"),
                               "prescribed bans from holding future office.") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         text = list(c(text,
                       if(type_declassification > 0)
                         paste(type_declassification,
                               ifelse(type_declassification == 1, "policy", "policies"),
                               "aimed to declassify the records of former state security agents.") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         text = list(c(text,
                       if(type_perjury > 0)
                         paste(type_perjury,
                               ifelse(type_perjury == 1, "policy", "policies"),
                               "included legal consequences for non-disclosure of relevant past activities.") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         text = str_flatten(text, " ") %>% 
           str_trim()
  ) %>% 
  ungroup() %>% 
  select(country_case, ccode_case, text)

### print 10 focus country profiles to Quarto files 

df <- db[["Countries"]] %>%
  filter(include) %>%
  select(country, country_case, ccode, ccode_case, ccode_ksg, beg, end, tjet_focus, 
         txt_intro, txt_regime, txt_conflict, txt_TJ) %>%
  arrange(country)

dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/focus/")

df %>% 
  filter(tjet_focus == 1 | country == "Uganda") %>%  
  select(country) %>%
  unlist(use.names = FALSE) %>%  
  map(., function(ctry) {
    temp <- df %>% 
      filter(country == ctry) %>%
      mutate(txt_intro = str_replace_all(str_trim(txt_intro), "\n", "\n\n"),
             txt_regime = str_replace_all(str_trim(txt_regime), "\n", "\n\n"),
             txt_conflict = str_replace_all(str_trim(txt_conflict), "\n", "\n\n"),
             txt_TJ = str_replace_all(str_trim(txt_TJ), "\n", "\n\n")) %>%
      select(txt_intro, txt_regime, txt_conflict, txt_TJ) %>% 
      unlist()
    paste("---\ntitle: ", ctry, "\nformat: docx\n---", 
          "\n\n## Introduction\n\n", 
          temp[["txt_intro"]], 
          "\n\n## Regime Background\n\n", 
          temp[["txt_regime"]], 
          "\n\n## Conflict Background\n\n", 
          temp[["txt_conflict"]], 
          "\n\n## Transitional Justice\n\n", 
          temp[["txt_TJ"]], 
          sep = "") %>% 
      write_file(., file = paste("~/Dropbox/TJLab/TimoDataWork/country_profiles/focus/", ctry, ".qmd", sep = ""))
    invisible()
  })

### for integrating into Airtable

autotxt <- df %>%
  select(country_case, ccode_case) %>%
  left_join(autotxt[["summary"]] %>%
              rename("summary" = "text"),
            by = c("country_case", "ccode_case")) %>%
  mutate(summary = ifelse(is.na(summary),
                          paste("TJET has found no information on transitional justice in ",
                                country_case, ".", sep = ""),
                          summary) ) %>%
  left_join(autotxt[["Transitions"]] %>%
              rename("regime" = "text"),
            by = c("country_case", "ccode_case")) %>%
  mutate(regime = ifelse(is.na(regime),
                         paste("TJET records no democratic transitions in",
                               country_case, 
                               "between 1970 and 2020."),
                         regime) ) %>%
  left_join(autotxt[["Conflicts"]] %>%
              rename("conflict" = "text"),
            by = c("country_case", "ccode_case")) %>%
  mutate(conflict = ifelse(is.na(conflict),
                           paste("Based on the Uppsala Conflict Data Program, TJET records no episodes of violent intrastate conflict in",
                                 country_case,
                                 "between 1970 and 2020."),
                           conflict) ) %>%
  left_join(autotxt[["Amnesties"]] %>%
              rename("amnesties" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["Domestic_cy"]] %>%
              rename("domestic" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["Foreign"]] %>%
              rename("foreign" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["Intl"]] %>%
              rename("intl" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["ICC"]] %>%
              rename("intl2" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["ICCaccused"]] %>%
              rename("intl3" = "text"),
            by = c("country_case", "ccode_case")) %>%
  rowwise() %>%
  mutate(intl = str_flatten(c(intl, intl2, intl3), collapse = " ", na.rm = TRUE), 
         intl = ifelse(intl == "", NA, intl) ) %>%
  ungroup() %>%
  select(-intl2, -intl3) %>%
  left_join(autotxt[["Reparations"]] %>%
              rename("reparations" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["TruthCommissions"]] %>%
              rename("tcs" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["Vettings"]] %>%
              rename("vetting" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["UNinvestigations"]] %>%
              rename("un" = "text"),
            by = c("country_case", "ccode_case"))
# write_csv(file = "~/Desktop/temp.csv", na = "")

### auto-text fields check 
### 'auto_' fields are generated here and need to be transfered to Airtable
### 'txt_' fields are manually adjusted in Airtable 
check_vars <- paste("chk_", c("summary", "regime", "conflict", "amnesties", "domestic", "foreign", "intl", "reparations", "tcs", "vetting", "un"), sep = "")
db[["Countries"]] %>% 
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

### print all country profiles with auto summaries to Quarto files 

dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/")
dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/original/")
# dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/edits/")
dir("~/Dropbox/TJLab/TimoDataWork/country_profiles")

map(df$country_case, function(ctry) {
  
  df1 <- df %>%
    filter(country_case == ctry) %>%
    mutate(txt_intro = str_replace_all(str_trim(txt_intro), "\n", "\n\n"),
           txt_regime = str_replace_all(str_trim(txt_regime), "\n", "\n\n"),
           txt_conflict = str_replace_all(str_trim(txt_conflict), "\n", "\n\n"),
           txt_TJ = str_replace_all(str_trim(txt_TJ), "\n", "\n\n")) %>%
    select(txt_intro, txt_regime, txt_conflict, txt_TJ) %>%
    unlist()
  
  new <- autotxt %>% 
    filter(country_case == ctry) %>%
    select(-country_case, -ccode_case) %>%
    unlist()
  
  # new <- map(autotxt, function(df2) {
  #   df2 %>%
  #     filter(country_case == ctry) %>%
  #     select(text) %>%
  #     unlist(use.names = FALSE)
  # })
  
  paste("---\ntitle: ", ctry, "\nformat: docx\n---\n\n",
        new[["summary"]],
        "\n\n## Country Background", 
        "\n\n### Democratic Transition\n\n",
        new[["regime"]], "\n\n", 
        df1[["txt_regime"]],
        "\n\n### Violent Conflict\n\n",
        new[["conflict"]],
        df1[["txt_conflict"]],
        "\n\n## Transitional Justice\n\n",
        df1[["txt_intro"]], "\n\n",
        df1[["txt_TJ"]], "\n\n",
        ifelse(!is.na(new[["amnesties"]]),
               paste("### Amnesties\n\n", new[["amnesties"]], "\n\n", sep = ""),
               ""),
        ifelse(!is.na(new[["domestic"]]),
               paste("### Domestic Trials\n\n", new[["domestic"]], "\n\n", sep = ""),
               ""),
        ifelse(!is.na(new[["intl"]]),
               paste("### International Trials\n\n", new[["intl"]], "\n\n", sep = ""),
               ""),
        ifelse(!is.na(new[["foreign"]]),
               paste("### Foreign Trials\n\n", new[["foreign"]], "\n\n", sep = ""),
               ""),
        ifelse(!is.na(new[["reparations"]]),
               paste("### Reparations\n\n", new[["reparations"]], "\n\n", sep = ""),
               ""),
        ifelse(!is.na(new[["tcs"]]),
               paste("### Truth Commissions\n\n", new[["tcs"]], "\n\n", sep = ""),
               ""),
        ifelse(!is.na(new[["un"]]),
               paste("### UN Investigations\n\n", new[["un"]], "\n\n", sep = ""),
               ""),
        ifelse(!is.na(new[["vetting"]]),
               paste("### Vetting\n\n", new[["vetting"]], "\n\n", sep = ""),
               ""),
        sep = "") %>% 
    write_file(., file = paste("~/Dropbox/TJLab/TimoDataWork/country_profiles/original/", ctry, ".qmd", sep = ""))
  
  invisible()
})

# file.copy(from = list.files("~/Dropbox/TJLab/TimoDataWork/country_profiles/original", full.names = TRUE),
#           to = "~/Dropbox/TJLab/TimoDataWork/country_profiles/edits/",
#           overwrite = FALSE, recursive = TRUE, copy.mode = FALSE)
