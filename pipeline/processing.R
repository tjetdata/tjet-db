### packages
require(tidyverse)

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
      ## should change this later to selecting only valid
      filter(invalid != 1) # %>% select(-invalid)
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
# str(db, 2)
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
                         "lastSentencingArrangement")), keep_empty = TRUE)

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
                                  str_sub(caseDescription, 1, charnum-1), 
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
  select(transitionID, ccode, trans_year_begin, 
         nsupport, sources, p5_year, ert_year, bmr_year)

### need to filter out non-HRs policies
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
  rename(year = year_ongoing) %>% 
  filter(year > 1969 & year < 2023) %>% 
  mutate(
    ccode_cow = ifelse(ccode_cow == 860 & year == 1999, 850, ccode_cow),
    ccode_cow = ifelse(ccode_cow == 541 & year == 1973, 235, ccode_cow)
  ) %>% 
  select(-source) %>%  
  left_join(db[["MegaBase"]][["Investigations"]] %>% 
              select(name, source) %>% 
              unnest(source) %>% 
              group_by(name) %>% 
              reframe(source = str_flatten(source, "; ")), 
            by = "name") %>%
  arrange(name, year) %>%
  mutate(name = str_split(name, " & ") ) %>% 
  unnest(name, keep_empty = TRUE) %>% 
  group_by(name, ccode_cow) %>%
  mutate(beg = min(year), 
         end = max(year)) %>% 
  ungroup() %>% 
  select(ccode_cow, beg, end, name, secgen, unsc, cohr, unga, ohchr, hrc,
         uninv_intlpros, uninv_evcoll, uninv_dompros, source) %>%
  distinct() %>% 
  mutate(country = str_split_i(name, "_", 1),
         country = str_remove(country, as.character(beg)),
         country = str_replace(country, "CAR", "Central African Republic"),
         country = str_replace(country, "DRC", "Democratic Republic of the Congo"),
         country = str_replace(country, "DPRK", "DPRK (North Korea)"),
         country = str_replace(country, "Lebanon2005", "Lebanon"),
         mandate = str_split_i(name, "_", 2), 
         mandate = ifelse(is.na(mandate) &ccode_cow == 93 & unsc == 1, "UNSC", mandate), 
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
         mandate = str_replace(mandate, "CHR ", "UN Commission on Human Rights "),
         goals = case_when(uninv_intlpros == 1 & uninv_evcoll == 1 & uninv_dompros == 1 ~ "support international prosecutions; collect evidence; encourage domestic prosecutions", 
                           uninv_intlpros == 1 & uninv_evcoll == 1 ~ "support international prosecutions; collect evidence",
                           uninv_intlpros == 1 & uninv_dompros == 1 ~ "support international prosecutions; encourage domestic prosecutions", 
                           uninv_evcoll == 1 & uninv_dompros == 1 ~ "collect evidence; encourage domestic prosecutions", 
                           uninv_intlpros == 1 ~ "support international prosecutions", 
                           uninv_evcoll == 1 ~ "collect evidence",
                           uninv_dompros == 1 ~ "encourage domestic prosecutions") 
         ) %>% 
  select(country, ccode_cow, beg, end, mandate, goals, source)

db[["MegaBase"]][["Investigations"]] <-
  db[["MegaBase"]][["Investigations"]] %>% 
  rename(year = year_ongoing) %>% 
  filter(year > 1969 & year < 2023) %>% 
  mutate(
         ccode_cow = ifelse(ccode_cow == 860 & year == 1999, 850, ccode_cow),
         ccode_cow = ifelse(ccode_cow == 541 & year == 1973, 235, ccode_cow), 
         uninv = 1) %>% 
    select(ccode_cow, year, uninv, 
           uninv_dompros, uninv_evcoll, uninv_intlpros) 

### compare numbers of records again
### last column is 
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
rm(select, checkbox_to_binary, dim_drop, dim_last, dim_now, dim_orig, 
   crimes, victims, multi_selects) 

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
                           region %in% c("Asia", "Africa"), "MENA", region), 
         txt_intro = str_trim(txt_intro), 
         txt_regime = str_trim(txt_regime), 
         txt_conflict = str_trim(txt_conflict), 
         txt_TJ = str_trim(txt_TJ) ) %>% 
  select(country, country_fr, ccode, ccode_case, ccode_ksg, m49, isoa3, 
         country_id_vdem, beg, end, micro_ksg, region, region_sub_un, region_wb, 
         focus, factsheet, txt_intro, txt_regime, txt_conflict, txt_TJ) %>% 
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
  select(country, country_case, country_fr, country_case_fr, ccode, ccode_case, ccode_ksg, m49, isoa3, 
         country_id_vdem, beg, end, micro_ksg, region, region_sub_un, region_wb, 
         tjet_focus, factsheet, txt_intro, txt_regime, txt_conflict, txt_TJ) 

## ccode_case / country_case for the countries on the 2020 map that data are matched to
countrylist %>%
  filter(country != country_case | ccode != ccode_case |
           ccode %in% c(255, 260, 265, 315, 316, 345, 365, 678, 679, 680, 816, 817)) %>%
  select(country_case, ccode_case, country, ccode, beg, end) %>%
  arrange(country_case, end) %>% I
  # write_csv("~/Desktop/temp.csv")

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
amnesties <- db$Amnesties %>% 
  arrange(ccode, amnestyYear) %>%  
  group_by(ccode, amnestyYear) %>%
  mutate(amnesties = n(),
         SGBV = max(SGBV)) %>% 
  ungroup() %>% 
  select(ccode, amnestyYear, amnesties, SGBV) %>% 
  distinct() %>% 
  rename("year" = "amnestyYear", 
         "amnesties_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

### preparing reparations list for merging into country-year dataset
reparations <- db$Reparations %>%
  arrange(ccode, yearCreated) %>%  
  mutate(SGBV = ifelse(harmsSexualViolence == 1 | 
                         genderCrimes == "yes" | 
                         lgbtqCrimes == "yes", 1, 0) ) %>%    
  group_by(ccode, yearCreated) %>%
  mutate(reparations = n(),
         SGBV = max(SGBV)) %>% 
  ungroup() %>% 
  select(ccode, yearCreated, reparations, SGBV) %>% 
  distinct() %>% 
  rename("year" = "yearCreated",
         "reparations_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

### preparing TCs list for merging into country-year dataset
tcs <- db$TruthCommissions %>%
  arrange(ccode, yearPassed) %>%  
  group_by(ccode, yearPassed) %>%
  mutate(tcs = n(),
         SGBV = max(SGBV)) %>% 
  ungroup() %>% 
  select(ccode, ccode, yearPassed, tcs, SGBV) %>% 
  distinct() %>% 
  rename("year" = "yearPassed",
         "tcs_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

### preparing vettings list for merging into country-year dataset
vettings <- db$Vettings %>%
  # select(vettingID, alterationOf, ccode, yearStart, yearEnd) %>% 
  filter(is.na(alterationOf)) %>% 
  arrange(ccode, yearStart) %>%  
  group_by(ccode, yearStart) %>%  
  mutate(vettings = n()) %>% 
  ungroup() %>% 
  select(ccode, yearStart, vettings) %>% 
  distinct() %>%
  rename("year" = "yearStart") %>% 
  filter(year >= 1970 & year <= 2020)

### preparing trials list for merging into country-year dataset
trials <- db$Trials %>%
  rename(ccode = "ccode_Accused") %>% 
  arrange(ccode, yearStart) %>%
  mutate(SGBV = ifelse(rape_Accused == 1 | 
                         sexualViolence_Accused == 1 | 
                         otherSGBV_Accused == 1, 1, 0) ) %>% 
  select(ccode, trialType, yearStart, SGBV) %>% 
  filter(yearStart >= 1970 & yearStart <= 2020)

### subsetting and coding domestic trials total count measures for website
domestic <- trials %>% 
  filter(trialType %in% c("domestic", "don't know")) %>% 
  group_by(ccode, yearStart) %>%
  mutate(trials_domestic = n(),
         trials_domestic_SGBV = max(SGBV)) %>%
  ungroup() %>%
  select(ccode, yearStart, trials_domestic, trials_domestic_SGBV) %>%
  rename("year" = "yearStart") %>% 
  distinct() 

### subsetting and coding foreign trials total count measures for website
foreign <- trials %>% 
  filter(trialType == "foreign") %>% 
  group_by(ccode, yearStart) %>%
  mutate(trials_foreign = n(),
         trials_foreign_SGBV = max(SGBV)) %>%
  ungroup() %>%
  select(ccode, yearStart, trials_foreign, trials_foreign_SGBV) %>%
  rename("year" = "yearStart") %>% 
  distinct() 

### subsetting and coding intl trials total count measures for website
intl <- trials %>% 
  filter(trialType %in% c("international", "international (hybrid)")) %>% 
  group_by(ccode, yearStart) %>%
  mutate(trials_intl = n(),
         trials_intl_SGBV = max(SGBV)) %>%
  ungroup() %>%
  select(ccode, yearStart, trials_intl, trials_intl_SGBV) %>%
  rename("year" = "yearStart") %>% 
  distinct() 

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
db$Countries <- countrylist %>% 
  mutate(beg = ifelse(beg < 1970, 1970, beg)) %>% 
  select(country, country_case, country_fr, ccode, ccode_case, ccode_ksg, beg, 
         end, m49, isoa3, micro_ksg, region, region_sub_un, region_wb, 
         tjet_focus, txt_intro, txt_regime, txt_conflict, txt_TJ)

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

### conflict dyads lookup table for database
db$ConflictDyads <- read_csv(here::here("conflicts", "confl_dyads.csv"), 
                             show_col_types = FALSE) %>% tibble()
attr(db$ConflictDyads, "spec") <- NULL
attr(db$ConflictDyads, "problems") <- NULL
# attributes(db$ConflictDyads) 

db[["SurveysMeta"]] <- db[["SurveysMeta"]] %>%
  unnest(country) %>% 
  rename(airtable_record_id = country) %>% 
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
rm(countrylist, translist, confllist, amnesties, reparations, tcs, vettings, 
   trials, domestic, intl, foreign) 

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
# str(multies, 2)

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

db[["ICC"]] <-
  db[["ICC"]] %>%  
  select(country, ccode_cow, 
         ICC_referral, ICC_prelim_exam, ICC_prelimEnd, ICC_investigation)

db[["ICCaccused"]] <- 
  db[["Accused"]] %>% 
  filter(!is.na(ICC_investigation)) %>% 
  left_join(db[["Trials"]] %>% 
              select(trialID, ccode_Crime), 
            by = "trialID") %>% 
  select(trialID, accusedID, ccode_Crime, nameOrDesc, ICC_arrest_warrant, ICC_arrestAppear, 
         ICC_confirm_charges, ICC_proceedings, ICC_withdrawnDismissed) %>% 
  mutate(ccode_Crime = as.integer(ccode_Crime)) %>% 
  arrange(ccode_Crime, ICC_arrest_warrant)

### helpers for CY measures
source("functions/AmnestyMeasure.R")
source("functions/ReparationMeasures.R")
source("functions/TCgoals.R")
source("functions/TCmeasure.R")
source("functions/TrialsMeasure.R")
source("functions/VettingMeasures.R")

sample_cy <- c(
  glo = "global", ### all, all the time, i.e. full dataset
  dtr = "democratic transition", ### binary, from first transition year
  aco = "all conflicts", ### binary, from first conflict year
  dco = "during conflict", ### binary, when conflict active
  pco = "post-conflict") ### binary, after active conflict ended

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
first <- c("country", "country_case", "year", "ccode_cow", "ccode_ksg", "m49", 
           "isoa3", "country_id_vdem", "region", "subregion", "intregion", 
           "region_wb", "micro_ksg")
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
  ### the creation of Archigos variables could be moved to cy-data
  mutate(ruler_exit = ifelse(year(lead_enddate_beg) == year, 1, 0), 
         ruler_days = case_when(
           year(lead_enddate_beg) != year ~ 
             as.integer(as_date(paste(year, 12, 31, sep = "-")) - lead_startdate_beg),
           year(lead_enddate_beg) == year ~ 
             as.integer(lead_enddate_beg - lead_startdate_beg)) 
         ) %>%
  left_join(db[["CountryYears"]] %>%
               select(!any_of(c("country", "cyID", "country_case", "ccode_case", 
                                "country_label", "beg", "end", "region", "tjet_focus", 
                                "reg_democ", "reg_autoc", "reg_trans", "conflict"))) %>%
               rename(ccode_cow = ccode), 
             by = c("ccode_cow", "ccode_ksg", "country_id_vdem", "year")) %>% # losing Slovenia 1991?
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
              select(ccode_Crime, ICC_arrest_warrant) %>%
              filter(!is.na(ICC_arrest_warrant)) %>% 
              mutate(icc_action = 1) %>%
              group_by(ccode_Crime, ICC_arrest_warrant) %>%  
              reframe(icc_action = sum(icc_action)),
            by = c("ccode_cow" = "ccode_Crime", "year" = "ICC_arrest_warrant")) %>% 
  rename(ICC_arrest_warrant = icc_action) %>% 
  full_join(db[["ICCaccused"]] %>% 
              select(ccode_Crime, ICC_arrestAppear) %>%
              filter(!is.na(ICC_arrestAppear)) %>%
              mutate(icc_action = 1) %>%
              group_by(ccode_Crime, ICC_arrestAppear) %>%  
              reframe(icc_action = sum(icc_action)),
            by = c("ccode_cow" = "ccode_Crime", "year" = "ICC_arrestAppear")) %>% 
  rename(ICC_arrestAppear = icc_action) %>% 
  full_join(db[["ICCaccused"]] %>% 
              select(ccode_Crime, ICC_confirm_charges) %>%
              filter(!is.na(ICC_confirm_charges)) %>%
              mutate(icc_action = 1) %>%
              group_by(ccode_Crime, ICC_confirm_charges) %>%  
              reframe(icc_action = sum(icc_action)),
            by = c("ccode_cow" = "ccode_Crime", "year" = "ICC_confirm_charges")) %>% 
  rename(ICC_confirm_charges = icc_action) %>% 
  full_join(db[["ICCaccused"]] %>% 
              select(ccode_Crime, ICC_proceedings) %>%
              filter(!is.na(ICC_proceedings)) %>%
              mutate(icc_action = 1) %>%
              group_by(ccode_Crime, ICC_proceedings) %>%  
              reframe(icc_action = sum(icc_action)),
            by = c("ccode_cow" = "ccode_Crime", "year" = "ICC_proceedings")) %>% 
  rename(ICC_proceedings = icc_action) %>% 
  full_join(db[["ICCaccused"]] %>% 
              select(ccode_Crime, ICC_withdrawnDismissed) %>%
              filter(!is.na(ICC_withdrawnDismissed)) %>%
              mutate(icc_action = 1) %>%
              group_by(ccode_Crime, ICC_withdrawnDismissed) %>%  
              reframe(icc_action = sum(icc_action)),
            by = c("ccode_cow" = "ccode_Crime", "year" = "ICC_withdrawnDismissed")) %>% 
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
    icc_sp = ifelse(is.na(icc_sp) & year != 2022, 0, icc_sp)
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
  left_join(db[["Investigations"]] %>% 
              select(ccode_cow, year, uninv, uninv_dompros, uninv_evcoll, uninv_intlpros) %>% 
              group_by(ccode_cow, year) %>% 
              reframe(across(all_of(c("uninv", "uninv_dompros", 
                                      "uninv_evcoll", "uninv_intlpros")), max)), 
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
#   select(country, ccode_cow, year)

### trials 

measures <- c(trs = "trials started", tro = "trials ongoing", 
              tfc = "trials with final convictions", cct = "conviction count", 
              crt = "conviction rate by all accused", sen = "sentence totals")

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "sta") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "int", nexus_vars = c("hrs", "con"), memb_opts = "opp") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "sta") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "for", nexus_vars = c("hrs", "con"), memb_opts = "opp") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "dom", nexus_vars = "hrs", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "dom", nexus_vars = "hrs", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "dom", nexus_vars = "hrs", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "dom", nexus_vars = "hrs", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "dom", nexus_vars = "hrs", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "dom", nexus_vars = "hrs", memb_opts = "sta") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "dom", nexus_vars = "hrs", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "dom", nexus_vars = "hrs", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "dom", nexus_vars = "hrs", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "dom", nexus_vars = "hrs", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "dom", nexus_vars = "hrs", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "dom", nexus_vars = "hrs", memb_opts = "opp") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "dom", nexus_vars = "hrs", excl_nexus_vars = c("dtj", "ctj"), memb_opts = "opp")

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "dom", nexus_vars = c("dtj", "con", "ctj"), excl_nexus_vars = "hrs", memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "dom", nexus_vars = c("dtj", "con", "ctj"), excl_nexus_vars = "hrs", memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "dom", nexus_vars = c("dtj", "con", "ctj"), excl_nexus_vars = "hrs", memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "dom", nexus_vars = c("dtj", "con", "ctj"), excl_nexus_vars = "hrs", memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "dom", nexus_vars = c("dtj", "con", "ctj"), excl_nexus_vars = "hrs", memb_opts = "opp")
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "dom", nexus_vars = c("dtj", "con", "ctj"), excl_nexus_vars = "hrs", memb_opts = "opp")

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta")
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "dom", nexus_vars = "dtj", memb_opts = "sta") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "sta", rank_opts = "hi") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "dom", nexus_vars = c("dtj", "ctj"), memb_opts = "opp") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "dom", nexus_vars = "con", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "dom", nexus_vars = "con", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "dom", nexus_vars = "con", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "dom", nexus_vars = "con", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "dom", nexus_vars = "con", memb_opts = "sta") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "dom", nexus_vars = "con", memb_opts = "sta") 

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "dom", nexus_vars = "con", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "dom", nexus_vars = "con", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "dom", nexus_vars = "con", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "dom", nexus_vars = "con", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "dom", nexus_vars = "con", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "dom", nexus_vars = "con", memb_opts = "opp") 

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
  select(-tcs_harms_binary)

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

### cleanup

first <- c(first, "dtr", "aco", "dco", "pco")
not <- c(not, "regime_sample", "reg_democ", "reg_autoc", "reg_trans", 
         "transition", "conflict", "conflict_active", 
         "sample_trans", "sample_confl", "sample_combi") 
then <- names(df)[!names(df) %in% c(first, not)]

df <- df %>% 
  select(all_of(first), all_of(then))

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

### downloads datasets 
# - include country IDs, transitions and our data
# - everything in our filters, but not other outcomes
# - variables that should not be in public CY downloads file?
#   - regime_sample, reg_democ, reg_autoc, reg_trans, conflict, transition

codebook <- db[["codebook"]] %>% 
  filter(tables == "tjet_cy") %>% 
  filter(colname != "lag_*")
names(df)[!names(df) %in% codebook$colname]
codebook$colname[!codebook$colname %in% names(df)]

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

db[["dl_tjet_cy"]] <- df %>%
  select(all_of(db[["dl_tjet_codebook"]]$colname)) %>% 
  filter(year >= 1970 & year <= 2020) %>% 
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

db[["Amnesties"]] <- db[["Amnesties"]] %>% 
  rename(ccode_cow = ccode) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_amnesties.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_amnesties.csv"), na = "")
db[["Reparations"]] <- db[["Reparations"]] %>%
  rename(ccode_cow = ccode) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_reparations.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_reparations.csv"), na = "")
db[["Trials"]] <- db[["Trials"]] %>% 
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
  rename(ccode_cow = ccode) %>% 
  mutate(tjet_version = timestamp) %>% 
  write_csv(here::here("tjet_datasets", "tjet_tcs.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_tcs.csv"), na = "")
db[["Vettings"]] <- db[["Vettings"]] %>% 
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
