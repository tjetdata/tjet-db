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
  "Investigations" = "pkey") # ccode_cow-year

### note that both bases have Countries, Transitions, Conflicts and Dyads tables
### but these should be the same
### ideally the two bases should be combined and using only one version each

### exclude these tables from the production database
exclude <- c("metadata", "select_options", "Experts", "NGOs", "Legal", 
             "ConflictDyadSpells", "UCDPcountries", "Mallinder", "Rozic", 
             "Challenges", "VettingComparison", "ICDB", "BIcomparison", 
             "TJETmembers", "SurveysMeta")

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
db <- map(names(to_download), function(basename) {
  names(select_tables) <- select_tables <- 
    to_download[[basename]][!to_download[[basename]] %in% exclude]
  map(select_tables, function(tab_name) {
    select_vars <- tjet[[basename]]$metadata %>%
      filter(incl_prod == 1 & 
               ### doing multi-select fields separately
               incl_data != "transform: multiple" &
               ### include these later if creating dummies
               table_name == tab_name) %>%
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
### (may not need this if we can get it with SQL queries)
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
  rename(ucdpConflictID = "dyad_id") %>%
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
  rename(ucdpConflictID = "dyad_id") %>%
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

### fixing missing Trials endYear
db[["Prosecutions"]][["Trials"]] <- 
  db[["Prosecutions"]][["Trials"]] %>% 
  mutate(yearEnd = ifelse(is.na(yearEnd) & ongoing == 1, 2023, yearEnd), 
         yearEnd = ifelse(is.na(yearEnd) & ongoing == 0 & CLs_final_year > 1970, CLs_final_year, yearEnd), 
         yearEnd = ifelse(is.na(yearEnd) & ongoing == 0, yearStart, yearEnd), ) 

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
         beg = ifelse(beg <= 1965, 1965, beg),
         beg = ifelse(country == "Slovenia", 1991, beg),
         beg = ifelse(country == "Andorra", 1993, beg),
         beg = ifelse(country == "Kiribati", 1997, beg),
         beg = ifelse(country == "Liechtenstein", 1990, beg),
         beg = ifelse(country == "Marshall Islands", 1991, beg),
         beg = ifelse(country == "Micronesia", 1992, beg),
         beg = ifelse(country == "Monaco", 1993, beg),
         end = ifelse(country == "Serbia & Montenegro", 2005, end),
         end = ifelse(country == "German Federal Republic (West)", 1989, end),
         end = ifelse(country == "Yemen Arab Republic (North)", 1989, end),
         region_sub_un = ifelse(is.na(intregion), subregion, intregion),
         txt_intro = str_squish(txt_intro), 
         txt_regime = str_squish(txt_regime), 
         txt_conflict = str_squish(txt_conflict), 
         txt_TJ = str_squish(txt_TJ) ) %>% 
  select(country, ccode, ccode_case, ccode_ksg, m49, country_id_vdem, 
         beg, end, micro_ksg, region, region_sub_un, region_wb, 
         focus, factsheet, txt_intro, txt_regime, txt_conflict, txt_TJ) %>% 
  rename("tjet_focus" = "focus") %>% 
  arrange(country)

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
  left_join(countrylist %>% select(ccode, ccode_case) %>% distinct(), 
            by = "ccode") %>% 
  arrange(ccode_case, amnestyYear) %>%  
  group_by(ccode_case, amnestyYear) %>%
  mutate(amnesties = n(),
         SGBV = max(SGBV)) %>% 
  ungroup() %>% 
  select(ccode_case, amnestyYear, amnesties, SGBV) %>% 
  distinct() %>% 
  rename("year" = "amnestyYear", 
         "amnesties_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

### preparing reparations list for merging into country-year dataset
reparations <- db$Reparations %>%
  left_join(countrylist %>% select(ccode, ccode_case) %>% distinct(), 
            by = "ccode") %>% 
  arrange(ccode_case, yearCreated) %>%  
  mutate(SGBV = ifelse(harmsSexualViolence == 1 | 
                         genderCrimes == "yes" | 
                         lgbtqCrimes == "yes", 1, 0) ) %>%    
  group_by(ccode_case, yearCreated) %>%
  mutate(reparations = n(),
         SGBV = max(SGBV)) %>% 
  ungroup() %>% 
  select(ccode_case, yearCreated, reparations, SGBV) %>% 
  distinct() %>% 
  rename("year" = "yearCreated",
         "reparations_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

### preparing TCs list for merging into country-year dataset
tcs <- db$TruthCommissions %>%
  left_join(countrylist %>% select(ccode, ccode_case) %>% distinct(), 
            by = "ccode") %>% 
  arrange(ccode_case, yearPassed) %>%  
  group_by(ccode_case, yearPassed) %>%
  mutate(tcs = n(),
         SGBV = max(SGBV)) %>% 
  ungroup() %>% 
  select(ccode_case, yearPassed, tcs, SGBV) %>% 
  distinct() %>% 
  rename("year" = "yearPassed",
         "tcs_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

### preparing vettings list for merging into country-year dataset
vettings <- db$Vettings %>%
  left_join(countrylist %>% select(ccode, ccode_case) %>% distinct(), 
            by = "ccode") %>% 
  # select(vettingID, alterationOf, ccode_case, yearStart, yearEnd) %>% 
  filter(is.na(alterationOf)) %>% 
  arrange(ccode_case, yearStart) %>%  
  group_by(ccode_case, yearStart) %>%  
  mutate(vettings = n()) %>% 
  ungroup() %>% 
  select(ccode_case, yearStart, vettings) %>% 
  distinct() %>%
  rename("year" = "yearStart") %>% 
  filter(year >= 1970 & year <= 2020)

### preparing trials list for merging into country-year dataset
trials <- db$Trials %>%
  rename(ccode = "ccode_Accused") %>% 
  left_join(countrylist %>% select(ccode, ccode_case) %>% distinct(), 
            by = "ccode") %>% 
  arrange(ccode_case, yearStart) %>%
  mutate(SGBV = ifelse(rape_Accused == 1 | 
                         sexualViolence_Accused == 1 | 
                         otherSGBV_Accused == 1, 1, 0) ) %>% 
  select(ccode_case, trialType, yearStart, SGBV) %>% 
  filter(yearStart >= 1970 & yearStart <= 2020)

### subsetting and coding domestic trials total count measures for website
domestic <- trials %>% 
  filter(trialType %in% c("domestic", "don't know")) %>% 
  group_by(ccode_case, yearStart) %>%
  mutate(trials_domestic = n(),
         trials_domestic_SGBV = max(SGBV)) %>%
  ungroup() %>%
  select(ccode_case, yearStart, trials_domestic, trials_domestic_SGBV) %>%
  rename("year" = "yearStart") %>% 
  distinct() 

### subsetting and coding foreign trials total count measures for website
foreign <- trials %>% 
  filter(trialType == "foreign") %>% 
  group_by(ccode_case, yearStart) %>%
  mutate(trials_foreign = n(),
         trials_foreign_SGBV = max(SGBV)) %>%
  ungroup() %>%
  select(ccode_case, yearStart, trials_foreign, trials_foreign_SGBV) %>%
  rename("year" = "yearStart") %>% 
  distinct() 

### subsetting and coding intl trials total count measures for website
intl <- trials %>% 
  filter(trialType %in% c("international", "international (hybrid)")) %>% 
  group_by(ccode_case, yearStart) %>%
  mutate(trials_intl = n(),
         trials_intl_SGBV = max(SGBV)) %>%
  ungroup() %>%
  select(ccode_case, yearStart, trials_intl, trials_intl_SGBV) %>%
  rename("year" = "yearStart") %>% 
  distinct() 

### preparing conflicts list for merging into country-year dataset
confllist <- read_csv("conflicts/confl_dyads.csv", 
                      show_col_types = FALSE) %>% 
  select(location, gwno_loc, ep_start_year, ep_end_year) %>% 
  rowwise() %>% 
  mutate(year = list(ep_start_year:ep_end_year)) %>% 
  ungroup() %>% 
  unnest_longer(year) %>% 
  select(-ep_start_year, -ep_end_year) %>% 
  distinct() %>% 
  mutate(conflict = 1) %>% 
  filter(year >= 1965 & year <= 2020)

### transitions dataset in country-year format

translist <- read_csv("transitions/transitions_new_revised.csv",
                      show_col_types = FALSE) %>% 
  mutate(country = ifelse(country == "Eswatini", "Swaziland (Eswatini)", country), 
         country = ifelse(country == "Republic of Vietnam", "Vietnam (Republic of / South)", country),
         country = ifelse(country == "St. Vincent", "St. Vincent & the Grenadines", country),
         country = ifelse(country == "South Yemen", "Yemen People's Republic (South)", country),
         country = ifelse(country == "North Yemen", "Yemen Arab Republic (North)", country),
         country = ifelse(country == "Yemen" & year < 1990, "Yemen Arab Republic (North)", country),
         country = ifelse(country == "Germany" & year < 1990, "German Federal Republic (West)", country),
         country = ifelse(country == "Russia" & year < 1992, "Soviet Union", country),
         country = ifelse(country == "Serbia" & year < 1992, "Yugoslavia", country),
         country = ifelse(country == "Serbia" & year %in% 1992:2005, "Serbia & Montenegro", country)
  ) %>% 
  full_join(countrylist %>% select(country, beg, end), by = "country") %>%
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
         ## India is the only ambiguous case by this algorithm, hence the last 
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
  select(cyID, country, year, ccode, ccode_case, ccode_ksg, 
         tjet_focus, region, regime_sample, reg_democ, reg_autoc, 
         reg_trans, conflict, transition, conflict_active) %>% 
  filter(year >= 1970) %>% 
  left_join(amnesties, by = c("ccode_case", "year") ) %>%  
  mutate(amnesties = ifelse(is.na(amnesties), 0, amnesties),
         amnesties_SGBV = ifelse(is.na(amnesties_SGBV), 
                                 0, amnesties_SGBV)) %>% 
  left_join(reparations, by = c("ccode_case", "year") ) %>%  
  mutate(reparations = ifelse(is.na(reparations), 
                              0, reparations),
         reparations_SGBV = ifelse(is.na(reparations_SGBV), 
                                   0, reparations_SGBV)) %>% 
  left_join(tcs, by = c("ccode_case", "year") ) %>% 
  mutate(tcs = ifelse(is.na(tcs), 0, tcs),
         tcs_SGBV = ifelse(is.na(tcs_SGBV), 
                           0, tcs_SGBV)) %>%
  left_join(vettings, by = c("ccode_case", "year") ) %>% 
  mutate(vettings = ifelse(is.na(vettings), 
                           0, vettings)) %>%
  left_join(domestic, by = c("ccode_case", "year") ) %>% 
  mutate(trials_domestic = ifelse(is.na(trials_domestic), 
                                  0, trials_domestic),
         trials_domestic_SGBV = ifelse(is.na(trials_domestic_SGBV), 
                                       0, trials_domestic_SGBV)) %>% 
  left_join(intl, by = c("ccode_case", "year") ) %>% 
  mutate(trials_intl = ifelse(is.na(trials_intl), 
                              0, trials_intl),
         trials_intl_SGBV = ifelse(is.na(trials_intl_SGBV), 
                                   0, trials_intl_SGBV)) %>% 
  left_join(foreign, by = c("ccode_case", "year") ) %>% 
  mutate(trials_foreign = ifelse(is.na(trials_foreign), 
                                 0, trials_foreign),
         trials_foreign_SGBV = ifelse(is.na(trials_foreign_SGBV), 
                                      0, trials_foreign_SGBV))

### countries table for database
db$Countries <- countrylist %>% 
  mutate(beg = ifelse(beg <= 1970, 1970, beg)) 

### data definition dictionary
db$dictionary <- read_csv(here::here("data", "dictionary.csv"), 
                          show_col_types = FALSE) %>%
  tibble()
attr(db$dictionary, "spec") <- NULL
attr(db$dictionary, "problems") <- NULL
# attributes(db$dictionary) 

### conflict dyads lookup table for database
db$ConflictDyads <- read_csv(here::here("conflicts", "confl_dyads.csv"), 
                             show_col_types = FALSE) %>%
  tibble()
attr(db$ConflictDyads, "spec") <- NULL
attr(db$ConflictDyads, "problems") <- NULL
# attributes(db$ConflictDyads) 

### checking data tables
# str(db, 1)

### saving database
save(db, file = here::here("data", "tjetdb.RData"))

### cleaning up workspace environment
rm(countrylist, translist, confllist, amnesties, 
   reparations, tcs, vettings, trials, domestic, intl, foreign) 
