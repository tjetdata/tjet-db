require(tidyverse)

load(here::here("data", "tjet.RData"), verbose = TRUE)
map(tjet, names)

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
  "Dyads" = "dyad_id")

exclude <- c("metadata", "select_options", "Experts", "NGOs", "Legal", 
             "ConflictDyadSpells", "UCDPcountries", "Mallinder", "Rozic", 
             "challenges", "comparison", 
             "amnesties_challenges", "vetting_comparison", 
             "BalkanInsight_comparison", "TJETmembers")

### check metadata table for non-existing fields
cat("These are fields listed in 'metadata' that are missing in the Airtable download.")
map(names(to_download), function(basename) {
  cat("Base:", basename, "\n\n")
  names(tables) <- tables <- to_download[[basename]][!to_download[[basename]] %in% exclude]
  map(tables, function(tab) {
    meta <- tjet[[basename]]$metadata %>%
      filter(table_name == tab) %>%
      select(field_name) %>%
      unlist(use.names = FALSE)
    meta[!meta %in% names(tjet[[basename]][[tab]])]
  }) %>% 
    print()
  return()
})

### prodDB tables

db <- map(names(to_download), function(basename) {
  names(select_tables) <- select_tables <- to_download[[basename]][!to_download[[basename]] %in% exclude]
  map(select_tables, function(tab_name) {
    select_vars <- tjet[[basename]]$metadata %>%
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
      select_vars[!(select_vars %in% names(tjet[[basename]][[tab_name]]))]
    tjet[[basename]][[tab_name]] %>%
      tibble() %>%
      ### adding empty fields as NA columns for now until they are recoded by RAs
      mutate(!!!setNames(rep(NA, length(missing_cols)), missing_cols)) %>%
      select(all_of(select_vars))
  })
})
names(db) <- names(to_download)
dim_orig <- map(db, function(dat) {
  map_vec(dat, nrow)
})

drop_invalids <- c("Amnesties", "Trials", "Accused", "TruthCommissions", "Reparations", "Vettings")
db <- map(names(to_download), function(basename) {
  drop_invalids <- drop_invalids[drop_invalids %in% names(db[[basename]])]
  base <- db[[basename]]
  base[drop_invalids] <- map(drop_invalids, function(tab_name) {
    base[[tab_name]] %>%
      ## will need to change this later to selecting only valids
      filter(invalid != 1) # %>% select(-invalid)
    
  })
  return(base)
})
names(db) <- names(to_download)

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
  names(select) <- select <- to_download[[basename]][!to_download[[basename]] %in% exclude]
  # db[[basename]][select] <- 
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

### dealing with multipleLookupValues (& multipleRecordLinks)
str(db, 2)
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

### ccodes for Crimes and Victims

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

db[["Prosecutions"]][["Trials"]] <- db[["Prosecutions"]][["Trials"]] %>%
  select(!all_of(c(crimes, victims)))

### multiselect fields

db[["MegaBase"]][["labels"]] <- tjet[["MegaBase"]][["select_options"]] %>%
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
  # tab_name = "Vettings"
  names(fields) <- fields <- tjet[["MegaBase"]][["metadata"]] %>%
    filter(incl_prod == 1 &
             incl_data == "transform: multiple" &
             table_name == tab_name) %>%
    select(field_name) %>%
    unlist(use.names = FALSE)
  map(fields, function(field) {
    # cat(field)
    # field = "other"
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

dim_now <- map(db, function(dat) {
  map_vec(dat, nrow)
})
cbind(orig = dim_orig[[1]], drop = dim_drop[[1]], now = dim_now[[1]][names(dim_orig[[1]])])
cbind(orig = dim_orig[[2]], drop = dim_drop[[2]], now = dim_now[[2]][names(dim_orig[[2]])])

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

db[["MegaBase"]][["Reparations"]] <- db[["MegaBase"]][["Reparations"]] %>%
  unnest_longer(ucdpConflictID, keep_empty = TRUE) %>%
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet[["MegaBase"]]$Conflicts %>% 
              select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id")

db[["Prosecutions"]][["Trials"]] <- db[["Prosecutions"]][["Trials"]] %>%
  unnest_longer(all_of(c("ucdpConflictID", "ucdpDyadID")), keep_empty = TRUE) %>%
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

db[["MegaBase"]][["Vettings"]] <- db[["MegaBase"]][["Vettings"]] %>%
  unnest_longer(all_of(c("ucdpConflictID", "ucdpDyadID")), keep_empty = TRUE) %>%
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

### this created an additional observation because relationship is actually one-to-many, so now doing this properly further below
# db[["MegaBase"]][["Amnesties"]] <- db[["MegaBase"]]$Amnesties %>%
#   unnest_longer(all_of(c("ucdpConflictID", "ucdpDyadID")), keep_empty = TRUE) %>%
#   rename(airtable_record_id = "ucdpConflictID") %>%
#   left_join(tjet[["MegaBase"]]$Conflicts %>% 
#               select(airtable_record_id, conflict_id),
#             by = "airtable_record_id") %>%
#   select(-airtable_record_id) %>%
#   rename(airtable_record_id = "ucdpDyadID") %>%
#   left_join(tjet[["MegaBase"]]$Dyads %>% 
#               select(airtable_record_id, dyad_id),
#             by = "airtable_record_id") %>%
#   select(-airtable_record_id) %>%
#   rename(ucdpConflictID = "conflict_id",
#          ucdpDyadID = "dyad_id")

### at the moment the trial-accused link is one-to-many, 
### but in theory, I could become many-to-many, 
### though there is no current plan to do so
# range(unlist(map(db$Accused$trialID, length)))

db[["Prosecutions"]][["Accused"]] <- db[["Prosecutions"]][["Accused"]] %>%
  unnest_longer(trialID, keep_empty = TRUE) %>%
  rename(airtable_record_id = "trialID") %>%
  left_join(tjet[["Prosecutions"]][["Trials"]] %>% 
              select(airtable_record_id, trialID),
            by = "airtable_record_id") %>%
  select(-airtable_record_id)

db[["Prosecutions"]][["CourtLevels"]] <- db[["Prosecutions"]][["CourtLevels"]] %>%
  unnest_longer(accusedID, keep_empty = TRUE) %>%
  rename(airtable_record_id = "accusedID") %>%
  left_join(tjet[["Prosecutions"]][["Accused"]] %>% 
              select(airtable_record_id, accusedID),
            by = "airtable_record_id") %>%
  select(-airtable_record_id)

## truth commissions and amnesties have one-to-many links

db[["MegaBase"]][["TruthCommissions_Conflicts"]] <- db[["MegaBase"]][["TruthCommissions"]] %>%
  select(truthcommissionID, ucdpConflictID) %>%
  unnest_longer(ucdpConflictID) %>%
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet[["MegaBase"]]$Conflicts %>%
              select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id") %>%
  drop_na()

db[["MegaBase"]][["TruthCommissions_Dyads"]] <- db[["MegaBase"]][["TruthCommissions"]] %>%
  select(truthcommissionID, ucdpDyadID) %>%
  unnest_longer(ucdpDyadID) %>%
  rename(airtable_record_id = "ucdpDyadID") %>%
  left_join(tjet[["MegaBase"]]$Dyads %>%
              select(airtable_record_id, dyad_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "dyad_id") %>%
  drop_na()

db[["MegaBase"]][["TruthCommissions"]] <- db[["MegaBase"]][["TruthCommissions"]] %>%
  select(-ucdpConflictID,-ucdpDyadID)

db[["MegaBase"]][["Amnesties_Conflicts"]] <- db[["MegaBase"]][["Amnesties"]] %>%
  select(amnestyID, ucdpConflictID) %>%
  unnest_longer(ucdpConflictID) %>%
  rename(airtable_record_id = "ucdpConflictID") %>%
  left_join(tjet[["MegaBase"]]$Conflicts %>%
              select(airtable_record_id, conflict_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "conflict_id") %>%
  drop_na()

db[["MegaBase"]][["Amnesties_Dyads"]] <- db[["MegaBase"]][["Amnesties"]] %>%
  select(amnestyID, ucdpDyadID) %>%
  unnest_longer(ucdpDyadID) %>%
  rename(airtable_record_id = "ucdpDyadID") %>%
  left_join(tjet[["MegaBase"]]$Dyads %>%
              select(airtable_record_id, dyad_id),
            by = "airtable_record_id") %>%
  select(-airtable_record_id) %>%
  rename(ucdpConflictID = "dyad_id") %>%
  drop_na()

db[["MegaBase"]][["Amnesties"]] <- db[["MegaBase"]][["Amnesties"]] %>%
  select(-ucdpConflictID,-ucdpDyadID)

## consistent ID field names

db[["MegaBase"]][["Conflicts"]] <- db[["MegaBase"]][["Conflicts"]] %>%
  rename(ucdpConflictID = "conflict_id")

db[["MegaBase"]][["Dyads"]] <- db[["MegaBase"]][["Dyads"]] %>%
  rename(ucdpConflictID = "conflict_id",
         ucdpDyadID = "dyad_id")

db[["Prosecutions"]][["Conflicts"]] <- db[["Prosecutions"]][["Conflicts"]] %>%
  rename(ucdpConflictID = "conflict_id")

db[["Prosecutions"]][["Dyads"]] <- db[["Prosecutions"]][["Dyads"]] %>%
  rename(ucdpConflictID = "conflict_id",
         ucdpDyadID = "dyad_id")

## other multi-select fields (lookup fields as list columns of length one)

db[["Prosecutions"]][["Accused"]] <- db[["Prosecutions"]][["Accused"]] %>% 
  unnest_longer(all_of(c("lastGuiltyYear", "lastVerdictYear", 
                         "lastVerdict", "lastSentencingTime", 
                         "lastSentencingArrangement")), keep_empty = TRUE) %>% 
  # filter(!(accusedID == 18133 & lastVerdict == "Guilty Overturned & Acquittal")) %>% # a temporary fixß
  distinct()

## need to figure out a better way to deal with the multi-select field 
## because this created duplicates 
# db[["Prosecutions"]][["Trials"]] <- db[["Prosecutions"]][["Trials"]] %>%
#   unnest_longer(all_of(c("oppositionType")), keep_empty = TRUE )
# db[["Prosecutions"]][["Trials"]] %>% 
#   group_by(trialID) %>%
#   mutate(n = n()) %>%
#   filter(n > 1) %>% select(trialID, oppositionType)

db[["Prosecutions"]][["Trials"]] <- db[["Prosecutions"]][["Trials"]] %>%
  # unnest_longer(all_of(c("oppositionType")), keep_empty = TRUE )
  mutate(oppositionType = str_c())

## other multi-select fields (lookup fields as list columns of length greater than one)

db[["Prosecutions"]][["Trials"]] <- db[["Prosecutions"]][["Trials"]] %>%
  mutate(membership = str_c(membership, sep =", "),
         membership = str_replace(membership, fixed("NA, "), ""),
         membership = str_replace(membership, fixed("NA"), ""),
         membership = str_replace(membership, fixed("c(\""), ""),
         membership = str_replace_all(membership, fixed("\")"), ""), 
         membership = str_replace_all(membership, fixed("\""), ""),
         membership = str_replace_all(membership, fixed("character(0)"), "")) 

db[["Prosecutions"]][["Trials_lastVerdict"]] <- db[["Prosecutions"]][["Trials"]] %>%
  select(trialID, lastVerdict) %>% 
  unnest_longer(lastVerdict, keep_empty = TRUE) %>% 
  filter(!is.na(lastVerdict)) 

db[["Prosecutions"]][["Trials_lastSentencingTime"]] <- db[["Prosecutions"]][["Trials"]] %>%
  select(trialID, lastSentencingTime) %>% 
  unnest_longer(lastSentencingTime, keep_empty = TRUE) %>% 
  filter(!is.na(lastSentencingTime)) 

db[["Prosecutions"]][["Trials"]] <- db[["Prosecutions"]][["Trials"]] %>%
  select(-lastVerdict, -lastSentencingTime)

## fixing missing Trials endYear

db[["Prosecutions"]][["Trials"]] <- db[["Prosecutions"]][["Trials"]] %>% 
  mutate(yearEnd = ifelse(is.na(yearEnd) & ongoing == 1, 2023, yearEnd), 
         yearEnd = ifelse(is.na(yearEnd) & ongoing == 0, yearStart, yearEnd)) 
  
## cleaning up transitions table; this will change once the transitions data are fully cleaned up

db[["MegaBase"]][["Transitions"]] <- db[["MegaBase"]]$Transitions %>%
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
         nsupport = p5 + bmr + ert,) %>% 
  rowwise() %>% 
  mutate(sources = str_flatten(c(
           case_when(!is.na(p5_year) ~ paste("Polity5 (", p5_year, ")", sep = "")),
           case_when(!is.na(bmr_year) ~ paste("BMR (", bmr_year, ")", sep = "")),
           case_when(!is.na(ert_year) ~ paste("VDem (", ert_year, ")", sep = ""))),
           collapse = " & ", na.rm = TRUE)) %>% 
  select(transitionID, ccode, trans_year_begin, nsupport, sources) %>% 
  ungroup()

### need to also filter non-HRs policies; check again amnesties, TCs, reparations, vettings

db[["Prosecutions"]][["Trials"]] <- db[["Prosecutions"]][["Trials"]] %>% 
  filter(generalOrSpecific == "event" & (IntraConfl == 1 | humanRights == 1 | HRs_charges > 0) )

db[["Prosecutions"]][["Accused"]] <- db[["Prosecutions"]][["Accused"]] %>% 
  filter(trialID %in% db[["Prosecutions"]][["Trials"]]$trialID)

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
db[["MegaBase"]][["Amnesties"]] <- db[["MegaBase"]][["Amnesties"]] %>% 
  filter(amnestyID %in% keep_amnesties)
rm(keep_amnesties)

### TO DO?
## - multi-select fields into dummies for data downloads 
##   - needs a consistent naming scheme

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

db <- c(db[["MegaBase"]], 
        db[["Prosecutions"]][c("Trials", "Accused", "CourtLevels", 
                               "Trials_Crimes", "Trials_Victims", 
                               "Trials_lastVerdict", 
                               "Trials_lastSentencingTime")])

rm(select, checkbox_to_binary, 
   dim_drop, dim_last, dim_now, dim_orig, 
   crimes, victims, multi_selects) 

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
         # txt_intro = iconv(txt_intro, from = "UTF-8", to ="latin1", sub = "byte"), 
         # txt_regime = iconv(txt_regime, from = "UTF-8", to ="latin1", sub = "byte"), 
         # txt_conflict = iconv(txt_conflict, from = "UTF-8", to ="latin1", sub = "byte"), 
         # txt_TJ = iconv(txt_TJ, from = "UTF-8", to ="latin1", sub = "byte"),
         txt_intro = str_squish(txt_intro), 
         txt_regime = str_squish(txt_regime), 
         txt_conflict = str_squish(txt_conflict), 
         txt_TJ = str_squish(txt_TJ) ) %>% 
  select(country, ccode, ccode_case, ccode_ksg, m49, country_id_vdem, 
         beg, end, micro_ksg, region, region_sub_un, region_wb, 
         focus, factsheet, txt_intro, txt_regime, txt_conflict, txt_TJ) %>% 
  rename("tjet_focus" = "focus") %>% 
  arrange(country)

translist <- read_csv("transitions/transitions_new_revised.csv") %>%
  # filter(!is.na(trans_year) & trans_year >= 1960) %>% 
  filter(country != "Serbia & Montenegro") %>% 
  mutate(
    # country = ifelse(country == "Democratic Republic of the Congo", "DR Congo", country),
    # country = ifelse(country == "Eswatini", "Swaziland (Eswatini)", country),
    # country = ifelse(country == "German Democratic Republic", "German Democratic Republic (East)", country),
    # country = ifelse(country == "North Yemen", "Yemen Arab Republic (North)", country),
    # country = ifelse(country == "Republic of the Congo", "Congo (Brazzaville)", country),
    # country = ifelse(country == "Republic of Vietnam", "Vietnam (Democratic Republic of)", country),
    # country = ifelse(country == "St. Vincent", "St. Vincent & the Grenadines", country),
    # country = ifelse(country == "South Yemen", "Yemen People's Republic (South)", country)
    # ccode = ifelse(country == "Serbia & Montenegro", 345, ccode)
  ) %>% 
  rename("country_trans" = "country") %>% 
  full_join(countrylist %>% select(country, ccode, beg, end), by = "ccode") %>%
  # full_join(countrylist %>% select(country, ccode, beg) , by = "country") %>%
  arrange(country, year) %>%
  filter(year >= 1965 & year >= beg & year <= end) %>% 
  # select(country, ccode, country_trans, beg, end, year, v2x_polyarchy) %>% filter(country != country_trans)  %>% print(n = Inf)
  mutate(transition = ifelse(is.na(trans_year), 0, 1), 
         dem_polity = ifelse(polity_p5 >= 6, 1, 0),
         dem_vdem = ifelse(str_detect(str_to_lower(v2x_regime_amb), "democracy"), 1, 0)) %>% 
  select(country, ccode, year, transition, 
         # p5_year, bmr_year, ert_year, trans_note, v2x_polyarchy,
         # dem_ep_start_year, dem_ep_end_year, v2x_regime_amb, polity_p5,
         dem_bmr, dem_polity, dem_vdem) %>% 
  group_by(ccode) %>% 
  mutate(dem_polity_min = min(dem_polity, na.rm = TRUE),
         dem_polity_max = max(dem_polity, na.rm = TRUE),
         dem_polity_max = ifelse(is.infinite(dem_polity_max), NA, dem_polity_max),
         dem_bmr_min = min(dem_bmr, na.rm = TRUE),
         dem_bmr_max = max(dem_bmr, na.rm = TRUE),
         dem_bmr_max = ifelse(is.infinite(dem_bmr_max), NA, dem_bmr_max),
         dem_vdem_min = min(dem_vdem, na.rm = TRUE),
         dem_vdem_max = max(dem_vdem, na.rm = TRUE),
         dem_vdem_max = ifelse(is.infinite(dem_vdem_max), NA, dem_vdem_max),
         sources = 3 - (is.na(dem_polity_max) + is.na(dem_bmr_max) + is.na(dem_vdem_max)),
         regime = max(transition, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(context_bmr = case_when(dem_bmr_min == 0 & dem_bmr_max == 0 ~ 0,
                                 dem_bmr_min == 1 & dem_bmr_max == 1 ~ 1),
         context_polity = case_when(dem_polity_min == 0 & dem_polity_max == 0 ~ 0,
                                    dem_polity_min == 1 & dem_polity_max == 1 ~ 1),
         context_vdem = case_when(dem_vdem_min == 0 & dem_vdem_max == 0 ~ 0,
                                  dem_vdem_min == 1 & dem_vdem_max == 1 ~ 1),
         context_dem = rowSums(across(all_of(c("context_bmr", "context_polity", "context_vdem"))), na.rm = TRUE),
         regime = case_when(regime == 1 ~ "transitional", 
                            regime == 0 & context_dem == 0 & sources > 0 ~ "autocratic",
                            regime == 0 & (context_dem == sources | context_dem > 1) ~ "democratic",
                            country == "India" ~ "democratic"),
         reg_democ = ifelse(regime == "democratic", 1, 0), 
         reg_autoc = ifelse(regime == "autocratic", 1, 0), 
         reg_trans = ifelse(regime == "transitional", 1, 0)
  ) %>% 
  select(country, ccode, year, transition, dem_bmr, dem_polity, dem_vdem, regime, reg_democ, reg_autoc, reg_trans)

confllist <- read_csv("conflicts/confl_dyads.csv") %>% 
  select(location, gwno_loc, ep_start_year, ep_end_year) %>% 
  rowwise() %>% 
  mutate(year = list(ep_start_year:ep_end_year)) %>% 
  ungroup() %>% 
  unnest_longer(year) %>% 
  select(-ep_start_year, -ep_end_year) %>% 
  distinct() %>% 
  mutate(conflict = 1) %>% 
  filter(year >= 1965 & year <= 2020)

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

reparations <- db$Reparations %>%
  left_join(countrylist %>% select(ccode, ccode_case) %>% distinct(), 
            by = "ccode") %>% 
  arrange(ccode_case, yearCreated) %>%  
  mutate(SGBV = ifelse(harmsSexualViolence == 1 | genderCrimes == "yes" | lgbtqCrimes == "yes", 1, 0) ) %>%    
  group_by(ccode_case, yearCreated) %>%
  mutate(reparations = n(),
         SGBV = max(SGBV)) %>% 
  ungroup() %>% 
  select(ccode_case, yearCreated, reparations, SGBV) %>% 
  distinct() %>% 
  rename("year" = "yearCreated",
         "reparations_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

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

trials <- db$Trials %>% 
  rename(ccode = "ccode_Accused") %>% 
  left_join(countrylist %>% select(ccode, ccode_case) %>% distinct(), 
            by = "ccode") %>% 
  arrange(ccode_case, yearStart) %>%  
  mutate(domestic = ifelse(str_detect(trialType, "domestic"), 1, 0), 
         SGBV = ifelse(rape_Accused == 1 | sexualViolence_Accused == 1 | otherSGBV_Accused == 1, 1, 0) ) %>% 
  select(ccode_case, yearStart, domestic, SGBV) 

other <- trials %>% 
  filter(domestic == 0) %>% 
  select(ccode_case, yearStart, SGBV) %>%
  group_by(ccode_case, yearStart) %>%
  mutate(trials_other = n(),
         SGBV = max(SGBV)) %>%
  ungroup() %>%
  select(ccode_case, yearStart, trials_other, SGBV) %>% 
  distinct() %>%
  rename("year" = "yearStart",
         "trials_other_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

domestic <- trials %>% 
  filter(domestic == 1) %>% 
  select(ccode_case, yearStart, SGBV) %>%
  group_by(ccode_case, yearStart) %>%
  mutate(trials_domestic = n(),
         SGBV = max(SGBV)) %>%
  ungroup() %>%
  select(ccode_case, yearStart, trials_domestic, SGBV) %>% 
  distinct() %>%
  rename("year" = "yearStart",
         "trials_domestic_SGBV" = "SGBV") %>% 
  filter(year >= 1970 & year <= 2020)

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
  select(cyID, country, year, ccode, ccode_case, ccode_ksg, tjet_focus, region, 
         regime, reg_democ, reg_autoc, reg_trans, conflict, transition, conflict_active) %>% 
  filter(year >= 1970) %>% 
  left_join(amnesties, by = c("ccode_case", "year") ) %>%  
  mutate(amnesties = ifelse(is.na(amnesties), 0, amnesties),
         amnesties_SGBV = ifelse(is.na(amnesties_SGBV), 0, amnesties_SGBV)) %>% 
  left_join(reparations, by = c("ccode_case", "year") ) %>%  
  mutate(reparations = ifelse(is.na(reparations), 0, reparations),
         reparations_SGBV = ifelse(is.na(reparations_SGBV), 0, reparations_SGBV)) %>% 
  left_join(tcs, by = c("ccode_case", "year") ) %>% 
  mutate(tcs = ifelse(is.na(tcs), 0, tcs),
         tcs_SGBV = ifelse(is.na(tcs_SGBV), 0, tcs_SGBV)) %>% 
  left_join(domestic, by = c("ccode_case", "year") ) %>% 
  mutate(trials_domestic = ifelse(is.na(trials_domestic), 0, trials_domestic),
         trials_domestic_SGBV = ifelse(is.na(trials_domestic_SGBV), 0, trials_domestic_SGBV)) %>% 
  left_join(other, by = c("ccode_case", "year") ) %>% 
  mutate(trials_other = ifelse(is.na(trials_other), 0, trials_other),
         trials_other_SGBV = ifelse(is.na(trials_other_SGBV), 0, trials_other_SGBV) ) # %>% write_csv("~/Desktop/temp.csv") 

db$Countries <- countrylist %>% 
  mutate(beg = ifelse(beg <= 1970, 1970, beg)) 

db$dictionary <- read_csv(here::here("pipeline", "dictionary.csv")) %>%
  tibble()

attr(db$dictionary, "spec") <- NULL
attr(db$dictionary, "problems") <- NULL
# attributes(db$dictionary) 

db$ConflictDyads <- read_csv(here::here("conflicts", "confl_dyads.csv")) %>%
  tibble()

attr(db$ConflictDyads, "spec") <- NULL
attr(db$ConflictDyads, "problems") <- NULL
# attributes(db$ConflictDyads) 

# str(db, 1)
save(db, file = here::here("data", "tjetdb.RData"))

rm(countrylist, translist, confllist, amnesties, reparations, tcs, trials, domestic, other) 
