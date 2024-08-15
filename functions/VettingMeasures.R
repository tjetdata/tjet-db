
## the approach to coding vetting measures is different from that of other measures 
## because the vetting measures were already constructed in Airtable 
## importantly, the data are only consistently coded for Eastern Europe and the Post-Soviet region

vet_ctry <- read_csv("data/vetting_countries.csv") %>%
  filter(include == 1) %>% 
  mutate(begin_year = as.integer(str_sub(begin_date, 1, 4)),
         end_year = as.integer(str_sub(end_date, 1, 4)), 
         country_new = str_replace(country, " & ", " and "), 
         country_new = ifelse(country_new == "Czech Republic", "Czechia", country_new), 
         country_new = ifelse(country_new == "Macedonia", "North Macedonia", country_new)) %>% 
  left_join(db[["Countries"]] %>% select(country, ccode, ccode_case), by = c("country_new" = "country")) %>% 
  select(country, ccode, ccode_case, begin_year, end_year, post_Soviet, vetting, coded) %>% 
  arrange(country)

vet_cols <- c(
  "policy_type", "type_dismissal", "type_ban", "type_declass", 
  "type_perjury", "groups_targeted", "grp_police", "grp_judiciary", "grp_public", 
  "grp_exe", "grp_legis", "grp_parties", "grp_military", "grp_other", 
  "inst_targeted", "inst_exe", "inst_legis", "inst_parties", "inst_judiciary", 
  "inst_police", "inst_public", "inst_military", "inst_other", 
  # "inst_education", "inst_business", 
  "conduct", "implementation", "public", "fairness", "ban_from_elected")

vet_incl <- vet_ctry %>% select(ccode) %>% 
  distinct() %>% 
  unlist(use.names = FALSE)

vet_spells <- db[["Vettings"]] %>% 
  filter(ccode %in% vet_incl) %>% 
  mutate(public = case_when(hearingsPublic == "public" ~ 1,
                            TRUE ~ 0), 
         conduct = case_when(str_detect(targetingWhy, "specific individual conduct") ~ 1,
                            TRUE ~ 0),
         yearEnd = ifelse(is.na(yearEnd) | yearEnd > 2020, 2020, yearEnd)) %>% 
  rowwise() %>%
  mutate(year = map2(yearStart, yearEnd, seq)) %>% 
  unnest(cols = c(year)) %>%
  select(ccode, year, implemented, fitsPostAutocraticTJ, fitsConflictTJ, 
         type_dismissal, type_ban, type_declassification, type_perjury, 
         policy_type, grp_exe, grp_legis, grp_judiciary, grp_public, grp_police, 
         grp_military, grp_parties, grp_other, groups_targeted, inst_exe, 
         inst_legis, inst_judiciary, inst_public, inst_police, inst_military, 
         inst_parties, inst_other, inst_targeted, sum_inst, ban_from_elected, 
         conduct, implementation, public, var_fairness, 
         # targetingWhy, targetingAffiliationRank, targetingPositionSought, 
         # targetingAffiliation, targetingPositionsRank, sanctions
         ) %>% 
  rename("type_declass" = "type_declassification", 
         "fairness" = "var_fairness") %>% 
  summarise(across(everything(), .fns = ~ max(.x, na.rm = TRUE)), .by = c(ccode, year)) %>%
  mutate(across(everything(), .fns = ~ ifelse(is.infinite(.x), NA, .x)) ) %>% 
  mutate(across(all_of(vet_cols), .fns = ~ ifelse(is.na(.x), 0, .x)) ) 

vet_ctry_incl <- vet_ctry %>% 
  select(ccode) %>% 
  distinct() %>% 
  unlist(use.names = FALSE)
rm(vet_ctry, vet_incl, vet_cols)

### to country-year (no longer needed, was part of analysis for briefing)
# new <- vet_spells %>%
#   mutate(type_both = type_ban + type_dismissal, 
#          type_both = ifelse(type_both > 0, 1, 0),
#          type_all = type_ban + type_dismissal + type_declass + type_perjury,
#          type_all = ifelse(type_all > 0, 1, 0),
#          parties = inst_parties + grp_parties,
#          parties = ifelse(parties > 0, 1, 0),
#          inst_security = inst_police + inst_military,
#          inst_security = ifelse(inst_security > 0, 1, 0),
#          inst_private = inst_other + inst_parties,
#          inst_private = ifelse(inst_private > 0, 1, 0),
#          inst_public_all = inst_exe + inst_legis + inst_security + inst_public + inst_judiciary,
#          inst_public_all = ifelse(inst_public_all > 0, 1, 0),
#          vetting_cat = case_when(policy_type == 0 & implementation == 0 ~ "none",
#                                      policy_type > 0 & implementation == 0 ~ "unimplemented",
#                                      policy_type > 0 & implementation == 1 ~ "implemented"),
#          vetting = case_when(vetting_cat == "none" ~ 0,
#                                  vetting_cat == "unimplemented" ~ 1,
#                                  vetting_cat == "implemented" ~ 2))

VettingMeasures <- function(cy = df, nexus_vars = "all") {
  ## options
  nexus <- c(all = "all", dtj = "fitsPostAutocraticTJ", ctj = "fitsConflictTJ") 

  ## input errors
  suffix <- "\n  or NULL if this aspect is not relevant to the new measure"
  error <- expression(
    stop("Missing or invalid argument for 'nexus_vars', select one or more of:", 
         "\n  ", paste(names(nexus), collapse = "; "), suffix) )
  if(sum(!nexus_vars %in% names(nexus)) > 0) eval(error)
  
  ## subsetting 
  vet <- vet_spells %>% 
    mutate(all = 1) %>% 
    filter(if_any(all_of(nexus[nexus_vars]), ~ . == 1)) %>%
    select(ccode, year, type_dismissal, type_ban, type_declass, type_perjury, 
           # policy_type, groups_targeted, inst_targeted, sum_inst, 
           ban_from_elected, conduct, implementation, public, fairness)

  non_na <- expr(ccode_cow %in% vet_ctry_incl & year %in% 1970:2020)
  vars <- c("type_dismissal", "vet_dismiss_created", "type_ban", 
            "vet_ban_created", "type_declass", "vet_declass_created", 
            "type_perjury", "vet_perjury_created", "ban_from_elected", 
            "conduct", "vet_conduct_created", "implementation", "public", "fairness")
  
  cy %>%
    left_join(vet, by = c("ccode_cow" = "ccode", "year" = "year")) %>%
    mutate(
      vet_dismiss_created = type_dismissal, 
      vet_ban_created = type_ban, 
      vet_declass_created = type_declass, 
      vet_perjury_created = type_perjury, 
      vet_conduct_created = conduct
    ) %>% 
    arrange(country_case, year) %>%
    group_by(country_case) %>%
    fill(type_dismissal, 
         type_ban, 
         type_declass, 
         type_perjury, 
         ban_from_elected, 
         conduct, 
         implementation, 
         public, 
         fairness, 
         .direction = "down") %>%
    ungroup() %>% 
    mutate(across(all_of(vars), 
                  ~ ifelse(eval(non_na) & is.na(.x), 0, ifelse(year > 2020, NA, .x)))) %>%
    rename("vet_dismiss" = "type_dismissal", 
           "vet_ban" = "type_ban", 
           "vet_declass" = "type_declass", 
           "vet_perjury" = "type_perjury", 
           "vet_ban_from_elected" = "ban_from_elected",
           "vet_conduct" = "conduct", 
           "vet_implemented" = "implementation", 
           "vet_public" = "public", 
           "vet_fairness" = "fairness") %>%  
    return()
}
