require(tidyverse)

load(here::here("data", "tjetdb.RData"), verbose = TRUE)

## FROM HERE > 

# icc <-
db[["Accused"]] %>% 
  select(accusedID, trialID, ICC_referral, ICC_prelim_exam, ICC_investigation, 
         ICC_arrest_warrant, ICC_arrestAppear, ICC_atLarge, 
         ICC_confirm_charges, ICC_proceedings, ICC_withdrawnDismissed) %>%
  distinct() %>% 
  left_join(db[["Trials"]] %>% 
              select(trialID, ccode_Accused, yearStart, legalSystem) %>%
              distinct(), 
            by = c("trialID")) 
  
  %>% 
  group_by(trialID) %>% 
  mutate(n_trialID = n()) %>%
  ungroup() %>% 
  group_by(accusedID) %>% 
  mutate(n_accusedID = n()) %>% 
  filter(n_trialID > 4)


db[["Accused"]][307, ]

db[["Trials"]][4786:4788, c("trialID", "ccode_Accused", "caseDescription", "yearStart", "legalSystem")]

%>% 
  filter(legalSystem == "ICC") %>% 
  left_join(db[["Countries"]] %>% 
              select(country, ccode) , 
            by = c("ccode_Accused" = "ccode")) %>% 
  rename(ccode = ccode_Accused) %>% 
  mutate(new = 0) %>% 
  select(country, ccode, new, ICC_referral, ICC_prelim_exam, ICC_investigation, 
         ICC_arrest_warrant, ICC_arrestAppear, 
         ICC_confirm_charges, ICC_proceedings, ICC_withdrawnDismissed) %>% 
  group_by(country) %>% 
  mutate(across(all_of(c("ICC_referral", "ICC_prelim_exam", "ICC_investigation", 
                         "ICC_arrest_warrant", "ICC_arrestAppear", 
                         "ICC_confirm_charges", "ICC_proceedings", 
                         "ICC_withdrawnDismissed")), 
                function(x) min(x, na.rm = TRUE ) ),
         across(all_of(c("ICC_referral", "ICC_prelim_exam", "ICC_investigation", 
                         "ICC_arrest_warrant", "ICC_arrestAppear", 
                         "ICC_confirm_charges", "ICC_proceedings", 
                         "ICC_withdrawnDismissed")), 
                function(x) ifelse(is.infinite(x), NA, x) )) %>% 
  distinct()

### new TJ variables 

trial_counts <- db[["Trials"]] %>% 
  mutate(HRs = ifelse(HRs_charges > 0 | humanRights == 1, 1, 0) ) %>%
  select(ccode_Accused, yearStart, HRs, fitsPostAutocraticTJ, fitsConflictTJ) %>%
  mutate(fitsBoth = ifelse(fitsPostAutocraticTJ + fitsConflictTJ > 0, 1, 0),
         HRsConfl = ifelse(HRs + fitsPostAutocraticTJ + fitsConflictTJ > 0, 1, 0)) %>%
  arrange(ccode_Accused, yearStart) %>% 
  group_by(ccode_Accused, yearStart) %>% 
  mutate(trials_HRs = sum(HRs) , 
         trials_PostAutocratic = sum(fitsPostAutocraticTJ), 
         trials_Conflict = sum(fitsConflictTJ), 
         trials_HRsConfl = sum(HRsConfl), 
         trials_unionFits = sum(fitsBoth) ) %>% 
  select(ccode_Accused, yearStart, trials_HRs, trials_PostAutocratic, trials_Conflict, trials_HRsConfl, trials_unionFits) %>% 
  distinct() %>% 
  rename(ccode = ccode_Accused, 
         year = yearStart) %>% 
  filter(year >= 1970 & year <= 2020) 

conviction_counts <- db[["Trials"]] %>% 
  mutate(HRs = ifelse(HRs_charges > 0 | humanRights == 1, 1, 0) ) %>%
  select(ccode_Accused, firstConvictionYear_min, HRs, fitsPostAutocraticTJ, fitsConflictTJ) %>%
  mutate(fitsBoth = ifelse(fitsPostAutocraticTJ + fitsConflictTJ > 0, 1, 0),
         firstConvictionYear_min = as.integer(firstConvictionYear_min)) %>%
  filter(!is.na(firstConvictionYear_min) ) %>% 
  arrange(ccode_Accused, firstConvictionYear_min) %>% 
  group_by(ccode_Accused, firstConvictionYear_min) %>% 
  mutate(convictions_HRs = sum(HRs) , 
         convictions_PostAutocratic = sum(fitsPostAutocraticTJ) , 
         convictions_Conflict = sum(fitsConflictTJ), 
         convictions_unionFits = sum(fitsBoth) ) %>% 
  select(ccode_Accused, firstConvictionYear_min, convictions_HRs, convictions_PostAutocratic, convictions_Conflict, convictions_unionFits) %>% 
  distinct() %>% 
  rename(ccode = ccode_Accused, 
         year = firstConvictionYear_min) %>% 
  filter(year >= 1970 & year <= 2020)

counts <- full_join(trial_counts, conviction_counts, by = c("ccode", "year")) %>% 
  mutate(ccode = ifelse(ccode == 679 & year < 1990, 678, ccode)) # Yemen YAR

# GD new measures: for foreign & intl trials (separately) 
# –	count of trials (by countryAccused & startYear)
# –	count of trials (by countryAccused & ongoing OR >= startYear & <=endYear; for every year check those)
# –	count of convictions (by countryAccused & conviction year OR end year OR start year) 
# –	count of all convictions on scale for prison time (but no death penalty, so 1-7)

db[["Accused"]] %>% 
  select(accusedID, trialID, ongoing, everGuilty, firstGuiltyYear,
         lastGuilty, lastGuiltyYear, lastSentencingTime, 
         lastSentencingArrangement, lastVerdict, lastVerdictYear) %>% summary 

  # left_join(db[["Trials"]] %>% 
  #             select(trialID, ccode_Accused, trialType, yearStart, yearEnd, ongoing) %>% 
  #             rename(ongoing_trial = ongoing), 
  #           by = "trialID", keep = FALSE) 

### merging 
  
## this does not work yet, so   
# download.file(url = "https://github.com/timothoms/country-year-dataset/raw/main/cy_covariates.RData?raw=True", 
#               destfile = here::here("data", "cy_covariates.RData"), method = "auto", cacheOK = FALSE, 
#               headers = c(Authorization = "Authorization: token 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", 
#                           Accept = "Accept: application/vnd.github.v3.raw")) 

df <- readRDS(here::here("data", "cy_covariates.rds")) %>%
  # mutate(base = TRUE) %>%
  mutate(ccode_cow = ifelse(is.na(ccode_cow) & country == "Soviet Union", 365, ccode_cow), 
         ccode_cow = ifelse(is.na(ccode_cow) & country == "Serbia", 345, ccode_cow), 
         ccode_cow = ifelse(is.na(ccode_cow) & country == "Serbia & Montenegro", 345, ccode_cow), 
         ccode_cow = ifelse(is.na(ccode_cow) & country == "North Vietnam", 816, ccode_cow), 
         ccode_cow = ifelse(year == 1990 & country == "West Germany", 255, ccode_cow)) %>% 
  left_join(db[["CountryYears"]] %>% 
              select(-cyID, -country, -region, -tjet_focus),
    by = c("ccode_cow" = "ccode", "ccode_ksg" = "ccode_ksg", "year" = "year")) %>% 
  # losing Slovenia 1991 here
  full_join(read_csv("data/tjet_ICC_new_fromGD.csv") %>% 
              select(-country, -new, -ICC_atLarge),
            by = c("ccode_cow" = "ccode") ) %>% 
  mutate(ICC_referral = case_when(is.na(ICC_referral) ~ 0, 
                                year < ICC_referral ~ 0, 
                                year >= ICC_referral ~ 1),
       ICC_prelim_exam = case_when(is.na(ICC_prelim_exam) ~ 0, 
                                   year < ICC_prelim_exam | year > ICC_prelimEnd | year > ICC_investigation ~ 0, 
                                   year >= ICC_prelim_exam ~ 1),
       ICC_investigation = case_when(is.na(ICC_investigation) ~ 0,
                                     year < ICC_investigation ~ 0,
                                     year >= ICC_investigation ~ 1),
       ICC_arrest_warrant = case_when(is.na(ICC_arrest_warrant) ~ 0, 
                                      year < ICC_arrest_warrant ~ 0, 
                                      year >= ICC_arrest_warrant ~ 1),
       ICC_arrestAppear = case_when(is.na(ICC_arrestAppear) ~ 0, 
                                    year < ICC_arrestAppear ~ 0, 
                                    year >= ICC_arrestAppear ~ 1),
       ICC_confirm_charges = case_when(is.na(ICC_confirm_charges) ~ 0, 
                                       year < ICC_confirm_charges ~ 0, 
                                       year >= ICC_confirm_charges ~ 1),
       ICC_proceedings = case_when(is.na(ICC_proceedings) ~ 0, 
                                   year < ICC_proceedings | year > ICC_proceedEnd ~ 0, 
                                   year >= ICC_proceedings & year <= ICC_proceedEnd ~ 1),
       ICC_withdrawnDismissed = case_when(is.na(ICC_withdrawnDismissed) ~ 0, 
                                          year < ICC_withdrawnDismissed ~ 0, 
                                          year >= ICC_withdrawnDismissed ~ 1) ) %>% 
  select(-ICC_prelimEnd, -ICC_proceedEnd) %>% 
  left_join(read_csv("data/icc_statesparty.csv") %>% 
              filter(ccode != 511) %>% 
              mutate(ccode = ifelse(ccode == 260 & year == 1990, 255, ccode)) %>% 
              select(-country), 
          by = c("ccode_cow" = "ccode", "year" = "year") ) %>% 
  mutate(sample_trans = ifelse(transition == 1, year, NA),
         sample_confl = ifelse(conflict_active == 1, year, NA)) %>% 
  group_by(ccode_case) %>% 
  mutate(sample_trans = min(sample_trans, na.rm = TRUE), 
         sample_confl = min(sample_confl, na.rm = TRUE) ) %>% 
  ungroup() %>% 
  mutate(sample_trans = ifelse(is.infinite(sample_trans), NA, sample_trans),
         sample_trans = case_when(is.na(sample_trans) ~ 0, 
                                  year < sample_trans ~ 0, 
                                  year >= sample_trans ~ 1),
         sample_confl = ifelse(is.infinite(sample_confl), NA, sample_confl),
         sample_confl = case_when(is.na(sample_confl) ~ 0, 
                                  year < sample_confl ~ 0, 
                                  year >= sample_confl ~ 1)) %>% 
  left_join(counts, by = c("ccode_cow" = "ccode", "year" = "year") ) %>% 
  ## losing Timor Leste prior to 2002 here; these should be incorporated into Indo
  mutate(across(all_of(c("trials_HRs", "trials_PostAutocratic", "trials_Conflict", 
                         "convictions_HRs", "convictions_PostAutocratic", "convictions_Conflict")), 
                function(x) ifelse(is.na(x), 0, x))) %>% 
  ## next four lines need to be incorporated into dataset building script
  mutate(deaths_nonstate_osv = ifelse(year >= 1989 & is.na(deaths_nonstate_osv), 0, deaths_nonstate_osv),
         deaths_state_osv = ifelse(year >= 1989 & is.na(deaths_state_osv), 0, deaths_state_osv), 
         prev_nonstate_svac = ifelse(year >= 1989 & !is.na(conflictyear_svac) & is.na(prev_nonstate_svac), 0, prev_nonstate_svac),
         prev_state_svac = ifelse(year >= 1989 & !is.na(conflictyear_svac) & is.na(prev_state_svac), 0, prev_state_svac)) %>% 
  arrange(ccode_case, year) %>% 
  group_by(ccode_case, isna = is.na(theta_mean_fariss) ) %>% 
  mutate(cum_theta_mean_fariss = ifelse(isna, NA, cummean(theta_mean_fariss)),
         sample_combi = ifelse(sample_trans + sample_confl > 0, 1, 0) ) %>% 
  ungroup() %>% 
  select(-isna) %>% 
  group_by(ccode_case) %>% 
  mutate(cum_trials_HRs = cumsum(trials_HRs), 
         cum_trials_PostAutocratic = cumsum(trials_PostAutocratic), 
         cum_trials_Conflict = cumsum(trials_Conflict),
         cum_convictions_HRs = cumsum(convictions_HRs), 
         cum_convictions_PostAutocratic = cumsum(convictions_PostAutocratic), 
         cum_convictions_Conflict = cumsum(convictions_Conflict)) %>% 
  ungroup() %>%
  group_by(ccode_case, sample_trans) %>% 
  mutate(trans_cum_trials_PostAutocratic = cumsum(trials_PostAutocratic), 
         trans_cum_trials_PostAutocratic = ifelse(sample_trans == 0, 0, trans_cum_trials_PostAutocratic),
         trans_cum_convictions_PostAutocratic = cumsum(convictions_PostAutocratic), 
         trans_cum_convictions_PostAutocratic = ifelse(sample_trans == 0, 0, trans_cum_convictions_PostAutocratic)) %>% 
  ungroup() %>% 
  group_by(ccode_case, sample_confl) %>% 
  mutate(confl_cum_trials_Conflict = cumsum(trials_Conflict), 
         confl_cum_trials_Conflict = ifelse(sample_confl == 0, 0, confl_cum_trials_Conflict),
         confl_cum_convictions_Conflict = cumsum(convictions_Conflict), 
         confl_cum_convictions_Conflict = ifelse(sample_confl == 0, 0, confl_cum_convictions_Conflict)) %>% 
  ungroup() %>% 
  group_by(ccode_case, sample_combi) %>% 
  mutate(combi_cum_trials_fits = cumsum(trials_unionFits), 
         combi_cum_trials_fits = ifelse(sample_combi == 0, 0, combi_cum_trials_fits),
         combi_cum_convictions_fits = cumsum(convictions_unionFits), 
         combi_cum_convictions_fits = ifelse(sample_combi == 0, 0, combi_cum_convictions_fits)) %>% 
  ungroup()

lags <- df %>%
  select(-country, -country_name, -histname, -ccode_cow, -ccode_case, -ccode_ksg, 
         -country_id_vdem, -m49, -isoa3, -cid_who, -micro_ksg,                           
         -region, -subregion, -intregion, -region_wb, -ldc, -lldc, -sids) %>%
  mutate(year = year + 1) %>%
  rename_with(~ paste0("lag_", .x))

df %>%
  left_join(lags, by = c("country_case" = "lag_country_case", "year" = "lag_year")) %>% 
  write_csv(here::here("data", "analysis", "tjet_analyses.csv"), na = "")

db[["Amnesties"]] %>%
  write_csv(here::here("data", "analysis", "tjet_amnesties.csv"), na = "")
db[["TruthCommissions"]] %>%
  write_csv(here::here("data", "analysis", "tjet_tcs.csv"), na = "")
db[["Reparations"]] %>%
  write_csv(here::here("data", "analysis", "tjet_reparations.csv"), na = "")
db[["Trials"]] %>%
  write_csv(here::here("data", "analysis", "tjet_trials.csv"), na = "")
db[["Accused"]] %>%
  write_csv(here::here("data", "analysis", "tjet_accused.csv"), na = "")
db[["Vettings"]] %>%
  write_csv(here::here("data", "analysis", "tjet_vettings.csv"), na = "")

rm(trial_counts, conviction_counts, counts, icc)

## FROM HERE >  

read_csv(here::here("data", "investigations.csv") ) %>% 
  select(-pkey) %>% 
  mutate(
    coi_fact_unga = ifelse(!is.na(coi_fact_unga), 1, 0), 
    coi_fact_unsc = ifelse(!is.na(coi_fact_unsc), 1, 0), 
    coi_fact_unhrc = ifelse(!is.na(coi_fact_unhrc), 1, 0), 
    coi_fact_uchr = ifelse(!is.na(coi_fact_uchr), 1, 0), 
    coi_fact_unsec = ifelse(!is.na(coi_fact_unsec), 1, 0), 
    coi_fact_ohchr = ifelse(!is.na(coi_fact_ohchr), 1, 0)) 

# df %>%
#   mutate(across(all_of(cols), .fns = ~ ifelse(is.na(.x), 0, .x)) ) 

