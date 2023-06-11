require(tidyverse)
require(here)

load(here("data", "tjetdb.RData"), verbose = TRUE)

icc <- db[["Accused"]] %>% 
  select(accusedID, trialID, ICC_referral, ICC_prelim_exam, ICC_investigation, 
         ICC_arrest_warrant, ICC_arrestAppear, ICC_atLarge, 
         ICC_confirm_charges, ICC_proceedings, ICC_withdrawnDismissed) %>% 
  left_join(db[["Trials"]] %>% 
              select(trialID, ccode_Accused, yearStart, legalSystem), 
            by = c("trialID")) %>% 
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
  distinct() # %>% write_csv(here("data", "tjet_ICC.csv"), na = "")

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
         year = yearStart)
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
         year = firstConvictionYear_min)

### merging 

read_csv("data/tjet_covariates.csv") %>% 
  select(-v2x_egaldem, -v2x_egaldem_sd, -v2x_egal, -v2x_egal_sd,
         -v2juhcind_mean, -v2juncind_mean,
         -v2exbribe_mean, -v2exbribe, -v2exbribe_sd,
         -v2exembez_mean, -v2exembez, -v2exembez_sd, 
         -v2excrptps_mean, -v2excrptps, -v2excrptps_sd, 
         -v2exthftps_mean, -v2exthftps, - v2exthftps_sd, 
         -v2lgcrrpt_mean, -v2jucorrdc_mean, -v2mecorrpt_mean) %>% 
  filter(!(country == "Andorra" & year < 1993)) %>%
  filter(!(country == "Brunei" & year < 1984)) %>%
  filter(!(country == "Kiribati" & year < 1997)) %>%
  filter(!(country == "Liechtenstein" & year < 1990)) %>%
  filter(!(country == "Marshall Islands" & year < 1991)) %>%
  filter(!(country == "Micronesia" & year < 1992)) %>%
  filter(!(country == "Monaco" & year < 1993)) %>%
  filter(!(country == "Palestinian Territory")) %>%
  mutate(ccode_cow = ifelse(is.na(ccode_cow) & country == "Soviet Union", 365, ccode_cow), 
         ccode_cow = ifelse(is.na(ccode_cow) & country == "Serbia", 345, ccode_cow), 
         ccode_cow = ifelse(is.na(ccode_cow) & country == "Serbia & Montenegro", 345, ccode_cow), 
         ccode_cow = ifelse(is.na(ccode_cow) & country == "North Vietnam", 816, ccode_cow), 
         ccode_cow = ifelse(year == 1990 & country == "West Germany", 255, ccode_cow) ) %>%
  full_join(db[["CountryYears"]] %>% rename(country_cy = country),
    by = c("ccode_cow" = "ccode", "year" = "year")) %>% 
  select(-cyID, -country_cy, -ccode_gw, -region, -tjet_focus) %>% 
  left_join(read_csv("data/tjet_ICC_new_fromGD.csv") %>% 
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
  left_join(read_csv("data/icc_statesparty.csv") %>% select(-country), 
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
  left_join(trial_counts, by = c("ccode_case" = "ccode", "year" = "year") ) %>% 
  left_join(conviction_counts, by = c("ccode_case" = "ccode", "year" = "year") ) %>% 
  mutate(across(all_of(c("trials_HRs", "trials_PostAutocratic", "trials_Conflict", 
                         "convictions_HRs", "convictions_PostAutocratic", "convictions_Conflict")), 
                function(x) ifelse(is.na(x), 0, x))) %>% 
  mutate(deaths_nonstate_osv = ifelse(year >= 1989 & is.na(deaths_nonstate_osv), 0, deaths_nonstate_osv),
         deaths_state_osv = ifelse(year >= 1989 & is.na(deaths_state_osv), 0, deaths_state_osv), 
         prev_gov_svac = ifelse(year >= 1989 & !is.na(conflictyear_svac) & is.na(prev_gov_svac), 0, prev_gov_svac),
         prev_nonstate_svac = ifelse(year >= 1989 & !is.na(conflictyear_svac) & is.na(prev_nonstate_svac), 0, prev_nonstate_svac),
         prev_oth_svac = ifelse(year >= 1989 & !is.na(conflictyear_svac) & is.na(prev_oth_svac), 0, prev_oth_svac),
         prev_pgm_svac = ifelse(year >= 1989 & !is.na(conflictyear_svac) & is.na(prev_pgm_svac), 0, prev_pgm_svac),
         prev_reb_svac = ifelse(year >= 1989 & !is.na(conflictyear_svac) & is.na(prev_reb_svac), 0, prev_reb_svac),
         prev_state_svac = ifelse(year >= 1989 & !is.na(conflictyear_svac) & is.na(prev_state_svac), 0, prev_state_svac)) %>% 
  arrange(ccode_case, year) %>% 
  group_by(ccode_case, isna = is.na(latentmean_fariss) ) %>% 
  mutate(cum_latentmean_fariss = ifelse(isna, NA, cummean(latentmean_fariss)),
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
  ungroup() %>% 
  write_csv("~/Documents/GitHub/TJET_Harvard_slides/data/tjet_analyses.csv", na = "")

db[["Amnesties"]] %>%
  write_csv("~/Documents/GitHub/TJET_Harvard_slides/data/tjet_amnesties.csv", na = "")
db[["TruthCommissions"]] %>%
  write_csv("~/Documents/GitHub/TJET_Harvard_slides/data/tjet_tcs.csv", na = "")
db[["Reparations"]] %>%
  write_csv("~/Documents/GitHub/TJET_Harvard_slides/data/tjet_reparations.csv", na = "")
db[["Trials"]] %>%
  write_csv("~/Documents/GitHub/TJET_Harvard_slides/data/tjet_trials.csv", na = "")
db[["Accused"]] %>%
  write_csv("~/Documents/GitHub/TJET_Harvard_slides/data/tjet_accused.csv", na = "")
db[["Vettings"]] %>%
  write_csv("~/Documents/GitHub/TJET_Harvard_slides/data/tjet_vettings.csv", na = "")

rm(trial_counts, conviction_counts, icc)
