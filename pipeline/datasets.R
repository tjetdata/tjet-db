### packages
require(tidyverse)

### reading database
load(here::here("data", "tjetdb.RData"), verbose = TRUE)

### new TJ variables 

guilty <- db[["CourtLevels"]] %>% 
  mutate(date = as_date(date)) %>%
  filter(!is.na(accusedID) & guilty == 1) %>% 
  arrange(accusedID, year) %>% 
  group_by(accusedID, year) %>% 
  mutate(max_date = max(date, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(is.infinite(max_date) | date == max_date) %>%
  select(accusedID, year, sentencingTime, sentencingArrangement) %>%
  # select(accusedID, date, year, sentencingTime, sentencingArrangement) %>%
  distinct() %>% 
  # group_by(accusedID, year) %>%
  # mutate(n = n()) %>%
  # ungroup() %>%
  # filter(n > 1) %>% print(n = Inf)
  
### need to deal with duplicate accused-years here first!!!

guilty_scale <- guilty %>% 
  filter(sentencingArrangement %in% c("Ordinary prison", "Don't Know")) %>% # others: "Special detention", "Suspended sentence"
  mutate(prison_scale = case_when(sentencingTime == "Less than 1 year" ~ 1, 
                                  sentencingTime == "1-3 years" ~ 2, 
                                  sentencingTime == "4-9 years" ~ 3, 
                                  sentencingTime == "10-19 years" ~ 4, 
                                  sentencingTime == "20+ years" ~ 5, 
                                  sentencingTime == "Life Imprisonment" ~ 6) ) %>% 
  filter(!is.na(prison_scale)) %>%  
  # select(accusedID, date, year, prison_scale) %>%  # CLID, verdict, guilty
  # distinct() %>% 
  # arrange(accusedID, year) %>% 
  # group_by(accusedID, year) %>% 
  # mutate(n = n(), 
  #        max_date = max(date, na.rm = TRUE)) %>%
  # ungroup() %>% 
  # filter(is.infinite(max_date) | date == max_date) %>%
  select(accusedID, year, prison_scale) %>% 
  group_by(accusedID, year) %>% 
  mutate(prison_scale = max(prison_scale)) %>%
  ungroup() %>% 
  distinct() 

convictions <- guilty %>%
  select(accusedID, year) 

trials <- db[["Trials"]] %>% 
  mutate(trialType = case_when(
    trialType %in% c("domestic", "don't know") ~ "domestic",
    trialType %in% c("international", "international (hybrid)") ~ "international",
    trialType == "foreign" ~ "foreign")) %>% 
  filter(HRs_charges > 0 | humanRights == 1 | IntraConfl == 1) %>% 
  filter(anyStateAgent == 1) %>% 
  select(trialID, ccode_Accused, trialType, yearStart, yearEnd, # ongoing, 
         fitsPostAutocraticTJ, fitsConflictTJ,
         anyOpposedToGov, anyHighRank, # anyStateAgent, 
         firstConvictionYear_min, finalConvictionYear_min, lastVerdictYear_max, 
         conviction, convictionHighRank, finalConviction, finalConvictionHighRank) 

trials_start <- trials %>% 
  group_by(ccode_Accused, trialType, yearStart) %>% 
  mutate(trials_yearStart = n()) %>%  
  ungroup() %>%  
  select(ccode_Accused, trialType, yearStart, trials_yearStart) %>% 
  arrange(ccode_Accused, trialType, yearStart) %>% 
  distinct() %>% 
  pivot_wider(names_from = trialType, values_from = trials_yearStart) %>% 
  rename(trials_domestic = "domestic",
         trials_foreign = "foreign",
         trials_intl = "international") 

trials_ongoing <- trials %>%
  rowwise() %>% 
  mutate(year = list(yearStart:yearEnd)) %>% 
  ungroup() %>% 
  unnest_longer(year) %>% 
  select(ccode_Accused, year, trialType, yearStart, yearEnd) %>%
  arrange(ccode_Accused, trialType, year) %>% 
  group_by(ccode_Accused, trialType, year) %>% 
  mutate(trials_ongoing = n()) %>% 
  ungroup() %>% 
  select(ccode_Accused, year, trialType, trials_ongoing) %>% 
  distinct() %>% 
  pivot_wider(names_from = trialType, values_from = trials_ongoing) %>% 
  rename(trials_domestic_ongoing = "domestic",
         trials_foreign_ongoing = "foreign",
         trials_intl_ongoing = "international") 

trials_convictions <- trials %>%
  arrange(ccode_Accused, trialType, yearEnd) %>% 
  group_by(ccode_Accused, trialType, yearEnd) %>% 
  mutate(convict_final = sum(finalConviction)) %>% 
  ungroup() %>% 
  select(ccode_Accused, yearEnd, trialType, convict_final) %>% 
  distinct() %>% 
  pivot_wider(names_from = trialType, values_from = convict_final) %>% 
  rename(convict_final_domestic = "domestic",
         convict_final_foreign = "foreign",
         convict_final_intl = "international") 
  
accused <- trials %>%
  select(trialID, ccode_Accused, trialType, yearStart, yearEnd) %>% 
  left_join(db[["Accused"]], by = "trialID") %>% 
  filter(!is.na(accusedID)) %>%
  filter(stateAgent == 1) %>%
  select(ccode_Accused, accusedID, trialID, trialType, yearStart, yearEnd, 
         # stateAgent, opposedToGovernment, lastVerdict, lastVerdictYear, 
         # lastSentencingTime, lastSentencingArrangement, ongoing, 
         highRank, everGuilty, firstGuiltyYear, lastGuilty, lastGuiltyYear) %>% I



### FROM HERE> 

# - for state agents and HRs or Confl trials
#   - for domestic, foreign & intl trials separately
#     - from trials
#       - count of trials (by countryAccused & startYear)
#         - merge: trials_start
#       - count of ongoing trials by year (by countryAccused & >= startYear & <=endYear)
#         - merge: trials_ongoing
#       - count of trials with final outcome a conviction by endYear
#         - merge: trials_convictions
#     - from accused BUT via trials for conditions
#       - count of high ranking accused in ongoing trials by year 


#       - count of convictions of accused (by countryAccused & conviction year)
#         - do we include all convictions of same accused at all levels? or only first
#       - count of convictions of high ranking accused by conviction year 
#         - same as above
#       - count of convictions as percentage of all accused on trial per year
#       - count of all convictions on scale for prison time (but no death penalty, so 1-7)
#         - to merge: guilty_scale via accused
#  - then same vars for trials of gender crimes
#    - trials vars: SGBV, rape_Accused, sexualViolence_Accused, otherSGBV_Accused
#    - accused vars: SGBV, rape, sexualViolence, otherSGBV, childVictim, LGBTQvictim, maleVictim, RSV









trial_counts <- db[["Trials"]] %>% 
  mutate(HRs = ifelse(HRs_charges > 0 | humanRights == 1, 1, 0) ) %>%
  select(ccode_Accused, yearStart, HRs, 
         fitsPostAutocraticTJ, fitsConflictTJ) %>%
  mutate(fitsBoth = ifelse(fitsPostAutocraticTJ + fitsConflictTJ > 0, 1, 0),
         HRsConfl = ifelse(HRs + fitsPostAutocraticTJ + fitsConflictTJ > 0, 
                           1, 0)) %>%
  arrange(ccode_Accused, yearStart) %>% 
  group_by(ccode_Accused, yearStart) %>% 
  mutate(trials_HRs = sum(HRs) , 
         trials_PostAuto = sum(fitsPostAutocraticTJ), 
         trials_Conflict = sum(fitsConflictTJ), 
         trials_HRsConfl = sum(HRsConfl), 
         trials_unionFit = sum(fitsBoth) ) %>% 
  select(ccode_Accused, yearStart, trials_HRs, trials_PostAuto, 
         trials_Conflict, trials_HRsConfl, trials_unionFit) %>% 
  distinct() %>% 
  rename(ccode = ccode_Accused, 
         year = yearStart) %>% 
  filter(year >= 1970 & year <= 2020) 

conviction_counts <- db[["Trials"]] %>% 
  mutate(HRs = ifelse(HRs_charges > 0 | humanRights == 1, 1, 0) ) %>%
  select(ccode_Accused, firstConvictionYear_min, HRs, 
         fitsPostAutocraticTJ, fitsConflictTJ) %>%
  mutate(fitsBoth = ifelse(fitsPostAutocraticTJ + fitsConflictTJ > 0, 1, 0),
         firstConvictionYear_min = as.integer(firstConvictionYear_min)) %>%
  filter(!is.na(firstConvictionYear_min) ) %>% 
  arrange(ccode_Accused, firstConvictionYear_min) %>% 
  group_by(ccode_Accused, firstConvictionYear_min) %>% 
  mutate(convict_HRs = sum(HRs) , 
         convict_PostAuto = sum(fitsPostAutocraticTJ) , 
         convict_Conflict = sum(fitsConflictTJ), 
         convict_unionFit = sum(fitsBoth) ) %>% 
  select(ccode_Accused, firstConvictionYear_min, convict_HRs, 
         convict_PostAuto, convict_Conflict, 
         convict_unionFit) %>% 
  distinct() %>% 
  rename(ccode = ccode_Accused, 
         year = firstConvictionYear_min) %>% 
  filter(year >= 1970 & year <= 2020)

### merging count variables together
counts <- full_join(trial_counts, conviction_counts, 
                    by = c("ccode", "year")) %>% 
  mutate(ccode = ifelse(ccode == 679 & year < 1990, 678, ccode)) # Yemen YAR

### if country-year-dataset repo is made public, could get directly from there
### this currently does not work, but here is sample code to do so; if public, 
### would have to reset token because had accidentally included here before
# download.file(
#   url = "https://github.com/timothoms/country-year-dataset/raw/main/cy_covariates.RData?raw=True",
#   destfile = here::here("data", "cy_covariates.RData"), 
#   method = "auto", cacheOK = FALSE,
#   headers = c(
#     Authorization = "Authorization: token INSERT TOKEN HERE",
#     Accept = "Accept: application/vnd.github.v3.raw"))

### merging it all together

df <- readRDS(here::here("data", "cy_covariates.rds")) %>%
  # mutate(base = TRUE) %>%
  mutate(ccode_cow = ifelse(is.na(ccode_cow) & country == "Soviet Union", 
                            365, ccode_cow), 
         ccode_cow = ifelse(is.na(ccode_cow) & country == "Serbia", 
                            345, ccode_cow), 
         ccode_cow = ifelse(is.na(ccode_cow) & country == "Serbia & Montenegro", 
                            345, ccode_cow), 
         ccode_cow = ifelse(is.na(ccode_cow) & country == "North Vietnam", 
                            816, ccode_cow), 
         ccode_cow = ifelse(year == 1990 & country == "West Germany", 
                            255, ccode_cow)) %>% 
  left_join(db[["CountryYears"]] %>% 
              select(-cyID, -country, -region, -tjet_focus), 
            by = c("ccode_cow" = "ccode", 
                   "ccode_ksg" = "ccode_ksg", 
                   "year" = "year")) %>% # losing Slovenia 1991 here
  full_join(db[["ICC"]] %>% select(-country),
            by = "ccode_cow" ) %>% 
  mutate(ICC_referral = case_when(is.na(ICC_referral) ~ 0, 
                                  year < ICC_referral ~ 0, 
                                  year >= ICC_referral ~ 1),
         ICC_prelim_exam = case_when(
           is.na(ICC_prelim_exam) ~ 0, 
           year < ICC_prelim_exam | 
             year > ICC_prelimEnd | 
             year > ICC_investigation ~ 0, 
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
         ICC_proceedings = case_when(
           is.na(ICC_proceedings) ~ 0, 
           year < ICC_proceedings | year > ICC_proceedEnd ~ 0, 
           year >= ICC_proceedings & year <= ICC_proceedEnd ~ 1),
         ICC_withdrawnDismissed = case_when(
           is.na(ICC_withdrawnDismissed) ~ 0, 
           year < ICC_withdrawnDismissed ~ 0, 
           year >= ICC_withdrawnDismissed ~ 1)
  ) %>% 
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
  ## warning here that is addressed in next mutate
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
  ## losing Timor Leste prior to 2002 here; these should be incorporated into Indonesia
  ### filling in NAs
  mutate(across(all_of(c("trials_HRs", "trials_PostAuto", "trials_Conflict", 
                         "convict_HRs", "convict_PostAuto", "convict_Conflict")), 
                function(x) ifelse(is.na(x), 0, x))) %>% 
  ### cumulative measures from here
  arrange(ccode_case, year) %>% 
  group_by(ccode_case, isna = is.na(theta_mean_fariss) ) %>% 
  mutate(cum_theta_mean_fariss = ifelse(isna, NA, cummean(theta_mean_fariss)),
         sample_combi = ifelse(sample_trans + sample_confl > 0, 1, 0) ) %>% 
  ungroup() %>% 
  select(-isna) %>% 
  group_by(ccode_case) %>% 
  mutate(cum_trials_HRs = cumsum(trials_HRs), 
         cum_trials_PostAuto = cumsum(trials_PostAuto), 
         cum_trials_Conflict = cumsum(trials_Conflict),
         cum_convict_HRs = cumsum(convict_HRs), 
         cum_convict_PostAuto = cumsum(convict_PostAuto), 
         cum_convict_Conflict = cumsum(convict_Conflict)) %>% 
  ungroup() %>%
  group_by(ccode_case, sample_trans) %>% 
  mutate(trans_cum_trials_PostAuto = cumsum(trials_PostAuto), 
         trans_cum_trials_PostAuto = ifelse(sample_trans == 0, 
                                            0, trans_cum_trials_PostAuto),
         trans_cum_convict_PostAuto = cumsum(convict_PostAuto), 
         trans_cum_convict_PostAuto = ifelse(sample_trans == 0, 
                                             0, trans_cum_convict_PostAuto)) %>% 
  ungroup() %>% 
  group_by(ccode_case, sample_confl) %>% 
  mutate(confl_cum_trials_Conflict = cumsum(trials_Conflict), 
         confl_cum_trials_Conflict = ifelse(sample_confl == 0, 
                                            0, confl_cum_trials_Conflict),
         confl_cum_convict_Conflict = cumsum(convict_Conflict), 
         confl_cum_convict_Conflict = ifelse(sample_confl == 0, 
                                             0, confl_cum_convict_Conflict)) %>% 
  ungroup() %>% 
  group_by(ccode_case, sample_combi) %>% 
  mutate(combi_cum_trials_fits = cumsum(trials_unionFit), 
         combi_cum_trials_fits = ifelse(
           sample_combi == 0, 0, combi_cum_trials_fits),
         combi_cum_convictions_fits = cumsum(convict_unionFit), 
         combi_cum_convictions_fits = ifelse(sample_combi == 0, 
                                             0, combi_cum_convictions_fits)) %>% 
  ungroup()

### if we need to integrate the UBN investigations
# db[["Investigations"]] %>% 
#   select(-pkey)

### finally, created lags and saving the analysis dataset
lags <- df %>%
  select(-country, -country_name, -histname, -ccode_cow, -ccode_case, 
         -ccode_ksg, -country_id_vdem, -m49, -isoa3, -iso3c_wb, -cid_who, 
         -micro_ksg, -region, -subregion, -intregion, -region_wb, 
         -ldc, -lldc, -sids, -income_wb) %>%
  mutate(year = year + 1) %>%
  rename_with(~ paste0("lag_", .x))
df %>%
  left_join(lags, by = c("country_case" = "lag_country_case", 
                         "year" = "lag_year")) %>% 
  write_csv(here::here("data", "analysis", "tjet_analyses.csv"), na = "")
rm(trial_counts, conviction_counts, counts)

### also saving individual mechanism tables
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

### sample code for applying same function to many columns
# df %>%
#   mutate(across(all_of(cols), .fns = ~ ifelse(is.na(.x), 0, .x)) ) 
