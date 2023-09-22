### packages
require(tidyverse)

### reading database
load(here::here("data", "tjetdb.RData"), verbose = TRUE)
# str(db, 1)

### helpers 
sample_cy <- c(
  glo = "global", ### all, all the time, i.e. full dataset
  dtr = "democratic transition", ### binary, from first transition year
  aco = "all conflicts", ### binary, from first conflict year
  dco = "during conflict", ### binary, when conflict active
  pco = "post-conflict") ### binary, after active conflict ended
source("functions/TCgoals.R")
source("functions/TCmeasure.R")
source("functions/TrialsMeasure.R")

### trials 

measures <- c(trs = "trials started", tro = "trials ongoing", 
              tfc = "trials with final convictions", cct = "conviction count", 
              crt = "conviction rate by all accused", sen = "sentence totals")

map(names(measures), function(x) {
  TrialsMeasure(
    type_opts = "int", nexus_vars = c("hrs", "ctj"), memb_opts = "sta", 
    rank_opts = NULL, charges_opts = NULL, measure = x) 
})

map(names(measures), function(x) {
  TrialsMeasure(
    type_opts = "int", nexus_vars = c("hrs", "ctj"), memb_opts = "opp", 
    rank_opts = NULL, charges_opts = NULL, measure = x) 
})

map(names(measures), function(x) {
  TrialsMeasure(
    type_opts = "for", nexus_vars = c("hrs", "ctj"), memb_opts = "sta", 
    rank_opts = NULL, charges_opts = NULL, measure = x) 
})

map(names(measures), function(x) {
  TrialsMeasure(
    type_opts = "for", nexus_vars = c("hrs", "ctj"), memb_opts = "opp", 
    rank_opts = NULL, charges_opts = NULL, measure = x) 
})

map(names(measures), function(x) {
  TrialsMeasure(
    type_opts = "dom", nexus_vars = "hrs", memb_opts = "sta", 
    rank_opts = NULL, charges_opts = NULL, measure = x) 
})

map(names(measures), function(x) {
  TrialsMeasure(
    type_opts = "dom", nexus_vars = "hrs", memb_opts = "opp", 
    rank_opts = NULL, charges_opts = NULL, measure = x) 
})

map(names(measures), function(x) {
  TrialsMeasure(
    type_opts = "dom", nexus_vars = "ctj", memb_opts = "sta", 
    rank_opts = NULL, charges_opts = NULL, measure = x) 
})

map(names(measures), function(x) {
  TrialsMeasure(
    type_opts = "dom", nexus_vars = "ctj", memb_opts = "opp", 
    rank_opts = NULL, charges_opts = NULL, measure = x) 
})

### TCs

TCmeasure(new_col_name = "tcs_pcj_all", start_year_var = "yearBeginOperation", 
          nexus_vars = "beganOperatingAfterIntraConfl", crimes_vars = "all",
          independence_opts = NULL, aims_opts = NULL, consult_vars = NULL, 
          powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL, 
          recommend_vars = NULL, monitor_vars = NULL)

TCmeasure(new_col_name = "tcs_pcj_victim_process", 
          start_year_var = "yearBeginOperation", 
          nexus = "beganOperatingAfterIntraConfl", crimes_vars = "all",
          aims_opts = c("truth for victims", "memorialization", "apology",
                        "recognition of victims", "reparation"), 
          independence_opts = NULL, consult_vars = "consultedVictims", 
          powers_vars = "allocateReparations", 
          testimony_vars = "encourageVictimTestimony", 
          reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL)
TCmeasure(new_col_name = "tcs_pcj_victim_outcome", 
          start_year_var = "yearCompleteOperation",
          nexus = "beganOperatingAfterIntraConfl", crimes_vars = "all",
          aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
          powers_vars = NULL, testimony_vars = NULL, 
          reports_vars = "reportPubliclyAvailable",
          recommend_vars = "recommendReparations",
          monitor_vars = "mandatePeriodicMonitoringImplementation") 

TCmeasure(new_col_name = "tcs_pcj_account_process", 
          start_year_var = "yearBeginOperation", 
          nexus = "beganOperatingAfterIntraConfl", crimes_vars = "all",
          aims_opts = c("accountability", "responsibility",
                        "prevention of human rights violations"),
          independence_opts = c("partially independent", "fully independent"), 
          consult_vars = NULL, 
          powers_vars = c("compelTestimony", "supportProsecutions", 
                          "namePerpetrators"),
          testimony_vars = "perpetratorTestimony",
          reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) 
TCmeasure(new_col_name = "tcs_pcj_account_outcome", 
          start_year_var = "yearCompleteOperation",
          nexus = "beganOperatingAfterIntraConfl", crimes_vars = "all",
          aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
          powers_vars = NULL, testimony_vars = NULL, 
          reports_vars = "reportPubliclyAvailable",
          recommend_vars = "recommendProsecutions",
          monitor_vars = "mandatePeriodicMonitoringImplementation")

TCmeasure(new_col_name = "tcs_pcj_peace_process", 
          start_year_var = "yearBeginOperation", 
          nexus = "beganOperatingAfterIntraConfl", crimes_vars = "all",
          aims_opts = c("reconciliation", "coexistence", "dialogue", 
                        "non-recurrence"),
          independence_opts = NULL, consult_vars = NULL,
          powers_vars = "grantAmnesty",
          testimony_vars = "heldPublicHearings",
          reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) 
TCmeasure(new_col_name = "tcs_pcj_peace_outcome", 
          start_year_var = "yearCompleteOperation",
          nexus = "beganOperatingAfterIntraConfl", crimes_vars = "all",
          aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
          powers_vars = NULL, testimony_vars = NULL, 
          reports_vars = "reportPubliclyAvailable",
          recommend_vars = NULL, monitor_vars = NULL)

TCmeasure(new_col_name = "tcs_pcj_reform_process", 
          start_year_var = "yearBeginOperation", 
          nexus = "beganOperatingAfterIntraConfl", crimes_vars = "all",
          aims_opts = c("historial truth", "institutional reform", 
                        "addressing corruption"),
          independence_opts = c("partially independent", "fully independent"), 
          consult_vars = NULL,
          powers_vars = "recommendInstitutionalReforms",
          testimony_vars = "heldPublicHearings",
          reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) 
TCmeasure(new_col_name = "tcs_pcj_reform_outcome", 
          start_year_var = "yearCompleteOperation",
          nexus = "beganOperatingAfterIntraConfl", crimes_vars = "all",
          aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
          powers_vars = NULL, testimony_vars = NULL, 
          reports_vars = "reportPubliclyAvailable",
          recommend_vars = "reportRecommendInstitutionalReform", 
          monitor_vars = "mandatePeriodicMonitoringImplementation")


### FROM HERE > need to create measures in separate functions and then merge in 


### merging it all together

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
              mutate(ccode = ifelse(ccode == 260 & 
                                      year == 1990, 255, ccode)) %>% 
              select(-country), 
            by = c("ccode_cow" = "ccode", "year" = "year") ) %>% 
  filter(year <= 2020) %>% 
  mutate(sample_trans = ifelse(transition == 1, year, NA),
         sample_confl = ifelse(conflict_active == 1, year, NA), 
         # active_confl = sample_confl,
         dco = ifelse(!is.na(sample_confl) & year == sample_confl, 1, 0) # dco = "during conflict", ### binary, when conflict active
  ) %>% 
  group_by(ccode_case) %>% 
  mutate(sample_trans = min(sample_trans, na.rm = TRUE), 
         sample_confl = min(sample_confl, na.rm = TRUE) ) %>% 
  ## warning here that is addressed in next mutate
  ungroup() %>%
  mutate(pco = ifelse(year > sample_confl & dco == 0, 1, 0), ## pco = "post-conflict"
         sample_trans = ifelse(is.infinite(sample_trans), NA, sample_trans),
         sample_trans = case_when(is.na(sample_trans) ~ 0, 
                                  year < sample_trans ~ 0, 
                                  year >= sample_trans ~ 1), 
         dtr = sample_trans, ## dtr = "democratic transition"
         sample_confl = ifelse(is.infinite(sample_confl), NA, sample_confl),
         sample_confl = case_when(is.na(sample_confl) ~ 0, 
                                  year < sample_confl ~ 0, 
                                  year >= sample_confl ~ 1), 
         aco = sample_confl ## aco = "all conflicts"
  ) %>% I
  ### the next line was merging in the old counts 
  ### and the remaining code deals with those 
  # left_join(counts, by = c("ccode_cow" = "ccode", "year" = "year") ) %>%
  ## losing Timor Leste prior to 2002; should be incorporated into Indonesia
  ### filling in NAs
  # mutate(across(all_of(c("trials_HRs", "trials_PostAuto", "trials_Conflict", 
  #                        "convict_HRs", "convict_PostAuto", 
  #                        "convict_Conflict")), 
  #               function(x) ifelse(is.na(x), 0, x))) %>% 
  ### old cumulative measures from here
  # arrange(ccode_case, year) %>% 
  # group_by(ccode_case, isna = is.na(theta_mean_fariss) ) %>% 
  # mutate(cum_theta_mean_fariss = ifelse(isna, NA, cummean(theta_mean_fariss)),
  #        sample_combi = ifelse(sample_trans + sample_confl > 0, 1, 0) ) %>% 
  # ungroup() %>% 
  # select(-isna) %>% 
  # group_by(ccode_case) %>% 
  # mutate(cum_trials_HRs = cumsum(trials_HRs), 
  #        cum_trials_PostAuto = cumsum(trials_PostAuto), 
  #        cum_trials_Conflict = cumsum(trials_Conflict),
  #        cum_convict_HRs = cumsum(convict_HRs), 
  #        cum_convict_PostAuto = cumsum(convict_PostAuto), 
  #        cum_convict_Conflict = cumsum(convict_Conflict)) %>% 
  # ungroup() %>%
  # group_by(ccode_case, sample_trans) %>% 
  # mutate(trans_cum_trials_PostAuto = cumsum(trials_PostAuto), 
  #        trans_cum_trials_PostAuto = ifelse(sample_trans == 0, 
  #                                           0, trans_cum_trials_PostAuto),
  #        trans_cum_convict_PostAuto = cumsum(convict_PostAuto), 
  #        trans_cum_convict_PostAuto = ifelse(sample_trans == 0, 
  #                                            0, trans_cum_convict_PostAuto)) %>% 
  # ungroup() %>% 
  # group_by(ccode_case, sample_confl) %>% 
  # mutate(confl_cum_trials_Conflict = cumsum(trials_Conflict), 
  #        confl_cum_trials_Conflict = ifelse(sample_confl == 0, 
  #                                           0, confl_cum_trials_Conflict),
  #        confl_cum_convict_Conflict = cumsum(convict_Conflict), 
  #        confl_cum_convict_Conflict = ifelse(sample_confl == 0, 
  #                                            0, confl_cum_convict_Conflict)) %>% 
  # ungroup() %>% 
  # group_by(ccode_case, sample_combi) %>% 
  # mutate(combi_cum_trials_fits = cumsum(trials_unionFit), 
  #        combi_cum_trials_fits = ifelse(
  #          sample_combi == 0, 0, combi_cum_trials_fits),
  #        combi_cum_convictions_fits = cumsum(convict_unionFit), 
  #        combi_cum_convictions_fits = ifelse(sample_combi == 0, 
  #                                            0, combi_cum_convictions_fits)) %>% 
  # ungroup()

### if we need to integrate the UN investigations
# db[["Investigations"]] %>% 
#   select(-pkey)

### last step, created lags and saving the analyses dataset
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
