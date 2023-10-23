### packages
require(tidyverse)

### reading database
load(here::here("data", "tjetdb.RData"), verbose = TRUE)
# str(db, 1)

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

### saving individual mechanism tables

dropbox_path <- "~/Dropbox/TJLab/TimoDataWork/analyses_datasets/"
db[["codebook"]] %>% 
  # write_csv(here::here("tjet_datasets", "tjet_codebook.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_codebook.csv"), na = "")
db[["Amnesties"]] %>% 
  write_csv(here::here("tjet_datasets", "tjet_amnesties.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_amnesties.csv"), na = "")
db[["TruthCommissions"]] %>%
  write_csv(here::here("tjet_datasets", "tjet_tcs.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_tcs.csv"), na = "")
db[["Reparations"]] %>%
  write_csv(here::here("tjet_datasets", "tjet_reparations.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_reparations.csv"), na = "")
db[["Trials"]] %>% 
  write_csv(here::here("tjet_datasets", "tjet_trials.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_trials.csv"), na = "")
db[["Accused"]] %>%
  write_csv(here::here("tjet_datasets", "tjet_accused.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_accused.csv"), na = "")
db[["CourtLevels"]] %>%
  write_csv(here::here("tjet_datasets", "tjet_courtlevels.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_courtlevels.csv"), na = "")
db[["Vettings"]] %>%
  write_csv(here::here("tjet_datasets", "tjet_vettings.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_vettings.csv"), na = "")

### helpers for CY measures
source("functions/TCgoals.R")
source("functions/TCmeasure.R")
source("functions/TrialsMeasure.R")
sample_cy <- c(
  glo = "global", ### all, all the time, i.e. full dataset
  dtr = "democratic transition", ### binary, from first transition year
  aco = "all conflicts", ### binary, from first conflict year
  dco = "during conflict", ### binary, when conflict active
  pco = "post-conflict") ### binary, after active conflict ended

### old version of trial counts 
# trial_counts <- db[["Trials"]] %>% 
#   mutate(HRs = ifelse(HRs_charges > 0 | humanRights == 1, 1, 0) ) %>%
#   select(ccode_Accused, yearStart, HRs, 
#          fitsPostAutocraticTJ, fitsConflictTJ) %>%
#   mutate(fitsBoth = ifelse(fitsPostAutocraticTJ + fitsConflictTJ > 0, 1, 0),
#          HRsConfl = ifelse(HRs + fitsPostAutocraticTJ + fitsConflictTJ > 0, 
#                            1, 0)) %>%
#   arrange(ccode_Accused, yearStart) %>% 
#   group_by(ccode_Accused, yearStart) %>% 
#   mutate(trials_HRs = sum(HRs) , 
#          trials_PostAuto = sum(fitsPostAutocraticTJ), 
#          trials_Conflict = sum(fitsConflictTJ), 
#          trials_HRsConfl = sum(HRsConfl), 
#          trials_unionFit = sum(fitsBoth) ) %>% 
#   select(ccode_Accused, yearStart, trials_HRs, trials_PostAuto, 
#          trials_Conflict, trials_HRsConfl, trials_unionFit) %>% 
#   distinct() %>% 
#   rename(ccode = ccode_Accused, 
#          year = yearStart) %>% 
#   filter(year >= 1970 & year <= 2020) 

### old version of trial conviction counts 
# conviction_counts <- db[["Trials"]] %>% 
#   mutate(HRs = ifelse(HRs_charges > 0 | humanRights == 1, 1, 0) ) %>%
#   select(ccode_Accused, firstConvictionYear_min, HRs, 
#          fitsPostAutocraticTJ, fitsConflictTJ) %>%
#   mutate(fitsBoth = ifelse(fitsPostAutocraticTJ + fitsConflictTJ > 0, 1, 0),
#          firstConvictionYear_min = as.integer(firstConvictionYear_min)) %>%
#   filter(!is.na(firstConvictionYear_min) ) %>% 
#   arrange(ccode_Accused, firstConvictionYear_min) %>% 
#   group_by(ccode_Accused, firstConvictionYear_min) %>% 
#   mutate(convict_HRs = sum(HRs) , 
#          convict_PostAuto = sum(fitsPostAutocraticTJ) , 
#          convict_Conflict = sum(fitsConflictTJ), 
#          convict_unionFit = sum(fitsBoth) ) %>% 
#   select(ccode_Accused, firstConvictionYear_min, convict_HRs, 
#          convict_PostAuto, convict_Conflict, 
#          convict_unionFit) %>% 
#   distinct() %>% 
#   rename(ccode = ccode_Accused, 
#          year = firstConvictionYear_min) %>% 
#   filter(year >= 1970 & year <= 2020)

### merging of old count variables 
# counts <- full_join(trial_counts, conviction_counts, by = c("ccode", "year")) %>% 
#   mutate(ccode = ifelse(ccode == 679 & year < 1990, 678, ccode)) # Yemen YAR

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

df <- readRDS(here::here("data", "cy_covariates.rds"))
not <- c("cid_who", "ldc", "lldc", "sids", "income_wb", "region_wb2")
first <- c("country", "country_case", "year", "ccode_cow", "ccode_ksg", 
           "country_id_vdem", "country_name", "histname", "m49", "isoa3", 
           "iso3c_wb", "region", "subregion", "intregion", "region_wb", 
           "micro_ksg")
then <- names(df)[!names(df) %in% c(first, not)]
df <- df %>% 
  select(all_of(first), all_of(then)) %>% 
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
              select(-cyID, -country, -country_case, -country_label, 
                     -ccode_case, -beg, -end, -region, -tjet_focus), 
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
  # filter(year <= 2020) %>% 
  mutate(sample_trans = ifelse(transition == 1, year, NA),
         sample_confl = ifelse(conflict_active == 1, year, NA), 
         # active_confl = sample_confl,
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
  ### the next line was merging in the old counts 
  ### and the remaining code deals with those 
  # left_join(counts, by = c("ccode_cow" = "ccode", "year" = "year") ) %>%
  ## losing Timor Leste prior to 2002; should be incorporated into Indonesia
  ### filling in NAs
  # mutate(across(all_of(c("trials_HRs", "trials_PostAuto", "trials_Conflict",
  #                        "convict_HRs", "convict_PostAuto",
  #                        "convict_Conflict")),
  #               function(x) ifelse(is.na(x), 0, x))) %>% 
  arrange(country_case, year) %>%
  group_by(country_case, isna = is.na(theta_mean_fariss) ) %>%
  mutate(cum_theta_mean_fariss = ifelse(isna, NA, cummean(theta_mean_fariss)),
         sample_combi = ifelse(sample_trans + sample_confl > 0, 1, 0) ) %>%
  ungroup() %>%
  select(-isna)

  ### old cumulative measures from here
  # group_by(country_case) %>% 
  # mutate(cum_trials_HRs = cumsum(trials_HRs), 
  #        cum_trials_PostAuto = cumsum(trials_PostAuto), 
  #        cum_trials_Conflict = cumsum(trials_Conflict),
  #        cum_convict_HRs = cumsum(convict_HRs), 
  #        cum_convict_PostAuto = cumsum(convict_PostAuto), 
  #        cum_convict_Conflict = cumsum(convict_Conflict)) %>% 
  # ungroup() %>%
  # group_by(country_case, sample_trans) %>% 
  # mutate(trans_cum_trials_PostAuto = cumsum(trials_PostAuto), 
  #        trans_cum_trials_PostAuto = ifelse(sample_trans == 0, 
  #                                           0, trans_cum_trials_PostAuto),
  #        trans_cum_convict_PostAuto = cumsum(convict_PostAuto), 
  #        trans_cum_convict_PostAuto = ifelse(sample_trans == 0, 
  #                                            0, trans_cum_convict_PostAuto)) %>% 
  # ungroup() %>% 
  # group_by(country_case, sample_confl) %>% 
  # mutate(confl_cum_trials_Conflict = cumsum(trials_Conflict), 
  #        confl_cum_trials_Conflict = ifelse(sample_confl == 0, 
  #                                           0, confl_cum_trials_Conflict),
  #        confl_cum_convict_Conflict = cumsum(convict_Conflict), 
  #        confl_cum_convict_Conflict = ifelse(sample_confl == 0, 
  #                                            0, confl_cum_convict_Conflict)) %>% 
  # ungroup() %>% 
  # group_by(country_case, sample_combi) %>% 
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

### trials 

measures <- c(trs = "trials started", tro = "trials ongoing", 
              tfc = "trials with final convictions", cct = "conviction count", 
              crt = "conviction rate by all accused", sen = "sentence totals")

## for dev only
# df <- df %>%
#   select(country, ccode_cow, year)

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

df <- TrialsMeasure(cy = df, measure = "trs", type_opts = "dom", nexus_vars = "hrs", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tro", type_opts = "dom", nexus_vars = "hrs", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "tfc", type_opts = "dom", nexus_vars = "hrs", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "cct", type_opts = "dom", nexus_vars = "hrs", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "crt", type_opts = "dom", nexus_vars = "hrs", memb_opts = "opp") 
df <- TrialsMeasure(cy = df, measure = "sen", type_opts = "dom", nexus_vars = "hrs", memb_opts = "opp") 

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

df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_all", 
                start_year_var = "yearBeginOperation", 
                nexus_vars = "fitsConflictTJ", crimes_vars = "all", 
                independence_opts = NULL, aims_opts = NULL, consult_vars = NULL, 
                powers_vars = NULL, testimony_vars = NULL, 
                reports_vars = NULL, recommend_vars = NULL, 
                monitor_vars = NULL) %>% 
  select(-tcs_ctj_all) 

df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_victim_process", 
                start_year_var = "yearBeginOperation", 
                nexus_vars = "fitsConflictTJ", crimes_vars = "all",
                aims_opts = c("truth for victims", "memorialization", "apology",
                              "recognition of victims", "reparation"), 
                independence_opts = NULL, consult_vars = "consultedVictims", 
                powers_vars = "allocateReparations", 
                testimony_vars = "encourageVictimTestimony", 
                reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL)
df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_victim_outcome", 
                start_year_var = "yearCompleteOperation",
                nexus_vars = "fitsConflictTJ", crimes_vars = "all",
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
                powers_vars = NULL, testimony_vars = NULL, 
                reports_vars = "reportPubliclyAvailable",
                recommend_vars = "recommendReparations",
                monitor_vars = "mandatePeriodicMonitoringImplementation") 

df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_account_process", 
                start_year_var = "yearBeginOperation", 
                nexus_vars = "fitsConflictTJ", crimes_vars = "all",
                aims_opts = c("accountability", "responsibility",
                              "prevention of human rights violations"),
                independence_opts = c("partially independent", 
                                      "fully independent"), 
                consult_vars = NULL, 
                powers_vars = c("compelTestimony", "supportProsecutions", 
                                "namePerpetrators"),
                testimony_vars = "perpetratorTestimony",
                reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) 
df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_account_outcome", 
                start_year_var = "yearCompleteOperation",
                nexus_vars = "fitsConflictTJ", crimes_vars = "all",
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
                powers_vars = NULL, testimony_vars = NULL, 
                reports_vars = "reportPubliclyAvailable",
                recommend_vars = "recommendProsecutions",
                monitor_vars = "mandatePeriodicMonitoringImplementation")

df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_peace_process", 
                start_year_var = "yearBeginOperation", 
                nexus_vars = "fitsConflictTJ", crimes_vars = "all",
                aims_opts = c("reconciliation", "coexistence", "dialogue", 
                              "non-recurrence"),
                independence_opts = NULL, consult_vars = NULL,
                powers_vars = "grantAmnesty",
                testimony_vars = "heldPublicHearings",
                reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) 
df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_peace_outcome", 
                start_year_var = "yearCompleteOperation",
                nexus_vars = "fitsConflictTJ", crimes_vars = "all",
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
                powers_vars = NULL, testimony_vars = NULL, 
                reports_vars = "reportPubliclyAvailable",
                recommend_vars = NULL, monitor_vars = NULL)

df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_reform_process", 
                start_year_var = "yearBeginOperation", 
                nexus_vars = "fitsConflictTJ", crimes_vars = "all",
                aims_opts = c("historial truth", "institutional reform", 
                              "addressing corruption"),
                independence_opts = c("partially independent", 
                                      "fully independent"), 
                consult_vars = NULL,
                powers_vars = "recommendInstitutionalReforms",
                testimony_vars = "heldPublicHearings",
                reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL) 
df <- TCmeasure(cy = df, new_col_name = "tcs_ctj_reform_outcome", 
                start_year_var = "yearCompleteOperation",
                nexus_vars = "fitsConflictTJ", crimes_vars = "all",
                aims_opts = NULL, independence_opts = NULL, consult_vars = NULL, 
                powers_vars = NULL, testimony_vars = NULL, 
                reports_vars = "reportPubliclyAvailable",
                recommend_vars = "reportRecommendInstitutionalReform", 
                monitor_vars = "mandatePeriodicMonitoringImplementation")

first <- c("country", "country_case", "year", "ccode_cow", # "ccode_case", 
           "ccode_ksg", "country_id_vdem", "country_name", "histname", "m49", 
           "isoa3", "iso3c_wb", "region", "subregion", "intregion", 
           "region_wb", "micro_ksg")
not <- c(not, "regime_sample", "reg_democ", "reg_autoc", "reg_trans", # website
         "transition", "conflict", "conflict_active") 
samples <- c("sample_trans", "sample_confl", "sample_combi", 
             "dtr", "aco", "dco", "pco") # dtr = sample_trans; aco = sample_confl
then <- names(df)[!names(df) %in% c(first, samples, not)]

df <- df %>% 
  select(all_of(first), all_of(samples), all_of(then))

# rm(trial_counts, conviction_counts, counts)

### last step, created lags and saving the analyses dataset

lags <- df %>%
  select(-country, -ccode_cow, -ccode_ksg, -country_id_vdem, 
         -country_name, -histname, -m49, -isoa3, -iso3c_wb, -micro_ksg, 
         -region, -subregion, -intregion, -region_wb, -sample_trans, 
         -sample_confl, -sample_combi, -dtr, -aco, -dco, -pco) %>%
  mutate(year = year + 1) %>%
  rename_with(~ paste0("lag_", .x))

df %>%
  left_join(lags, 
            by = c("country_case" = "lag_country_case", 
                   "year" = "lag_year")) %>% 
  write_csv(here::here("tjet_datasets", "tjet_cy_analyses.csv"), na = "") %>% 
  write_csv(here::here(dropbox_path, "tjet_cy_analyses.csv"), na = "")

### downloads datasets 
# - include country IDs, transitions and our data
# - everything in our filters, but not other outcomes
# - variables that should not be in public CY downloads file?
#   - regime_sample, reg_democ, reg_autoc, reg_trans, conflict, transition

codebook <- db[["codebook"]] %>% 
  filter(tables == "tjet_cy_analyses.csv") %>% 
  filter(colname != "lag_*") %>% 
  # filter(colname %in% c("sample_trans", "sample_confl", "sample_combi") ) %>% 
  filter(is.na(source) | 
           source %in% c("TJET", "COW", "Kristian S. Gleditsch", 
                         "UN Statistics Division", "World Bank") | 
           colname %in% c("country_id_vdem", "country_name", "histname") ) %>% 
  select(colname, definition, source) %>% 
  left_join(read_csv(here::here("data", "sources.csv")), 
            by = "source") %>% 
  write_csv(here::here("tjet_datasets", "tjet_codebook.csv"), na = "")
# codebook$colname[!codebook$colname %in% names(df)]

df %>%
  select(all_of(codebook$colname)) %>% 
  write_csv(here::here("tjet_datasets", "tjet_cy.csv"), na = "")
