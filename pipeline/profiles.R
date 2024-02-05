### packages
library("tidyverse")
library("readxl")
library("writexl")

load(here::here("data", "tjetdb.RData"))

data <- list()

countries <- db[["Countries"]] %>% 
  filter(!country %in% c("Serbia and Montenegro", "Soviet Union", "Yugoslavia") ) %>% 
  select(country, country_case, ccode) 

# db[["Trials"]] %>% 
#   mutate(HRs_charges = ifelse(HRs_charges > 0, 1, 0) ) %>%  
#   select(trialID, humanRights, IntraConfl, HRs_charges, 
#          fitsPostAutocraticTJ, fitsConflictTJ, 
#          beganDuringIntraConfl, beganAfterIntraConfl) %>% 
#   select(humanRights, fitsConflictTJ) %>% 
#   table()

data[["Amnesties"]] <- db[["Amnesties"]] %>%
  left_join(countries, by = c(ccode_cow = "ccode")) %>% 
  filter(amnestyYear <= 2020) %>%
  mutate(what_hrv = ifelse(str_detect(whatCrimes, "human rights violations"), 1, 0), 
         who_pol = ifelse(str_detect(whoWasAmnestied, "protesters / political prisoners"), 1, 0)) %>% 
  group_by(country_case) %>%
  mutate(count_all = n(), 
         count_demtrans = sum(fitsPostAutocraticTJ),
         count_conflict = sum(fitsConflictTJ),
         count_dcj = sum(dcj),
         count_pcj = sum(pcj),
         count_peaceagree = sum(peaceSettlement), 
         count_prisoners = sum(who_pol),
         count_hrv = sum(what_hrv),
         ) %>%
  ungroup() %>%
  select(country_case, count_all, count_demtrans, count_conflict, count_dcj, 
         count_pcj, count_peaceagree, count_prisoners, count_hrv) %>%
  rename(country = "country_case") %>%
  distinct() %>%
  arrange(country)

data[["Trials"]] <- db[["dl_tjet_cy"]] %>% 
  select(-country) %>% 
  rename(country = "country_case") %>%
  arrange(country, year) %>% 
  group_by(country) %>% 
  reframe(trials_domestic = sum(trials_domestic), 
          xord_trs_dom_dtj_sta = sum(xord_trs_dom_dtj_sta),
          xord_trs_dom_ctj_sta = sum(xord_trs_dom_ctj_sta),
          ordy_trs_dom_sta = sum(ordy_trs_dom_sta),
          tfc_dom_hrs_con_sta = sum(xord_tfc_dom_dtj_ctj_sta + ordy_tfc_dom_sta),
          xord_trs_dom_dtj_ctj_sta_hi = sum(xord_trs_dom_dtj_ctj_sta_hi),
          xord_tfc_dom_dtj_ctj_sta_hi = sum(xord_tfc_dom_dtj_ctj_sta_hi),
          xord_trs_dom_ctj_opp = sum(xord_trs_dom_ctj_opp),
          trials_intl = sum(trials_intl),
          trs_int_hrs_con_all = sum(trs_int_sta + trs_int_opp), 
          tfc_int_hrs_con_all = sum(tfc_int_sta + tfc_int_opp),
          trials_foreign = sum(trials_foreign),
          trs_for_hrs_con_all = sum(trs_for_sta + trs_for_opp), 
          tfc_for_hrs_con_all = sum(tfc_for_sta + tfc_for_opp))

data[["Foreign"]] <- db[["Trials"]] %>%
    left_join(countries, by = c(ccode_Accused = "ccode")) %>%
    rename(countryAccused = country) %>%
    left_join(countries, by = c(ccode_Trial = "ccode")) %>%
    rename(countryTrial = country) %>%
    filter(trialType == "foreign") %>% 
    select(trialID, countryAccused, ccode_Accused, countryTrial, ccode_Trial, yearStart, yearEnd, caseDescription) %>% 
    arrange(countryAccused, yearStart)
  
data[["Reparations"]] <- db[["Reparations"]] %>%
  left_join(countries, by = c(ccode_cow = "ccode")) %>% 
  filter(yearCreated <= 2020) %>%
  group_by(country_case) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  select(country_case, count, reparationID, yearCreated, yearBegin, yearEnd, 
           individualReparations, collectiveReparations, beneficiariesCount) %>%
  rename(country = "country_case") %>%
  arrange(country, yearCreated)

data[["TruthCommissions"]] <- db[["TruthCommissions"]] %>%
  left_join(countries, by = c(ccode_cow = "ccode")) %>% 
  filter(yearPassed <= 2020) %>%
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
         "reform_vetting" = "vetting", 
         ) %>% 
  select(country_case, count, truthcommissionID, yearPassed, yearBeginOperation, 
         yearCompleteOperation, finalReportIssued, reportPubliclyAvailable,
         rec_prosecutions, rec_reparations, rec_reforms, reform_HRs, 
         reform_legal, reform_judicial, reform_gender, reform_corruption, 
         reform_SSR, reform_vetting) %>%
  rename(country = "country_case") %>%
  arrange(country, yearPassed)

data[["Vettings"]] <- db[["Vettings"]] %>%
  left_join(countries, by = c(ccode_cow = "ccode")) %>% 
  filter(yearStart <= 2020) %>%
  mutate(individual_conduct = case_when(
    str_detect(targetingWhy, "specific individual conduct") ~ 1,
    TRUE ~ 0)) %>%
  select(country_case, vettingID, alterationOf, yearStart, yearEnd, 
         individual_conduct, type_dismissal, type_ban, type_declassification, 
         type_perjury, numberInvestigated, dateLaw) %>%
  rename(country = "country_case") %>%
  arrange(country, yearStart)

data[["ICC-interventions"]] <- db[["ICC"]] %>% 
  select(country, ccode_cow, ICC_referral, ICC_prelim_exam, ICC_prelimEnd, ICC_investigation) %>%
  arrange(country, ICC_prelim_exam) 

data[["ICC-accused"]] <- db[["ICCaccused"]] %>% 
  select(country, nameOrDesc, 
         ICC_arrest_warrant, ICC_arrestAppear, ICC_confirm_charges, 
         ICC_proceedings, ICC_withdrawnDismissed, trialID, accusedID, ccode_Crime) %>%
  arrange(country, ICC_arrest_warrant) 

data[["UN-investigations"]] <- db[["Investigations"]] %>% 
  select(country, beg, end, mandate, goals, ccode_cow) %>%
  arrange(country, beg) 

### peace agreements: "https://peaceaccords.nd.edu/wp-content/uploads/2019/08/PAM_ID-V.1.5-Updated-29JULY2015.xlsx"
data[["peace-agreements"]] <- readxl::read_xlsx("data/PAM_ID-V.1.5-Updated-29JULY2015.xlsx") %>% 
  rename(accord_name = "accord name") %>%
  group_by(pam_caseid) %>% 
  mutate(beg = min(year), 
         end = max(year)) %>% 
  ungroup() %>%
  mutate(war_start = as_date(war_start), 
         cease_date = as_date(cease_date)) %>%  
  select(pam_caseid, country, cowcode, accord_name, 
         war_start, cease_date, beg, end, 
         amnest_prov, humrts_prov, prisr_prov, repar_prov, truth_prov) %>%
  distinct() %>%
  arrange(country, cease_date)


### PAX: https://www.peaceagreements.org/search
data[["PAX"]] <- read_csv("data/pax_all_agreements_data_v6.csv") %>%
  select(Con, PP, PPName, AgtId, Agt, Dat, Status, Agtp, Stage, Loc1GWNO, 
         Loc2GWNO, UcdpCon, UcdpAgr, PamAgr, 
         TjGen, TjAm, TjAmPro, TjSan, TjPower, TjAmBan, TjCou, TjJaNc, TjJaIc, 
         TjMech, TjPrire, TjVet, TjVic, TjMis, TjRep, TjRSym, TjRMa, TjNR)

### write to file 
write_xlsx(data, path = "~/Dropbox/TJLab/TimoDataWork/country_profiles/summary_data.xlsx")

### load the raw data tables
load(here::here("data", "tjet.RData"), verbose = TRUE)
map(tjet, names)

df <- tjet[["MegaBase"]][["Countries"]] %>% 
  tibble() %>%
  filter(include) %>%
  mutate(focus = ifelse(is.na(focus), 0, 1) ) %>%
  select(country, ccode, ccode_case, beg, end, focus, 
         txt_intro, txt_regime, txt_conflict, txt_TJ) %>%
  arrange(country)

dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/")
dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/original/")
dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/edits/")
dir("~/Dropbox/TJLab/TimoDataWork/country_profiles")

map(df$country, function(ctry) {
  dir.create(paste("~/Dropbox/TJLab/TimoDataWork/country_profiles/original", ctry, sep = "/"))
  dir.create(paste("~/Dropbox/TJLab/TimoDataWork/country_profiles/edits", ctry, sep = "/"))
  df %>% 
    filter(country == ctry) %>%
    select(txt_intro) %>%
    unlist(use.names = FALSE) %>%
    write_file(file = paste("~/Dropbox/TJLab/TimoDataWork/country_profiles/original/", ctry, "/txt_intro.md", sep = ""))
  df %>% 
    filter(country == ctry) %>%
    select(txt_regime) %>%
    unlist(use.names = FALSE) %>%
    write_file(file = paste("~/Dropbox/TJLab/TimoDataWork/country_profiles/original/", ctry, "/txt_regime.md", sep = ""))
  df %>% 
    filter(country == ctry) %>%
    select(txt_conflict) %>%
    unlist(use.names = FALSE) %>%
    write_file(file = paste("~/Dropbox/TJLab/TimoDataWork/country_profiles/original/", ctry, "/txt_conflict.md", sep = ""))
  df %>% 
    filter(country == ctry) %>%
    select(txt_TJ) %>%
    unlist(use.names = FALSE) %>%
    write_file(file = paste("~/Dropbox/TJLab/TimoDataWork/country_profiles/original/", ctry, "/txt_TJ.md", sep = ""))
})

file.copy(from = list.files("~/Dropbox/TJLab/TimoDataWork/country_profiles/original", full.names = TRUE), 
          to = "~/Dropbox/TJLab/TimoDataWork/country_profiles/edits/", 
          overwrite = FALSE, recursive = TRUE, copy.mode = FALSE)  


dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/focus/")
df %>% 
  filter(focus == 1 | country == "Uganda") %>%  
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
  })
  