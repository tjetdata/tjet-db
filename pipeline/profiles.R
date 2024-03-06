### packages
library("tidyverse")
library("readxl")
library("writexl")

options(scipen = 999) 

load(here::here("data", "tjetdb.RData"))

countries <- db[["Countries"]] %>%
    # filter(include) %>% 
    filter(!country %in% c("Serbia and Montenegro",
                         "Soviet Union",
                         "Yugoslavia") ) %>%
  select(country, country_case, ccode, ccode_case, ccode_ksg)

# countries %>% 
#   select(country_case, ccode_case) %>% 
#   distinct() %>% 
#   group_by(ccode_case) %>% 
#   mutate(n = n()) %>% 
#   filter(n > 1) 

autotxt <- temp <- data <- list()

### summaries prep

temp$amnesties <- db[["Amnesties"]] %>%
  select(ccode_cow, amnestyID, amnestyYear) %>%
  left_join(countries %>% 
              select(ccode, ccode_case), 
            by = c("ccode_cow" = "ccode")) %>% 
  group_by(ccode_case) %>%
  reframe(amnesties = n(), 
          amnesties_yrs = list(unique(amnestyYear)))

temp$trials <- db[["Trials"]] %>%
  select(ccode_Accused, trialID, trialType, yearStart) %>%
  left_join(countries %>% 
              select(ccode, ccode_case), 
            by = c("ccode_Accused" = "ccode")) %>% 
  select(-ccode_Accused) 

temp$domestic <- temp$trials %>%
  filter(is.na(trialType) |
           trialType %in% c("domestic", "national", 
                            "don't know", "ne sait pas")) %>%
  group_by(ccode_case) %>%
  reframe(domestic = n(), 
          domestic_yrs = list(unique(yearStart)))

temp$foreign <- temp$trials %>%
  filter(trialType %in% c("foreign", "étranger")) %>%
  group_by(ccode_case) %>%
  reframe(foreign = n(), 
          foreign_yrs = list(unique(yearStart)))

temp$intl <- temp$trials %>%
  filter(trialType %in% c("international", "international (hybrid)", 
                          "international (hybride)")) %>%
  group_by(ccode_case) %>%
  reframe(intl = n(), 
          intl_yrs = list(unique(yearStart)))

temp$reparations <- db[["Reparations"]] %>%
  select(ccode_cow, reparationID, yearCreated) %>%
  left_join(countries %>% 
              select(ccode, ccode_case), 
            by = c("ccode_cow" = "ccode")) %>% 
  group_by(ccode_case) %>%
  reframe(reparations = n(), 
          reparations_yrs = list(unique(yearCreated)))

temp$tcs <- db[["TruthCommissions"]] %>%
  select(ccode_cow, truthcommissionID, yearPassed) %>%
  left_join(countries %>% 
              select(ccode, ccode_case), 
            by = c("ccode_cow" = "ccode")) %>% 
  group_by(ccode_case) %>%
  reframe(tcs = n(), 
          tcs_yrs = list(unique(yearPassed)))

temp$vettings <- db[["Vettings"]] %>%
  select(ccode_cow, vettingID, yearStart) %>%
  left_join(countries %>% 
              select(ccode, ccode_case), 
            by = c("ccode_cow" = "ccode")) %>% 
  group_by(ccode_case) %>%
  reframe(vettings = n(), 
          vettings_yrs = list(unique(yearStart)))
 
temp <- temp[names(temp) != "trials"]

temp$summary <- reduce(temp, 
                       function(x, y) full_join(x, y, by = "ccode_case")) %>% 
  mutate(amnesties = ifelse(is.na(amnesties), 0, amnesties), 
         domestic = ifelse(is.na(domestic), 0, domestic), 
         foreign = ifelse(is.na(foreign), 0, foreign), 
         intl = ifelse(is.na(intl), 0, intl), 
         reparations = ifelse(is.na(reparations), 0, reparations), 
         tcs = ifelse(is.na(tcs), 0, tcs), 
         vettings = ifelse(is.na(vettings), 0, vettings)) %>%
  left_join(countries %>% 
              select(country_case, ccode_case) %>% 
              distinct(), 
            by = "ccode_case") %>%
  arrange(country_case) 

temp$transitions <- db[["Transitions"]] %>% 
    left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode = "ccode")) %>%
  select(country_case, ccode_case, trans_year_begin) %>%
  arrange(country_case, trans_year_begin) %>%
  group_by(country_case) %>% 
  reframe(ccode_case = unique(ccode_case), 
          trans_year_begin = list(unique(trans_year_begin)))

temp$conflicts <- db[["ConflictDyads"]] %>% 
  mutate(internationalized = ifelse(str_detect(intensity, "Internationalized"), 1, 0)) %>% 
  rowwise() %>% 
  mutate(years = list(ep_start_year:ep_end_year) ) %>% 
  ungroup() %>% 
  select(dyad_id, conflict_id, gwno_loc, ep_start_year, ep_end_year, years, internationalized) %>%
  left_join(countries %>% 
              select(country_case, ccode_ksg, ccode_case) %>% 
              distinct(), 
            by = c("gwno_loc" = "ccode_ksg")) %>%
  select(country_case, ccode_case, dyad_id, conflict_id, years, internationalized) %>%
  group_by(country_case) %>%
  reframe(ccode_case = unique(ccode_case), 
          conflicts = length(unique(conflict_id)), 
          dyads = length(unique(dyad_id)), 
          episodes = n(),
          years = list(sort(unique(unlist(years)))),
          years = list(unlist(years)[unlist(years) %in% 1970:2020]),
          int_ep = sum(internationalized))

### data for summary spreadsheet

data[["Amnesties"]] <- db[["Amnesties"]] %>%
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  mutate(what_hrv = ifelse(str_detect(whatCrimes, 
                                      "human rights violations"), 1, 0), 
         who_pol = ifelse(str_detect(whoWasAmnestied, 
                                     "protesters / political prisoners"), 1, 0)) %>% 
  group_by(country_case) %>%
  mutate(beg = min(amnestyYear), 
         end = max(amnestyYear), 
         count_all = n(), 
         count_demtrans = sum(fitsPostAutocraticTJ),
         count_conflict = sum(fitsConflictTJ),
         count_dcj = sum(dcj),
         count_pcj = sum(pcj),
         count_peaceagree = sum(peaceSettlement), 
         count_prisoners = sum(who_pol),
         count_hrv = sum(what_hrv),
         ) %>%
  ungroup() %>%
  select(country_case, ccode_case, beg, end, count_all, count_demtrans, count_conflict, count_dcj, 
         count_pcj, count_peaceagree, count_prisoners, count_hrv) %>%
  distinct() %>%
  arrange(country_case) %>%
  print(n = Inf) 

vars_dom <- c("xord_trs_dom_dtj_sta", "xord_trs_dom_ctj_sta", 
              "xord_trs_dom_dtj_ctj_sta", "ordy_trs_dom_sta", 
              "xord_tfc_dom_dtj_ctj_sta", "ordy_tfc_dom_sta", 
              "xord_trs_dom_dtj_ctj_sta_hi", "xord_tfc_dom_dtj_ctj_sta_hi", 
              "xord_trs_dom_ctj_opp", "xord_tfc_dom_ctj_opp", 
              "lcon_trs_dom_sta_opp", "lcon_tfc_dom_sta_opp")
vars_int <- c("trials_intl", "trs_int_sta", "trs_int_opp", 
              "tfc_int_sta", "tfc_int_opp") 
vars_for <- c("trials_foreign", "trs_for_sta", "trs_for_opp", 
              "tfc_for_sta", "tfc_for_opp") 

data[["Domestic_totals"]] <- db[["dl_tjet_cy"]] %>% 
  select(-country_case) %>% 
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  arrange(country_case, year) %>% 
  filter(if_any(all_of(vars_dom), ~ . > 0)) %>%
  group_by(country_case) %>% 
  reframe(ccode_case = unique(ccode_case), 
          beg = min(year), 
          end = max(year),
          total = sum(xord_trs_dom_dtj_ctj_sta + ordy_trs_dom_sta + xord_trs_dom_ctj_opp + lcon_trs_dom_sta_opp), 
          xord_trs_dom_dtj_sta = sum(xord_trs_dom_dtj_sta), 
          xord_trs_dom_ctj_sta = sum(xord_trs_dom_ctj_sta), 
          xord_trs_dom_dtj_ctj_sta = sum(xord_trs_dom_dtj_ctj_sta), 
          xord_tfc_dom_dtj_ctj_sta = sum(xord_tfc_dom_dtj_ctj_sta), 
          xord_trs_dom_dtj_ctj_sta_hi = sum(xord_trs_dom_dtj_ctj_sta_hi), 
          xord_tfc_dom_dtj_ctj_sta_hi = sum(xord_tfc_dom_dtj_ctj_sta_hi), 
          ordy_trs_dom_sta = sum(ordy_trs_dom_sta), 
          ordy_tfc_dom_sta = sum(ordy_tfc_dom_sta), 
          xord_trs_dom_ctj_opp = sum(xord_trs_dom_ctj_opp), 
          xord_tfc_dom_ctj_opp = sum(xord_tfc_dom_ctj_opp), 
          lcon_trs_dom_sta_opp = sum(lcon_trs_dom_sta_opp), 
          lcon_tfc_dom_sta_opp = sum(lcon_tfc_dom_sta_opp))

data[["Intl_totals"]] <- db[["dl_tjet_cy"]] %>% 
  select(-country_case) %>% 
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  arrange(country_case, year) %>% 
  filter(if_any(all_of(vars_int), ~ . > 0)) %>%
  group_by(country_case) %>% 
  reframe(ccode_case = unique(ccode_case), 
          beg = min(year), 
          end = max(year),
          trials_intl = sum(trials_intl),
          trs_int_hrs_con_all = sum(trs_int_sta + trs_int_opp), 
          tfc_int_hrs_con_all = sum(tfc_int_sta + tfc_int_opp))

data[["Foreign_totals"]] <- db[["dl_tjet_cy"]] %>% 
  select(-country_case) %>% 
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  arrange(country_case, year) %>% 
  filter(if_any(all_of(vars_for), ~ . > 0)) %>%
  group_by(country_case) %>% 
  reframe(ccode_case = unique(ccode_case), 
          beg = min(year), 
          end = max(year), 
          trials_foreign = sum(trials_foreign),
          trs_for_hrs_con_all = sum(trs_for_sta + trs_for_opp), 
          tfc_for_hrs_con_all = sum(tfc_for_sta + tfc_for_opp)) 

data[["Foreign"]] <- db[["Trials"]] %>%
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_Accused = "ccode")) %>%
  select(-ccode_Accused) %>%  
  rename("countryAccused" = "country_case", 
         "ccode_Accused" = "ccode_case") %>%
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_Trial = "ccode")) %>%
  select(-ccode_Trial) %>%  
  rename("countryTrial" = "country_case", 
         "ccode_Trial" = "ccode_case") %>%
  filter(trialType == "foreign") %>% 
  select(trialID, countryAccused, ccode_Accused, countryTrial, ccode_Trial, yearStart, yearEnd, caseDescription) %>% 
  arrange(countryAccused, yearStart) 

data[["Reparations"]] <- db[["Reparations"]] %>%
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  group_by(country_case) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  select(country_case, ccode_case, count, reparationID, yearCreated, yearBegin, yearEnd, 
         individualReparations, collectiveReparations, beneficiariesCount) %>%
  arrange(country_case, yearCreated)

data[["TruthCommissions"]] <- db[["TruthCommissions"]] %>%
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
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
         "reform_vetting" = "vetting") %>% 
  select(country_case, ccode_case, count, truthcommissionID, yearPassed, 
         yearBeginOperation, yearCompleteOperation, finalReportIssued, 
         reportPubliclyAvailable, rec_prosecutions, rec_reparations, 
         rec_reforms, reform_HRs, reform_legal, reform_judicial, reform_gender, 
         reform_corruption, reform_SSR, reform_vetting, consultedVictims, 
         commissionersVictimGroups, encourageVictimTestimony
  ) %>%
  arrange(country_case, yearPassed)

data[["Vettings"]] <- db[["Vettings"]] %>%
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  mutate(
    individual_conduct = case_when(
      str_detect(targetingWhy, "specific individual conduct") ~ 1,
      TRUE ~ 0)) %>%
  select(country_case, ccode_case, vettingID, alterationOf, yearStart, yearEnd, 
         individual_conduct, type_dismissal, type_ban, type_declassification, 
         type_perjury, numberInvestigated, dateLaw) %>%
  arrange(country_case, ccode_case, yearStart)

data[["ICC-interventions"]] <- db[["ICC"]] %>% 
  select(-country) %>%  
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  select(country_case, ccode_case, ICC_referral, ICC_prelim_exam, ICC_prelimEnd, ICC_investigation) %>%
  arrange(country_case, ICC_prelim_exam) 

data[["ICC-accused"]] <- db[["ICCaccused"]] %>% 
  select(-country) %>%  
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_Crime = "ccode")) %>% 
  select(country_case, ccode_case, nameOrDesc, ICC_arrest_warrant, 
         ICC_arrestAppear, ICC_confirm_charges, ICC_proceedings, 
         ICC_withdrawnDismissed, trialID, accusedID) %>%
  arrange(country_case, ICC_arrest_warrant) 

data[["Investigations"]] <- db[["Investigations"]] %>% 
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(ccode_cow = "ccode")) %>% 
  select(country, country_case, ccode_case, beg, end, mandate, 
         uninv_dompros, uninv_evcoll, uninv_intlpros, goals) %>%
  arrange(country_case, beg)

### peace agreements: "https://peaceaccords.nd.edu/wp-content/uploads/2019/08/PAM_ID-V.1.5-Updated-29JULY2015.xlsx"
data[["peace-agreements"]] <- readxl::read_xlsx("data/PAM_ID-V.1.5-Updated-29JULY2015.xlsx") %>% 
  rename("accord_name" = "accord name") %>%
  group_by(pam_caseid) %>% 
  mutate(beg = min(year), 
         end = max(year)) %>% 
  ungroup() %>%
  mutate(war_start = as_date(war_start), 
         cease_date = as_date(cease_date)) %>%
  left_join(countries %>% 
              select(country_case, ccode, ccode_case), 
            by = c(cowcode = "ccode")) %>% 
  
  select(pam_caseid, country_case, ccode_case, accord_name, war_start, 
         cease_date, beg, end, amnest_prov, humrts_prov, prisr_prov, repar_prov, 
         truth_prov) %>%
  distinct() %>%
  arrange(country_case, cease_date)

### PAX: https://www.peaceagreements.org/search
tj_vars <- c("TjGen", "TjAm", "TjAmPro", "TjSan", "TjPower", "TjCou", "TjJaNc", 
             "TjJaIc", "TjMech", "TjPrire", "TjVet", "TjVic", "TjMis", "TjRep", 
             "TjRSym", "TjRMa", "TjNR")
## have not adjusted this to ccode_case
data[["PAX"]] <- read_csv("data/pax_all_agreements_data_v6.csv") %>% 
  arrange(Con, PPName, Dat) %>% 
  filter(Stage %in% c("SubComp", "SubPar")) %>% 
  filter(if_any(all_of(tj_vars), ~ . == 1)) %>%
  select(Con, PP, PPName, AgtId, Agt, Dat, Status, Agtp, Stage, Loc1GWNO, 
         Loc2GWNO, UcdpCon, UcdpAgr, PamAgr, all_of(tj_vars)) 

### write to file 
write_xlsx(data, path = "~/Dropbox/TJLab/TimoDataWork/country_profiles/summary_data.xlsx")

rm(tj_vars, vars_dom, vars_for, vars_int) 

### automating written summaries 

n_transform <- function(x) {
  x %>% 
    paste(" ", ., sep = "") %>%  
    str_replace_all(" 0 ", " none ") %>% 
    str_replace_all(" 1 ", " one ") %>% 
    str_replace_all(" 2 ", " two ") %>% 
    str_replace_all(" 3 ", " three ") %>% 
    str_replace_all(" 4 ", " four ") %>% 
    str_replace_all(" 5 ", " five ") %>% 
    str_replace_all(" 6 ", " six ") %>% 
    str_replace_all(" 7 ", " seven ") %>% 
    str_replace_all(" 8 ", " eight ") %>% 
    str_replace_all(" 9 ", " nine ") %>% 
    str_replace_all(" 10 ", " ten ") %>%
    str_replace_all(" 11 ", " eleven ") %>% 
    str_replace_all(" 12 ", " twelve ") %>% 
    str_trim() %>%
    return() 
}

### summary 

autotxt[["intro"]] <- temp[["summary"]] %>%
  rowwise() %>%
  mutate(
    text = paste("For ", 
                 country_case, 
                 ", TJET has collected information on: ",  
                 str_flatten(
                   c(
                     if(amnesties > 0) paste(amnesties, 
                                             ifelse(amnesties == 1,
                                                    "amnesty", "amnesties"), 
                                             ifelse(min(amnesties_yrs) == max(amnesties_yrs), 
                                                    paste("in", unique(amnesties_yrs)), 
                                                    paste("between", 
                                                          min(amnesties_yrs), 
                                                          "and", 
                                                          max(amnesties_yrs) )) ), 
                     if(domestic > 0) paste(domestic, "domestic", 
                                            ifelse(domestic == 1,
                                                   "trial", "trials"), 
                                            "starting", 
                                            ifelse(min(domestic_yrs) == max(domestic_yrs), 
                                                   paste("in", unique(domestic_yrs)), 
                                                   paste("between", 
                                                         min(domestic_yrs), 
                                                         "and", 
                                                         max(domestic_yrs) ))), 
                     if(foreign > 0) paste(foreign, "foreign", 
                                           ifelse(foreign == 1,
                                                  "trial", "trials"), 
                                           "starting", 
                                           ifelse(min(foreign_yrs) == max(foreign_yrs), 
                                                  paste("in", unique(foreign_yrs)), 
                                                  paste("between", 
                                                        min(foreign_yrs), 
                                                        "and", 
                                                        max(foreign_yrs) ))),
                     if(intl > 0) paste(intl, "international", 
                                        ifelse(intl == 1,
                                               "trial", "trials"), 
                                        "starting", 
                                        ifelse(min(intl_yrs) == max(intl_yrs), 
                                               paste("in", unique(intl_yrs)), 
                                               paste("between", 
                                                     min(intl_yrs), 
                                                     "and", 
                                                     max(intl_yrs) ))),
                     if(reparations > 0) paste(reparations, "reparations", 
                                               ifelse(reparations == 1,
                                                      "policy", "policies"), 
                                               "created", 
                                               ifelse(min(reparations_yrs) == max(reparations_yrs), 
                                                      paste("in", unique(reparations_yrs)), 
                                                      paste("between", 
                                                            min(reparations_yrs), 
                                                            "and", 
                                                            max(reparations_yrs) )) ), 
                     if(tcs > 0) paste(tcs, "truth", 
                                       ifelse(tcs == 1,
                                              "commission", "commissions"), 
                                       "mandated", 
                                       ifelse(min(tcs_yrs) == max(tcs_yrs), 
                                              paste("in", unique(tcs_yrs)), 
                                              paste("between", 
                                                    min(tcs_yrs), 
                                                    "and", 
                                                    max(tcs_yrs) )) ),
                     if(vettings > 0) paste(vettings, "vetting", 
                                            ifelse(vettings == 1,
                                                   "policy", "policies"), 
                                            "starting", 
                                            ifelse(min(vettings_yrs) == max(vettings_yrs), 
                                                   paste("in", unique(vettings_yrs)), 
                                                   paste("between", 
                                                         min(vettings_yrs), 
                                                         "and", 
                                                         max(vettings_yrs) )) )
                   ), 
                   collapse = "; ", last = "; and "), 
                 ".", sep = "") %>% 
      n_transform(), 
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>% 
  select(country_case, ccode_case, text) 

### transitions 

autotxt[["Transitions"]] <- temp[["transitions"]] %>%
  rowwise() %>%
  mutate(
    text = paste("Based on well-known democracy data, TJET records ", 
                 length(trans_year_begin), 
                 " democratic ", 
                 ifelse(length(trans_year_begin) == 1,
                        "transition", "transitions"), 
                 " starting in ", 
                 str_flatten_comma(trans_year_begin, ", and "), 
                 ".", sep = "") %>% 
      n_transform(), 
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text) 
  
### violent intrastate conflict episodes

autotxt[["Conflicts"]] <- temp[["conflicts"]] %>%
  rowwise() %>%
  mutate(text = "", 
    text = paste("Based on the Uppsala Conflict Data Program, TJET records ",
                 episodes,
                 " violent intrastate conflict ",
                 ifelse(episodes == 1, "episode ", "episodes "),
                 ifelse(min(years) == max(years),
                        paste("in", unique(years)),
                        paste("between",
                              min(years),
                              "and",
                              max(years) )),
                 ifelse(length(years) > 1 & length(years) < max(years) - min(years) + 1, 
                        paste(" (during ", 
                              length(years), 
                              " calendar years)", 
                              sep = ""), 
                        ""), 
                 ", involving ",
                 dyads,
                 ifelse(dyads == 1,
                        " armed opposition group",
                        " distinct armed opposition groups"),
                 " fighting against the government.",
                 sep = "") %>%
      n_transform(),
    text = list(c(text, 
                  if(int_ep > 0) 
                    paste(int_ep, 
                          "conflict", 
                          ifelse(int_ep == 1, "episode was", "episodes were"), 
                          "internationalized by involvement of external state actors.") %>% 
                    n_transform() %>% 
                    str_to_sentence() )), 
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text)

autotxt[["Amnesties"]] <- data[["Amnesties"]] %>% 
  rowwise() %>%
  mutate(
    text = list(paste(country_case, 
                      " had ", 
                      count_all, 
                      ifelse(count_all == 1, " amnesty ", " amnesties "), 
                      ifelse(beg == end, 
                             paste("in", beg), 
                             paste("between", beg, "and", end)), 
                      ".", sep = "") %>% 
                  n_transform() ), 
    text = list(c(text, 
                  if(count_demtrans > 0) 
                    paste(count_demtrans, 
                          "occurred in the context of democratic transition.") %>% 
                    n_transform() %>% 
                    str_to_sentence() )), 
    text = list(c(text, 
                  if(count_dcj > 0) 
                    paste(count_dcj, 
                          ifelse(count_dcj == 1, "was", "were"), 
                          "passed during ongoing internal armed conflict.") %>% 
                    n_transform() %>% 
                    str_to_sentence() )), 
    text = list(c(text, 
                  if(count_pcj > 0) 
                    paste(count_pcj, 
                          ifelse(count_pcj == 1, "was", "were"), 
                          "passed after internal armed conflict.") %>% 
                    n_transform() %>% 
                    str_to_sentence() )), 
    text = list(c(text, 
                  if(count_peaceagree > 0) 
                    paste(count_peaceagree, 
                          ifelse(count_peaceagree == 1, "was", "were"), 
                          "part of a peace agreement.") %>% 
                    n_transform() %>% 
                    str_to_sentence() )), 
    text = list(c(text, 
                  if(count_prisoners > 0) 
                    paste(count_prisoners, 
                          ifelse(count_prisoners == 1, "amnesty", "amnesties"), 
                          "released political prisoners.") %>% 
                    n_transform() %>% 
                    str_to_sentence() )),
    text = list(c(text, 
                  if(count_hrv > 0) 
                    paste(count_hrv, 
                          ifelse(count_hrv == 1, "amnesty", "amnesties"), 
                          "forgave human rights violations.") %>% 
                    n_transform() %>% 
                    str_to_sentence() )), 
    text = str_flatten(text, " ") %>% 
      str_trim()
  ) %>%
  ungroup() %>% 
  select(country_case, ccode_case, text)

autotxt[["Domestic_totals"]] <- data[["Domestic_totals"]] %>%
  rowwise() %>%
  mutate(text = "", 
         text = paste(country_case,
                      ifelse(total == 1, "had", "had a total of"),
                      total,
                      "domestic human rights or conflict",
                      ifelse(total == 1, "prosecution", "prosecutions"),
                      ifelse(beg == end,
                             paste("in ", beg, ".", sep = ""),
                             paste("between ", beg, " and ", end, ".", sep = "")
                             )
                      ) %>%
           n_transform(),
         text = list(c(text,
                       if(xord_trs_dom_dtj_sta > 0)
                         paste("There",
                               ifelse(xord_trs_dom_dtj_sta == 1, "was", "were"),
                               xord_trs_dom_dtj_sta,
                               "extraordinary",
                               ifelse(xord_trs_dom_dtj_sta == 1,
                                      "prosecution of state agents in a democratic transition context.",
                                      "prosecutions of state agents in democratic transition contexts.")
                               ) %>%
                         n_transform())),
         text = list(c(text,
                       if(xord_trs_dom_ctj_sta > 0)
                         paste("There",
                               ifelse(xord_trs_dom_ctj_sta == 1, "was", "were"),
                               xord_trs_dom_ctj_sta,
                               "extraordinary",
                               ifelse(xord_trs_dom_ctj_sta == 1,
                                      "prosecution of state agents in a conflict context.",
                                      "prosecutions of state agents in conflict contexts.")
                               ) %>%
                         n_transform())),
         text = list(c(text,
                       if(xord_tfc_dom_dtj_ctj_sta > 0)
                         paste("There",
                               ifelse(xord_tfc_dom_dtj_ctj_sta == 1, "was", "were"),
                               xord_tfc_dom_dtj_ctj_sta,
                               ifelse(xord_tfc_dom_dtj_ctj_sta == 1,
                                      "final conviction of a state agent in an extraordinary prosecution.",
                                      "final convictions of state agents in extraordinary prosecutions.")) %>%
                         n_transform())),
         text = list(c(text,
                       if(xord_trs_dom_dtj_ctj_sta_hi > 0)
                         paste("Of",
                               xord_trs_dom_dtj_ctj_sta_hi,
                               ifelse(xord_trs_dom_dtj_ctj_sta_hi == 1,
                                      "extraordinary prosecution of high-ranking state agents,",
                                      "extraordinary prosecutions of high-ranking state agents,"),
                               xord_tfc_dom_dtj_ctj_sta_hi,
                               ifelse(xord_tfc_dom_dtj_ctj_sta_hi == 1,
                                      "led to a final conviction.",
                                      "led to final convictions.")
                               ) %>%
                         n_transform())),
         text = list(c(text,
                       if(ordy_trs_dom_sta > 0)
                         paste("There",
                               ifelse(ordy_trs_dom_sta == 1, "was", "were"),
                               ordy_trs_dom_sta,
                               ifelse(ordy_trs_dom_sta == 1,
                                      "ordinary human rights prosecution of state agents;",
                                      "ordinary human rights prosecutions of state agents;"),
                               ordy_tfc_dom_sta,
                               ifelse(ordy_tfc_dom_sta <= 1,
                                      "led to a final conviction.",
                                      "led to final convictions.")
                               ) %>%
                         n_transform())),
         text = list(c(text,
                       if(xord_trs_dom_ctj_opp > 0)
                         paste("There",
                               ifelse(xord_trs_dom_ctj_opp == 1, "was", "were"),
                               xord_trs_dom_ctj_opp,
                               ifelse(xord_trs_dom_ctj_opp == 1,
                                      "extraordinary prosecution of opposition members in a conflict context;",
                                      "extraordinary prosecutions of opposition members in conflict contexts;"),
                               xord_tfc_dom_ctj_opp,
                               ifelse(xord_tfc_dom_ctj_opp <= 1,
                                      "led to a final conviction.",
                                      "led to final convictions.")
                               ) %>%
                         n_transform())),
         text = list(c(text,
                       if(lcon_trs_dom_sta_opp > 0)
                         paste("There",
                               ifelse(lcon_trs_dom_sta_opp == 1, "was", "were"),
                               lcon_trs_dom_sta_opp,
                               ifelse(lcon_trs_dom_sta_opp == 1,
                                      "prosecution of opposition members in a low-level conflict context;",
                                      "prosecutions of opposition members in low-level conflict contexts;"), 
                               lcon_tfc_dom_sta_opp,
                               ifelse(lcon_tfc_dom_sta_opp <= 1,
                                      "led to a final conviction.",
                                      "led to final convictions.")
                         ) %>%
                         n_transform())),
         text = str_flatten(text, " ") %>% 
           str_trim()
         ) %>%
  ungroup() %>% 
  select(country_case, ccode_case, 
         # beg, end, total, # total domestic trials between beg and end
         # xord_trs_dom_dtj_sta, # extraordinary prosecutions of state agents in democratic transition context
         # xord_trs_dom_ctj_sta, # extraordinary prosecutions of state agents in conflict or post-conflict context
         # xord_trs_dom_dtj_ctj_sta, # extraordinary prosecutions of state agents
         # xord_tfc_dom_dtj_ctj_sta, # total final convictions in extraordinary prosecutions of state agents
         # xord_trs_dom_dtj_ctj_sta_hi, # extraordinary prosecutions of high-ranking state agents
         # xord_tfc_dom_dtj_ctj_sta_hi, # total final convictions in extraordinary prosecutions of high-ranking state agents
         # ordy_trs_dom_sta, # ordinary human rights prosecutions of state agents
         # ordy_tfc_dom_sta, # total final convictions in ordinary human rights prosecutions of state agents
         # xord_trs_dom_ctj_opp, # extraordinary prosecutions of opposition members in conflict or post-conflict context
         # xord_tfc_dom_ctj_opp, # total final convictions in extraordinary prosecutions of opposition members in conflict or post-conflict context
         # lcon_trs_dom_sta_opp, # prosecutions of state agents or opposition members in low-level conflict context
         # lcon_tfc_dom_sta_opp, # total final convictions in prosecutions of state agents or opposition members in low-level conflict context
         text)

autotxt[["Foreign"]] <- data[["Foreign"]] %>% 
  group_by(countryAccused) %>% 
  mutate(count = n(), 
         countryTrial = list(sort(unique(unlist(countryTrial)))),
         yearStart = list(unlist(yearStart)),
         yearEnd = list(unlist(yearEnd))) %>% 
  select(countryAccused, ccode_Accused, count, countryTrial, yearStart, yearEnd) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(countryTrial = str_flatten_comma(countryTrial, last = ", and ") %>% 
           str_replace(" Netherlands", " the Netherlands") %>% 
           str_replace(" United Kingdom", " the United Kingdom") %>% 
           str_replace(" United States of America", " the United States of America") , 
         text = paste("Nationals of ", 
                      countryAccused, 
                      " were defendants in ", 
                      count, 
                      ifelse(count == 1, " foreign prosecution", " foreign prosecutions"), 
                      " in ",
                      countryTrial, 
                      " beginning ", 
                      ifelse(length(unique(yearStart)) == 1, 
                             paste("in", unique(yearStart)), 
                             paste("between", 
                                   min(yearStart), 
                                   "and", 
                                   max(yearStart))), 
                      ".", sep = "") %>% 
           n_transform()
  ) %>% 
  ungroup() %>% 
  select(countryAccused, ccode_Accused, text) %>% 
  rename("country_case" = "countryAccused",
         "ccode_case" = "ccode_Accused")

autotxt[["Intl"]] <- data[["Intl_totals"]] %>%
  rowwise() %>% 
  mutate(text = paste("Nationals of ", 
                      country_case, 
                      " were subject to ", 
                      trs_int_hrs_con_all, 
                      " international ", 
                      ifelse(trs_int_hrs_con_all == 1, 
                             "prosecution ", "prosecutions "), 
                      # " starting ",
                      ifelse(beg == end, 
                             paste("in", beg), 
                             paste("between", 
                                   beg, 
                                   "and", 
                                   end) 
                             ), 
                      ", ", 
                      tfc_int_hrs_con_all, 
                      " of which led to a final conviction.", 
                      sep = "") %>% 
           n_transform()
  ) %>% 
  ungroup() %>% 
  select(country_case, ccode_case, text) 

autotxt[["ICC"]] <- data[["ICC-interventions"]] %>% 
  rowwise() %>%
  mutate(
    text = ifelse(is.na(ICC_prelimEnd), 
                  paste("The ICC's Office of the Prosecutor opened a preliminary examination of the situation in ", 
                        country_case, " in ", ICC_prelim_exam, ".", sep = ""), 
                  paste("The ICC's Office of the Prosecutor carried out a preliminary examination of the situation in ", 
                        country_case, " from ", ICC_prelim_exam, " until ", ICC_prelimEnd, ".", sep = "")), 
    text = list(c(if(!is.na(ICC_referral))
      paste(country_case, " was referred to the ICC in ", ICC_referral, ".", sep = ""), 
      text)), 
    text = list(c(text,
                  if(!is.na(ICC_investigation))
                    paste("The first investigation of a specific case began in ", ICC_investigation, ".", sep = ""))),
    text = str_flatten(text, " ") %>% 
      str_trim()
  ) %>%
  ungroup() %>% 
  select(country_case, ccode_case, text)

autotxt[["ICCaccused"]] <- data[["ICC-accused"]] %>% 
  group_by(country_case) %>%
  mutate(count = n(),
         count_appear = sum(!is.na(ICC_arrestAppear)),
         count_proceed = sum(!is.na(ICC_proceedings)),
         ICC_arrest_warrant = list(unlist(ICC_arrest_warrant)),
         ICC_arrestAppear = list(unlist(ICC_arrestAppear)),
         ICC_proceedings = list(unlist(ICC_proceedings))) %>%
  select(country_case, ccode_case, count, ICC_arrest_warrant, count_appear, ICC_arrestAppear, 
         count_proceed, ICC_proceedings) %>% 
  ungroup() %>% 
  distinct() %>% 
  rowwise() %>%
  mutate(ICC_arrestAppear = list(ICC_arrestAppear[!is.na(ICC_arrestAppear)]),
         ICC_proceedings = list(ICC_proceedings[!is.na(ICC_proceedings)]),
         text = paste("Starting in ", 
                      min(ICC_arrest_warrant), 
                      ", the ICC issued ", 
                      count, 
                      " arrest warrants, ",
                      count_appear, 
                      " of which resulted in court appearances.", 
                      sep = "") %>% 
           n_transform(), 
         text = list(c(text,
                       if(count_proceed > 0)
                         paste("Proceedings began in ",
                               count_proceed, 
                               ifelse(count_proceed == 1, " case ", " cases "), 
                               ifelse(length(unique(ICC_proceedings)) == 1, 
                                      paste("in", unique(ICC_proceedings)), 
                                      paste("between", 
                                            min(ICC_proceedings, na.rm = TRUE), 
                                            "and", 
                                            max(ICC_proceedings, na.rm = TRUE))), 
                               ".", sep = "") %>% 
                         n_transform()
         )),
         text = str_flatten(text, " ") %>% 
           str_trim()
  ) %>% 
  ungroup() %>% 
  select(country_case, ccode_case, text)

autotxt[["UNinvestigations"]] <- data[["Investigations"]] %>%
  mutate(country = ifelse(country == "Iran/Iraq" & ccode_case == 630, "Iran", country), 
         country = ifelse(country == "Iran/Iraq" & ccode_case == 645, "Iraq", country)) %>%
  select(country, country_case, ccode_case, mandate, beg, end, 
         uninv_dompros, uninv_evcoll, uninv_intlpros) %>% 
  arrange(country_case, mandate) %>% 
  group_by(ccode_case, mandate) %>%
  mutate(beg = ifelse(ccode_case == 437 & mandate == "Office of the UN High Commissioner for Human Rights", beg, min(beg)), 
         end = ifelse(ccode_case == 437 & mandate == "Office of the UN High Commissioner for Human Rights", end, max(end)), 
         uninv_dompros = max(uninv_dompros),
         uninv_evcoll = max(uninv_evcoll),
         uninv_intlpros = max(uninv_intlpros)) %>% 
  distinct() %>% 
  arrange(ccode_case, beg) %>% 
  group_by(ccode_case) %>%
  mutate(count = n(), 
         beg = list(unlist(beg)),
         end = list(unlist(end)),
         uninv_dompros = sum(uninv_dompros),
         uninv_evcoll = sum(uninv_evcoll),
         uninv_intlpros = sum(uninv_intlpros)
  ) %>% 
  select(country, country_case, ccode_case, count, beg, end, uninv_dompros, uninv_evcoll, uninv_intlpros) %>%
  distinct() %>% 
  mutate(country = ifelse(country == "Yugoslavia", "Serbia and Montenegro", country)) %>% 
  rowwise() %>%
  mutate(text = paste(country, 
                      " was subject to ", 
                      count, 
                      ifelse(count == 1, " UN investigation ", " UN investigations "), 
                      ifelse(min(beg) == max(end), 
                             paste("in", min(beg)),
                             paste("between", min(beg), "and", max(end))), 
                      ".", sep = "") %>% 
           n_transform(), 
         text = list(c(text,
                       if(uninv_dompros > 0)
                         paste(uninv_dompros, 
                               ifelse(uninv_dompros == 1, "investigation", "investigations"), 
                               "aimed to encourage domestic prosecutions.") %>% 
                         n_transform() %>% 
                         str_to_sentence() %>% 
                         str_trim()
         )),
         text = list(c(text,
                       if(uninv_intlpros > 0)
                         paste(uninv_intlpros, 
                               ifelse(uninv_intlpros == 1, "investigation", "investigations"), 
                               "aimed to support international prosecutions.") %>% 
                         n_transform() %>% 
                         str_to_sentence()
         )),
         text = list(c(text,
                       if(uninv_evcoll > 0)
                         paste(uninv_evcoll, 
                               ifelse(uninv_evcoll == 1, "investigation", "investigations"), 
                               "aimed to collect evidence for prosecutions.") %>% 
                         n_transform() %>% 
                         str_to_sentence()
         )),
         text = str_flatten(text, " ") %>% 
           str_trim()
  ) %>%
  ungroup() %>% 
  select(country_case, ccode_case, text)

autotxt[["Reparations"]] <- data[["Reparations"]] %>%
  mutate(implemented = ifelse(!is.na(yearBegin) | !is.na(yearEnd) | !is.na(beneficiariesCount), 1, 0), 
         individual = case_when(individualReparations == "yes" ~ 1, 
                                TRUE ~ 0), 
         collective = case_when(collectiveReparations == "yes" ~ 1, 
                                TRUE ~ 0), 
         beneficiariesCount = ifelse(is.na(beneficiariesCount), 0, beneficiariesCount) 
  ) %>% 
  group_by(country_case) %>% 
  mutate(yearCreated = list(unlist(yearCreated)),
         yearBegin = list(unlist(yearBegin)),
         yearEnd = list(unlist(yearEnd)),
         individual = sum(individual),
         collective = sum(collective),
         implemented = sum(implemented), 
         beneficiariesCount = sum(beneficiariesCount) 
  ) %>% 
  select(country_case, ccode_case, count, yearCreated, yearBegin, yearEnd, 
         individual, collective, beneficiariesCount, implemented) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(yearBegin = list(yearBegin[!is.na(yearBegin)]),
         yearEnd = list(yearEnd[!is.na(yearEnd)]),
         text = "", 
         text = list(c(text, 
                       if(length(yearBegin) == 0 & length(yearEnd) == 0)
                         paste(country_case, 
                               " mandated ", 
                               count, 
                               ifelse(count == 1, " reparations policy in ", " reparations policies in "), 
                               str_flatten_comma(unique(yearCreated), last = ", and "),
                               # ifelse(length(yearCreated) == 1, 
                               #        paste("in", unique(yearCreated)), 
                               #        paste("between", min(yearCreated), "and", max(yearCreated))
                               # ), 
                               ".", sep = "") %>% 
                         n_transform()
         )), 
         text = list(c(text, 
                       if(length(yearBegin) == 0 & length(yearEnd) == 1)
                         paste(country_case, 
                               " mandated ", 
                               count, 
                               ifelse(count == 1, " reparations policy ", " reparations policies "), 
                               ifelse(length(unique(yearCreated)) == 1, 
                                      paste("in", unique(yearCreated)), 
                                      paste("between", min(yearCreated), "and", max(yearCreated))
                               ), 
                               ", which ended by ", 
                               max(yearEnd),
                               ".", sep = ""
                         ) %>% 
                         n_transform()
         )), 
         text = list(c(text, 
                       if(length(yearBegin) > 0 & length(yearBegin) == length(yearEnd))
                         paste(country_case, 
                               " implemented ", 
                               count, 
                               ifelse(count == 1, " reparations policy ", " reparations policies "), 
                               ifelse(min(yearBegin) == max(yearEnd), 
                                      paste("in", min(yearBegin)), 
                                      paste("between", min(yearBegin), "and", max(yearEnd))
                               ), 
                               ".", sep = ""
                         ) %>% 
                         n_transform()
         )), 
         text = list(c(text, 
                       if(length(yearBegin) > 0 & length(yearBegin) > length(yearEnd))
                         paste(country_case, 
                               " implemented ", 
                               count, 
                               ifelse(count == 1, " reparations policy, ", " reparations policies, "), 
                               "starting in ", 
                               min(yearBegin),
                               ".", sep = ""
                         ) %>% 
                         n_transform()
         )), 
         text = list(c(text, 
                       if(beneficiariesCount > 0)
                         paste("According to available information, there was a total of", 
                               beneficiariesCount, 
                               "individual beneficiaries."
                         ) %>% 
                         n_transform() %>% 
                         str_to_sentence()
         )),
         text = list(c(text, 
                       if(beneficiariesCount == 0)
                         paste("TJET found no information on the total number of beneficiaries.")
         )),
         text = list(c(text, 
                       if(collective > 0)
                         paste(collective, 
                               ifelse(collective == 1, " reparations policy ", " reparations policies "), 
                               "provided collective benefits.", 
                               sep = ""
                         ) %>% 
                         n_transform() %>% 
                         str_to_sentence() %>% 
                         str_trim()
         )),
         text = list(c(text, 
                       if(implemented > 0 & implemented < count)
                         paste("TJET found evidence on implementation only for", 
                               implemented, 
                               ifelse(implemented == 1, " reparations policy.", " reparations policies.")) %>% 
                         n_transform()
         )), 
         text = list(c(text, 
                       if(implemented == 0)
                         paste("TJET found no evidence on implementation.")
         )), 
         text = str_flatten(text, " ") %>% 
           str_trim()
  ) %>%
  ungroup() %>% 
  select(country_case, ccode_case, text)

autotxt[["TruthCommissions"]] <- data[["TruthCommissions"]] %>%
  mutate(finalReportIssued = case_when(finalReportIssued == "Yes" ~ 1,
                                       TRUE ~ 0),
         reportPubliclyAvailable = case_when(reportPubliclyAvailable == "Yes" ~ 1,
                                             TRUE ~ 0),
         victims = case_when(consultedVictims == 1 | 
                               commissionersVictimGroups == "Yes" | 
                               encourageVictimTestimony == "Yes" ~ 1,
                             TRUE ~ 0), 
         yearBeginOperation = ifelse(is.na(yearBeginOperation) & !is.na(yearCompleteOperation), 
                                     yearPassed, yearBeginOperation) 
         ) %>% 
  group_by(country_case) %>% 
  mutate(yearPassed = list(unlist(yearPassed)),
         yearBeginOperation = list(unlist(yearBeginOperation)),
         yearCompleteOperation = list(unlist(yearCompleteOperation)),
         finalReportIssued = sum(finalReportIssued),
         reportPubliclyAvailable = sum(reportPubliclyAvailable),
         rec_prosecutions = sum(rec_prosecutions),
         rec_reparations = sum(rec_reparations),
         rec_reforms = sum(rec_reforms),
         victims = sum(victims)
  ) %>%
  select(country_case, ccode_case, count, yearPassed, yearBeginOperation, yearCompleteOperation, 
         finalReportIssued, reportPubliclyAvailable, rec_prosecutions,
         rec_reparations, rec_reforms, victims) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(yearBeginOperation = list(yearBeginOperation[!is.na(yearBeginOperation)]),
         yearCompleteOperation = list(yearCompleteOperation[!is.na(yearCompleteOperation)]),
         text = paste(country_case,
                      " mandated ",
                      count,
                      " truth ",
                      ifelse(count == 1, "commission", "commissions"),
                      " in ",
                      str_flatten_comma(unique(yearPassed), last = ", and "),
                      ".", sep = "") %>%
           n_transform(),
         ### one but not completed
         text = list(c(text,
                       if(length(yearBeginOperation) == count & length(yearCompleteOperation) == 0 & count == 1)
                         paste("The commission began its operations in ",
                               min(yearBeginOperation),
                               "; TJET has found no evidence of completion.", sep = "")
         )),
         ### all started but not all completed
         text = list(c(text,
                       if(length(yearBeginOperation) == count & length(yearCompleteOperation) != 0 & length(yearCompleteOperation) < count)
                         paste(length(yearCompleteOperation), 
                               " completed ",
                               ifelse(length(yearCompleteOperation) == 1, "its", "their"), 
                               " operations by ", 
                               max(yearCompleteOperation), 
                               ".", sep = "") %>% 
                         n_transform() %>%
                         str_to_sentence()
         )),
         ### not all started, but all that started also completed
         text = list(c(text,
                       if(length(yearBeginOperation) < count & length(yearCompleteOperation) == length(yearBeginOperation))
                         paste(length(yearBeginOperation),
                               " of these operated between ",
                               min(yearBeginOperation),
                               " and ",
                               max(yearCompleteOperation),
                               ".", sep = "") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         ### not all started, and none completed
         text = list(c(text,
                       if(length(yearBeginOperation) < count & length(yearCompleteOperation) == 0)
                         paste(length(yearBeginOperation), 
                               "began operations but TJET has found no evidence of completion.") %>% 
                         n_transform() %>%
                         str_to_sentence() %>%
                         str_replace("tjet", "TJET")
         )),
         ### all completed
         text = list(c(text,
                       if(length(yearCompleteOperation) == count)
                         paste(ifelse(count == 1, 
                                      "The commission completed its", 
                                      "The commissions completed their"),
                               " operations in ",
                               str_flatten_comma(unique(yearCompleteOperation), last = ", and "),
                               ".", sep = "") %>%
                         n_transform()
         )),
         text = list(c(text,
                       if(finalReportIssued > 0)
                         paste(ifelse(finalReportIssued == count,
                                      ifelse(count == 1,
                                             "The commission issued",
                                             "The commissions issued"),
                                      paste(finalReportIssued, "of the commissions issued")),
                               ifelse(finalReportIssued == 1, " a ", " "),
                               ifelse(finalReportIssued == 1, "final report", "final reports"),
                               ifelse(reportPubliclyAvailable > 0 & reportPubliclyAvailable < finalReportIssued,
                                      paste(",",
                                            reportPubliclyAvailable,
                                            "of which"),
                                      ", which"),
                               ifelse(reportPubliclyAvailable == 1,
                                      " is", " are"),
                               " publicly available.",
                               sep = ""
                         ) %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         text = list(c(text,
                       if(finalReportIssued > 0 &
                          (rec_prosecutions > 0 | rec_reparations > 0 | rec_reforms > 0))
                         paste("The ",
                               ifelse(finalReportIssued == 1, "report", "reports"),
                               " included recommendations for ",
                               str_flatten_comma(
                                 c(if(rec_prosecutions > 0) "prosecutions",
                                   if(rec_reparations > 0) "reparations",
                                   if(rec_reforms > 0) "institutional reforms"
                                   ),
                                 last = ", and ", na.rm = TRUE
                               ),
                               ".", sep = ""
                         ) %>%
                         n_transform() %>%
                         str_to_sentence() %>%
                         str_trim()
         )),
         text = str_flatten(text, " ") %>% 
           str_trim()
  ) %>%
  ungroup() %>% 
  select(country_case, ccode_case, text)

autotxt[["Vettings"]] <- data[["Vettings"]] %>% 
  mutate(alterationOf = ifelse(is.na(alterationOf), vettingID, alterationOf), 
         alterationOf = str_replace(alterationOf, fixed("?"), "")) %>% 
  select(-vettingID) %>%  
  rename("vettingID" = "alterationOf") %>%
  group_by(vettingID) %>% 
  mutate(yearStart = min(yearStart), 
         yearEnd = max(yearEnd),
         individual_conduct = max(individual_conduct), 
         type_dismissal = max(type_dismissal), 
         type_ban = max(type_ban), 
         type_declassification = max(type_declassification), 
         type_perjury = max(type_perjury)) %>% 
  select(country_case, ccode_case, vettingID, yearStart, yearEnd, individual_conduct, 
         type_dismissal, type_ban, type_declassification, type_perjury) %>% 
  distinct() %>% 
  group_by(country_case) %>% 
  mutate(count = n(), 
         yearStart = list(unlist(yearStart)),
         yearEnd = list(unlist(yearEnd)),
         individual_conduct = sum(individual_conduct), 
         type_dismissal = sum(type_dismissal), 
         type_ban = sum(type_ban), 
         type_declassification = sum(type_declassification), 
         type_perjury = sum(type_perjury)
         ) %>% 
  ungroup() %>% 
  select(country_case, ccode_case, count, yearStart, yearEnd, individual_conduct, 
         type_dismissal, type_ban, type_declassification, type_perjury) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(yearStart = list(yearStart[!is.na(yearStart)]),
         yearEnd = list(yearEnd[!is.na(yearEnd)]),
         text = "",
         text = list(c(text,
                       if(length(yearStart) == count & 
                          length(yearEnd) == count)
                         paste(country_case, 
                               " had ", 
                               count, 
                               " vetting ", 
                               ifelse(count == 1, "policy ", "policies "), 
                               ifelse(min(yearStart) == max(yearEnd),
                                      paste("in", max(yearEnd)),
                                      paste("between", min(yearStart), "and", max(yearEnd))
                               ),
                               ".", sep = "") %>%
                         n_transform()
         )),
         text = list(c(text,
                       if(length(yearStart) == count & 
                          length(yearEnd) > 0 & 
                          length(yearEnd) < count)
                         paste(country_case, 
                               " had ", 
                               count, 
                               " vetting ", 
                               ifelse(count == 1, "policy,", "policies,"), 
                               " starting in ", 
                               min(yearStart), 
                               "; ", 
                               length(yearEnd), 
                               " of these ended by ", 
                               max(yearEnd), 
                               ".", sep = "") %>%
                         n_transform()
         )),
         text = list(c(text,
                       if(length(yearStart) == count & 
                          length(yearEnd) == 0)
                         paste(country_case, 
                               " had ", 
                               count, 
                               " vetting ", 
                               ifelse(count == 1, "policy,", "policies,"), 
                               " starting in ", 
                               min(yearStart), 
                               "; TJET found no information on whether or when the ",
                               ifelse(count == 1, "policy", "policies"), 
                               " ended.",
                               sep = "") %>%
                         n_transform()
         )),
         text = list(c(text,
                       if(individual_conduct == count)
                         paste(ifelse(individual_conduct == 1, "This policy", "These policies"),
                               "provided sanctions based on past individual conduct.") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         text = list(c(text,
                       if(individual_conduct > 0 & individual_conduct < count)
                         paste(individual_conduct,
                               ifelse(individual_conduct == 1, "policy", "policies"),
                               "provided sanctions based on past individual conduct.") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         # don't use next
         # text = list(c(text,
         #               if(type_dismissal == count &
         #                  type_dismissal == type_ban)
         #                 paste(ifelse(type_dismissal == 1, "This policy", "These policies"),
         #                       "prescribed both dismissals from current employment and bans from holding future office.") %>%
         #                 n_transform() %>%
         #                 str_to_sentence()
         # )),
         text = list(c(text,
                       if(type_dismissal > 0 &
                          # type_dismissal != count &
                          type_dismissal == type_ban)
                         paste(type_dismissal,
                               ifelse(type_dismissal == 1, "policy", "policies"),
                               "prescribed both dismissals from current employment and bans from holding future office.") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         text = list(c(text,
                       if(type_dismissal > 0 &
                          type_dismissal != type_ban)
                         paste(type_dismissal,
                               ifelse(type_dismissal == 1, "policy", "policies"),
                               "prescribed dismissals from current employment.") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         text = list(c(text,
                       if(type_ban > 0 &
                          type_dismissal != type_ban)
                         paste(type_ban,
                               ifelse(type_ban == 1, "policy", "policies"),
                               "prescribed bans from holding future office.") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         text = list(c(text,
                       if(type_declassification > 0)
                         paste(type_declassification,
                               ifelse(type_declassification == 1, "policy", "policies"),
                               "aimed to declassify the records of former state security agents.") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         text = list(c(text,
                       if(type_perjury > 0)
                         paste(type_perjury,
                               ifelse(type_perjury == 1, "policy", "policies"),
                               "included legal consequences for non-disclosure of relevant past activities.") %>%
                         n_transform() %>%
                         str_to_sentence()
         )),
         text = str_flatten(text, " ") %>% 
           str_trim()
         ) %>% 
  ungroup() %>% 
  select(country_case, ccode_case, text)

### print 10 focus country profiles to Quarto files 

df <- db[["Countries"]] %>%
  filter(include) %>%
  select(country, country_case, ccode, ccode_case, ccode_ksg, beg, end, tjet_focus, 
         txt_intro, txt_regime, txt_conflict, txt_TJ) %>%
  arrange(country)
  
dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/focus/")

df %>% 
  filter(tjet_focus == 1 | country == "Uganda") %>%  
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

### for integrating into Airtable

autotxt <- df %>%
  select(country_case, ccode_case) %>%
  left_join(autotxt[["intro"]] %>%
              rename("intro" = "text"),
            by = c("country_case", "ccode_case")) %>%
  mutate(intro = ifelse(is.na(intro),
                        paste("TJET has found no information on transitional justice in ",
                              country_case, ".", sep = ""),
                        intro) ) %>%
  left_join(autotxt[["Transitions"]] %>%
              rename("regime" = "text"),
            by = c("country_case", "ccode_case")) %>%
  mutate(regime = ifelse(is.na(regime),
                         paste("TJET records no democratic transitions in",
                               country_case, 
                               "between 1970 and 2020."),
                         regime) ) %>%
  left_join(autotxt[["Conflicts"]] %>%
              rename("conflict" = "text"),
            by = c("country_case", "ccode_case")) %>%
  mutate(conflict = ifelse(is.na(conflict),
                           paste("Based on the Uppsala Conflict Data Program, TJET records no episodes of violent intrastate conflict in",
                                 country_case,
                                 "between 1970 and 2020."),
                           conflict) ) %>%
  left_join(autotxt[["Amnesties"]] %>%
              rename("amnesties" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["Domestic_totals"]] %>%
              rename("domestic" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["Foreign"]] %>%
              rename("foreign" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["Intl"]] %>%
              rename("intl" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["ICC"]] %>%
              rename("intl2" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["ICCaccused"]] %>%
              rename("intl3" = "text"),
            by = c("country_case", "ccode_case")) %>%
  rowwise() %>%
  mutate(intl = str_flatten(c(intl, intl2, intl3), collapse = " ", na.rm = TRUE), 
         intl = ifelse(intl == "", NA, intl) ) %>%
  ungroup() %>%
  select(-intl2, -intl3) %>%
  left_join(autotxt[["Reparations"]] %>%
              rename("reparations" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["TruthCommissions"]] %>%
              rename("tcs" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["Vettings"]] %>%
              rename("vetting" = "text"),
            by = c("country_case", "ccode_case")) %>%
  left_join(autotxt[["UNinvestigations"]] %>%
              rename("un" = "text"),
            by = c("country_case", "ccode_case"))
  # write_csv(file = "~/Desktop/temp.csv", na = "")

### auto-text fields check 
### 'auto_' fields are generated here and need to be transfered to Airtable
### 'txt_' fields are manually adjusted in Airtable 
check_vars <- paste("chk_", c("summary", "regime", "conflict", "amnesties", "domestic", "foreign", "intl", "reparations", "tcs", "vetting", "un"), sep = "")
db[["Countries"]] %>% 
  filter(include) %>% 
  full_join(autotxt, 
            by = c("country_case", "ccode_case")) %>% 
  mutate(chk_summary = ifelse(auto_summary != intro, 1, 0), 
         chk_regime = ifelse(auto_regime != regime, 1, 0), 
         chk_conflict = ifelse(auto_conflict != conflict, 1, 0), 
         chk_amnesties = ifelse(auto_amnesties != amnesties, 1, 0), 
         chk_domestic = ifelse(auto_domestic != domestic, 1, 0), 
         chk_foreign = ifelse(auto_foreign != foreign, 1, 0), 
         chk_intl = ifelse(auto_intl != intl, 1, 0), 
         chk_reparations = ifelse(auto_reparations != reparations, 1, 0), 
         chk_tcs = ifelse(auto_tcs != tcs, 1, 0), 
         chk_vetting = ifelse(auto_vetting != vetting, 1, 0), 
         chk_un = ifelse(auto_un != un, 1, 0), ) %>%
  select(country_case, all_of(check_vars), intro, auto_summary, regime, 
         auto_regime, conflict, auto_conflict, amnesties, auto_amnesties, 
         domestic, auto_domestic, intl, auto_intl, foreign, auto_foreign, 
         reparations, auto_reparations, tcs, auto_tcs, 
         vetting, auto_vetting, un, auto_un) %>% 
  filter(if_any(all_of(check_vars), ~ . == 1)) %>% 
  write_csv("~/Desktop/temp.csv") %>%
  print(n = Inf)

### print all country profiles with auto summaries to Quarto files 

dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/")
dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/original/")
# dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/edits/")
dir("~/Dropbox/TJLab/TimoDataWork/country_profiles")

map(df$country_case, function(ctry) {
    
  df1 <- df %>%
    filter(country_case == ctry) %>%
    mutate(txt_intro = str_replace_all(str_trim(txt_intro), "\n", "\n\n"),
           txt_regime = str_replace_all(str_trim(txt_regime), "\n", "\n\n"),
           txt_conflict = str_replace_all(str_trim(txt_conflict), "\n", "\n\n"),
           txt_TJ = str_replace_all(str_trim(txt_TJ), "\n", "\n\n")) %>%
    select(txt_intro, txt_regime, txt_conflict, txt_TJ) %>%
    unlist()
  
  new <- autotxt %>% 
    filter(country_case == ctry) %>%
    select(-country_case, -ccode_case) %>%
    unlist()

  # new <- map(autotxt, function(df2) {
  #   df2 %>%
  #     filter(country_case == ctry) %>%
  #     select(text) %>%
  #     unlist(use.names = FALSE)
  # })
  
  paste("---\ntitle: ", ctry, "\nformat: docx\n---\n\n",
        new[["intro"]],
        "\n\n## Country Background", 
        "\n\n### Democratic Transition\n\n",
        new[["regime"]], "\n\n", 
        df1[["txt_regime"]],
        "\n\n### Violent Conflict\n\n",
        new[["conflict"]],
        df1[["txt_conflict"]],
        "\n\n## Transitional Justice\n\n",
        df1[["txt_intro"]], "\n\n",
        df1[["txt_TJ"]], "\n\n",
        ifelse(!is.na(new[["amnesties"]]),
               paste("### Amnesties\n\n", new[["amnesties"]], "\n\n", sep = ""),
               ""),
        ifelse(!is.na(new[["domestic"]]),
               paste("### Domestic Trials\n\n", new[["domestic"]], "\n\n", sep = ""),
               ""),
        ifelse(!is.na(new[["intl"]]),
               paste("### International Trials\n\n", new[["intl"]], "\n\n", sep = ""),
               ""),
        ifelse(!is.na(new[["foreign"]]),
               paste("### Foreign Trials\n\n", new[["foreign"]], "\n\n", sep = ""),
               ""),
        ifelse(!is.na(new[["reparations"]]),
               paste("### Reparations\n\n", new[["reparations"]], "\n\n", sep = ""),
               ""),
        ifelse(!is.na(new[["tcs"]]),
               paste("### Truth Commissions\n\n", new[["tcs"]], "\n\n", sep = ""),
               ""),
        ifelse(!is.na(new[["un"]]),
               paste("### UN Investigations\n\n", new[["un"]], "\n\n", sep = ""),
               ""),
        ifelse(!is.na(new[["vetting"]]),
               paste("### Vetting\n\n", new[["vetting"]], "\n\n", sep = ""),
               ""),
        sep = "") %>% 
    write_file(., file = paste("~/Dropbox/TJLab/TimoDataWork/country_profiles/original/", ctry, ".qmd", sep = ""))
})

# file.copy(from = list.files("~/Dropbox/TJLab/TimoDataWork/country_profiles/original", full.names = TRUE),
#           to = "~/Dropbox/TJLab/TimoDataWork/country_profiles/edits/",
#           overwrite = FALSE, recursive = TRUE, copy.mode = FALSE)
