### packages
library("tidyverse")
library("readxl")
library("writexl")

options(scipen = 999) 

load(here::here("data", "tjetdb.RData"))

countries <- db[["Countries"]] %>% 
  filter(!country %in% c("Serbia and Montenegro", "Soviet Union", "Yugoslavia") ) %>% 
  select(country, country_case, ccode) 

data <- list()

# db[["Trials"]] %>% 
#   mutate(HRs_charges = ifelse(HRs_charges > 0, 1, 0) ) %>%  
#   select(trialID, humanRights, IntraConfl, HRs_charges, 
#          fitsPostAutocraticTJ, fitsConflictTJ, 
#          beganDuringIntraConfl, beganAfterIntraConfl) %>% 
#   select(humanRights, fitsConflictTJ) %>% 
#   table()

db[["Transitions"]]

db[["ConflictDyads"]] 

data[["Amnesties"]] <- db[["Amnesties"]] %>%
  left_join(countries, by = c(ccode_cow = "ccode")) %>% 
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
  rename("country" = "country_case") %>%
  distinct() %>%
  arrange(country)

vars_dom <- c("xord_trs_dom_dtj_sta", "xord_trs_dom_ctj_sta", 
              "xord_trs_dom_dtj_ctj_sta", "ordy_trs_dom_sta", 
              "xord_tfc_dom_dtj_ctj_sta", "ordy_tfc_dom_sta", 
              "xord_trs_dom_dtj_ctj_sta_hi", "xord_tfc_dom_dtj_ctj_sta_hi", 
              "xord_trs_dom_ctj_opp", "xord_tfc_dom_ctj_opp", 
              "lcon_trs_dom_sta_opp", "lcon_tfc_dom_sta_opp")
vars_int <- c("trials_intl", "trs_int_sta", "trs_int_opp", "tfc_int_sta", "tfc_int_opp") 
vars_for <- c("trials_foreign", "trs_for_sta", "trs_for_opp", "tfc_for_sta", "tfc_for_opp") 

data[["Domestic_totals"]] <- db[["dl_tjet_cy"]] %>% 
  select(-country) %>% 
  rename("country" = "country_case") %>%
  arrange(country, year) %>% 
  filter(year <= 2020) %>% 
  filter(if_any(all_of(vars_dom), ~ . > 0)) %>%
  group_by(country) %>% 
  reframe(beg = min(year), 
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
  select(-country) %>% 
  rename("country" = "country_case") %>%
  arrange(country, year) %>% 
  filter(year <= 2020) %>% 
  filter(if_any(all_of(vars_int), ~ . > 0)) %>%
  group_by(country) %>% 
  reframe(beg = min(year), 
          end = max(year),
          trials_intl = sum(trials_intl),
          trs_int_hrs_con_all = sum(trs_int_sta + trs_int_opp), 
          tfc_int_hrs_con_all = sum(tfc_int_sta + tfc_int_opp))

data[["Foreign_totals"]] <- db[["dl_tjet_cy"]] %>% 
  select(-country) %>% 
  rename("country" = "country_case") %>%
  arrange(country, year) %>% 
  filter(year <= 2020) %>%
  filter(if_any(all_of(vars_for), ~ . > 0)) %>%
  group_by(country) %>% 
  reframe(beg = min(year), 
          end = max(year), 
          trials_foreign = sum(trials_foreign),
          trs_for_hrs_con_all = sum(trs_for_sta + trs_for_opp), 
          tfc_for_hrs_con_all = sum(tfc_for_sta + tfc_for_opp)) 

data[["Foreign"]] <- db[["Trials"]] %>%
  left_join(countries, by = c(ccode_Accused = "ccode")) %>%
  rename("countryAccused" = "country") %>%
  left_join(countries, by = c(ccode_Trial = "ccode")) %>%
  rename("countryTrial" = "country") %>%
  filter(trialType == "foreign") %>% 
  select(trialID, countryAccused, ccode_Accused, countryTrial, ccode_Trial, yearStart, yearEnd, caseDescription) %>% 
  arrange(countryAccused, yearStart) 
  
data[["Reparations"]] <- db[["Reparations"]] %>%
  left_join(countries, by = c(ccode_cow = "ccode")) %>% 
  group_by(country_case) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  select(country_case, count, reparationID, yearCreated, yearBegin, yearEnd, 
           individualReparations, collectiveReparations, beneficiariesCount) %>%
  rename("country" = "country_case") %>%
  arrange(country, yearCreated)

data[["TruthCommissions"]] <- db[["TruthCommissions"]] %>%
  left_join(countries, by = c(ccode_cow = "ccode")) %>% 
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
  select(country_case, count, truthcommissionID, yearPassed, yearBeginOperation, 
         yearCompleteOperation, finalReportIssued, reportPubliclyAvailable,
         rec_prosecutions, rec_reparations, rec_reforms, reform_HRs,
         reform_legal, reform_judicial, reform_gender, reform_corruption,
         reform_SSR, reform_vetting, 
         consultedVictims, commissionersVictimGroups, encourageVictimTestimony
         ) %>%
  rename("country" = "country_case") %>%
  arrange(country, yearPassed)

data[["Vettings"]] <- db[["Vettings"]] %>%
  left_join(countries, by = c(ccode_cow = "ccode")) %>% 
  mutate(individual_conduct = case_when(
    str_detect(targetingWhy, "specific individual conduct") ~ 1,
    TRUE ~ 0)) %>%
  select(country_case, vettingID, alterationOf, yearStart, yearEnd, 
         individual_conduct, type_dismissal, type_ban, type_declassification, 
         type_perjury, numberInvestigated, dateLaw) %>%
  rename("country" = "country_case") %>%
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
  select(country, beg, end, mandate, 
         uninv_dompros, uninv_evcoll, uninv_intlpros, goals, ccode_cow) %>%
  arrange(country, beg) 

### peace agreements: "https://peaceaccords.nd.edu/wp-content/uploads/2019/08/PAM_ID-V.1.5-Updated-29JULY2015.xlsx"
data[["peace-agreements"]] <- readxl::read_xlsx("data/PAM_ID-V.1.5-Updated-29JULY2015.xlsx") %>% 
  rename("accord_name" = "accord name") %>%
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
tj_vars <- c("TjGen", "TjAm", "TjAmPro", "TjSan", "TjPower", "TjCou", "TjJaNc", 
             "TjJaIc", "TjMech", "TjPrire", "TjVet", "TjVic", "TjMis", "TjRep", 
             "TjRSym", "TjRMa", "TjNR")
data[["PAX"]] <- read_csv("data/pax_all_agreements_data_v6.csv") %>% 
  arrange(Con, PPName, Dat) %>% 
  filter(Stage %in% c("SubComp", "SubPar")) %>% 
  filter(if_any(all_of(tj_vars), ~ . == 1)) %>%
  select(Con, PP, PPName, AgtId, Agt, Dat, Status, Agtp, Stage, Loc1GWNO, 
         Loc2GWNO, UcdpCon, UcdpAgr, PamAgr, all_of(tj_vars)) 

### write to file 
write_xlsx(data, path = "~/Dropbox/TJLab/TimoDataWork/country_profiles/summary_data.xlsx")

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

auto_text <- list() 

auto_text[["Amnesties"]] <- data[["Amnesties"]] %>% 
  rowwise() %>%
  mutate(
    text = list(paste(country, "had", count_all, ifelse(count_all == 1, "amnesty", "amnesties"), "in total.") %>% 
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
  select(country, text)

auto_text[["Domestic_totals"]] <- data[["Domestic_totals"]] %>%
  rowwise() %>%
  mutate(text = "", 
         text = paste(country,
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
  select(country, 
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

auto_text[["Foreign"]] <- data[["Foreign"]] %>% 
  group_by(countryAccused) %>% 
  mutate(count = n(), 
         countryTrial = list(sort(unique(unlist(countryTrial)))),
         yearStart = list(unlist(yearStart)),
         yearEnd = list(unlist(yearEnd))) %>% 
  select(countryAccused, count, countryTrial, yearStart, yearEnd) %>% 
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
  select(countryAccused, text) %>% 
  rename("country" = "countryAccused")

auto_text[["Intl"]] <- data[["Intl_totals"]] %>%
  rowwise() %>% 
  mutate(text = paste(country, 
                      " was subject to ", 
                      trs_int_hrs_con_all, 
                      " international prosecutions starting ",
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
  select(country, text) 

auto_text[["ICC"]] <- data[["ICC-interventions"]] %>% 
  mutate(country = ifelse(country == "DR Congo", "Democratic Republic of the Congo", country) ) %>% 
  rowwise() %>%
  mutate(
    text = ifelse(is.na(ICC_prelimEnd), 
                  paste("The ICC's Office of the Prosecutor opened a preliminary examination of the situation in ", 
                        country, " in ", ICC_prelim_exam, ".", sep = ""), 
                  paste("The ICC's Office of the Prosecutor carried out a preliminary examination of the situation in ", 
                        country, " from ", ICC_prelim_exam, " until ", ICC_prelimEnd, ".", sep = "")), 
    text = list(c(if(!is.na(ICC_referral))
      paste(country, " was referred to the ICC in ", ICC_referral, ".", sep = ""), 
      text)), 
    text = list(c(text,
                  if(!is.na(ICC_investigation))
                    paste("The first investigation of a specific case began in ", ICC_investigation, ".", sep = ""))),
    text = str_flatten(text, " ") %>% 
      str_trim()
  ) %>%
  ungroup() %>% 
  select(country, text)

auto_text[["ICCaccused"]] <- data[["ICC-accused"]] %>% 
  group_by(country) %>%
  mutate(count = n(),
         count_appear = sum(!is.na(ICC_arrestAppear)),
         count_proceed = sum(!is.na(ICC_proceedings)),
         ICC_arrest_warrant = list(unlist(ICC_arrest_warrant)),
         ICC_arrestAppear = list(unlist(ICC_arrestAppear)),
         ICC_proceedings = list(unlist(ICC_proceedings))) %>%
  select(country, count, ICC_arrest_warrant, count_appear, ICC_arrestAppear, 
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
  select(country, text)

auto_text[["UN"]] <- data[["UN-investigations"]] %>% 
  # filter(beg <= 2020) %>%
  mutate(country = ifelse(country == "Iran/Iraq" & ccode_cow == 630, "Iran", country), 
         country = ifelse(country == "Iran/Iraq" & ccode_cow == 645, "Iraq", country)) %>% 
  select(country, ccode_cow, mandate, beg, end, # goals, 
         uninv_dompros, uninv_evcoll, uninv_intlpros) %>% 
  arrange(country, mandate) %>% 
  group_by(ccode_cow, mandate) %>%
  mutate(beg = ifelse(ccode_cow == 437 & mandate == "Office of the UN High Commissioner for Human Rights", beg, min(beg)), 
         end = ifelse(ccode_cow == 437 & mandate == "Office of the UN High Commissioner for Human Rights", end, max(end)), 
         uninv_dompros = max(uninv_dompros),
         uninv_evcoll = max(uninv_evcoll),
         uninv_intlpros = max(uninv_intlpros)) %>% 
  distinct() %>% 
  arrange(ccode_cow, beg) %>% 
  group_by(ccode_cow) %>%
  mutate(count = n(), 
         beg = list(unlist(beg)),
         end = list(unlist(end)),
         uninv_dompros = sum(uninv_dompros),
         uninv_evcoll = sum(uninv_evcoll),
         uninv_intlpros = sum(uninv_intlpros)
  ) %>% 
  select(country, ccode_cow, count, beg, end, uninv_dompros, uninv_evcoll, uninv_intlpros) %>%
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
  select(ccode_cow, text)

auto_text[["Reparations"]] <- data[["Reparations"]] %>%
  mutate(implemented = ifelse(!is.na(yearBegin) | !is.na(yearEnd) | !is.na(beneficiariesCount), 1, 0), 
         individual = case_when(individualReparations == "yes" ~ 1, 
                                TRUE ~ 0), 
         collective = case_when(collectiveReparations == "yes" ~ 1, 
                                TRUE ~ 0), 
         beneficiariesCount = ifelse(is.na(beneficiariesCount), 0, beneficiariesCount) 
  ) %>% 
  group_by(country) %>% 
  mutate(yearCreated = list(unlist(yearCreated)),
         yearBegin = list(unlist(yearBegin)),
         yearEnd = list(unlist(yearEnd)),
         individual = sum(individual),
         collective = sum(collective),
         implemented = sum(implemented), 
         beneficiariesCount = sum(beneficiariesCount) 
  ) %>% 
  select(country, count, yearCreated, yearBegin, yearEnd, 
         individual, collective, beneficiariesCount, implemented) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(yearBegin = list(yearBegin[!is.na(yearBegin)]),
         yearEnd = list(yearEnd[!is.na(yearEnd)]),
         text = "", 
         text = list(c(text, 
                       if(length(yearBegin) == 0 & length(yearEnd) == 0)
                         paste(country, 
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
                         paste(country, 
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
                         paste(country, 
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
                         paste(country, 
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
  select(country, text)

auto_text[["TruthCommissions"]] <- data[["TruthCommissions"]] %>%
  filter(yearPassed < 2021) %>% 
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
  group_by(country) %>% 
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
  select(country, count, yearPassed, yearBeginOperation, yearCompleteOperation, 
         finalReportIssued, reportPubliclyAvailable, rec_prosecutions,
         rec_reparations, rec_reforms, victims) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(yearBeginOperation = list(yearBeginOperation[!is.na(yearBeginOperation)]),
         yearCompleteOperation = list(yearCompleteOperation[!is.na(yearCompleteOperation)]),
         text = paste(country,
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
  select(country, text)

auto_text[["Vettings"]] <- data[["Vettings"]] %>% 
  filter(yearStart < 2021) %>%  
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
  select(country, vettingID, yearStart, yearEnd, individual_conduct, 
         type_dismissal, type_ban, type_declassification, type_perjury) %>% 
  distinct() %>% 
  group_by(country) %>% 
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
  select(country, count, yearStart, yearEnd, individual_conduct, 
         type_dismissal, type_ban, type_declassification, type_perjury) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(yearStart = list(yearStart[!is.na(yearStart)]),
         yearEnd = list(yearEnd[!is.na(yearEnd)]),
         text = "",
         text = list(c(text,
                       if(length(yearStart) == count & 
                          length(yearEnd) == count)
                         paste(country, 
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
                         paste(country, 
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
                         paste(country, 
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
  select(country, count, yearStart, yearEnd, text) %>% I
  # print(n = Inf) 

# data[["PAX"]] %>%
#   select(Con, PPName, AgtId, Agt, Dat, Status, Agtp, Stage, UcdpCon)

# TjGen: Transitional Justice General
# TjAm: Amnesty/pardon
# TjAmPro: Amnesty/pardon proper
# TjSan: Relief of other Sanctions
# TjPower: Power to amnesty
# TjCou: Courts
# TjJaNc: National Courts
# TjJaIc: International Courts
# TjMech: Mechanism
# TjPrire: Prisoner release
# TjVet: Vetting
# TjVic: Victims
# TjMis: Missing
# TjRep: Reparations
# TjRSym: symbolic reparations
# TjRMa: Material reparations (includes compensation)
# TjNR: Reconciliation

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

### print 10 focus country profiles to Quarto files 

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

### print all country profiles with auto summaries to Quarto files 

dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/")
dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/original/")
dir.create("~/Dropbox/TJLab/TimoDataWork/country_profiles/edits/")
dir("~/Dropbox/TJLab/TimoDataWork/country_profiles")

auto_text[["UN"]] <- auto_text[["UN"]] %>% 
  left_join(df %>% 
              select(country, ccode_case), 
            by = c("ccode_cow" = "ccode_case")) %>%
  select(country, text) 

map(df$country, function(ctry) {
    
  temp <- df %>% 
    filter(country == ctry) %>%
    mutate(txt_intro = str_replace_all(str_trim(txt_intro), "\n", "\n\n"),
           txt_regime = str_replace_all(str_trim(txt_regime), "\n", "\n\n"),
           txt_conflict = str_replace_all(str_trim(txt_conflict), "\n", "\n\n"),
           txt_TJ = str_replace_all(str_trim(txt_TJ), "\n", "\n\n")) %>%
    select(txt_intro, txt_regime, txt_conflict, txt_TJ) %>% 
    unlist()
  
  new <- map(auto_text, function(df) {
    df %>% 
      filter(country == ctry) %>% 
      select(text) %>% 
      unlist(use.names = FALSE)
  })
  
  paste("---\ntitle: ", ctry, "\nformat: docx\n---", 
        "\n\n## Introduction\n\n", 
        temp[["txt_intro"]], 
        "\n\n## Regime Background\n\n", 
        temp[["txt_regime"]], 
        "\n\n## Conflict Background\n\n", 
        temp[["txt_conflict"]], 
        "\n\n## Transitional Justice\n\n", 
        temp[["txt_TJ"]], 
        "\n\n## TJET Data", 
        ifelse(length(new[["Amnesties"]]) > 0, 
               paste("\n\n", 
                     new[["Amnesties"]], 
                     sep = ""), 
               ""),
        ifelse(length(new[["Domestic_totals"]]) > 0, 
               paste("\n\n", 
                     new[["Domestic_totals"]], 
                     sep = ""), 
               ""),
        ifelse(length(new[["UN"]]) > 0 | length(new[["Intl"]]) > 0 | length(new[["ICC"]]) > 0,
               "\n\n", ""), 
        ifelse(length(new[["UN"]]) > 0, 
               paste(new[["UN"]],
                     " ",
                     sep = ""), 
               ""),
        ifelse(length(new[["Intl"]]) > 0, 
               paste(new[["Intl"]],
                     " ",
                     sep = ""), 
               ""),
        ifelse(length(new[["ICC"]]) > 0, 
               paste(new[["ICC"]],
                     " ", 
                     new[["ICCaccused"]], 
                     sep = ""), 
               ""),
        ifelse(length(new[["Foreign"]]) > 0, 
               paste("\n\n", 
                     new[["Foreign"]], 
                     sep = ""), 
               ""),
        ifelse(length(new[["Reparations"]]) > 0, 
               paste("\n\n", 
                     new[["Reparations"]], 
                     sep = ""), 
               ""),
        ifelse(length(new[["TruthCommissions"]]) > 0, 
               paste("\n\n", 
                     new[["TruthCommissions"]], 
                     sep = ""), 
               ""),
        ifelse(length(new[["Vettings"]]) > 0, 
               paste("\n\n", 
                     new[["Vettings"]], 
                     sep = ""), 
               ""),
        sep = "") %>% 
    write_file(., file = paste("~/Dropbox/TJLab/TimoDataWork/country_profiles/original/", ctry, ".qmd", sep = ""))
  
})

file.copy(from = list.files("~/Dropbox/TJLab/TimoDataWork/country_profiles/original", full.names = TRUE),
          to = "~/Dropbox/TJLab/TimoDataWork/country_profiles/edits/",
          overwrite = FALSE, recursive = TRUE, copy.mode = FALSE)
