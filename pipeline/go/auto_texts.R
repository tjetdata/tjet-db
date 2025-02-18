### auto-texts for country profiles

options(scipen = 999)

countries <- db[["Countries"]] %>%
  filter(!country %in% c(
    "Serbia and Montenegro",
    "Soviet Union",
    "Yugoslavia"
  )) %>%
  select(country, country_case, ccode, ccode_case, ccode_ksg)

# countries %>%
#   select(country_case, ccode_case) %>%
#   distinct() %>%
#   group_by(ccode_case) %>%
#   mutate(n = n()) %>%
#   filter(n > 1)

autotxt <- autoprep <- data <- list()

### summaries autoprep

autoprep$amnesties <- amnesties %>%
  left_join(countries %>% select(ccode, ccode_case),
    by = "ccode"
  ) %>%
  group_by(ccode_case) %>%
  reframe(
    amnesties = sum(amnesties),
    amnesties_yrs = list(sort(unique(year)))
  )

autoprep$domestic <- domestic %>%
  left_join(countries %>% select(ccode, ccode_case),
    by = "ccode"
  ) %>%
  group_by(ccode_case) %>%
  reframe(
    domestic = sum(trials_domestic),
    domestic_yrs = list(sort(unique(year)))
  )

autoprep$foreign <- foreign %>%
  left_join(countries %>% select(ccode, ccode_case),
    by = "ccode"
  ) %>%
  group_by(ccode_case) %>%
  reframe(
    foreign = sum(trials_foreign),
    foreign_yrs = list(sort(unique(year)))
  )

autoprep$intl <- intl %>%
  left_join(countries %>% select(ccode, ccode_case),
    by = "ccode"
  ) %>%
  group_by(ccode_case) %>%
  reframe(
    intl = sum(trials_intl),
    intl_yrs = list(sort(unique(year)))
  )

autoprep$reparations <- reparations %>%
  left_join(countries %>% select(ccode, ccode_case),
    by = "ccode"
  ) %>%
  group_by(ccode_case) %>%
  reframe(
    reparations = sum(reparations),
    reparations_yrs = list(sort(unique(year)))
  )

autoprep$tcs <- tcs %>%
  left_join(countries %>% select(ccode, ccode_case),
    by = "ccode"
  ) %>%
  group_by(ccode_case) %>%
  reframe(
    tcs = sum(tcs),
    tcs_yrs = list(sort(unique(year)))
  )

autoprep$vettings <- vettings %>%
  left_join(countries %>% select(ccode, ccode_case),
    by = "ccode"
  ) %>%
  group_by(ccode_case) %>%
  reframe(
    vettings = sum(vettings),
    vettings_yrs = list(sort(unique(year)))
  )

autoprep$summary <- reduce(
  autoprep,
  function(x, y) full_join(x, y, by = "ccode_case")
) %>%
  mutate(
    amnesties = ifelse(is.na(amnesties), 0, amnesties),
    domestic = ifelse(is.na(domestic), 0, domestic),
    foreign = ifelse(is.na(foreign), 0, foreign),
    intl = ifelse(is.na(intl), 0, intl),
    reparations = ifelse(is.na(reparations), 0, reparations),
    tcs = ifelse(is.na(tcs), 0, tcs),
    vettings = ifelse(is.na(vettings), 0, vettings)
  ) %>%
  left_join(
    countries %>%
      select(country_case, ccode_case) %>%
      distinct(),
    by = "ccode_case"
  ) %>%
  arrange(country_case)

autoprep$transitions <- db[["Transitions"]] %>%
  left_join(
    countries %>%
      select(country_case, ccode, ccode_case),
    by = c(ccode_cow = "ccode")
  ) %>%
  select(country_case, ccode_case, trans_year_begin) %>%
  arrange(country_case, trans_year_begin) %>%
  group_by(country_case) %>%
  reframe(
    ccode_case = unique(ccode_case),
    trans_year_begin = list(unique(trans_year_begin))
  )

autoprep$conflicts <- db[["ConflictDyads"]] %>%
  mutate(internationalized = ifelse(str_detect(intensity, "Internationalized"), 1, 0)) %>%
  rowwise() %>%
  mutate(years = list(ep_start_year:ep_end_year)) %>%
  ungroup() %>%
  select(dyad_id, conflict_id, gwno_loc, ep_start_year, ep_end_year, years, internationalized) %>%
  left_join(
    countries %>%
      select(country_case, ccode_ksg, ccode_case) %>%
      distinct(),
    by = c("gwno_loc" = "ccode_ksg")
  ) %>%
  select(country_case, ccode_case, dyad_id, conflict_id, years, internationalized) %>%
  group_by(country_case) %>%
  reframe(
    ccode_case = unique(ccode_case),
    conflicts = length(unique(conflict_id)),
    dyads = length(unique(dyad_id)),
    episodes = n(),
    years = list(sort(unique(unlist(years)))),
    years = list(unlist(years)[unlist(years) %in% 1970:2020]),
    int_ep = sum(internationalized)
  )

autoprep[["rankings"]] <- db[["dl_tjet_cy"]] %>%
  select(-country_case) %>%
  left_join(
    countries %>%
      select(country_case, ccode, ccode_case),
    by = c(ccode_cow = "ccode")
  ) %>%
  filter(year == 2020) %>%
  # filter(!is.na(access_rank) | !is.na(legacy_rank)) %>%
  filter(!is.na(legacy_rank)) %>%
  arrange(legacy_rank) %>%
  select(country_case, ccode_case, country_fr, legacy_rank) %>%
  mutate(n = max(legacy_rank))

### data for summary spreadsheet & auto texts

data[["Amnesties"]] <- db[["Amnesties"]] %>%
  left_join(
    countries %>%
      select(country_case, ccode, ccode_case),
    by = c(ccode_cow = "ccode")
  ) %>%
  mutate(
    what_hrv = ifelse(str_detect(
      whatCrimes,
      "human rights violations"
    ), 1, 0),
    who_pol = ifelse(str_detect(
      whoWasAmnestied,
      "protesters / political prisoners"
    ), 1, 0)
  ) %>%
  group_by(country_case) %>%
  mutate(
    beg = min(amnestyYear),
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
  select(
    country_case, ccode_case, beg, end, count_all, count_demtrans, count_conflict, count_dcj,
    count_pcj, count_peaceagree, count_prisoners, count_hrv
  ) %>%
  distinct() %>%
  arrange(country_case)

vars_dom <- c(
  "trials_domestic", "tran_trs_dom_dtj_sta", "tran_trs_dom_ctj_sta",
  "tran_trs_dom_dtj_ctj_sta", "regu_trs_dom_sta",
  "tran_cce_dom_dtj_ctj_sta", "regu_cce_dom_sta",
  "tran_trs_dom_dtj_ctj_sta_hi", "tran_cce_dom_dtj_ctj_sta_hi",
  "tran_trs_dom_ctj_opp", "tran_cce_dom_ctj_opp",
  "oppo_trs_dom_sta_opp", "oppo_cce_dom_sta_opp"
)
vars_int <- c(
  "trials_intl", "trs_int_sta", "trs_int_opp",
  "cce_int_sta", "cce_int_opp"
)
vars_for <- c(
  "trials_foreign", "trs_for_sta", "trs_for_opp",
  "cce_for_sta", "cce_for_opp"
)

data[["Domestic_cy"]] <- db[["dl_tjet_cy"]] %>%
  select(-country_case) %>%
  left_join(
    countries %>%
      select(country_case, ccode, ccode_case),
    by = c(ccode_cow = "ccode")
  ) %>%
  ### why this craziness? because what is outwardly the country-case does not line up perfectly with how we need to assign TJ events internally for consistency
  arrange(country_case, year) %>%
  filter(if_any(all_of(vars_dom), ~ . > 0)) %>%
  group_by(country_case) %>%
  reframe(
    ccode_case = unique(ccode_case),
    beg = min(year),
    end = max(year),
    total = sum(trials_domestic, na.rm = TRUE),
    tran_trs_dom_dtj_sta = sum(tran_trs_dom_dtj_sta, na.rm = TRUE),
    tran_cce_dom_dtj_sta = sum(tran_cce_dom_dtj_sta, na.rm = TRUE),
    tran_trs_dom_ctj_sta = sum(tran_trs_dom_ctj_sta, na.rm = TRUE),
    tran_cce_dom_ctj_sta = sum(tran_cce_dom_ctj_sta, na.rm = TRUE),
    tran_trs_dom_dtj_ctj_sta = sum(tran_trs_dom_dtj_ctj_sta, na.rm = TRUE),
    tran_cce_dom_dtj_ctj_sta = sum(tran_cce_dom_dtj_ctj_sta, na.rm = TRUE),
    tran_trs_dom_dtj_ctj_sta_hi = sum(tran_trs_dom_dtj_ctj_sta_hi, na.rm = TRUE),
    tran_cce_dom_dtj_ctj_sta_hi = sum(tran_cce_dom_dtj_ctj_sta_hi, na.rm = TRUE),
    regu_trs_dom_sta = sum(regu_trs_dom_sta, na.rm = TRUE),
    regu_cce_dom_sta = sum(regu_cce_dom_sta, na.rm = TRUE),
    tran_trs_dom_ctj_opp = sum(tran_trs_dom_ctj_opp, na.rm = TRUE),
    tran_cce_dom_ctj_opp = sum(tran_cce_dom_ctj_opp, na.rm = TRUE),
    oppo_trs_dom_sta_opp = sum(oppo_trs_dom_sta_opp, na.rm = TRUE),
    oppo_cce_dom_sta_opp = sum(oppo_cce_dom_sta_opp, na.rm = TRUE)
  )

data[["Intl_cy"]] <- db[["dl_tjet_cy"]] %>%
  select(-country_case) %>%
  left_join(
    countries %>%
      select(country_case, ccode, ccode_case),
    by = c(ccode_cow = "ccode")
  ) %>%
  arrange(country_case, year) %>%
  filter(if_any(all_of(vars_int), ~ . > 0)) %>%
  group_by(country_case) %>%
  reframe(
    ccode_case = unique(ccode_case),
    beg = min(year),
    end = max(year),
    trials_intl = sum(trials_intl, na.rm = TRUE),
    trs_int_hrs_con_all = sum(trs_int_sta + trs_int_opp, na.rm = TRUE),
    cce_int_hrs_con_all = sum(cce_int_sta + cce_int_opp, na.rm = TRUE)
  )

data[["Foreign_cy"]] <- db[["dl_tjet_cy"]] %>%
  select(-country_case) %>%
  left_join(
    countries %>%
      select(country_case, ccode, ccode_case),
    by = c(ccode_cow = "ccode")
  ) %>%
  arrange(country_case, year) %>%
  filter(if_any(all_of(vars_for), ~ . > 0)) %>%
  group_by(country_case) %>%
  reframe(
    ccode_case = unique(ccode_case),
    beg = min(year),
    end = max(year),
    trials_foreign = sum(trials_foreign, na.rm = TRUE),
    trs_for_hrs_con_all = sum(trs_for_sta + trs_for_opp, na.rm = TRUE),
    cce_for_hrs_con_all = sum(cce_for_sta + cce_for_opp, na.rm = TRUE)
  )

data[["Foreign"]] <- db[["Trials"]] %>%
  left_join(
    countries %>%
      select(country_case, ccode, ccode_case),
    by = c(ccode_Accused = "ccode")
  ) %>%
  select(-ccode_Accused) %>%
  rename(
    "countryAccused" = "country_case",
    "ccode_Accused" = "ccode_case"
  ) %>%
  left_join(
    countries %>%
      select(country_case, ccode, ccode_case),
    by = c(ccode_Trial = "ccode")
  ) %>%
  select(-ccode_Trial) %>%
  rename(
    "countryTrial" = "country_case",
    "ccode_Trial" = "ccode_case"
  ) %>%
  filter(trialType == "foreign") %>%
  select(trialID, countryAccused, ccode_Accused, countryTrial, ccode_Trial, yearStart, yearEnd, caseDescription) %>%
  arrange(countryAccused, yearStart)

data[["Reparations"]] <- db[["Reparations"]] %>%
  left_join(
    countries %>%
      select(country_case, ccode, ccode_case),
    by = c(ccode_cow = "ccode")
  ) %>%
  group_by(country_case) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  select(
    country_case, ccode_case, count, reparationID, yearCreated, yearBegin, yearEnd,
    individualReparations, collectiveReparations, beneficiariesCount
  ) %>%
  arrange(country_case, yearCreated)

data[["TruthCommissions"]] <- db[["TruthCommissions"]] %>%
  left_join(
    countries %>%
      select(country_case, ccode, ccode_case),
    by = c(ccode_cow = "ccode")
  ) %>%
  group_by(country_case) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  rename(
    "rec_prosecutions" = "recommendProsecutions",
    "rec_reparations" = "recommendReparations",
    "rec_reforms" = "reportRecommendInstitutionalReform",
    "reform_HRs" = "humanRightsReforms",
    "reform_legal" = "legalReform",
    "reform_judicial" = "judicialReforms",
    "reform_gender" = "genderReform",
    "reform_corruption" = "corruptionReforms",
    "reform_SSR" = "SecuritySectorReforms",
    "reform_vetting" = "vetting"
  ) %>%
  select(
    country_case, ccode_case, count, truthcommissionID, yearPassed,
    yearBeginOperation, yearCompleteOperation, finalReportIssued,
    reportPubliclyAvailable, rec_prosecutions, rec_reparations,
    rec_reforms, reform_HRs, reform_legal, reform_judicial, reform_gender,
    reform_corruption, reform_SSR, reform_vetting, consultedVictims,
    commissionersVictimGroups, encourageVictimTestimony
  ) %>%
  arrange(country_case, yearPassed)

data[["Vettings"]] <- db[["Vettings"]] %>%
  left_join(countries %>% select(country_case, ccode, ccode_case),
    by = c(ccode_cow = "ccode")
  ) %>%
  mutate(
    individual_conduct = case_when(
      str_detect(targetingWhy, "specific individual conduct") ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(
    country_case, ccode_case, vettingID, alterationOf, yearStart, yearEnd,
    individual_conduct, type_dismissal, type_ban, type_declassification,
    dateLaw
  ) %>%
  arrange(country_case, ccode_case, yearStart)

data[["ICC-interventions"]] <- db[["ICC"]] %>%
  select(-country) %>%
  left_join(countries %>% select(country_case, ccode, ccode_case),
    by = c(ccode_cow = "ccode")
  ) %>%
  select(country_case, ccode_case, ICC_referral, ICC_prelim_exam, ICC_prelimEnd, ICC_investigation) %>%
  arrange(country_case, ICC_prelim_exam)

data[["ICC-accused"]] <- db[["ICCaccused"]] %>%
  select(-country) %>%
  left_join(countries %>% select(country_case, ccode, ccode_case),
    by = c(ccode_Accused = "ccode")
  ) %>%
  select(
    country_case, ccode_case, nameOrDesc, ICC_arrest_warrant,
    ICC_arrestAppear, ICC_confirm_charges, ICC_proceedings,
    ICC_withdrawnDismissed, trialID, accusedID
  ) %>%
  arrange(country_case, ICC_arrest_warrant)

data[["Investigations"]] <- db[["Investigations"]] %>%
  left_join(countries %>% select(country_case, ccode, ccode_case),
    by = c(ccode_cow = "ccode")
  ) %>%
  select(
    country, country_case, ccode_case, beg, end, mandate,
    uninv_dompros, uninv_evcoll, uninv_intlpros, goals
  ) %>%
  arrange(country_case, beg)

### peace agreements: "https://peaceaccords.nd.edu/wp-content/uploads/2019/08/PAM_ID-V.1.5-Updated-29JULY2015.xlsx"
data[["peace-agreements"]] <- readxl::read_xlsx("data/PAM_ID-V.1.5-Updated-29JULY2015.xlsx") %>%
  rename("accord_name" = "accord name") %>%
  group_by(pam_caseid) %>%
  mutate(
    beg = min(year),
    end = max(year)
  ) %>%
  ungroup() %>%
  mutate(
    war_start = as_date(war_start),
    cease_date = as_date(cease_date)
  ) %>%
  left_join(
    countries %>%
      select(country_case, ccode, ccode_case),
    by = c(cowcode = "ccode")
  ) %>%
  select(
    pam_caseid, country_case, ccode_case, accord_name, war_start,
    cease_date, beg, end, amnest_prov, humrts_prov, prisr_prov, repar_prov,
    truth_prov
  ) %>%
  distinct() %>%
  arrange(country_case, cease_date)

### PAX: https://www.peaceagreements.org/search
tj_vars <- c(
  "TjGen", "TjAm", "TjAmPro", "TjSan", "TjPower", "TjCou", "TjJaNc",
  "TjJaIc", "TjMech", "TjPrire", "TjVet", "TjVic", "TjMis", "TjRep",
  "TjRSym", "TjRMa", "TjNR"
)
## have not adjusted this to ccode_case
data[["PAX"]] <- read_csv("data/pax_all_agreements_data_v6.csv") %>%
  arrange(Con, PPName, Dat) %>%
  filter(Stage %in% c("SubComp", "SubPar")) %>%
  filter(if_any(all_of(tj_vars), ~ . == 1)) %>%
  select(
    Con, PP, PPName, AgtId, Agt, Dat, Status, Agtp, Stage, Loc1GWNO,
    Loc2GWNO, UcdpCon, UcdpAgr, PamAgr, all_of(tj_vars)
  )

### write to file
### WE DON'T NEED THIS ANYMORE
# write_xlsx(data, path = "~/Dropbox/TJLab/TimoDataWork/country_profiles/summary_data.xlsx")

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

n_transform_nth <- function(x) {
  x %>%
    paste(" ", ., sep = "") %>%
    str_replace_all("1 ", "1st ") %>%
    str_replace_all("2 ", "2nd ") %>%
    str_replace_all("3 ", "3rd ") %>%
    str_replace_all("4 ", "4th ") %>%
    str_replace_all("5 ", "5th ") %>%
    str_replace_all("6 ", "6th ") %>%
    str_replace_all("7 ", "7th ") %>%
    str_replace_all("8 ", "8th ") %>%
    str_replace_all("9 ", "9th ") %>%
    str_replace_all("0 ", "0th ") %>%
    str_trim() %>%
    return()
}

### summary

autotxt[["summary"]] <- autoprep[["summary"]] %>%
  rowwise() %>%
  mutate(
    text = paste("For ",
      country_case,
      ", TJET has collected information on: ",
      str_flatten(
        c(
          if (amnesties > 0) {
            paste(
              amnesties,
              ifelse(amnesties == 1,
                "amnesty", "amnesties"
              ),
              ifelse(min(amnesties_yrs) == max(amnesties_yrs),
                paste("in", unique(amnesties_yrs)),
                paste(
                  "between",
                  min(amnesties_yrs),
                  "and",
                  max(amnesties_yrs)
                )
              )
            )
          },
          if (domestic > 0) {
            paste(
              domestic, "domestic",
              ifelse(domestic == 1,
                "trial", "trials"
              ),
              "starting",
              ifelse(min(domestic_yrs) == max(domestic_yrs),
                paste("in", unique(domestic_yrs)),
                paste(
                  "between",
                  min(domestic_yrs),
                  "and",
                  max(domestic_yrs)
                )
              )
            )
          },
          if (foreign > 0) {
            paste(
              foreign, "foreign",
              ifelse(foreign == 1,
                "trial", "trials"
              ),
              "starting",
              ifelse(min(foreign_yrs) == max(foreign_yrs),
                paste("in", unique(foreign_yrs)),
                paste(
                  "between",
                  min(foreign_yrs),
                  "and",
                  max(foreign_yrs)
                )
              )
            )
          },
          if (intl > 0) {
            paste(
              intl, "international",
              ifelse(intl == 1,
                "trial", "trials"
              ),
              "starting",
              ifelse(min(intl_yrs) == max(intl_yrs),
                paste("in", unique(intl_yrs)),
                paste(
                  "between",
                  min(intl_yrs),
                  "and",
                  max(intl_yrs)
                )
              )
            )
          },
          if (reparations > 0) {
            paste(
              reparations, "reparations",
              ifelse(reparations == 1,
                "policy", "policies"
              ),
              "created",
              ifelse(min(reparations_yrs) == max(reparations_yrs),
                paste("in", unique(reparations_yrs)),
                paste(
                  "between",
                  min(reparations_yrs),
                  "and",
                  max(reparations_yrs)
                )
              )
            )
          },
          if (tcs > 0) {
            paste(
              tcs, "truth",
              ifelse(tcs == 1,
                "commission", "commissions"
              ),
              "mandated",
              ifelse(min(tcs_yrs) == max(tcs_yrs),
                paste("in", unique(tcs_yrs)),
                paste(
                  "between",
                  min(tcs_yrs),
                  "and",
                  max(tcs_yrs)
                )
              )
            )
          },
          if (vettings > 0) {
            paste(
              vettings, "vetting",
              ifelse(vettings == 1,
                "policy", "policies"
              ),
              "starting",
              ifelse(min(vettings_yrs) == max(vettings_yrs),
                paste("in", unique(vettings_yrs)),
                paste(
                  "between",
                  min(vettings_yrs),
                  "and",
                  max(vettings_yrs)
                )
              )
            )
          }
        ),
        collapse = "; ", last = "; and "
      ),
      ".",
      sep = ""
    ) %>%
      n_transform(),
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text)

### transitions

autotxt[["Transitions"]] <- autoprep[["transitions"]] %>%
  rowwise() %>%
  mutate(
    text = paste("Based on well-known democracy data, TJET records ",
      length(trans_year_begin),
      " democratic ",
      ifelse(length(trans_year_begin) == 1,
        "transition", "transitions"
      ),
      " starting in ",
      str_flatten_comma(trans_year_begin, ", and "),
      ".",
      sep = ""
    ) %>%
      n_transform(),
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text)

### violent intrastate conflict episodes

autotxt[["Conflicts"]] <- autoprep[["conflicts"]] %>%
  rowwise() %>%
  mutate(
    text = "",
    text = paste("Based on the Uppsala Conflict Data Program, TJET records ",
      episodes,
      " violent intrastate conflict ",
      ifelse(episodes == 1, "episode ", "episodes "),
      ifelse(min(years) == max(years),
        paste("in", unique(years)),
        paste(
          "between",
          min(years),
          "and",
          max(years)
        )
      ),
      ifelse(length(years) > 1 & length(years) < max(years) - min(years) + 1,
        paste(" (during ",
          length(years),
          " calendar years)",
          sep = ""
        ),
        ""
      ),
      ", involving ",
      dyads,
      ifelse(dyads == 1,
        " armed opposition group",
        " distinct armed opposition groups"
      ),
      " fighting against the government.",
      sep = ""
    ) %>%
      n_transform(),
    text = list(c(
      text,
      if (int_ep > 0) {
        paste(
          int_ep,
          "conflict",
          ifelse(int_ep == 1, "episode was", "episodes were"),
          "internationalized by involvement of external state actors."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text)

### indices

pluriel <- c(
  "Philippines", "United States of America", "Maldives", "Fiji",
  "Comoros", "Netherlands", "Solomon Islands", "Seychelles", "United Arab Emirates"
)

autoprep[["rankings"]] %>%
  rowwise() %>%
  mutate(
    legacy = paste(
      "As of 2020,", country_case, "ranks",
      n_transform_nth(paste(legacy_rank, " ", sep = "")),
      "out of", n, "on TJET's legacy of violence index"
    ),
    legacy = str_flatten(legacy, " ") %>%
      str_trim(),
    legacy_fr = paste(
      "En 2020,", country_fr, "se classe",
      paste(legacy_rank, "e", sep = ""), "sur", n,
      "dans l'indice d'héritage de la violence de TJET."
    ),
    legacy_fr = str_flatten(legacy_fr, " ") %>%
      str_trim(),
    legacy_fr = ifelse(country_case %in% pluriel,
      str_replace(legacy_fr, "se classe", "se classent"),
      legacy_fr
    ),
  ) %>%
  ungroup() %>%
  # write_csv("~/Desktop/temp.csv")
  saveRDS(here::here("data", "rankings.rds"))

### TJ mechanisms

autotxt[["Amnesties"]] <- data[["Amnesties"]] %>%
  rowwise() %>%
  mutate(
    text = list(paste(country_case,
      " had ",
      count_all,
      ifelse(count_all == 1, " amnesty ", " amnesties "),
      ifelse(beg == end,
        paste("in", beg),
        paste("between", beg, "and", end)
      ),
      ".",
      sep = ""
    ) %>%
      n_transform()),
    text = list(c(
      text,
      if (count_demtrans > 0) {
        paste(
          count_demtrans,
          "occurred in the context of democratic transition."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = list(c(
      text,
      if (count_dcj > 0) {
        paste(
          count_dcj,
          ifelse(count_dcj == 1, "was", "were"),
          "passed during ongoing internal armed conflict."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = list(c(
      text,
      if (count_pcj > 0) {
        paste(
          count_pcj,
          ifelse(count_pcj == 1, "was", "were"),
          "passed after internal armed conflict."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = list(c(
      text,
      if (count_peaceagree > 0) {
        paste(
          count_peaceagree,
          ifelse(count_peaceagree == 1, "was", "were"),
          "part of a peace agreement."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = list(c(
      text,
      if (count_prisoners > 0) {
        paste(
          count_prisoners,
          ifelse(count_prisoners == 1, "amnesty", "amnesties"),
          "released political prisoners."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = list(c(
      text,
      if (count_hrv > 0) {
        paste(
          count_hrv,
          ifelse(count_hrv == 1, "amnesty", "amnesties"),
          "forgave human rights violations."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text)

autotxt[["Domestic_cy"]] <- data[["Domestic_cy"]] %>%
  rowwise() %>%
  mutate(
    text = list(c(
      "",
      if (total > 0) {
        paste(
          "TJET has compiled data on",
          total,
          "domestic",
          ifelse(total == 1, "prosecution", "prosecutions"),
          ifelse(beg == end,
            paste("in ", beg, ".", sep = ""),
            paste("between ", beg, " and ", end, ".", sep = "")
          )
        ) %>%
          n_transform()
      }
    )),
    text = list(c(
      text,
      if (tran_trs_dom_dtj_sta > 0 | regu_trs_dom_sta > 0 |
        tran_trs_dom_ctj_sta > 0 | tran_trs_dom_ctj_opp > 0 |
        oppo_trs_dom_sta_opp > 0) {
        paste("These include ",
          str_flatten(
            c(
              if (tran_trs_dom_dtj_sta > 0) {
                paste(
                  tran_trs_dom_dtj_sta,
                  "transitional human rights",
                  ifelse(tran_trs_dom_dtj_sta == 1,
                    "prosecution", "prosecutions"
                  ),
                  "of state agents, in which",
                  tran_cce_dom_dtj_sta,
                  ifelse(tran_cce_dom_dtj_sta == 1,
                    "person was", "persons were"
                  ),
                  "convicted"
                )
              },
              if (regu_trs_dom_sta > 0) {
                paste(
                  regu_trs_dom_sta,
                  "regular human rights",
                  ifelse(regu_trs_dom_sta == 1,
                    "prosecution", "prosecutions"
                  ),
                  "of state agents, in which",
                  regu_cce_dom_sta,
                  ifelse(regu_cce_dom_sta == 1,
                    "person was", "persons were"
                  ),
                  "convicted"
                )
              },
              if (tran_trs_dom_ctj_sta > 0) {
                paste(
                  tran_trs_dom_ctj_sta,
                  "intrastate conflict",
                  ifelse(tran_trs_dom_ctj_sta == 1,
                    "prosecution", "prosecutions"
                  ),
                  "of state agents, in which",
                  tran_cce_dom_ctj_sta,
                  ifelse(tran_cce_dom_ctj_sta == 1,
                    "person was", "persons were"
                  ),
                  "convicted"
                )
              },
              if (tran_trs_dom_ctj_opp > 0) {
                paste(
                  tran_trs_dom_ctj_opp,
                  "intrastate conflict",
                  ifelse(tran_trs_dom_ctj_opp == 1,
                    "prosecution", "prosecutions"
                  ),
                  "of opposition members, in which",
                  tran_cce_dom_ctj_opp,
                  ifelse(tran_cce_dom_ctj_opp == 1,
                    "person was", "persons were"
                  ),
                  "convicted"
                )
              },
              if (oppo_trs_dom_sta_opp > 0) {
                paste(
                  oppo_trs_dom_sta_opp,
                  "opposition",
                  ifelse(oppo_trs_dom_sta_opp == 1,
                    "prosecution", "prosecutions"
                  ),
                  "of state agents or opposition members, in which",
                  oppo_cce_dom_sta_opp,
                  ifelse(oppo_cce_dom_sta_opp == 1,
                    "person was", "persons were"
                  ),
                  "convicted"
                )
              }
            ),
            collapse = "; ", last = "; and ", na.rm = TRUE
          ),
          ".",
          sep = ""
        ) %>%
          n_transform() %>%
          str_replace_all("none persons were", "no one was")
      }
    )),
    # "Of XX trials that involved high-ranking state agents, XX were convicted."
    text = list(c(
      text,
      if (tran_trs_dom_dtj_ctj_sta_hi > 0) {
        paste(
          "In",
          tran_trs_dom_dtj_ctj_sta_hi,
          ifelse(tran_trs_dom_dtj_ctj_sta_hi == 1,
            "trial", "trials"
          ),
          "that involved high-ranking state agents,",
          tran_cce_dom_dtj_ctj_sta_hi,
          ifelse(tran_cce_dom_dtj_ctj_sta_hi == 1,
            "person was", "persons were"
          ),
          "convicted."
        ) %>%
          n_transform() %>%
          str_replace("none persons were", "no one was")
      }
    )),
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(
    country_case, ccode_case,
    # beg, end, total, # total domestic trials between beg and end
    # tran_trs_dom_dtj_sta, # transitional prosecutions of state agents in democratic transition context
    # tran_trs_dom_ctj_sta, # transitional prosecutions of state agents in conflict or post-conflict context
    # tran_trs_dom_dtj_ctj_sta, # transitional prosecutions of state agents
    # tran_cce_dom_dtj_ctj_sta, # convictions in transitional prosecutions of state agents
    # tran_trs_dom_dtj_ctj_sta_hi, # transitional prosecutions of high-ranking state agents
    # tran_cce_dom_dtj_ctj_sta_hi, # convictions in transitional prosecutions of high-ranking state agents
    # regu_trs_dom_sta, # regular human rights prosecutions of state agents
    # regu_cce_dom_sta, # convictions in regular human rights prosecutions of state agents
    # tran_trs_dom_ctj_opp, # transitional prosecutions of opposition members in conflict or post-conflict context
    # tran_cce_dom_ctj_opp, # convictions in transitional prosecutions of opposition members in conflict or post-conflict context
    # oppo_trs_dom_sta_opp, # prosecutions of state agents or opposition members in opposition context
    # oppo_cce_dom_sta_opp, # convictions in prosecutions of state agents or opposition members in opposition context
    text
  )

autotxt[["Foreign"]] <- data[["Foreign"]] %>%
  group_by(countryAccused) %>%
  mutate(
    count = n(),
    countryTrial = list(sort(unique(unlist(countryTrial)))),
    yearStart = list(unlist(yearStart)),
    yearEnd = list(unlist(yearEnd))
  ) %>%
  select(countryAccused, ccode_Accused, count, countryTrial, yearStart, yearEnd) %>%
  distinct() %>%
  rowwise() %>%
  mutate(
    countryTrial = str_flatten_comma(countryTrial, last = ", and ") %>%
      str_replace(" Netherlands", " the Netherlands") %>%
      str_replace(" United Kingdom", " the United Kingdom") %>%
      str_replace(" United States of America", " the United States of America"),
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
        paste(
          "between",
          min(yearStart),
          "and",
          max(yearStart)
        )
      ),
      ".",
      sep = ""
    ) %>%
      n_transform()
  ) %>%
  ungroup() %>%
  select(countryAccused, ccode_Accused, text) %>%
  rename(
    "country_case" = "countryAccused",
    "ccode_case" = "ccode_Accused"
  )

autotxt[["Intl"]] <- data[["Intl_cy"]] %>%
  rowwise() %>%
  mutate(
    text = paste("Nationals of ",
      country_case,
      " were subject to ",
      trs_int_hrs_con_all,
      " international ",
      ifelse(trs_int_hrs_con_all == 1,
        "prosecution ", "prosecutions "
      ),
      # " starting ",
      ifelse(beg == end,
        paste("in", beg),
        paste(
          "between",
          beg,
          "and",
          end
        )
      ),
      ", which led to ",
      cce_int_hrs_con_all,
      ifelse(cce_int_hrs_con_all == 1,
        " conviction. ", " convictions. "
      ),
      sep = ""
    ) %>%
      n_transform(),
    text = str_replace_all(text, "none convictions.", "no convictions.")
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text)

autotxt[["ICC"]] <- data[["ICC-interventions"]] %>%
  rowwise() %>%
  mutate(
    text = ifelse(is.na(ICC_prelimEnd),
      paste("The ICC's Office of the Prosecutor opened a preliminary examination of the situation in ",
        country_case, " in ", ICC_prelim_exam, ".",
        sep = ""
      ),
      paste("The ICC's Office of the Prosecutor carried out a preliminary examination of the situation in ",
        country_case, " from ", ICC_prelim_exam, " until ", ICC_prelimEnd, ".",
        sep = ""
      )
    ),
    text = list(c(
      if (!is.na(ICC_referral)) {
        paste(country_case, " was referred to the ICC in ", ICC_referral, ".", sep = "")
      },
      text
    )),
    text = list(c(
      text,
      if (!is.na(ICC_investigation)) {
        paste("The first investigation of a specific case began in ", ICC_investigation, ".", sep = "")
      }
    )),
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text)

autotxt[["ICCaccused"]] <- data[["ICC-accused"]] %>%
  group_by(country_case) %>%
  mutate(
    count = n(),
    count_appear = sum(!is.na(ICC_arrestAppear)),
    count_proceed = sum(!is.na(ICC_proceedings)),
    ICC_arrest_warrant = list(unlist(ICC_arrest_warrant)),
    ICC_arrestAppear = list(unlist(ICC_arrestAppear)),
    ICC_proceedings = list(unlist(ICC_proceedings))
  ) %>%
  select(
    country_case, ccode_case, count, ICC_arrest_warrant, count_appear, ICC_arrestAppear,
    count_proceed, ICC_proceedings
  ) %>%
  ungroup() %>%
  distinct() %>%
  rowwise() %>%
  mutate(
    ICC_arrestAppear = list(ICC_arrestAppear[!is.na(ICC_arrestAppear)]),
    ICC_proceedings = list(ICC_proceedings[!is.na(ICC_proceedings)]),
    text = paste("Starting in ",
      min(ICC_arrest_warrant),
      ", the ICC issued ",
      count,
      " arrest warrants, ",
      count_appear,
      " of which resulted in court appearances.",
      sep = ""
    ) %>%
      n_transform(),
    text = list(c(
      text,
      if (count_proceed > 0) {
        paste("Proceedings began in ",
          count_proceed,
          ifelse(count_proceed == 1, " case ", " cases "),
          ifelse(length(unique(ICC_proceedings)) == 1,
            paste("in", unique(ICC_proceedings)),
            paste(
              "between",
              min(ICC_proceedings, na.rm = TRUE),
              "and",
              max(ICC_proceedings, na.rm = TRUE)
            )
          ),
          ".",
          sep = ""
        ) %>%
          n_transform()
      }
    )),
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text)

autotxt[["UNinvestigations"]] <- data[["Investigations"]] %>%
  mutate(
    country = ifelse(country == "Iran/Iraq" & ccode_case == 630, "Iran", country),
    country = ifelse(country == "Iran/Iraq" & ccode_case == 645, "Iraq", country)
  ) %>%
  select(
    country, country_case, ccode_case, mandate, beg, end,
    uninv_dompros, uninv_evcoll, uninv_intlpros
  ) %>%
  arrange(country_case, mandate) %>%
  group_by(ccode_case, mandate) %>%
  mutate(
    beg = ifelse(ccode_case == 437 & mandate == "Office of the UN High Commissioner for Human Rights", beg, min(beg)),
    end = ifelse(ccode_case == 437 & mandate == "Office of the UN High Commissioner for Human Rights", end, max(end)),
    uninv_dompros = max(uninv_dompros),
    uninv_evcoll = max(uninv_evcoll),
    uninv_intlpros = max(uninv_intlpros)
  ) %>%
  distinct() %>%
  arrange(ccode_case, beg) %>%
  group_by(ccode_case) %>%
  mutate(
    count = n(),
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
  mutate(
    text = paste(country,
      " was subject to ",
      count,
      ifelse(count == 1, " UN investigation ", " UN investigations "),
      ifelse(min(beg) == max(end),
        paste("in", min(beg)),
        paste("between", min(beg), "and", max(end))
      ),
      ".",
      sep = ""
    ) %>%
      n_transform(),
    text = list(c(
      text,
      if (uninv_dompros > 0) {
        paste(
          uninv_dompros,
          ifelse(uninv_dompros == 1, "investigation", "investigations"),
          "aimed to encourage domestic prosecutions."
        ) %>%
          n_transform() %>%
          str_to_sentence() %>%
          str_trim()
      }
    )),
    text = list(c(
      text,
      if (uninv_intlpros > 0) {
        paste(
          uninv_intlpros,
          ifelse(uninv_intlpros == 1, "investigation", "investigations"),
          "aimed to support international prosecutions."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = list(c(
      text,
      if (uninv_evcoll > 0) {
        paste(
          uninv_evcoll,
          ifelse(uninv_evcoll == 1, "investigation", "investigations"),
          "aimed to collect evidence for prosecutions."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text)

autotxt[["Reparations"]] <- data[["Reparations"]] %>%
  mutate(
    implemented = ifelse(!is.na(yearBegin) | !is.na(yearEnd) | !is.na(beneficiariesCount), 1, 0),
    individual = case_when(
      individualReparations == "yes" ~ 1,
      TRUE ~ 0
    ),
    collective = case_when(
      collectiveReparations == "yes" ~ 1,
      TRUE ~ 0
    ),
    beneficiariesCount = ifelse(is.na(beneficiariesCount), 0, beneficiariesCount)
  ) %>%
  group_by(country_case) %>%
  mutate(
    yearCreated = list(unlist(yearCreated)),
    yearBegin = list(unlist(yearBegin)),
    yearEnd = list(unlist(yearEnd)),
    individual = sum(individual),
    collective = sum(collective),
    implemented = sum(implemented),
    beneficiariesCount = sum(beneficiariesCount)
  ) %>%
  select(
    country_case, ccode_case, count, yearCreated, yearBegin, yearEnd,
    individual, collective, beneficiariesCount, implemented
  ) %>%
  distinct() %>%
  rowwise() %>%
  mutate(
    yearBegin = list(yearBegin[!is.na(yearBegin)]),
    yearEnd = list(yearEnd[!is.na(yearEnd)]),
    text = "",
    text = list(c(
      text,
      if (length(yearBegin) == 0 & length(yearEnd) == 0) {
        paste(country_case,
          " mandated ",
          count,
          ifelse(count == 1, " reparations policy in ", " reparations policies in "),
          str_flatten_comma(unique(yearCreated), last = ", and "),
          # ifelse(length(yearCreated) == 1,
          #        paste("in", unique(yearCreated)),
          #        paste("between", min(yearCreated), "and", max(yearCreated))
          # ),
          ".",
          sep = ""
        ) %>%
          n_transform()
      }
    )),
    text = list(c(
      text,
      if (length(yearBegin) == 0 & length(yearEnd) == 1) {
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
          ".",
          sep = ""
        ) %>%
          n_transform()
      }
    )),
    text = list(c(
      text,
      if (length(yearBegin) > 0 & length(yearBegin) == length(yearEnd)) {
        paste(country_case,
          " implemented ",
          count,
          ifelse(count == 1, " reparations policy ", " reparations policies "),
          ifelse(min(yearBegin) == max(yearEnd),
            paste("in", min(yearBegin)),
            paste("between", min(yearBegin), "and", max(yearEnd))
          ),
          ".",
          sep = ""
        ) %>%
          n_transform()
      }
    )),
    text = list(c(
      text,
      if (length(yearBegin) > 0 & length(yearBegin) > length(yearEnd)) {
        paste(country_case,
          " implemented ",
          count,
          ifelse(count == 1, " reparations policy, ", " reparations policies, "),
          "starting in ",
          min(yearBegin),
          ".",
          sep = ""
        ) %>%
          n_transform()
      }
    )),
    text = list(c(
      text,
      if (beneficiariesCount > 0) {
        paste(
          "According to available information, there was a total of",
          beneficiariesCount,
          "individual beneficiaries."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = list(c(
      text,
      if (beneficiariesCount == 0) {
        paste("TJET found no information on the total number of beneficiaries.")
      }
    )),
    text = list(c(
      text,
      if (collective > 0) {
        paste(collective,
          ifelse(collective == 1, " reparations policy ", " reparations policies "),
          "provided collective benefits.",
          sep = ""
        ) %>%
          n_transform() %>%
          str_to_sentence() %>%
          str_trim()
      }
    )),
    text = list(c(
      text,
      if (implemented > 0 & implemented < count) {
        paste(
          "TJET found evidence on implementation only for",
          implemented,
          ifelse(implemented == 1, " reparations policy.", " reparations policies.")
        ) %>%
          n_transform()
      }
    )),
    text = list(c(
      text,
      if (implemented == 0) {
        paste("TJET found no evidence on implementation.")
      }
    )),
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text)

autotxt[["TruthCommissions"]] <- data[["TruthCommissions"]] %>%
  mutate(
    finalReportIssued = case_when(
      finalReportIssued == "Yes" ~ 1,
      TRUE ~ 0
    ),
    reportPubliclyAvailable = case_when(
      reportPubliclyAvailable == "Yes" ~ 1,
      TRUE ~ 0
    ),
    victims = case_when(
      consultedVictims == 1 |
        commissionersVictimGroups == "Yes" |
        encourageVictimTestimony == "Yes" ~ 1,
      TRUE ~ 0
    ),
    yearBeginOperation = ifelse(is.na(yearBeginOperation) & !is.na(yearCompleteOperation),
      yearPassed, yearBeginOperation
    )
  ) %>%
  group_by(country_case) %>%
  mutate(
    yearPassed = list(unlist(yearPassed)),
    yearBeginOperation = list(unlist(yearBeginOperation)),
    yearCompleteOperation = list(unlist(yearCompleteOperation)),
    finalReportIssued = sum(finalReportIssued),
    reportPubliclyAvailable = sum(reportPubliclyAvailable),
    rec_prosecutions = sum(rec_prosecutions),
    rec_reparations = sum(rec_reparations),
    rec_reforms = sum(rec_reforms),
    victims = sum(victims)
  ) %>%
  select(
    country_case, ccode_case, count, yearPassed, yearBeginOperation, yearCompleteOperation,
    finalReportIssued, reportPubliclyAvailable, rec_prosecutions,
    rec_reparations, rec_reforms, victims
  ) %>%
  distinct() %>%
  rowwise() %>%
  mutate(
    yearBeginOperation = list(yearBeginOperation[!is.na(yearBeginOperation)]),
    yearCompleteOperation = list(yearCompleteOperation[!is.na(yearCompleteOperation)]),
    text = paste(country_case,
      " mandated ",
      count,
      " truth ",
      ifelse(count == 1, "commission", "commissions"),
      " in ",
      str_flatten_comma(unique(yearPassed), last = ", and "),
      ".",
      sep = ""
    ) %>%
      n_transform(),
    ### one but not completed
    text = list(c(
      text,
      if (length(yearBeginOperation) == count & length(yearCompleteOperation) == 0 & count == 1) {
        paste("The commission began its operations in ",
          min(yearBeginOperation),
          "; TJET has found no evidence of completion.",
          sep = ""
        )
      }
    )),
    ### all started but not all completed
    text = list(c(
      text,
      if (length(yearBeginOperation) == count & length(yearCompleteOperation) != 0 & length(yearCompleteOperation) < count) {
        paste(length(yearCompleteOperation),
          " completed ",
          ifelse(length(yearCompleteOperation) == 1, "its", "their"),
          " operations by ",
          max(yearCompleteOperation),
          ".",
          sep = ""
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    ### not all started, but all that started also completed
    text = list(c(
      text,
      if (length(yearBeginOperation) < count & length(yearCompleteOperation) == length(yearBeginOperation)) {
        paste(length(yearBeginOperation),
          " of these operated between ",
          min(yearBeginOperation),
          " and ",
          max(yearCompleteOperation),
          ".",
          sep = ""
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    ### not all started, and none completed
    text = list(c(
      text,
      if (length(yearBeginOperation) < count & length(yearCompleteOperation) == 0) {
        paste(
          length(yearBeginOperation),
          "began operations but TJET has found no evidence of completion."
        ) %>%
          n_transform() %>%
          str_to_sentence() %>%
          str_replace("tjet", "TJET")
      }
    )),
    ### all completed
    text = list(c(
      text,
      if (length(yearCompleteOperation) == count) {
        paste(
          ifelse(count == 1,
            "The commission completed its",
            "The commissions completed their"
          ),
          " operations in ",
          str_flatten_comma(unique(yearCompleteOperation), last = ", and "),
          ".",
          sep = ""
        ) %>%
          n_transform()
      }
    )),
    text = list(c(
      text,
      if (finalReportIssued > 0) {
        paste(
          ifelse(finalReportIssued == count,
            ifelse(count == 1,
              "The commission issued",
              "The commissions issued"
            ),
            paste(finalReportIssued, "of the commissions issued")
          ),
          ifelse(finalReportIssued == 1, " a ", " "),
          ifelse(finalReportIssued == 1, "final report", "final reports"),
          ifelse(reportPubliclyAvailable > 0 & reportPubliclyAvailable < finalReportIssued,
            paste(
              ",",
              reportPubliclyAvailable,
              "of which"
            ),
            ", which"
          ),
          ifelse(reportPubliclyAvailable == 1,
            " is", " are"
          ),
          " publicly available.",
          sep = ""
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = list(c(
      text,
      if (finalReportIssued > 0 &
        (rec_prosecutions > 0 | rec_reparations > 0 | rec_reforms > 0)) {
        paste("The ",
          ifelse(finalReportIssued == 1, "report", "reports"),
          " included recommendations for ",
          str_flatten_comma(
            c(
              if (rec_prosecutions > 0) "prosecutions",
              if (rec_reparations > 0) "reparations",
              if (rec_reforms > 0) "institutional reforms"
            ),
            last = ", and ", na.rm = TRUE
          ),
          ".",
          sep = ""
        ) %>%
          n_transform() %>%
          str_to_sentence() %>%
          str_trim()
      }
    )),
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text)

autotxt[["Vettings"]] <- data[["Vettings"]] %>%
  mutate(
    alterationOf = ifelse(is.na(alterationOf), vettingID, alterationOf),
    alterationOf = str_replace(alterationOf, fixed("?"), "")
  ) %>%
  select(-vettingID) %>%
  rename("vettingID" = "alterationOf") %>%
  group_by(vettingID) %>%
  mutate(
    yearStart = min(yearStart),
    yearEnd = max(yearEnd),
    individual_conduct = max(individual_conduct),
    type_dismissal = max(type_dismissal),
    type_ban = max(type_ban),
    type_declassification = max(type_declassification)
  ) %>%
  select(
    country_case, ccode_case, vettingID, yearStart, yearEnd, individual_conduct,
    type_dismissal, type_ban, type_declassification
  ) %>%
  distinct() %>%
  group_by(country_case) %>%
  mutate(
    count = n(),
    yearStart = list(unlist(yearStart)),
    yearEnd = list(unlist(yearEnd)),
    individual_conduct = sum(individual_conduct),
    type_dismissal = sum(type_dismissal),
    type_ban = sum(type_ban),
    type_declassification = sum(type_declassification)
  ) %>%
  ungroup() %>%
  select(
    country_case, ccode_case, count, yearStart, yearEnd, individual_conduct,
    type_dismissal, type_ban, type_declassification
  ) %>%
  distinct() %>%
  rowwise() %>%
  mutate(
    yearStart = list(yearStart[!is.na(yearStart)]),
    yearEnd = list(yearEnd[!is.na(yearEnd)]),
    text = "",
    text = list(c(
      text,
      if (length(yearStart) == count &
        length(yearEnd) == count) {
        paste(country_case,
          " had ",
          count,
          " vetting ",
          ifelse(count == 1, "policy ", "policies "),
          ifelse(min(yearStart) == max(yearEnd),
            paste("in", max(yearEnd)),
            paste("between", min(yearStart), "and", max(yearEnd))
          ),
          ".",
          sep = ""
        ) %>%
          n_transform()
      }
    )),
    text = list(c(
      text,
      if (length(yearStart) == count &
        length(yearEnd) > 0 &
        length(yearEnd) < count) {
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
          ".",
          sep = ""
        ) %>%
          n_transform()
      }
    )),
    text = list(c(
      text,
      if (length(yearStart) == count &
        length(yearEnd) == 0) {
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
          sep = ""
        ) %>%
          n_transform()
      }
    )),
    text = list(c(
      text,
      if (individual_conduct == count) {
        paste(
          ifelse(individual_conduct == 1, "This policy", "These policies"),
          "provided sanctions based on past individual conduct."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = list(c(
      text,
      if (individual_conduct > 0 & individual_conduct < count) {
        paste(
          individual_conduct,
          ifelse(individual_conduct == 1, "policy", "policies"),
          "provided sanctions based on past individual conduct."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
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
    text = list(c(
      text,
      if (type_dismissal > 0 &
        # type_dismissal != count &
        type_dismissal == type_ban) {
        paste(
          type_dismissal,
          ifelse(type_dismissal == 1, "policy", "policies"),
          "prescribed both dismissals from current employment and bans from holding future office."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = list(c(
      text,
      if (type_dismissal > 0 &
        type_dismissal != type_ban) {
        paste(
          type_dismissal,
          ifelse(type_dismissal == 1, "policy", "policies"),
          "prescribed dismissals from current employment."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = list(c(
      text,
      if (type_ban > 0 &
        type_dismissal != type_ban) {
        paste(
          type_ban,
          ifelse(type_ban == 1, "policy", "policies"),
          "prescribed bans from holding future office."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    text = list(c(
      text,
      if (type_declassification > 0) {
        paste(
          type_declassification,
          ifelse(type_declassification == 1, "policy", "policies"),
          "aimed to declassify the records of former state security agents."
        ) %>%
          n_transform() %>%
          str_to_sentence()
      }
    )),
    # text = list(c(
    #   text,
    #   if (type_perjury > 0) {
    #     paste(
    #       type_perjury,
    #       ifelse(type_perjury == 1, "policy", "policies"),
    #       "included legal consequences for non-disclosure of relevant past activities."
    #     ) %>%
    #       n_transform() %>%
    #       str_to_sentence()
    #   }
    # )),
    text = str_flatten(text, " ") %>%
      str_trim()
  ) %>%
  ungroup() %>%
  select(country_case, ccode_case, text)

### for integrating into Airtable

autotxt <- db[["Countries"]] %>%
  filter(include) %>%
  select(country_case, ccode_case) %>%
  arrange(country_case) %>%
  left_join(
    autotxt[["summary"]] %>%
      rename("summary" = "text"),
    by = c("country_case", "ccode_case")
  ) %>%
  mutate(summary = ifelse(is.na(summary),
    paste("TJET has found no information on transitional justice in ",
      country_case, ".",
      sep = ""
    ),
    summary
  )) %>%
  left_join(
    autotxt[["Transitions"]] %>%
      rename("regime" = "text"),
    by = c("country_case", "ccode_case")
  ) %>%
  mutate(regime = ifelse(is.na(regime),
    paste(
      "TJET records no democratic transitions in",
      country_case,
      "between 1970 and 2020."
    ),
    regime
  )) %>%
  left_join(
    autotxt[["Conflicts"]] %>%
      rename("conflict" = "text"),
    by = c("country_case", "ccode_case")
  ) %>%
  mutate(conflict = ifelse(is.na(conflict),
    paste(
      "Based on the Uppsala Conflict Data Program, TJET records no episodes of violent intrastate conflict in",
      country_case,
      "between 1970 and 2020."
    ),
    conflict
  )) %>%
  left_join(
    autotxt[["Amnesties"]] %>%
      rename("amnesties" = "text"),
    by = c("country_case", "ccode_case")
  ) %>%
  left_join(
    autotxt[["Domestic_cy"]] %>%
      rename("domestic" = "text"),
    by = c("country_case", "ccode_case")
  ) %>%
  left_join(
    autotxt[["Foreign"]] %>%
      rename("foreign" = "text"),
    by = c("country_case", "ccode_case")
  ) %>%
  left_join(
    autotxt[["Intl"]] %>%
      rename("intl" = "text"),
    by = c("country_case", "ccode_case")
  ) %>%
  left_join(
    autotxt[["ICC"]] %>%
      rename("intl2" = "text"),
    by = c("country_case", "ccode_case")
  ) %>%
  left_join(
    autotxt[["ICCaccused"]] %>%
      rename("intl3" = "text"),
    by = c("country_case", "ccode_case")
  ) %>%
  rowwise() %>%
  mutate(
    intl = str_flatten(c(intl, intl2, intl3), collapse = " ", na.rm = TRUE),
    intl = ifelse(intl == "", NA, intl)
  ) %>%
  ungroup() %>%
  select(-intl2, -intl3) %>%
  left_join(
    autotxt[["Reparations"]] %>%
      rename("reparations" = "text"),
    by = c("country_case", "ccode_case")
  ) %>%
  left_join(
    autotxt[["TruthCommissions"]] %>%
      rename("tcs" = "text"),
    by = c("country_case", "ccode_case")
  ) %>%
  left_join(
    autotxt[["Vettings"]] %>%
      rename("vetting" = "text"),
    by = c("country_case", "ccode_case")
  ) %>%
  left_join(
    autotxt[["UNinvestigations"]] %>%
      rename("un" = "text"),
    by = c("country_case", "ccode_case")
  )
# write_csv(file = "~/Desktop/temp.csv", na = "")



### auto-text fields check
### 'auto_' fields are generated here and need to be transfered to Airtable
### 'txt_' fields are manually adjusted in Airtable
check_vars <- paste("chk_", c("summary", "regime", "conflict", "amnesties", "domestic", "foreign", "intl", "reparations", "tcs", "vetting", "un"), sep = "")
chk <- db[["Countries"]] %>%
  filter(include) %>%
  full_join(autotxt,
    by = c("country_case", "ccode_case")
  ) %>%
  mutate(
    chk_summary = ifelse(
      auto_summary != summary | (!is.na(summary) & is.na(auto_summary)), 1, 0
    ),
    chk_regime = ifelse(
      auto_regime != regime | (!is.na(regime) & is.na(auto_regime)), 1, 0
    ),
    chk_conflict = ifelse(
      auto_conflict != conflict | (!is.na(conflict) & is.na(auto_conflict)), 1, 0
    ),
    chk_amnesties = ifelse(
      auto_amnesties != amnesties | (!is.na(amnesties) & is.na(auto_amnesties)), 1, 0
    ),
    chk_domestic = ifelse(
      auto_domestic != domestic | (!is.na(domestic) & is.na(auto_domestic)), 1, 0
    ),
    chk_foreign = ifelse(
      auto_foreign != foreign | (!is.na(foreign) & is.na(auto_foreign)), 1, 0
    ),
    chk_intl = ifelse(
      auto_intl != intl | (!is.na(intl) & is.na(auto_intl)), 1, 0
    ),
    chk_reparations = ifelse(
      auto_reparations != reparations | (!is.na(reparations) & is.na(auto_reparations)), 1, 0
    ),
    chk_tcs = ifelse(
      auto_tcs != tcs | (!is.na(tcs) & is.na(auto_tcs)), 1, 0
    ),
    chk_vetting = ifelse(
      auto_vetting != vetting | (!is.na(vetting) & is.na(auto_vetting)), 1, 0
    ),
    chk_un = ifelse(
      auto_un != un | (!is.na(un) & is.na(auto_un)), 1, 0
    ),
  ) %>%
  select(
    country_case, all_of(check_vars), summary, regime, conflict, amnesties,
    domestic, intl, foreign, reparations, tcs, vetting, un
  ) %>%
  filter(if_any(all_of(check_vars), ~ . == 1)) %>%
  write_csv("~/Desktop/temp.csv", na = "") %>%
  print(n = Inf)

if (nrow(chk) > 0) warning("Some auto-texts in Airtable do not match the current data!")
