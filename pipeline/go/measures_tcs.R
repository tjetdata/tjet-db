df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  independence_opts = NULL, aims_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_dtj, -tcs_dtj_beg) 

df <- TCmeasure(
  cy = df, new_col_name = "tcs_ctj",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  independence_opts = NULL, aims_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_ctj, -tcs_ctj_beg) 

df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_ctj",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  independence_opts = NULL, aims_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_dtj_ctj, -tcs_dtj_ctj_beg) 

df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_victim_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = c(
    "truth for victims", "memorialization", "apology",
    "recognition of victims", "reparation"
  ),
  independence_opts = NULL,
  consult_vars = "consultedVictims",
  powers_vars = "allocateReparations",
  testimony_vars = "encourageVictimTestimony",
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_dtj_victim_process_binary, -tcs_dtj_victim_process_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_victim_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = "recommendReparations",
  monitor_vars = "mandatePeriodicMonitoringImplementation"
) %>%
  select(-tcs_dtj_victim_outcome_binary, -tcs_dtj_victim_outcome_created) 

df <- TCmeasure(
  cy = df, new_col_name = "tcs_ctj_victim_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = c(
    "truth for victims", "memorialization", "apology",
    "recognition of victims", "reparation"
  ),
  independence_opts = NULL,
  consult_vars = "consultedVictims",
  powers_vars = "allocateReparations",
  testimony_vars = "encourageVictimTestimony",
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_ctj_victim_process_binary, -tcs_ctj_victim_process_created) 
df <- TCmeasure(
  cy = df, new_col_name = "tcs_ctj_victim_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = "recommendReparations",
  monitor_vars = "mandatePeriodicMonitoringImplementation"
) %>%
  select(-tcs_ctj_victim_outcome_binary, -tcs_ctj_victim_outcome_created) 
 
df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_ctj_victim_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = c(
    "truth for victims", "memorialization", "apology",
    "recognition of victims", "reparation"
  ),
  independence_opts = NULL,
  consult_vars = "consultedVictims",
  powers_vars = "allocateReparations",
  testimony_vars = "encourageVictimTestimony",
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_dtj_ctj_victim_process_binary, -tcs_dtj_ctj_victim_process_created) 
df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_ctj_victim_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = "recommendReparations",
  monitor_vars = "mandatePeriodicMonitoringImplementation"
) %>%
  select(-tcs_dtj_ctj_victim_outcome_binary, -tcs_dtj_ctj_victim_outcome_created) 

df <- TCmeasure(
  cy = df, new_col_name = "tcs_victim_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = c(
    "truth for victims", "memorialization", "apology",
    "recognition of victims", "reparation"
  ),
  independence_opts = NULL,
  consult_vars = "consultedVictims",
  powers_vars = "allocateReparations",
  testimony_vars = "encourageVictimTestimony",
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_victim_process_binary, -tcs_victim_process_created) 
df <- TCmeasure(
  cy = df, new_col_name = "tcs_victim_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = "recommendReparations",
  monitor_vars = "mandatePeriodicMonitoringImplementation"
) %>%
  select(-tcs_victim_outcome_binary, -tcs_victim_outcome_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_recreparations",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = "recommendReparations",
  monitor_vars = NULL
) %>% 
  select(-tcs_recreparations_binary, -tcs_recreparations_created) 

df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_account_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = c(
    "accountability", "responsibility",
    "prevention of human rights violations"
  ),
  independence_opts = c(
    "partially independent",
    "fully independent"
  ),
  consult_vars = NULL,
  powers_vars = c(
    "compelTestimony", "supportProsecutions",
    "namePerpetrators"
  ),
  testimony_vars = "perpetratorTestimony",
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>% 
  select(-tcs_dtj_account_process_binary, -tcs_dtj_account_process_created) 
df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_account_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = "recommendProsecutions",
  monitor_vars = "mandatePeriodicMonitoringImplementation"
) %>%
  select(-tcs_dtj_account_outcome_binary, -tcs_dtj_account_outcome_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_ctj_account_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = c(
    "accountability", "responsibility",
    "prevention of human rights violations"
  ),
  independence_opts = c(
    "partially independent",
    "fully independent"
  ),
  consult_vars = NULL,
  powers_vars = c(
    "compelTestimony", "supportProsecutions",
    "namePerpetrators"
  ),
  testimony_vars = "perpetratorTestimony",
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_ctj_account_process_binary, -tcs_ctj_account_process_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_ctj_account_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = "recommendProsecutions",
  monitor_vars = "mandatePeriodicMonitoringImplementation"
) %>%
  select(-tcs_ctj_account_outcome_binary, -tcs_ctj_account_outcome_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_ctj_account_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = c(
    "accountability", "responsibility",
    "prevention of human rights violations"
  ),
  independence_opts = c(
    "partially independent",
    "fully independent"
  ),
  consult_vars = NULL,
  powers_vars = c(
    "compelTestimony", "supportProsecutions",
    "namePerpetrators"
  ),
  testimony_vars = "perpetratorTestimony",
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_dtj_ctj_account_process_binary, -tcs_dtj_ctj_account_process_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_ctj_account_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = "recommendProsecutions",
  monitor_vars = "mandatePeriodicMonitoringImplementation"
) %>%
  select(-tcs_dtj_ctj_account_outcome_binary, -tcs_dtj_ctj_account_outcome_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_peace_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = c(
    "reconciliation", "coexistence", "dialogue",
    "non-recurrence"
  ),
  independence_opts = NULL, consult_vars = NULL,
  powers_vars = "grantAmnesty",
  testimony_vars = "heldPublicHearings",
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_dtj_peace_process_binary, -tcs_dtj_peace_process_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_peace_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_dtj_peace_outcome_binary, -tcs_dtj_peace_outcome_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_ctj_peace_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = c(
    "reconciliation", "coexistence", "dialogue",
    "non-recurrence"
  ),
  independence_opts = NULL, consult_vars = NULL,
  powers_vars = "grantAmnesty",
  testimony_vars = "heldPublicHearings",
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_ctj_peace_process_binary, -tcs_ctj_peace_process_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_ctj_peace_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_ctj_peace_outcome_binary, -tcs_ctj_peace_outcome_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_ctj_peace_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = c(
    "reconciliation", "coexistence", "dialogue",
    "non-recurrence"
  ),
  independence_opts = NULL, consult_vars = NULL,
  powers_vars = "grantAmnesty",
  testimony_vars = "heldPublicHearings",
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_dtj_ctj_peace_process_binary, -tcs_dtj_ctj_peace_process_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_ctj_peace_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_dtj_ctj_peace_outcome_binary, -tcs_dtj_ctj_peace_outcome_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_reform_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = c(
    "historial truth", "institutional reform",
    "addressing corruption"
  ),
  independence_opts = c("partially independent", "fully independent"),
  consult_vars = NULL,
  powers_vars = "recommendInstitutionalReforms",
  testimony_vars = "heldPublicHearings",
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_dtj_reform_process_binary, -tcs_dtj_reform_process_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_reform_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = "reportRecommendInstitutionalReform",
  monitor_vars = "mandatePeriodicMonitoringImplementation"
) %>%
  select(-tcs_dtj_reform_outcome_binary, -tcs_dtj_reform_outcome_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_ctj_reform_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = c(
    "historial truth", "institutional reform",
    "addressing corruption"
  ),
  independence_opts = c("partially independent", "fully independent"),
  consult_vars = NULL,
  powers_vars = "recommendInstitutionalReforms",
  testimony_vars = "heldPublicHearings",
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_ctj_reform_process_binary, -tcs_ctj_reform_process_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_ctj_reform_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = "reportRecommendInstitutionalReform",
  monitor_vars = "mandatePeriodicMonitoringImplementation"
) %>%
  select(-tcs_ctj_reform_outcome_binary, -tcs_ctj_reform_outcome_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_ctj_reform_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = c(
    "historial truth", "institutional reform",
    "addressing corruption"
  ),
  independence_opts = c("partially independent", "fully independent"),
  consult_vars = NULL,
  powers_vars = "recommendInstitutionalReforms",
  testimony_vars = "heldPublicHearings",
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_dtj_ctj_reform_process_binary, -tcs_dtj_ctj_reform_process_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_dtj_ctj_reform_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = "fitsConflictTJ",
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = "reportRecommendInstitutionalReform",
  monitor_vars = "mandatePeriodicMonitoringImplementation"
) %>%
  select(-tcs_dtj_ctj_reform_outcome_binary, -tcs_dtj_ctj_reform_outcome_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_metcriteria", operated = FALSE,
  start_year_var = "yearPassed",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = NULL, monitor_vars = NULL
) %>% 
  select(-tcs_metcriteria, -tcs_metcriteria_beg) 

df <- TCmeasure(
  cy = df, new_col_name = "tcs_operated",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = NULL, monitor_vars = NULL
) %>% 
  select(-tcs_operated, -tcs_operated_beg) 

df <- TCmeasure(
  cy = df, new_col_name = "tcs_goalstruth",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = c("historial truth", "truth for victims"),
  independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = NULL, monitor_vars = NULL
) %>% 
  select(-tcs_goalstruth_binary, -tcs_goalstruth_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_independent",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL,
  independence_opts = c("partially independent", "fully independent"),
  consult_vars = NULL, powers_vars = NULL, testimony_vars = NULL,
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_independent_binary, -tcs_independent_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_ind_no",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL,
  independence_opts = c("don't know", "not independent"),
  consult_vars = NULL, powers_vars = NULL, testimony_vars = NULL,
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_ind_no_binary, -tcs_ind_no_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_ind_part",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL,
  independence_opts = c("partially independent"),
  consult_vars = NULL, powers_vars = NULL, testimony_vars = NULL,
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_ind_part_binary, -tcs_ind_part_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_ind_full",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL,
  independence_opts = c("fully independent"),
  consult_vars = NULL, powers_vars = NULL, testimony_vars = NULL,
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_ind_full_binary, -tcs_ind_full_created)
df <- df %>%
  mutate(tcs_independent_scale = case_when(
    tcs_ind_no == 1 ~ 0,
    tcs_ind_part == 1 ~ 1,
    tcs_ind_full == 1 ~ 2,
    TRUE ~ 0
  )) %>%
  select(-tcs_ind_no, -tcs_ind_no_beg, -tcs_ind_part, -tcs_ind_part_beg, 
         -tcs_ind_full, -tcs_ind_full_beg)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_harms",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  harms_vars = c(
    "torture", "death", "disappearance", "SGBV",
    "forcedDisplacement", "corruption", "crimesOther"
  ),
  aims_opts = NULL,
  independence_opts = NULL,
  consult_vars = NULL, powers_vars = NULL, testimony_vars = NULL,
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_harms_binary, -tcs_harms_created) %>%
  mutate(
    tcs_harms = case_when(
      tcs_harms == 0 ~ 0,
      tcs_harms %in% 1:2 ~ 1,
      tcs_harms %in% 3:5 ~ 2,
      tcs_harms %in% 6:7 ~ 3), 
    tcs_harms_beg = case_when(
      tcs_harms_beg == 0 ~ 0,
      tcs_harms_beg %in% 1:2 ~ 1,
      tcs_harms_beg %in% 3:5 ~ 2,
      tcs_harms_beg %in% 6:7 ~ 3)
    )

df <- TCmeasure(
  cy = df, new_col_name = "tcs_powers",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = c(
    "investigateStateMembers", "compelTestimony",
    "accessOfficialDocuments", "namePerpetrators",
    "recommendInstitutionalReforms"
  ),
  testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_powers_binary, -tcs_powers_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_power_investigate",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = c("investigateStateMembers"),
  testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_power_investigate_binary, -tcs_power_investigate_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_power_compel",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = c("compelTestimony"),
  testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_power_compel_binary, -tcs_power_compel_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_power_docs",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = c("accessOfficialDocuments"),
  testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_power_docs_binary, -tcs_power_docs_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_power_name",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = c("namePerpetrators"),
  testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_power_name_binary, -tcs_power_name_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_power_reform",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = c("recommendInstitutionalReforms"),
  testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_power_reform_binary, -tcs_power_reform_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_report",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL,
  independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "finalReportIssued",
  recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_report_binary, -tcs_report_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_public_process",
  start_year_var = "yearBeginOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL,
  testimony_vars = "heldPublicHearings",
  reports_vars = NULL, recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_public_process_binary, -tcs_public_process_created)
df <- TCmeasure(
  cy = df, new_col_name = "tcs_public_outcome",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL,
  reports_vars = "reportPubliclyAvailable",
  recommend_vars = NULL, monitor_vars = NULL
) %>%
  select(-tcs_public_outcome_binary, -tcs_public_outcome_created)

### this should be called reform recommendations & change in codebook
df <- TCmeasure(
  cy = df, new_col_name = "tcs_recommendations",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = c(
    "vetting", "SecuritySectorReforms",
    "legalReform", "judicialReforms",
    "humanRightsReforms", "corruptionReforms",
    "genderReform"
  ),
  monitor_vars = NULL
) %>%
  select(-tcs_recommendations_binary, -tcs_recommendations_created)

df <- TCmeasure(
  cy = df, new_col_name = "tcs_monitoring",
  start_year_var = "yearCompleteOperation",
  filter_nexus_vars = NULL,
  filter_crimes_vars = "all",
  aims_opts = NULL, independence_opts = NULL, consult_vars = NULL,
  powers_vars = NULL, testimony_vars = NULL, reports_vars = NULL,
  recommend_vars = NULL,
  monitor_vars = "mandatePeriodicMonitoringImplementation"
) %>%
  select(-tcs_monitoring_binary, -tcs_monitoring_created)
