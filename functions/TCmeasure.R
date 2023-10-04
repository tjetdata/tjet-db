## function for subsetting TC data and calculating indices
## include fields (or "all") or options (for independence & aims) 
## for indicators to be included; use NULL for those to exclude
## there are error checks built into the function
TCmeasure <- function(cy, new_col_name, start_year_var, nexus_vars, crimes_vars, 
                      independence_opts, aims_opts, consult_vars, powers_vars, 
                      testimony_vars, reports_vars, recommend_vars, monitor_vars
){
  
  suffix <- "\n  OR NULL if this aspect is not relevant to the new measure"
  
  if(missing(new_col_name) | length(new_col_name) != 1 | 
     class(new_col_name) != "character") 
    stop("Missing argument: provide a unique variable name in 'new_col_name';", 
         "\n  must be a string")
  
  all_year_vars <- c("yearBeginOperation", "yearCompleteOperation")
  error <- expression(
    stop("Missing argument: specify in 'start_year_var' which year variable to use as",
         "\n  the starting point for the TC concept to be measured; choose from:", 
         "\n  ", paste(all_year_vars, collapse = " OR ")) )  
  if(missing(start_year_var)) eval(error) 
  else if(length(start_year_var) != 1 | 
          !start_year_var %in% all_year_vars) eval(error) 
  
  all_nexus_vars <- c("all", "fitsPostAutocraticTJ", "fitsConflictTJ", 
                      "beganOperatingDuringIntraConfl", 
                      "beganOperatingAfterIntraConfl")
  if(missing(nexus_vars) | sum(!nexus_vars %in% all_nexus_vars) > 0) 
    stop("Missing argument: for subsetting, specify in 'nexus_vars' one or more of:", 
         "\n  ", paste(all_nexus_vars, collapse = "; "))
  
  all_crimes_vars <- c("all", "torture", "death", "disappearance", "SGBV")
  if(missing(crimes_vars) | sum(!crimes_vars %in% all_crimes_vars) > 0) 
    stop("Missing argument: for subsetting, specify in 'crimes_vars' one or more of:",
         "\n  ", paste(all_crimes_vars, collapse = "; ")) 
  
  all_independence_opts <- c("all", "don't know", "not independent", 
                             "partially independent", "fully independent")
  error <- expression(
    stop("Missing argument: for a scale, specify in 'independence_opts' one or more of:",
                           "\n  ", paste(all_independence_opts, collapse = "; "), suffix) )
  if(missing(independence_opts) ) eval(error) 
  else if(length(independence_opts) > 0) {
    if(sum(!independence_opts %in% all_independence_opts) > 0) eval(error) 
  }
  
  all_aims_opts <- c('accountability', 'addressing corruption', 'apology', 
                     'coexistence', 'dialogue', 'historial truth', 
                     'institutional reform', 'memorialization', 'non-recurrence', 
                     'prevention of human rights violations', 
                     'recognition of victims', 'reconciliation', 'reparation', 
                     'responsibility', 'restitution', 'truth for victims')
  error <- expression(
    stop("Missing argument: for a scale, specify in 'aims_opts' one or more of:",
         "\n  ", paste(all_aims_opts, collapse = "; "), suffix) )
  if(missing(aims_opts) ) eval(error) 
  else if(length(aims_opts) > 0) {
    if(sum(!aims_opts %in% all_aims_opts) > 0) eval(error)
  }  

  all_consult_vars <- c("consultedVictims")
  error <- expression(
    stop("Missing argument: for a scale, specify in 'consult_vars': ",
         paste(all_consult_vars, collapse = "; "), suffix) ) 
  if(missing(consult_vars) ) eval(error)
  else if(length(consult_vars) > 0) {
    if(sum(!consult_vars %in% all_consult_vars) > 0) eval(error)
  }
  
  all_powers_vars <- c("compelTestimony", "grantAmnesty", "supportProsecutions", 
                       "namePerpetrators", "recommendInstitutionalReforms", 
                       "allocateReparations")
  error <- expression(
    stop("Missing argument: for a scale, specify in 'powers_vars' one or more of:",
         "\n  ", paste(all_powers_vars, collapse = "; "), suffix) ) 
  if(missing(powers_vars) ) eval(error)
  else if(length(powers_vars) > 0) {
    if(sum(!powers_vars %in% all_powers_vars) > 0) eval(error)
  }
  
  all_testimony_vars <- c("heldPublicHearings", "testimonies", 
                          "encourageVictimTestimony", "perpetratorTestimony")
  error <- expression(
    stop("Missing argument: for a scale, specify in 'testimony_vars' one or more of:",
         "\n  ", paste(all_testimony_vars, collapse = "; "), suffix) ) 
  if(missing(testimony_vars) ) eval(error)
  else if(length(testimony_vars) > 0) {
    if(sum(!testimony_vars %in% all_testimony_vars) > 0) eval(error)
  }
  
  all_reports_vars = c("finalReportIssued", "reportPubliclyAvailable", 
                       "finalReportRecommendations")
  error <- expression(
    stop("Missing argument: for a scale, specify in 'reports_vars' one or more of:",
         "\n  ", paste(all_reports_vars, collapse = "; "), suffix) )
  if(missing(reports_vars) ) eval(error) 
  else if(length(reports_vars) > 0) {
    if(sum(!reports_vars %in% all_reports_vars) > 0) eval(error) 
  }
  
  all_recommend_vars = c("recommendProsecutions", "recommendReparations", 
                         "reportRecommendInstitutionalReform", "vetting")
  error <- expression(
    stop("Missing argument: for a scale, specify in 'recommend_vars' one or more of:",
         "\n  ", paste(all_recommend_vars, collapse = "; "), suffix) )
  if(missing(recommend_vars) ) eval(error)
  else if(length(recommend_vars) > 0) {
    if(sum(!recommend_vars %in% all_recommend_vars) > 0) eval(error)
  }
  
  all_monitor_vars = c("mandatePeriodicMonitoringImplementation")
  error <- expression(
    stop("Missing argument: for a scale, specify in 'monitor_vars': ",
         paste(all_monitor_vars, collapse = "; "), suffix) )
  if(missing(monitor_vars)) eval(error)
  else if(length(monitor_vars) > 0) {
    if(sum(!monitor_vars %in% all_monitor_vars) > 0) eval(error)
  }
  
  ## subsetting & calc
  
  new <- db[["TruthCommissions"]] %>%
    mutate(all = 1, 
           heldPublicHearings = ifelse(is.na(heldPublicHearings), "Unknown",
                                       heldPublicHearings),
           heldPublicHearings = ifelse(heldPublicHearings == "Yes", 1, 0),
           testimonies = ifelse(is.na(testimonies), "Unknown", testimonies),
           testimonies = ifelse(testimonies == "Yes", 1, 0),
           encourageVictimTestimony = ifelse(
             is.na(encourageVictimTestimony),
             "Unknown", encourageVictimTestimony),
           encourageVictimTestimony = ifelse(encourageVictimTestimony == "Yes", 
                                             1, 0), 
           perpetratorTestimony = ifelse(is.na(perpetratorTestimony),
                                         "Unknown", perpetratorTestimony),
           perpetratorTestimony = ifelse(perpetratorTestimony == "Yes", 
                                         1, 0), 
           finalReportIssued = ifelse(is.na(finalReportIssued), "Unknown", 
                                      finalReportIssued),
           finalReportIssued = ifelse(finalReportIssued == "Yes", 1, 0),
           reportPubliclyAvailable = ifelse(is.na(reportPubliclyAvailable), 
                                            "Unknown", reportPubliclyAvailable),
           reportPubliclyAvailable = ifelse(
             reportPubliclyAvailable == "Yes", 1, 0),
           finalReportRecommendations = ifelse(
             is.na(finalReportRecommendations), 
             "Unknown", finalReportRecommendations),
           finalReportRecommendations = ifelse(
             finalReportRecommendations == "Yes", 1, 0), 
           mandatePeriodicMonitoringImplementation = ifelse(
             is.na(mandatePeriodicMonitoringImplementation), 
             "Unknown", mandatePeriodicMonitoringImplementation),
           mandatePeriodicMonitoringImplementation = ifelse(
             mandatePeriodicMonitoringImplementation == "Yes", 1, 0) ) %>% 
    filter(!is.na(yearBeginOperation) & neverOperated == 0) %>% ## operated
    filter(authorizedByState == 1 & temporaryBodyReport == 1 & ## met criteria
             focusedPast == 1 & investigatePatternAbuse == 1) %>% ## met criteria
    filter(if_any(all_of(nexus_vars), ~ . == 1)) %>% ## context binary indicators
    filter(if_any(all_of(crimes_vars), ~ . == 1)) %>% ## crimes included
    mutate(goals = ifelse(!is.null(aims_opts) & 
                            truthcommissionID %in% TCGoals(aims_opts), 1, 0), 
           indep = ifelse(!is.null(independence_opts) & 
                            formallyIndependent %in% independence_opts, 1, 0),
           consl = ifelse(!is.null(consult_vars) & 
                            if_any(all_of(consult_vars), ~ . == 1), 1, 0), 
           power = rowSums(across(all_of(powers_vars))), 
           testi = rowSums(across(all_of(testimony_vars))), 
           repor = rowSums(across(all_of(reports_vars))), 
           recom = rowSums(across(all_of(recommend_vars))), 
           monit = ifelse(!is.null(monitor_vars) & 
                            if_any(all_of(monitor_vars), ~ . == 1), 1, 0),
           scale = goals + indep + consl + power + 
             testi + repor + recom + monit ) %>%
    rename(year_start = .env$start_year_var) %>%
    filter(!is.na(year_start)) %>%
    arrange(ccode, year_start) %>%
    group_by(ccode, year_start) %>% 
    mutate(scale = max(scale, na.rm = TRUE), 
           n = n()) %>% 
    ungroup() %>%
    select(ccode, year_start, 
           # goals, indep, consl, power, testi, repor, recom, monit, 
           scale, n) %>% 
    distinct()

  cy %>% 
    left_join(new, by = c("ccode_cow" = "ccode", "year" = "year_start")) %>% 
    arrange(ccode_cow, year) %>% 
    group_by(ccode_cow) %>% 
    fill(scale, n, .direction = "down") %>% 
    mutate(scale = ifelse(is.na(scale), 0, scale), 
           n = ifelse(is.na(n), 0, n)) %>%
    rename_with(.fn = ~ new_col_name, .cols = scale) %>%
    rename_with(.fn = ~ paste(new_col_name, "n", sep = "_"), .cols = n) %>%
    return()
}
