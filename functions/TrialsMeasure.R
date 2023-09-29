
TrialsMeasure <- function(type_opts, nexus_vars, memb_opts, rank_opts, 
                          charges_opts, measure) {
  
  ## options
  type_trial <- c(int = "international", "for" = "foreign", dom = "domestic")
  nexus_trial <- c(hrs = "human rights", ctj = "conflict", 
                   dtj = "fitPostAutoTJ", dcj = "during conflict justice", 
                   pcj = "post-conflict justice") 
  membership_acc <- c(all = "all perpetrators", sta = "state agent", 
                      opp = "opposition")
  rank_acc <- c(hi = "high rank", lo = "not high rank")
  charges_acc <- c(rap = "rape", sva = "all sexual violence", kil = "killing",
                   tor = "torture", atr = "atrocity")
  measures <- c(trs = "trials started", tro = "trials ongoing", 
                tfc = "trials with final convictions", cct = "conviction count", 
                crt = "conviction rate by all accused", sen = "sentence totals")
  
  ## input errors
  suffix <- "\n  or NULL if this aspect is not relevant to the new measure"
  error <- expression(
    stop("Missing or invalid argument for 'type_opts', select one or more of:", 
         "\n  ", paste(names(type_trial), collapse = "; "), suffix) )
  if(missing(type_opts)) eval(error)
  else if(sum(!type_opts %in% names(type_trial)) > 0) eval(error)
  error <- expression(
    stop("Missing or invalid argument for 'nexus_vars', select one or more of:", 
         "\n  ", paste(names(nexus_trial), collapse = "; "), suffix) )
  if(missing(nexus_vars)) eval(error)
  else if(sum(!nexus_vars %in% names(nexus_trial)) > 0) eval(error)
  error <- expression(
    stop("Missing or invalid argument for 'memb_opts', select one or more of:", 
         "\n  ", paste(names(membership_acc), collapse = "; "), suffix) )
  if(missing(memb_opts)) eval(error)
  else if(sum(!memb_opts %in% names(membership_acc)) > 0) eval(error)
  error <- expression(
    stop("Missing or invalid argument for 'rank_opts', select one or more of:", 
         "\n  ", paste(names(rank_acc), collapse = "; "), suffix) )
  if(missing(rank_opts)) eval(error)
  else if(sum(!rank_opts %in% names(rank_acc)) > 0) eval(error)
  error <- expression(
    stop("Missing or invalid argument for 'charges_opts', select one or more of:", 
         "\n  ", paste(names(charges_acc), collapse = "; "), suffix) )
  if(missing(charges_opts)) eval(error)
  else if(sum(!charges_opts %in% names(charges_acc)) > 0) eval(error)
  error <- expression(
    stop("Missing or invalid argument for 'measure', select one or more of:", 
         "\n  ", paste(names(measures), collapse = "; ")) )
  if(missing(measure)) eval(error)
  else if(sum(!measure %in% names(measures)) > 0) eval(error)
  
  var_name <- str_flatten(c(measure, type_opts, nexus_vars, memb_opts, rank_opts, 
                            charges_opts), collapse = "_")
  
  ## CLs data
  guilty <- db[["CourtLevels"]] %>%
    mutate(date = as_date(date)) %>%
    filter(!is.na(accusedID) & guilty == 1) %>% 
    arrange(accusedID, year) %>% 
    group_by(accusedID, year) %>% 
    # mutate(max_date = max(date, na.rm = TRUE) ) %>%
    mutate(n = n(), 
           any_last = max(last), 
           max_date = ifelse(sum(!is.na(date)) > 0, max(date, na.rm = TRUE), NA ),
           max_date = as_date(max_date)) %>%
    ungroup() %>% 
    filter(is.infinite(max_date) | is.na(max_date) | date == max_date) %>% 
    filter(!(n > 1 & any_last != last)) %>% 
    # select(n, accusedID, any_last, last, year, sentencingTime, sentencingArrangement) %>%
    select(accusedID, year, sentencingTime, sentencingArrangement) %>%
    distinct() %>% 
    filter(!(accusedID == 15441 & is.na(sentencingTime)) ) %>% ## temp fix
    filter(!(accusedID == 17424 & sentencingTime == "4-9 years")) %>% I ## temp fix
    # group_by(accusedID, year) %>%
    # mutate(n = n()) %>%
    # ungroup() %>%
    # filter(n > 1) %>% print(n = Inf)
  
  convictions <- guilty %>%
    select(accusedID, year) 
  
  guilty_scale <- guilty %>% 
    filter(sentencingArrangement %in% c("Ordinary prison", "Don't Know")) %>% 
    ## others: "Special detention", "Suspended sentence"
    mutate(prison_scale = case_when(
      sentencingTime == "Less than 1 year" ~ 1, 
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

  ## Accused data
  db[["Accused"]] %>% 
    select(accusedID, trialID, stateAgent, opposedToGovernment, atrocity, 
           murderKilling, disappearance, torture, rape, SGBV, sexualViolence) 
  
  ## Trials data
  db[["Trials"]] %>% 
    mutate(trialType = case_when(
      trialType %in% c("domestic", "don't know") ~ "domestic",
      trialType %in% c("international", "international (hybrid)") ~ "international",
      trialType == "foreign" ~ "foreign")) %>% 
    select(trialID, ccode_Accused, trialType, humanRights, HRs_charges, 
           fitsPostAutocraticTJ, fitsConflictTJ, beganDuringIntraConfl, 
           beganAfterIntraConfl) 

    
### FROM HERE > 

  
  ### subsetting trials
  trials <- db[["Trials"]] %>% 
    filter(HRs_charges > 0 | humanRights == 1 | IntraConfl == 1) %>% 
    filter(anyStateAgent == 1) %>% 
    select(trialID, ccode_Accused, trialType, yearStart, yearEnd, # ongoing, 
           fitsPostAutocraticTJ, fitsConflictTJ,
           anyOpposedToGov, anyHighRank, # anyStateAgent, 
           firstConvictionYear_min, finalConvictionYear_min, lastVerdictYear_max, 
           conviction, convictionHighRank, finalConviction, 
           finalConvictionHighRank) 
  
  ### when do trials start?
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
  
  ### when are trials ongoing? 
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
  
  ### convictions at trials level
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
  
  ## merging accused and trials 
  accused <- trials %>%
    select(trialID, ccode_Accused, trialType, yearStart, yearEnd) %>% 
    left_join(db[["Accused"]], by = "trialID") %>% 
    filter(!is.na(accusedID)) %>%
    filter(stateAgent == 1) %>%
    select(ccode_Accused, accusedID, trialID, trialType, yearStart, yearEnd, 
           # stateAgent, opposedToGovernment, lastVerdict, lastVerdictYear, 
           # lastSentencingTime, lastSentencingArrangement, ongoing, 
           highRank, everGuilty, firstGuiltyYear, lastGuilty, lastGuiltyYear)
  
  ### old version of trial counts 
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
  
  ### old version of trial conviction counts 
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
  
  ### merging of old count variables 
  counts <- full_join(trial_counts, conviction_counts, 
                      by = c("ccode", "year")) %>% 
    mutate(ccode = ifelse(ccode == 679 & year < 1990, 678, ccode)) # Yemen YAR
  
  
  
  
  
  
  
  return(var_name)
}

# - for state agents and HRs or Confl trials
#   - for domestic, foreign & intl trials separately
#     - from trials
#       - ts: count of trials (by countryAccused & startYear)
#         - merge: trials_start
#       - to: count of ongoing trials by year 
#         - (by countryAccused & >= startYear & <=endYear)
#         - merge: trials_ongoing
#       - tfc: count of trials with final outcome a conviction by endYear
#         - merge: trials_convictions
#     - from accused BUT via trials for conditions
#       - NOT: count of high ranking accused in ongoing trials by year 
#       - cct: count of convictions of accused (by countryAccused & conviction year)
#         - include all convictions of same accused at all levels? only first?
#       - count of convictions of high ranking accused by conviction year 
#         - same as above
#       - crt: yearly count of convictions as percentage of all accused on trial
#       - sen: count of all convictions on scale for prison time
#         - but no death penalty, so 1-7
#         - to merge: guilty_scale via accused
#  - then same vars for trials of gender crimes
#    - trials vars: SGBV, rape_Accused, sexualViolence_Accused, 
#                   otherSGBV_Accused
#    - accused vars: SGBV, rape, sexualViolence, otherSGBV, childVictim, 
#                    LGBTQvictim, maleVictim, RSV