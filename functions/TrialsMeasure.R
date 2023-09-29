TrialsMeasure <- function(cy, type_opts, nexus_vars, memb_opts, 
                          rank_opts = NULL, charges_opts = NULL, measure) {
  ## options
  type_trial <- c(int = "international", 
                  "for" = "foreign", 
                  dom = "domestic")
  nexus_trial <- c(hrs = "humanRights", 
                   ctj = "fitsConflictTJ", 
                   dtj = "fitsPostAutocraticTJ", 
                   dcj = "beganDuringIntraConfl", 
                   pcj = "beganAfterIntraConfl") 
  membership_acc <- c(all = "all", 
                      sta = "stateAgent", 
                      opp = "opposedToGovernment")
  rank_acc <- c(hi = "highRank", # hi = "high rank"
                lo = "lowRank") # lo = "not high rank"
  charges_acc <- c(rap = "rape", 
                   sva = "SGBV", # sva = "all sexual violence"
                   kil = "murderKilling",
                   tor = "torture", 
                   atr = "atrocity")
  measures <- c(trs = "trials started", 
                tro = "trials ongoing", 
                tfc = "trials with final convictions", 
                cct = "conviction count", 
                crt = "conviction rate by all accused", 
                sen = "sentence totals")
  
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
  # if(missing(rank_opts)) eval(error)
  # else 
  if(sum(!rank_opts %in% names(rank_acc)) > 0) eval(error)

  error <- expression(
    stop("Missing or invalid argument for 'charges_opts', select one or more of:", 
         "\n  ", paste(names(charges_acc), collapse = "; "), suffix) )
  # if(missing(charges_opts)) eval(error)
  # else 
  if(sum(!charges_opts %in% names(charges_acc)) > 0) eval(error)
  
  error <- expression(
    stop("Missing or invalid argument for 'measure', select one or more of:", 
         "\n  ", paste(names(measures), collapse = "; ")) )
  if(missing(measure)) eval(error)
  else if(sum(!measure %in% names(measures)) > 0) eval(error)
  
  var_name <- str_flatten(c(measure, type_opts, nexus_vars, memb_opts, rank_opts, 
                            charges_opts), collapse = "_")
  
  ## CLs data
  
  # accused_ended <- db[["CourtLevels"]] %>%
  #   mutate(date = as_date(date)) %>%
  #   filter(!is.na(accusedID)) %>%
  #   filter(verdict %in% c("Acquittal", "Acquittal Upheld", "Amnesty applies", 
  #                         "Dismissal", "Dismissal upheld", "Mistrial",
  #                         "Guilty Overturned", "Guilty Overturned & Acquittal", 
  #                         "Guilty Overturned & Dismissal")) %>%
  #   select(accusedID, year, date, last_fx, last) %>% 
  #   arrange(accusedID, year, date) %>% 
  #   group_by(accusedID, year) %>% 
  #   mutate(n = n(), 
  #          any_last = max(last),
  #          max_date = ifelse(sum(!is.na(date)) > 0, max(date, na.rm = TRUE), NA ), 
  #          max_date = as_date(max_date)) %>%
  #   filter(!(n > 1 & !is.na(max_date) & date != max_date)) %>% 
  #   filter(!(n > 1 & last != any_last)) %>% 
  #   distinct() %>% 
  #   # mutate(n = n()) %>%
  #   # filter(n > 1) %>% 
  #   ungroup() %>%
  #   select(accusedID, year) %>%
  #   rename(year_ended = year) %>% 
  #   group_by(accusedID) %>% 
  #   mutate(n = n()) %>% 
  #   filter(!(n > 1 & is.na(year_ended))) %>% 
  #   mutate(max_year = max(year_ended)) %>% 
  #   filter(!(n > 1 & !is.na(max_year) & year_ended != max_year)) %>% 
  #   select(accusedID, year_ended) 
  
  ### the above is now replaced with the following lines; safer approach
  accused_ended <- db[["CourtLevels"]] %>%
    filter(!is.na(accusedID)) %>%
    select(accusedID, year, date, last_fx, last) %>% 
    arrange(accusedID, year) %>% 
    group_by(accusedID) %>% 
    mutate(n = n(), 
           any_last = max(last_fx)) %>% 
    filter(!(n > 1 & last_fx != any_last)) %>%
    # mutate(n = n()) %>%
    # filter(n > 1) %>%
    select(accusedID, year) %>%
    rename(year_ended = year)
  
  guilty <- db[["CourtLevels"]] %>%
    mutate(date = as_date(date)) %>% 
    filter(!is.na(accusedID) & guilty == 1) %>% 
    arrange(accusedID, year, date) %>% 
    group_by(accusedID, year) %>% 
    mutate(n = n(), 
           any_last = max(last), 
           max_date = ifelse(sum(!is.na(date)) > 0, max(date, na.rm = TRUE), NA ),
           max_date = as_date(max_date)) %>%
    filter(!(n > 1 & !is.na(max_date) & date != max_date)) %>%
    filter(!(n > 1 & last != any_last)) %>%
    filter(!(accusedID == 15441 & is.na(sentencingTime)) ) %>% ## temp fix
    filter(!(accusedID == 17424 & sentencingTime == "4-9 years")) %>%  ## temp fix
    # select(n, accusedID, last_fx, last, any_last, year, date, max_date, sentencingTime, sentencingArrangement) %>%
    select(accusedID, year, last_fx, guilty, sentencingTime, sentencingArrangement) %>%
    distinct() %>% 
    # mutate(n = n()) %>%
    # filter(n > 1) %>% 
    ungroup()
  
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
    arrange(accusedID, year) %>% 
    group_by(accusedID, year) %>% 
    mutate(prison_scale = max(prison_scale)) %>%
    ungroup() %>%
    distinct() %>%
    rename(year_scale = year)
  
  ## Accused data
  accused <- db[["Accused"]] %>% 
    mutate(all = 1, 
           lowRank = 1 - highRank) %>% 
    filter(if_any(all_of(membership_acc[memb_opts]), ~ . == 1)) %>% 
    filter(if_any(all_of(rank_acc[rank_opts]), ~ . == 1)) %>% 
    filter(if_any(all_of(charges_acc[charges_opts]), ~ . == 1)) %>% 
    arrange(accusedID) %>% 
    select(accusedID, trialID)
           # everGuilty, firstGuiltyYear, lastGuilty, lastGuiltyYear
  
  final_convictions <- guilty %>%
    filter(last_fx == 1) %>% 
    select(accusedID, year) %>% 
    # group_by(accusedID) %>%  
    # summarise(year = max(year)) 
    rename(year_convict_final = year)

  ## Trials data  
  trials <- db[["Trials"]] %>%
    mutate(
      humanRights = ifelse(HRs_charges > 0 & humanRights == 0, 1, humanRights), 
      trialType = case_when(
        trialType %in% c("domestic", "don't know") ~ "domestic",
        trialType %in% c("international", "international (hybrid)") ~ "international",
        trialType == "foreign" ~ "foreign") ) %>% 
    filter(trialType == type_trial[type_opts]) %>%
    filter(if_any(all_of(nexus_trial[nexus_vars]), ~ . == 1)) %>% 
    select(trialID, ccode_Accused, yearStart, yearEnd)
           # ongoing, anyOpposedToGov, anyHighRank, anyStateAgent, 
           # firstConvictionYear_min, finalConvictionYear_min, 
           # lastVerdictYear_max, conviction, convictionHighRank, 
           # finalConviction, finalConvictionHighRank
  
  ## combining the trials and accused subsets; one-to-many, unit is accused 
  subset_accused <- trials %>%
    inner_join(accused, by = "trialID") %>% 
    left_join(final_convictions, by = "accusedID") %>% 
    arrange(accusedID)

  ## combining with guilty verdicts; makes unit accused-year
  subset_accused_convictions <- subset_accused %>%
    left_join(guilty %>% select(accusedID, year, last_fx, guilty),  
              by = "accusedID") %>%
    select(accusedID, ccode_Accused, yearStart, yearEnd, last_fx, guilty, 
           year, year_convict_final)

  ## combining with prison scale; makes unit accused-year
  subset_accused_year_scale <- subset_accused %>%
    left_join(guilty_scale, by = "accusedID") 
  
  ### different measures 
  
  if(measure == "trs") { ## "trials started": count by countryAccused & startYear
    trials_start <- trials %>%
      group_by(ccode_Accused, yearStart) %>% 
      mutate(trials_yearStart = n()) %>%  
      ungroup() %>%  
      select(ccode_Accused, yearStart, trials_yearStart) %>% 
      arrange(ccode_Accused, yearStart) %>% 
      distinct() # %>% 
      # pivot_wider(names_from = trialType, values_from = trials_yearStart) %>% 
      # rename(convict_final_domestic = "domestic",
      #        convict_final_foreign = "foreign",
      #        convict_final_intl = "international") 
    cy <- cy %>% 
      left_join(trials_start, by = c("ccode_cow" = "ccode_Accused", 
                                     "year" = "yearStart")) %>% 
      mutate(trials_yearStart = ifelse(is.na(trials_yearStart), 
                                       0, trials_yearStart)) %>% 
      rename_with(.fn = ~ var_name, .cols = trials_yearStart)
  }

  if(measure == "tro") { ## "trials ongoing": count of ongoing by year (by countryAccused & >= startYear & <=endYear)
    trials_ongoing <- trials %>%
      rowwise() %>% 
      mutate(year = list(yearStart:yearEnd)) %>% 
      ungroup() %>% 
      unnest_longer(year) %>% 
      select(ccode_Accused, year, yearStart, yearEnd) %>%
      arrange(ccode_Accused, year) %>% 
      group_by(ccode_Accused, year) %>% 
      mutate(trials_ongoing = n()) %>% 
      ungroup() %>%
      select(ccode_Accused, year, trials_ongoing) %>% 
      distinct()
    cy <- cy %>%
      left_join(trials_ongoing, by = c("ccode_cow" = "ccode_Accused", 
                                       "year" = "year")) %>% 
      mutate(trials_ongoing = ifelse(is.na(trials_ongoing), 
                                       0, trials_ongoing)) %>% 
      rename_with(.fn = ~ var_name, .cols = trials_ongoing)
  }
  
  if(measure == "tfc") { ## tfc = "trials with final convictions": count of trials with final outcome a conviction by endYear
    trials_convictions <- subset_accused %>%
      mutate(convict_final = ifelse(is.na(year_convict_final), 0, 1)) %>%
      select(trialID, ccode_Accused, yearEnd, convict_final) %>%
      # select(trialID, ccode_Accused, yearEnd, convict_final, year_convict_final) %>%
      # filter(yearEnd != year_convict_final) %>% 
      group_by(trialID) %>% 
      mutate(convict_final = max(convict_final)) %>% 
      ungroup() %>% 
      distinct() %>% 
      arrange(ccode_Accused, yearEnd) %>% 
      group_by(ccode_Accused, yearEnd) %>% 
      mutate(count_trial_convict_final = sum(convict_final)) %>% 
      ungroup() %>% 
      select(-trialID, -convict_final) %>% 
      distinct() %>% I
      # group_by(ccode_Accused, yearEnd) %>% 
      # mutate(n = n()) %>% 
      # filter(n > 1) 
    cy <- cy %>% 
      left_join(trials_convictions, by = c("ccode_cow" = "ccode_Accused", 
                                           "year" = "yearEnd")) %>% 
      mutate(count_trial_convict_final = ifelse(is.na(count_trial_convict_final), 
                                     0, count_trial_convict_final)) %>% 
      rename_with(.fn = ~ var_name, .cols = count_trial_convict_final)
  }
  
  if(measure %in% c("cct", "crt")) {

    convictions <- subset_accused_convictions %>%
      mutate(year = ifelse(is.na(year), year_convict_final, year), 
             year = ifelse(guilty == 1 & is.na(year), yearEnd, year) ) %>% 
      select(accusedID, ccode_Accused, year, guilty) %>% 
      filter(!is.na(guilty) | guilty != 1) %>% 
      distinct() %>% 
      arrange(ccode_Accused, year) %>% 
      group_by(ccode_Accused, year) %>% 
      mutate(convictions = sum(guilty)) %>%
      ungroup() %>% 
      select(ccode_Accused, year, convictions) %>%
      distinct()
    cy <- cy %>% 
      left_join(convictions, by = c("ccode_cow" = "ccode_Accused", 
                                    "year" = "year")) %>% 
      mutate(convictions = ifelse(is.na(convictions), 0, convictions)) 
    
    if(measure == "cct") { ## "conviction count"
      cy <- cy %>%
        rename_with(.fn = ~ var_name, .cols = convictions)
    }
    
    if(measure == "crt") { ## "conviction rate by all accused": yearly count of convictions as percentage of all accused on trial
      acc_var_name <- str_flatten(c("acc", type_opts, nexus_vars, memb_opts, 
                                    rank_opts, charges_opts), collapse = "_")
      accused_ongoing <- subset_accused %>%
        left_join(accused_ended, by = "accusedID") %>%
        mutate(yearEnd = ifelse(!is.na(year_ended) & yearEnd > year_ended,
                                year_ended, yearEnd) ) %>%
        select(accusedID, ccode_Accused, yearStart, yearEnd, year_convict_final) %>% 
        filter(!yearEnd < 1970) %>% 
        rowwise() %>% 
        mutate(year = list(yearStart:yearEnd)) %>% 
        ungroup() %>% 
        unnest(year) %>% 
        select(ccode_Accused, year) %>% 
        arrange(ccode_Accused, year) %>% 
        group_by(ccode_Accused, year) %>% 
        reframe(accused_n = n())
        
      # cy <- 
      cy %>% 
        left_join(accused_ongoing, by = c("ccode_cow" = "ccode_Accused", 
                                          "year" = "year")) %>% 
        mutate(accused_n = ifelse(is.na(accused_n), 0, accused_n), 
               new = convictions / accused_n
               ) %>% 
        select(country, ccode_cow, year, convictions, accused_n, new) %>% 
        filter(is.nan(new)) 

        # rename_with(.fn = ~ acc_var_name, .cols = accused_n) %>% 
        # filter(trs_dom_hrs_sta > 0 | acc_dom_hrs_sta > 0) 
      
      subset_accused_convictions %>% 
        filter(ccode_Accused == 703) %>% 
        print(n = Inf)
      
      
      # country    ccode_cow  year convictions accused_n   new
      # <chr>          <dbl> <dbl>       <dbl>     <dbl> <dbl>
      #   1 Haiti             41  2015           1         0   Inf
      # 2 Venezuela        101  1993           2         0   Inf
      # 3 Uruguay          165  2019           1         0   Inf
      # 4 Uruguay          165  2020           1         0   Inf
      # 5 Belgium          211  2015           3         0   Inf
      # 6 Bulgaria         355  2009           1         0   Inf
      # 7 Belarus          370  2002           3         0   Inf
      # 8 Gambia           420  2017           2         0   Inf
      # 9 Zimbabwe         552  2019           1         0   Inf
      # 10 Kyrgyzstan       703  2020           1         0   Inf
      # 11 Nepal            790  2017           3         0   Inf      
      
      
      
    }

  }
  
  if(measure == "sen") { ## sen = "sentence totals"
    ## should the scale be standardized; how exactly?
    sentences <- subset_accused_year_scale %>% 
      filter(!is.na(prison_scale)) %>% 
      mutate(year_scale = ifelse(is.na(year_scale), yearEnd, year_scale)) %>%
      select(ccode_Accused, accusedID, year_scale, prison_scale) %>%
      arrange(ccode_Accused, year_scale) %>%  
      group_by(ccode_Accused, year_scale) %>%
      mutate(prison_scale = sum(prison_scale)) %>% 
      select(ccode_Accused, year_scale, prison_scale) %>%
      distinct() 
    cy <- cy %>% 
      left_join(sentences, by = c("ccode_cow" = "ccode_Accused", 
                                  "year" = "year_scale")) %>% 
      mutate(prison_scale = ifelse(is.na(prison_scale), 0, prison_scale)) %>% 
      rename_with(.fn = ~ var_name, .cols = prison_scale)
  }

  return(cy)
}
#  - then same vars for trials of gender crimes
#    - trials vars: SGBV, rape_Accused, sexualViolence_Accused, 
#                   otherSGBV_Accused
#    - accused vars: SGBV, rape, sexualViolence, otherSGBV, childVictim, 
#                    LGBTQvictim, maleVictim, RSV