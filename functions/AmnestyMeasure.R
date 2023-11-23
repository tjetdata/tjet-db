AmnestyMeasure <- function(cy, nexus_vars, who_opts, what_opts = NULL) {
  ## options
  nexus <- c(dtj = "fitsPostAutocraticTJ", ctj = "fitsConflictTJ", 
             dcj = "dcj", pcj = "pcj") 
  who <- c(all = "all", sta = "who_sta", opp = "who_opp", oth = "who_oth")
  what <- c(all = "all", reb = "what_reb", hrs = "what_hrs", 
            pol = "what_pol", oth = "what_oth")

  ## input errors
  suffix <- "\n  or NULL if this aspect is not relevant to the new measure"
  
  error <- expression(
    stop("Missing or invalid argument for 'nexus_vars', select one or more of:", 
         "\n  ", paste(names(nexus), collapse = "; "), suffix) )
  if(missing(nexus_vars)) eval(error)
  else if(sum(!nexus_vars %in% names(nexus)) > 0) eval(error)
  
  error <- expression(
    stop("Missing or invalid argument for 'who_opts', select one or more of:", 
         "\n  ", paste(names(who), collapse = "; "), suffix) )
  if(missing(who_opts)) eval(error)
  else if(sum(!who_opts %in% names(who)) > 0) eval(error)
  
  error <- expression(
    stop("Missing or invalid argument for 'what_opts', select one or more of:", 
         "\n  ", paste(names(what), collapse = "; "), suffix) )
  # if(missing(what_opts)) eval(error)
  # else 
  if(sum(!what_opts %in% names(what)) > 0) eval(error)

  var_name <- str_flatten(c("amnesty", nexus_vars, who_opts, what_opts), collapse = "_")
  
  amn <- db[["Amnesties"]] %>% 
    mutate(who_sta = ifelse(str_detect(whoWasAmnestied, "state agents"), 1, 0), 
           who_opp = ifelse(str_detect(whoWasAmnestied, "protesters / political prisoners") | 
                              str_detect(whoWasAmnestied, "armed opposition"), 1, 0),  
           who_oth = ifelse(str_detect(whoWasAmnestied, "collaborators") | 
                              str_detect(whoWasAmnestied, "draft dodgers / deserters") | 
                              str_detect(whoWasAmnestied, "refugees / exiles") | 
                              str_detect(whoWasAmnestied, "regular convicts") | 
                              str_detect(whoWasAmnestied, "other"), 1, 0), 
           what_reb = ifelse(str_detect(whatCrimes, "armed opposition") |
                               str_detect(whatCrimes, "terrorism"), 1, 0), 
           what_hrs = ifelse(str_detect(whatCrimes, "human rights violations"), 1, 0), 
           what_pol = ifelse(str_detect(whatCrimes, "dissent / political crimes"), 1, 0), 
           what_rev = ifelse(str_detect(whatCrimes, "armed violence against rebels"), 1, 0), 
           what_oth = ifelse(str_detect(whoWasAmnestied, "collaboration") | 
                               str_detect(whoWasAmnestied, "regular crime") | 
                               str_detect(whoWasAmnestied, "corruption or other economic crimes") | 
                               str_detect(whoWasAmnestied, "dereliction of duty") | 
                               str_detect(whoWasAmnestied, "other"), 1, 0), 
    ) %>% 
    select(amnestyID, ccode, amnestyYear, fitsPostAutocraticTJ, fitsConflictTJ, 
           dcj, pcj, who_sta, who_opp, who_oth, 
           what_reb, what_hrs, what_pol, what_oth, what_rev) %>% 
    filter(if_any(all_of(nexus[nexus_vars]), ~ . == 1)) %>% 
    filter(if_any(all_of(who[who_opts]), ~ . == 1)) %>% 
    filter(if_any(all_of(what[what_opts]), ~ . == 1)) %>% 
    select(amnestyID, ccode, amnestyYear) %>% 
    arrange(ccode, amnestyYear) %>%
    group_by(ccode, amnestyYear) %>%
    mutate(new = n(),
           new = ifelse(new > 0, 1, 0)) %>%
    ungroup() %>%
    select(-amnestyID) %>% 
    distinct()
    
  cy <- cy %>%
    left_join(amn, by = c("ccode_cow" = "ccode", "year" = "amnestyYear")) %>%
    mutate(new = ifelse(year %in% 1970:2020 & is.na(new), 0, new)) %>%
    rename_with(.fn = ~ var_name, .cols = new)

  return(cy)
}