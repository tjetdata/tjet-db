ReparationMeasures <- function(cy, 
                               start_year_var = "yearCreated", 
                               nexus_vars = "all") {
  
  ## options
  # measures <- c("peaceagree", "collective", "symbolic", "compensation", 
  #               "services", "victim_centered", "scope")
  year_vars <- c("yearCreated", "yearBegin")
  nexus <- c(all = "all", dtj = "fitsPostAutocraticTJ", ctj = "fitsConflictTJ", 
             dcj = "reparationConflictDuring", pcj = "reparationConflictAfter")
  
  ## input errors
  suffix <- "\n  or NULL if this aspect is not relevant to the new measure"

  error <- expression(
    stop("Invalid argument for 'start_year_var', select one of:", 
         "\n  ", paste(year_vars, collapse = "; ")) )
  if(!start_year_var %in% year_vars) eval(error)

  error <- expression(
    stop("Invalid argument for 'nexus_vars', select one or more of:", 
         "\n  ", paste(names(nexus), collapse = "; "), suffix) )
  if(sum(!nexus_vars %in% names(nexus)) > 0) eval(error)
  
  ## subsetting & new measures

  reps <- db[["Reparations"]] %>% 
    mutate(
      all = 1, 
      peaceagree = case_when(str_detect(legalBasis, "peace agreement") ~ 1, 
                             TRUE ~ 0),
      collective = case_when(collectiveReparations == "yes" ~ 1, 
                             TRUE ~ 0),
      symbolic = case_when(str_detect(collectiveReparationsForm, "symbolic") ~ 1, 
                           TRUE ~ 0),
      compensation = case_when(
        str_detect(individualReparationsForm, "compensation") |
          str_detect(collectiveReparationsForm, "compensation") ~ 1, 
        TRUE ~ 0),
      services = case_when(
        str_detect(individualReparationsForm, "rehabilitation") |
          str_detect(individualReparationsForm, "restitution") |
          str_detect(collectiveReparationsForm, "services") ~ 1, 
        TRUE ~ 0),
      diffAmount = case_when(diffAmount == "yes" ~ 1,
                             TRUE ~ 0),
      outreach = case_when(outreach == "yes" ~ 1,
                           TRUE ~ 0),
      alterationEffect = case_when(str_detect(alterationEffect, "expanded") ~ 1,
                                   TRUE ~ 0),
      foreclose = case_when(foreclose == "no" ~ 1,
                            TRUE ~ 0),
      accessibility = ifelse(!is.na(accessibility), 1, 0),
      victim_centered = diffAmount + outreach + alterationEffect + foreclose + accessibility, 
      scope = case_when(is.na(beneficiariesCount) | beneficiariesCount == 0 ~ 0, 
                        beneficiariesCount > 0 & beneficiariesCount < 1039 ~ 1, 
                        beneficiariesCount >= 1039 & beneficiariesCount < 6164 ~ 2, # above 1st quartile 
                        beneficiariesCount >= 6164 & beneficiariesCount < 28778 ~ 3, # above median 
                        beneficiariesCount >= 28778 ~ 4) # above 3rd quartile 
    ) %>% 
    filter(if_any(all_of(nexus[[nexus_vars]]), ~ . == 1)) %>% 
    rename(year = .env$start_year_var) %>% 
    arrange(ccode, year) %>%
    group_by(ccode, year) %>% 
    mutate(n = n(), 
           binary = ifelse(n > 0, 1, 0),
           peaceagree = max(peaceagree), 
           collective = max(collective), 
           symbolic = max(symbolic) , 
           compensation = max(compensation), 
           services = max(services), 
           victim_centered = max(victim_centered), 
           scope = max(scope)
           ) %>%
    ungroup() %>%
    select(ccode, year, binary, peaceagree, collective, symbolic, 
           compensation, services, victim_centered, scope) %>% 
    distinct() 
  
  prefix <- paste("rep", nexus_vars, sep = "_") %>% 
    str_replace(fixed("_all"), "") 
  
  ## merging 
  cy %>%
    left_join(reps, by = c("ccode_cow" = "ccode", "year" = "year")) %>%
    arrange(ccode_cow, year) %>% 
    group_by(ccode_cow) %>% 
    fill(binary, 
         peaceagree, 
         collective, 
         symbolic, 
         compensation, 
         services, 
         victim_centered, 
         scope, 
         .direction = "down") %>%
    ungroup() %>% 
    mutate(
      binary = ifelse(year %in% 1970:2020 & is.na(binary), 0, binary),
      peaceagree = ifelse(year %in% 1970:2020 & is.na(peaceagree), 0, peaceagree),
      collective = ifelse(year %in% 1970:2020 & is.na(collective), 0, collective),
      symbolic = ifelse(year %in% 1970:2020 & is.na(symbolic), 0, symbolic),
      compensation = ifelse(year %in% 1970:2020 & is.na(compensation), 0, compensation),
      services = ifelse(year %in% 1970:2020 & is.na(services), 0, services),
      victim_centered = ifelse(year %in% 1970:2020 & is.na(victim_centered), 0, victim_centered),
      scope = ifelse(year %in% 1970:2020 & is.na(scope), 0, scope),
    ) %>%
    rename_with(.fn = ~ paste(prefix, "binary", sep = "_"), .cols = binary) %>% 
    rename_with(.fn = ~ paste(prefix, "peaceagree", sep = "_"), .cols = peaceagree) %>% 
    rename_with(.fn = ~ paste(prefix, "collective", sep = "_"), .cols = collective) %>% 
    rename_with(.fn = ~ paste(prefix, "symbolic", sep = "_"), .cols = symbolic) %>% 
    rename_with(.fn = ~ paste(prefix, "compensation", sep = "_"), .cols = compensation) %>% 
    rename_with(.fn = ~ paste(prefix, "services", sep = "_"), .cols = services) %>% 
    rename_with(.fn = ~ paste(prefix, "victim_centered", sep = "_"), .cols = victim_centered) %>% 
    rename_with(.fn = ~ paste(prefix, "scope", sep = "_"), .cols = scope) %>% 
    return()
}