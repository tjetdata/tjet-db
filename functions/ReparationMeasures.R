ReparationMeasures <- function(cy = df, 
                               start_year_var = "yearCreated", 
                               nexus_vars = "all") {
  ## options
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
                        # beneficiariesCount > 0 & beneficiariesCount < 1039 ~ 1, 
                        # beneficiariesCount >= 1039 & beneficiariesCount < 6164 ~ 2, # above 1st quartile 
                        # beneficiariesCount >= 6164 & beneficiariesCount < 28778 ~ 3, # above median 
                        # beneficiariesCount >= 28778 ~ 4, 
                        beneficiariesCount > 0 & beneficiariesCount < 6164 ~ 1, 
                        beneficiariesCount >= 6164 ~ 2)
    ) %>% 
    filter(if_any(all_of(nexus[[nexus_vars]]), ~ . == 1)) %>% 
    rename(year = .env$start_year_var) %>% 
    arrange(ccode, year) %>%
    group_by(ccode, year) %>% 
    reframe(binary = ifelse(n() > 0, 1, 0),
            peaceagree = max(peaceagree), 
            collective = max(collective), 
            symbolic = max(symbolic), 
            compensation = max(compensation), 
            services = max(services), 
            victim_centered = max(victim_centered), 
            diffAmount = max(diffAmount), 
            outreach = max(outreach), 
            alterationEffect = max(alterationEffect), 
            foreclose = max(foreclose), 
            accessibility = max(accessibility),
            scope = max(scope)) %>% 
    rename(diffamount = diffAmount, 
           alteration = alterationEffect)
  
  prefix <- paste("rep", nexus_vars, sep = "_") %>% 
    str_replace(fixed("_all"), "") 
  
  vars <- c("binary", "peaceagree", "collective", "symbolic", "compensation", 
            "services", "victim_centered", "diffamount", "outreach", 
            "alteration", "foreclose", "accessibility", "scope") 
  
  ## merging 
  cy %>%
    left_join(reps, by = c("ccode_cow" = "ccode", "year" = "year")) %>%
    arrange(country_case, year) %>% 
    group_by(country_case) %>% 
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
      diffamount = ifelse(year %in% 1970:2020 & is.na(diffamount), 0, diffamount),
      outreach = ifelse(year %in% 1970:2020 & is.na(outreach), 0, outreach),
      alteration = ifelse(year %in% 1970:2020 & is.na(alteration), 0, alteration),
      foreclose  = ifelse(year %in% 1970:2020 & is.na(foreclose), 0, foreclose),
      accessibility  = ifelse(year %in% 1970:2020 & is.na(accessibility), 0, accessibility)
    ) %>%
    rename_with(.fn = ~ paste(prefix, .x, sep = "_"), .cols = all_of(vars)) %>% 
    return()
}
