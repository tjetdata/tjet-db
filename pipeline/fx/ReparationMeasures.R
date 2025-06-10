ReparationMeasures <- function(
    cy = df,
    start_year_var = "yearCreated",
    nexus_vars = "all"
    ) {
  ## options
  year_vars <- c("yearCreated", "yearBegin")
  nexus <- c(
    all = "all", dtj = "fitsPostAutocraticTJ", ctj = "fitsConflictTJ",
    dcj = "reparationConflictDuring", pcj = "reparationConflictAfter"
  )

  ## input errors
  suffix <- "\n  or NULL if this aspect is not relevant to the new measure"

  error <- expression(
    stop(
      "Invalid argument for 'start_year_var', select one of:",
      "\n  ", paste(year_vars, collapse = "; ")
    )
  )
  if (!start_year_var %in% year_vars) eval(error)

  error <- expression(
    stop(
      "Invalid argument for 'nexus_vars', select one or more of:",
      "\n  ", paste(names(nexus), collapse = "; "), suffix
    )
  )
  if (sum(!nexus_vars %in% names(nexus)) > 0) eval(error)

  ## subsetting & new measures
  
  reps <- db[["Reparations"]] %>%
    mutate(
      all = 1,
      peaceagree = case_when(
        str_detect(legalBasis, "peace agreement") ~ 1,
        TRUE ~ 0
      ),
      collective = case_when(
        collectiveReparations == "yes" ~ 1,
        TRUE ~ 0
      ),
      individual = case_when(
        individualReparations == "yes" ~ 1,
        TRUE ~ 0
      ),
      symbolic = case_when(
        str_detect(collectiveReparationsForm, "symbolic") ~ 1,
        TRUE ~ 0
      ),
      compensation = case_when(
        str_detect(individualReparationsForm, "compensation") |
          str_detect(collectiveReparationsForm, "compensation") ~ 1,
        TRUE ~ 0
      ),
      services = case_when(
        str_detect(individualReparationsForm, "rehabilitation") |
          str_detect(individualReparationsForm, "restitution") |
          str_detect(collectiveReparationsForm, "services") ~ 1,
        TRUE ~ 0
      ),
      diffAmount = case_when(
        diffAmount == "yes" ~ 1,
        TRUE ~ 0
      ),
      outreach = case_when(
        outreach == "yes" ~ 1,
        TRUE ~ 0
      ),
      alterationEffect = case_when(
        str_detect(alterationEffect, "expanded") ~ 1,
        TRUE ~ 0
      ),
      foreclose = case_when(
        foreclose == "no" ~ 1,
        TRUE ~ 0
      ),
      accessibility = ifelse(!is.na(accessibility), 1, 0),
      victim_centered = diffAmount + outreach + alterationEffect + foreclose + accessibility,
      scope = case_when(
        is.na(individualsRepairedEstimate) | individualsRepairedEstimate == 0 ~ 0,
        individualsRepairedEstimate > 0 & individualsRepairedEstimate < 4481 ~ 1,
        individualsRepairedEstimate >= 4481 ~ 2
      ),
      harms = harmsMurder + harmsTorture + harmsDetention + harmsDisappearance +
        harmsChildRecruitment + harmsDisplacement + harmsSexualViolence + harmsOther
    ) %>%
    filter(if_any(all_of(nexus[[nexus_vars]]), ~ . == 1)) %>%
    rename(year = .env$start_year_var) %>%
    arrange(ccode, year) %>%
    group_by(ccode, year) %>%
    reframe(
      # binary = ifelse(n() > 0, 1, 0),
      count = n(),
      peaceagree_created = sum(peaceagree),
      individual_created = sum(individual),
      collective_created = sum(collective),
      symbolic_created = sum(symbolic),
      compensation_created = sum(compensation),
      services_created = sum(services),
      paidout_created = sum(rep_paid),
      ### 
      diffAmount = max(diffAmount),
      outreach = max(outreach),
      alterationEffect = max(alterationEffect),
      foreclose = max(foreclose),
      accessibility = max(accessibility),
      ### 
      victim_centered = max(victim_centered), #
      scope = max(scope), #
      harms = max(harms) #
    ) %>%
    rename(
      diffamount = diffAmount,
      alteration = alterationEffect
    )
  
  prefix <- paste("rep", nexus_vars, sep = "_") %>%
    str_replace(fixed("_all"), "")

  vars <- c(
    "binary", "created", "peaceagree", "peaceagree_created", "individual", 
    "individual_created", "collective", "collective_created", "compensation", 
    "compensation_created", "symbolic", "symbolic_created", "services", 
    "services_created", "paidout", "paidout_created", "victim_centered", 
    "victim_centered_beg", "scope", "scope_beg", "harms", "harms_beg", 
    "diffamount", "outreach", "alteration", "foreclose", "accessibility"
  )

  ## merging
  cy %>%
    left_join(reps, by = c("ccode_cow" = "ccode", "year" = "year")) %>%
    mutate(
      binary = ifelse(count > 0, 1, 0), 
      peaceagree = ifelse(peaceagree_created > 0, 1, 0), 
      individual = ifelse(individual_created > 0, 1, 0), 
      collective = ifelse(collective_created > 0, 1, 0), 
      compensation = ifelse(compensation_created > 0, 1, 0), 
      symbolic = ifelse(symbolic_created > 0, 1, 0), 
      services = ifelse(services_created > 0, 1, 0), 
      paidout = ifelse(paidout_created > 0, 1, 0), 
      victim_centered_beg = victim_centered, 
      scope_beg = scope,
      harms_beg = harms,
    ) %>%
    rename(created = count) |>  
    arrange(country_case, year) %>%
    group_by(country_case) %>%
    fill(
      binary, peaceagree, collective, individual, symbolic, compensation, 
      services, paidout, victim_centered, scope, harms, 
      .direction = "down"
    ) %>%
    ungroup() %>%
    mutate(across(
      all_of(vars),
      ~ ifelse(year %in% 1970:2023 & is.na(.x), 0, ifelse(year > 2023, NA, .x))
    )) %>%
    rename_with(.fn = ~ paste(prefix, .x, sep = "_"), .cols = all_of(vars)) %>%
    return()
}
