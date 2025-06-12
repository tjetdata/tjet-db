## the approach to coding vetting measures is different from that of other measures
## because the vetting measures were already constructed in Airtable (THIS HAS CHANGED) 
## so far, the data are only consistently coded for Eastern Europe and the Post-Soviet region

vet_spells <- db[["Vettings"]] %>% 
  rename(
    "type_declass" = "type_declassification"
  ) %>%
  mutate(
    public = case_when(
      hearingsPublic == "yes" ~ 1,
      TRUE ~ 0
    ),
    conduct = case_when(
      str_detect(targetingWhy, "specific individual conduct") ~ 1,
      TRUE ~ 0
    ),
    yearEnd = ifelse(is.na(yearEnd) | yearEnd > 2024, 2024, yearEnd)
  ) %>%
  rowwise() %>%
  mutate(year = map2(yearStart, yearEnd, seq)) %>%
  unnest(cols = c(year)) %>%
  mutate(
    type_dismissal_created = ifelse(is.na(alterationOf) & year == yearStart, type_dismissal, 0), 
    type_ban_created = ifelse(is.na(alterationOf) & year == yearStart, type_ban, 0),
    type_declass_created = ifelse(is.na(alterationOf) & year == yearStart, type_declass, 0)
  ) %>% 
  select(
    ccode, year, fitsPostAutocraticTJ, fitsConflictTJ, type_dismissal, 
    type_dismissal_created, type_ban, type_ban_created, type_declass, 
    type_declass_created, ban_from_elected, conduct, public, fairness,
    # targetingWhy, targetingAffiliationRank, targetingPositionSought,
    # targetingAffiliation, targetingPositionsRank, sanctions, 
    # sum_inst, inst_targeted, inst_exe, inst_legis, inst_judiciary, inst_public, 
    # inst_police, inst_military, inst_parties, inst_other, 
  ) %>%
  reframe(.by = c(ccode, year), 
    across(
      all_of(
        c("type_dismissal_created", "type_ban_created", "type_declass_created")
      ), 
      .fns = ~ sum(.x, na.rm = TRUE)
    ),
    across(
      all_of(
        c("fitsPostAutocraticTJ", "fitsConflictTJ", "type_dismissal", "type_ban", 
          "type_declass", "ban_from_elected", "conduct", "public", "fairness") 
      ), 
      .fns = ~ max(.x, na.rm = TRUE)
    )
  ) %>%
  mutate(across(everything(), .fns = ~ ifelse(is.infinite(.x), NA, .x))) %>%
  mutate(across(everything(), .fns = ~ ifelse(is.na(.x), 0, .x)))

VettingMeasures <- function(cy = df, nexus_vars = "all") {
  ## options
  nexus <- c(all = "all", dtj = "fitsPostAutocraticTJ", ctj = "fitsConflictTJ")

  ## input errors
  suffix <- "\n  or NULL if this aspect is not relevant to the new measure"
  error <- expression(
    stop(
      "Missing or invalid argument for 'nexus_vars', select one or more of:",
      "\n  ", paste(names(nexus), collapse = "; "), suffix
    )
  )
  if (sum(!nexus_vars %in% names(nexus)) > 0) eval(error)

  ## subsetting
  vars <- c(
    "type_dismissal", "type_dismissal_created", "type_ban", "type_ban_created", 
    "type_declass", "type_declass_created", "ban_from_elected", "conduct", 
    "public", "fairness")
  
  vet <- vet_spells %>%
    mutate(all = 1) %>%
    filter(if_any(all_of(nexus[nexus_vars]), ~ . == 1)) %>%
    select(ccode, year, all_of(vars))
  
  cy %>%
    left_join(vet, by = c("ccode_cow" = "ccode", "year" = "year")) %>%
    arrange(country_case, year) %>%
    group_by(country_case) %>%
    fill(
      type_dismissal, type_ban, type_declass, ban_from_elected, 
      conduct, public, fairness,
      .direction = "down"
    ) %>%
    ungroup() %>%
    mutate(across(all_of(vars), ~ ifelse(year %in% 1970:2024 & is.na(.x), 0, ifelse(year > 2024, NA, .x)))) %>%
    rename(
      "vet_dismiss" = "type_dismissal",
      "vet_dismiss_created" = "type_dismissal_created",
      "vet_ban" = "type_ban",
      "vet_ban_created" = "type_ban_created",
      "vet_declass" = "type_declass",
      "vet_declass_created" = "type_declass_created",
      "vet_ban_from_elected" = "ban_from_elected",
      "vet_conduct" = "conduct",
      "vet_public" = "public",
      "vet_fairness" = "fairness"
    ) %>%
    return()
}
