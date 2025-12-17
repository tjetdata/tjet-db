## functions for subsetting TC data and calculating indices

TCGoals <- function(aims) {
  db[["TruthCommissions_tcAims"]] |> # Goals (binaries)
    left_join(db[["labels"]], by = "labelID") |>
    select(-labelID) |>
    # mutate(
    #   new = 1,
    #   label = str_replace_all(label, fixed(" "), "_")
    # ) |>
    #   pivot_wider(names_from = label, values_from = new) |>
    #   mutate(across(!truthcommissionID, ~ ifelse(is.na(.x), 0, .x)))
    filter(label %in% aims) |>
    select(truthcommissionID) |>
    arrange(truthcommissionID) |>
    distinct() |>
    unlist(use.names = FALSE)
}

## include fields (or "all") or options (for independence & aims)
## for indicators to be included; use NULL for those to exclude
## there are error checks built into the function
TCmeasure <- function(
  cy,
  new_col_name,
  start_year_var, # which year variable to use as first year
  filter_nexus_vars, # filter variables
  filter_crimes_vars, # filter variables
  independence_opts, # these are included in index as binary
  aims_opts, # these are included in index as binary
  consult_vars, # these are included in index as binary
  harms_vars = NULL, # same as filter_crimes_vars but to be summed for index, not filtered
  powers_vars, # these are summed before inclusion in index
  testimony_vars, # these are summed before inclusion in index
  reports_vars, # these are summed before inclusion in index
  recommend_vars, # these are summed before inclusion in index
  monitor_vars, # included in index as binary
  confl_df = FALSE
) {
  if (confl_df) {
    unit_id <- c("conflict_id" = "ucdpConflictID")
    group_var <- "conflict_id"
    confl_df_cond <- expr(!is.na(ucdpConflictID))
  } else {
    unit_id <- c("ccode_cow" = "ccode")
    group_var <- "country_case"
    confl_df_cond <- TRUE
  }

  ## options
  all_year_vars <- c(
    "yearPassed",
    "yearBeginOperation",
    "yearCompleteOperation"
  )
  all_nexus_vars <- c(
    "all",
    "fitsPostAutocraticTJ",
    "fitsConflictTJ",
    "beganOperatingDuringIntraConfl",
    "beganOperatingAfterIntraConfl"
  )
  all_crimes_vars <- c(
    "all",
    "torture",
    "death",
    "disappearance",
    "SGBV",
    "forcedDisplacement",
    "corruption",
    "crimesOther"
  )
  all_independence_opts <- c(
    "all",
    "don't know",
    "not independent",
    "partially independent",
    "fully independent"
  )
  all_aims_opts <- c(
    "accountability",
    "addressing corruption",
    "apology",
    "coexistence",
    "dialogue",
    "historical truth",
    "institutional reform",
    "memorialization",
    "non-recurrence",
    "prevention of human rights violations",
    "recognition of victims",
    "reconciliation",
    "reparation",
    "responsibility",
    "restitution",
    "truth for victims"
  )
  all_consult_vars <- c("consultedVictims")
  all_powers_vars <- c(
    "investigateStateMembers",
    "compelTestimony",
    "grantAmnesty",
    "supportProsecutions",
    "namePerpetrators",
    "recommendInstitutionalReforms",
    "allocateReparations",
    "penalizeIndividuals",
    "accessOfficialDocuments"
  )
  all_testimony_vars <- c(
    "heldPublicHearings",
    "testimonies",
    "encourageVictimTestimony",
    "perpetratorTestimony"
  )
  all_reports_vars <- c(
    "finalReportIssued",
    "reportPubliclyAvailable",
    "finalReportRecommendations"
  )
  all_recommend_vars <- c(
    "recommendProsecutions",
    "recommendReparations",
    "reportRecommendInstitutionalReform",
    "vetting",
    "SecuritySectorReforms",
    "legalReform",
    "judicialReforms",
    "humanRightsReforms",
    "corruptionReforms",
    "genderReform"
  )
  all_monitor_vars <- c("mandatePeriodicMonitoringImplementation")

  suffix <- "\n  OR NULL if this aspect is not relevant to the new measure"

  if (
    missing(new_col_name) |
      length(new_col_name) != 1 |
      class(new_col_name) != "character"
  ) {
    stop(
      "Missing argument: provide a unique variable name in 'new_col_name';",
      "\n  must be a string"
    )
  }

  error <- expression(
    stop(
      "Missing argument: specify in 'start_year_var' which year variable to use as",
      "\n  the starting point for the TC concept to be measured; choose from:",
      "\n  ",
      paste(all_year_vars, collapse = " OR ")
    )
  )
  if (missing(start_year_var)) {
    eval(error)
  } else if (length(start_year_var) != 1 | !start_year_var %in% all_year_vars) {
    eval(error)
  }

  if (
    missing(filter_nexus_vars) | sum(!filter_nexus_vars %in% all_nexus_vars) > 0
  ) {
    stop(
      "Missing argument: for subsetting, specify in 'filter_nexus_vars' one or more of:",
      "\n  ",
      paste(all_nexus_vars, collapse = "; ")
    )
  }

  if (
    missing(filter_crimes_vars) |
      sum(!filter_crimes_vars %in% all_crimes_vars) > 0
  ) {
    stop(
      "Missing argument: for subsetting, specify in 'filter_crimes_vars' one or more of:",
      "\n  ",
      paste(all_crimes_vars, collapse = "; ")
    )
  }

  if (!is.null(harms_vars)) {
    if (
      !(filter_crimes_vars == "all" &
        is.null(aims_opts) &
        is.null(independence_opts) &
        is.null(consult_vars) &
        is.null(powers_vars) &
        is.null(testimony_vars) &
        is.null(reports_vars) &
        is.null(recommend_vars) &
        is.null(monitor_vars))
    ) {
      stop(
        "The 'harms_vars' can only be used with filter_crimes_vars == 'all'",
        "  and other index arguments set to NULL."
      )
    }
    error <- expression(
      stop(
        "Incorrect argument: for an index, specify in 'harms_vars' one or more of:",
        "\n  ",
        paste(all_crimes_vars, collapse = "; "),
        suffix
      )
    )
    if (length(harms_vars) > 0) {
      if (sum(!harms_vars %in% all_crimes_vars) > 0) eval(error)
    }
  }

  error <- expression(
    stop(
      "Missing argument: for an index, specify in 'independence_opts' one or more of:",
      "\n  ",
      paste(all_independence_opts, collapse = "; "),
      suffix
    )
  )
  if (missing(independence_opts)) {
    eval(error)
  } else if (length(independence_opts) > 0) {
    if (sum(!independence_opts %in% all_independence_opts) > 0) eval(error)
  }

  error <- expression(
    stop(
      "Missing argument: for an index, specify in 'aims_opts' one or more of:",
      "\n  ",
      paste(all_aims_opts, collapse = "; "),
      suffix
    )
  )
  if (missing(aims_opts)) {
    eval(error)
  } else if (length(aims_opts) > 0) {
    if (sum(!aims_opts %in% all_aims_opts) > 0) eval(error)
  }

  error <- expression(
    stop(
      "Missing argument: for an index, specify in 'consult_vars': ",
      paste(all_consult_vars, collapse = "; "),
      suffix
    )
  )
  if (missing(consult_vars)) {
    eval(error)
  } else if (length(consult_vars) > 0) {
    if (sum(!consult_vars %in% all_consult_vars) > 0) eval(error)
  }

  error <- expression(
    stop(
      "Missing argument: for an index, specify in 'powers_vars' one or more of:",
      "\n  ",
      paste(all_powers_vars, collapse = "; "),
      suffix
    )
  )
  if (missing(powers_vars)) {
    eval(error)
  } else if (length(powers_vars) > 0) {
    if (sum(!powers_vars %in% all_powers_vars) > 0) eval(error)
  }

  error <- expression(
    stop(
      "Missing argument: for an index, specify in 'testimony_vars' one or more of:",
      "\n  ",
      paste(all_testimony_vars, collapse = "; "),
      suffix
    )
  )
  if (missing(testimony_vars)) {
    eval(error)
  } else if (length(testimony_vars) > 0) {
    if (sum(!testimony_vars %in% all_testimony_vars) > 0) eval(error)
  }

  error <- expression(
    stop(
      "Missing argument: for an index, specify in 'reports_vars' one or more of:",
      "\n  ",
      paste(all_reports_vars, collapse = "; "),
      suffix
    )
  )
  if (missing(reports_vars)) {
    eval(error)
  } else if (length(reports_vars) > 0) {
    if (sum(!reports_vars %in% all_reports_vars) > 0) eval(error)
  }

  error <- expression(
    stop(
      "Missing argument: for an index, specify in 'recommend_vars' one or more of:",
      "\n  ",
      paste(all_recommend_vars, collapse = "; "),
      suffix
    )
  )
  if (missing(recommend_vars)) {
    eval(error)
  } else if (length(recommend_vars) > 0) {
    if (sum(!recommend_vars %in% all_recommend_vars) > 0) eval(error)
  }

  error <- expression(
    stop(
      "Missing argument: for an index, specify in 'monitor_vars': ",
      paste(all_monitor_vars, collapse = "; "),
      suffix
    )
  )
  if (missing(monitor_vars)) {
    eval(error)
  } else if (length(monitor_vars) > 0) {
    if (sum(!monitor_vars %in% all_monitor_vars) > 0) eval(error)
  }

  ## subsetting & calc

  new <- db[["TruthCommissions"]] |>
    mutate(
      all = 1,
      heldPublicHearings = ifelse(
        is.na(heldPublicHearings),
        "Unknown",
        heldPublicHearings
      ),
      heldPublicHearings = ifelse(heldPublicHearings == "Yes", 1, 0),
      testimonies = ifelse(is.na(testimonies), "Unknown", testimonies),
      testimonies = ifelse(testimonies == "Yes", 1, 0),
      encourageVictimTestimony = ifelse(
        is.na(encourageVictimTestimony),
        "Unknown",
        encourageVictimTestimony
      ),
      encourageVictimTestimony = ifelse(
        encourageVictimTestimony == "Yes",
        1,
        0
      ),
      perpetratorTestimony = ifelse(
        is.na(perpetratorTestimony),
        "Unknown",
        perpetratorTestimony
      ),
      perpetratorTestimony = ifelse(perpetratorTestimony == "Yes", 1, 0),
      finalReportIssued = ifelse(
        is.na(finalReportIssued),
        "Unknown",
        finalReportIssued
      ),
      finalReportIssued = ifelse(finalReportIssued == "Yes", 1, 0),
      reportPubliclyAvailable = ifelse(
        is.na(reportPubliclyAvailable),
        "Unknown",
        reportPubliclyAvailable
      ),
      reportPubliclyAvailable = ifelse(
        reportPubliclyAvailable == "Yes",
        1,
        0
      ),
      finalReportRecommendations = ifelse(
        is.na(finalReportRecommendations),
        "Unknown",
        finalReportRecommendations
      ),
      finalReportRecommendations = ifelse(
        finalReportRecommendations == "Yes",
        1,
        0
      ),
      mandatePeriodicMonitoringImplementation = ifelse(
        is.na(mandatePeriodicMonitoringImplementation),
        "Unknown",
        mandatePeriodicMonitoringImplementation
      ),
      mandatePeriodicMonitoringImplementation = ifelse(
        mandatePeriodicMonitoringImplementation == "Yes",
        1,
        0
      )
    ) |>
    # filter(authorizedByState == 1 & temporaryBodyReport == 1 & ## met criteria
    #   focusedPast == 1 & investigatePatternAbuse == 1 & ## met criteria
    #     neverOperated == 0 ) |>
    filter(eval(confl_df_cond)) |>
    filter(if_any(all_of(filter_nexus_vars), ~ . == 1)) |> ## context binary indicators
    filter(if_any(all_of(filter_crimes_vars), ~ . == 1)) |> ## crimes included
    mutate(
      goals = ifelse(
        !is.null(aims_opts) & truthcommissionID %in% TCGoals(aims_opts),
        1,
        0
      ),
      indep = ifelse(
        !is.null(independence_opts) &
          formallyIndependent %in% independence_opts,
        1,
        0
      ),
      consl = ifelse(
        !is.null(consult_vars) & if_any(all_of(consult_vars), ~ . == 1),
        1,
        0
      ),
      harms = rowSums(across(all_of(harms_vars))),
      power = rowSums(across(all_of(powers_vars))),
      testi = rowSums(across(all_of(testimony_vars))),
      repor = rowSums(across(all_of(reports_vars))),
      recom = rowSums(across(all_of(recommend_vars))),
      monit = ifelse(
        !is.null(monitor_vars) & if_any(all_of(monitor_vars), ~ . == 1),
        1,
        0
      ),
      index = goals +
        indep +
        consl +
        harms +
        power +
        testi +
        repor +
        recom +
        monit
    ) |>
    rename(year_start = .env$start_year_var) |>
    select(
      ccode,
      ucdpConflictID,
      year_start,
      index
    ) |>
    filter(!is.na(year_start)) |>
    arrange(ccode, ucdpConflictID, year_start) |>
    mutate(ucdpConflictID = str_split(ucdpConflictID, "; ")) |>
    unnest(ucdpConflictID) |>
    mutate(ucdpConflictID = as.integer(ucdpConflictID)) |>
    reframe(
      .by = c(all_of(unname(unit_id)), year_start),
      n = sum(index, na.rm = TRUE),
      index = max(index, na.rm = TRUE),
      binary = ifelse(n() > 0, 1, 0), # indicates filtered TCs, does not correspond to the index
      created = n() # number of filtered TCs
    )

  cy |>
    left_join(
      new,
      by = c(unit_id, "year" = "year_start")
    ) |>
    arrange(pick(all_of(group_var)), year) |>
    group_by(pick(all_of(group_var))) |>
    mutate(beg = index) |>
    fill(index, binary, .direction = "down") |>
    ungroup() |>
    mutate(
      index = ifelse(year %in% 1970:2024 & is.na(index), 0, index),
      n = ifelse(year %in% 1970:2024 & is.na(n), 0, n),
      beg = ifelse(year %in% 1970:2024 & is.na(beg), 0, beg),
      binary = ifelse(year %in% 1970:2024 & is.na(binary), 0, binary),
      created = ifelse(year %in% 1970:2024 & is.na(created), 0, created)
    ) |>
    rename_with(.fn = ~new_col_name, .cols = index) |>
    rename_with(.fn = ~ paste(new_col_name, "n", sep = "_"), .cols = n) |>
    rename_with(.fn = ~ paste(new_col_name, "beg", sep = "_"), .cols = beg) |>
    rename_with(
      .fn = ~ paste(new_col_name, "binary", sep = "_"),
      .cols = binary
    ) |>
    rename_with(
      .fn = ~ paste(new_col_name, "created", sep = "_"),
      .cols = created
    )
}
