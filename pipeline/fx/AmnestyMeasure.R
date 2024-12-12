AmnestyMeasure <- function(cy,
                           nexus_vars = "all", # NULL also works
                           peace_vars = NULL,
                           who_opts = "all", # NULL also works
                           what_opts = "all" # NULL also works
) {
  ## options
  peace <- c("peaceSettlement")
  nexus <- c(
    all = "all", dtj = "fitsPostAutocraticTJ",
    ctj = "fitsConflictTJ", dcj = "dcj", pcj = "pcj"
  )
  who <- c(
    all = "all",
    sta = "who_sta", # state agents
    opp = "who_opp", # armed opposition
    pol = "who_pol", # political prisoners, protesters
    oth = "who_oth"
  )
  what <- c(
    all = "all",
    reb = "what_reb", # rebellion & terrorism
    hrv = "what_hrv", # human rights violations
    dis = "what_dis", # dissent
    coi = "what_coi", #  armed violence against rebels
    oth = "what_oth"
  )

  ## input errors
  suffix <- "\n  or NULL if this aspect is not relevant to the new measure"

  error <- expression(
    stop(
      "Missing or invalid argument for 'nexus_vars', select one or more of:",
      "\n  ", paste(names(nexus), collapse = "; "), suffix
    )
  )
  # if(missing(nexus_vars)) {
  #   eval(error)
  # } else
  if (sum(!nexus_vars %in% names(nexus)) > 0) eval(error)

  error <- expression(
    stop(
      "Invalid argument for 'peace_vars', select one or more of:",
      "\n  ", paste(peace, collapse = "; "), suffix
    )
  )
  # if(missing(peace_vars)) {
  #   eval(error)
  # } else
  if (length(peace_vars) > 0) if (sum(!peace_vars %in% peace) > 0) eval(error)

  error <- expression(
    stop(
      "Missing or invalid argument for 'who_opts', select one or more of:",
      "\n  ", paste(names(who), collapse = "; "), suffix
    )
  )
  # if(missing(who_opts)) {
  #   eval(error)
  # } else
  if (sum(!who_opts %in% names(who)) > 0) eval(error)

  error <- expression(
    stop(
      "Missing or invalid argument for 'what_opts', select one or more of:",
      "\n  ", paste(names(what), collapse = "; "), suffix
    )
  )
  # if(missing(what_opts)) {
  #   eval(error)
  # } else
  if (sum(!what_opts %in% names(what)) > 0) eval(error)

  if (length(peace_vars) > 0) {
    settlement <- "peaceagree"
  } else {
    settlement <- NULL
  }

  var_name <- str_flatten(c(
    "amnesty", nexus_vars, settlement,
    who_opts, what_opts
  ), collapse = "_") %>%
    str_replace_all(fixed("_all"), "")

  amn <- db[["Amnesties"]] %>%
    mutate(
      all = 1,
      who_sta = ifelse(str_detect(whoWasAmnestied, "state agents"), 1, 0),
      who_opp = ifelse(str_detect(whoWasAmnestied, "armed opposition"), 1, 0),
      who_pol = ifelse(str_detect(whoWasAmnestied, "protesters / political prisoners"), 1, 0),
      who_oth = ifelse(str_detect(whoWasAmnestied, "collaborators") |
        str_detect(whoWasAmnestied, "draft dodgers / deserters") |
        str_detect(whoWasAmnestied, "refugees / exiles") |
        str_detect(whoWasAmnestied, "regular convicts") |
        str_detect(whoWasAmnestied, "other"), 1, 0),
      what_reb = ifelse(str_detect(whatCrimes, "armed opposition") |
        str_detect(whatCrimes, "terrorism"), 1, 0),
      what_hrv = ifelse(str_detect(whatCrimes, "human rights violations"), 1, 0),
      what_dis = ifelse(str_detect(whatCrimes, "dissent / political crimes"), 1, 0),
      what_coi = ifelse(str_detect(whatCrimes, "armed violence against rebels"), 1, 0),
      what_oth = ifelse(str_detect(whoWasAmnestied, "collaboration") |
        str_detect(whoWasAmnestied, "regular crime") |
        str_detect(whoWasAmnestied, "corruption or other economic crimes") |
        str_detect(whoWasAmnestied, "dereliction of duty") |
        str_detect(whoWasAmnestied, "other"), 1, 0),
    ) %>%
    select(
      amnestyID, ccode, amnestyYear, fitsPostAutocraticTJ, fitsConflictTJ,
      dcj, pcj, all, all_of(peace_vars), # whoWasAmnestied,
      who_sta, who_opp, who_pol, who_oth,
      what_reb, what_hrv, what_dis, what_oth, what_coi
    ) %>%
    filter(if_any(all_of(nexus[nexus_vars]), ~ . == 1)) %>%
    filter(if_any(all_of(peace_vars), ~ . == 1)) %>%
    filter(if_any(all_of(who[who_opts]), ~ . == 1)) %>%
    filter(if_any(all_of(what[what_opts]), ~ . == 1)) %>%
    select(amnestyID, ccode, amnestyYear) %>%
    arrange(ccode, amnestyYear) %>%
    group_by(ccode, amnestyYear) %>%
    mutate(binary = ifelse(n() > 0, 1, 0)) %>%
    ungroup() %>%
    select(-amnestyID) %>%
    distinct()

  cy %>%
    left_join(amn, by = c("ccode_cow" = "ccode", "year" = "amnestyYear")) %>%
    mutate(
      binary = ifelse(year %in% 1970:2020 & is.na(binary), 0, binary),
      binary = ifelse(year > 2020, NA, binary)
    ) %>%
    rename_with(.fn = ~var_name, .cols = binary) %>%
    return()
}
