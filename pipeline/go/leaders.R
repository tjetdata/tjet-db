library(tidyverse)

load(here::here("data", "tjet.RData"), verbose = FALSE)
map(tjet, names)

lead <- tjet[["Leaders"]] |>
  map(as_tibble)

countries <- lead[["Countries"]] |>
  select(
    airtable_record_id,
    country,
    ccode
  )

leaders <- lead[["Leaders"]] |>
  unnest(country, keep_empty = TRUE) |>
  left_join(
    countries |>
      rename(country_leader = country),
    by = c("country" = "airtable_record_id")
  ) |>
  mutate(
    leader = str_squish(leader)
  ) |>
  rowwise() |>
  mutate(
    yrfirst = min(as.integer(yrfirst), na.rm = TRUE),
    yrlast = max(as.integer(yrlast), na.rm = TRUE),
    position_vdem_whogov = str_flatten(
      unique(position_vdem_whogov),
      collapse = "; "
    ),
    type_vdem = str_flatten(unique(type_vdem), collapse = "; "),
    source_leaders = str_flatten(source_leaders, collapse = "; ")
  ) |>
  ungroup() |>
  mutate(
    type_vdem = case_when(
      type_vdem == "" ~ NA,
      str_detect(type_vdem, "HOG") & str_detect(type_vdem, "HOS") ~ "leader",
      str_detect(type_vdem, "leader") ~ "leader",
      .default = type_vdem
    )
  ) |>
  # filter(n_valid_pros > 0) |>
  select(
    airtable_record_id,
    leaderID,
    leader,
    country_leader,
    ccode,
    yrfirst,
    yrlast,
    type_vdem,
    power_vdem,
    n_valid_pros
  )

leaders |>
  select(
    leaderID,
    leader,
    country_leader,
    ccode,
    yrfirst,
    yrlast,
    n_valid_pros
  ) |>
  arrange(country_leader, yrfirst, yrlast) |>
  filter(country_leader != "Palestine") |>
  write_csv(file = here::here("tjet_datasets", "leaders.csv"))

leader_cases <- lead[["LegalCases"]] |>
  unnest(countryTrial, keep_empty = TRUE) |>
  left_join(
    countries,
    by = c("countryTrial" = "airtable_record_id")
  ) |>
  select(-countryTrial) |>
  rename(
    country_trial = country,
    ccode_trial = ccode
  ) |>
  mutate(
    invalid = case_when(
      invalid ~ 1,
      is.na(invalid) ~ 0,
      .default = 0
    ),
    description = str_squish(description)
  ) |>
  filter(invalid == 0 & case_type == "criminal") |>
  rowwise() |>
  mutate(
    charges_type = str_flatten(charges_type, collapse = "; ")
  ) |>
  ungroup() |>
  select(
    airtable_record_id,
    leaderIDs,
    caseID,
    year_start,
    trial_type,
    # description,
    charges_type,
    country_trial,
    ccode_trial,
    # eventIDs,
    n_leaders,
    n_events,
  ) |>
  unnest(leaderIDs) |>
  left_join(
    leaders |>
      select(airtable_record_id, leaderID, country_leader, ccode, yrfirst),
    by = c("leaderIDs" = "airtable_record_id")
  ) |>
  rename(ccode_leader = ccode) |>
  filter(year_start >= yrfirst & country_leader != "Palestine") |>
  mutate(
    ccode_leader = if_else(
      ccode_leader == 265 & year_start >= 1990,
      255,
      ccode_leader
    )
  )

leader_cases |>
  select(
    caseID,
    country_trial,
    ccode_trial,
    year_start,
    trial_type,
    country_leader,
    ccode_leader,
    leaderID,
    charges_type,
    n_leaders
  ) |>
  arrange(country_leader, year_start) |>
  write_csv(file = here::here("tjet_datasets", "leaders_cases.csv"))

leader_case_events <- lead[["CaseEvents"]] |>
  unnest(country_event, keep_empty = TRUE) |>
  left_join(
    countries,
    by = c("country_event" = "airtable_record_id")
  ) |>
  select(-country_event) |>
  rename(
    country_event = country,
    ccode_event = ccode
  ) |>
  mutate(
    date = make_date(
      year,
      coalesce(month, 1L),
      coalesce(day, 1L)
    ),
    actor_or_court = str_squish(actor_or_court),
    event = as.factor(event),
    sentence_scale = as.factor(sentence_scale),
  ) |>
  # filter(is.na(date))
  rowwise() |>
  mutate(
    conviction_notes = str_flatten(conviction_notes, collapse = "; ")
  ) |>
  ungroup() |>
  mutate(
    conviction_notes = if_else(conviction_notes == "", NA, conviction_notes)
  ) |>
  select(
    airtable_record_id,
    leaderID,
    caseID,
    eventID,
    country_event,
    ccode_event,
    year,
    date,
    actor_or_court,
    event,
    sentence_scale,
    conviction_notes,
    n_leaders,
    n_cases,
    n_valid,
  ) |>
  unnest(c(leaderID, caseID))

leader_case_events |>
  rename(temp = caseID) |>
  left_join(
    leader_cases |>
      select(airtable_record_id, caseID) |>
      distinct(),
    by = c("temp" = "airtable_record_id")
  ) |>
  filter(!is.na(caseID)) |>
  select(
    eventID,
    caseID,
    country_event,
    ccode_event,
    year,
    date,
    actor_or_court,
    event,
    sentence_scale,
    conviction_notes
  ) |>
  write_csv(file = here::here("tjet_datasets", "leaders_case_events.csv"))

cases <- leader_cases |>
  mutate(
    hrs = if_else(
      str_detect(charges_type, "HRs violations") |
        str_detect(charges_type, "Sexual or Gender-based Violence"),
      1,
      0
    ),
    # ecn = if_else(str_detect(charges_type, "Economic crimes"), 1, 0),
    oth = if_else(
      str_detect(charges_type, "Economic crimes") |
        str_detect(charges_type, "Crimes against the state") |
        str_detect(charges_type, "Interpersonal or ordinary crime"),
      1,
      0
    ),
    trial_type = case_when(
      trial_type %in%
        c("ICC", "ICTR", "ICTY", "hybrid", "foreign") ~ "non-domestic",
      .default = trial_type
    ) |>
      as.factor()
  ) |>
  reframe(
    .by = c(
      airtable_record_id,
      country_leader,
      ccode_leader,
      caseID,
      country_trial,
      trial_type,
      year_start
    ),
    n_leaders = n(),
    hrs = sum(hrs),
    # ecn = sum(ecn),
    oth = sum(oth),
  ) |>
  rename(year = year_start) |>
  arrange(country_leader, year)

case_convictions <- leader_case_events |>
  filter(str_detect(event, "guilty")) |>
  reframe(
    .by = c(caseID, leaderID),
    year = min(year),
    convict_first = 1
  ) |>
  reframe(
    .by = c(caseID, year),
    convict_first = sum(convict_first)
  ) |>
  arrange(caseID, year) |>
  inner_join(
    cases |>
      select(
        airtable_record_id,
        country_leader,
        ccode_leader,
        trial_type,
        hrs,
        # ecn,
        oth
      ),
    by = c("caseID" = "airtable_record_id")
  ) |>
  arrange(country_leader, caseID, year)

lead_cy <- list()

lead_cy[["lea_trs_dom"]] <- cases |>
  filter(trial_type == "domestic") |>
  reframe(
    .by = c(ccode_leader, year),
    lea_trs_dom = sum(n_leaders),
    lea_trs_hrs_dom = sum(hrs),
    # lea_trs_ecn_dom = sum(ecn),
    lea_trs_oth_dom = sum(oth)
  )

lead_cy[["lea_trs_non"]] <- cases |>
  filter(trial_type == "non-domestic") |>
  reframe(
    .by = c(ccode_leader, year),
    lea_trs_non = sum(n_leaders),
    lea_trs_hrs_non = sum(hrs),
    # lea_trs_ecn_non = sum(ecn),
    lea_trs_oth_non = sum(oth)
  )

lead_cy[["lea_cec_dom"]] <- case_convictions |>
  filter(trial_type == "domestic") |>
  reframe(
    .by = c(ccode_leader, year),
    lea_cec_dom = sum(convict_first, na.rm = TRUE),
  )

lead_cy[["lea_cec_non"]] <- case_convictions |>
  filter(trial_type == "non-domestic") |>
  reframe(
    .by = c(ccode_leader, year),
    lea_cec_non = sum(convict_first, na.rm = TRUE),
  )

lead_cy[["lea_cec_hrs_dom"]] <- case_convictions |>
  filter(trial_type == "domestic" & hrs > 0) |>
  reframe(
    .by = c(ccode_leader, year),
    lea_cec_hrs_dom = sum(convict_first, na.rm = TRUE),
  )

# lead_cy[["lea_cec_ecn_dom"]] <- case_convictions |>
#   filter(trial_type == "domestic" & ecn > 0) |>
#   reframe(
#     .by = c(ccode_leader, year),
#     lea_cec_ecn_dom = sum(convict_first, na.rm = TRUE),
#   )

lead_cy[["lea_cec_oth_dom"]] <- case_convictions |>
  filter(trial_type == "domestic" & oth > 0) |>
  reframe(
    .by = c(ccode_leader, year),
    lea_cec_oth_dom = sum(convict_first, na.rm = TRUE),
  )

lead_cy[["lea_cec_hrs_non"]] <- case_convictions |>
  filter(trial_type == "non-domestic" & hrs > 0) |>
  reframe(
    .by = c(ccode_leader, year),
    lea_cec_hrs_non = sum(convict_first, na.rm = TRUE),
  )

# lead_cy[["lea_cec_ecn_non"]] <- case_convictions |>
#   filter(trial_type == "non-domestic" & ecn > 0) |>
#   reframe(
#     .by = c(ccode_leader, year),
#     lea_cec_ecn_non = sum(convict_first, na.rm = TRUE),
#   )

lead_cy[["lea_cec_oth_non"]] <- case_convictions |>
  filter(trial_type == "non-domestic" & oth > 0) |>
  reframe(
    .by = c(ccode_leader, year),
    lea_cec_oth_non = sum(convict_first, na.rm = TRUE),
  )

lead_cy <- lead_cy |>
  reduce(full_join, by = c("ccode_leader", "year")) |>
  mutate(across(!c(ccode_leader, year), ~ if_else(is.na(.x), 0, .x)))

# lead_cy |>
#   filter(.by = c(ccode_leader, year), n() > 1 )
