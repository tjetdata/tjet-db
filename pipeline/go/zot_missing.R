message(
  "Analyzing missing sourcing for TJET mechanisms and saving results locally..."
)

### amnesties
ids <- read_csv(here::here("tjet_datasets/tjet_amnesties.csv")) |>
  select(amnestyID) |>
  arrange(amnestyID) |>
  unlist(use.names = FALSE)
temp <- lookup |>
  filter(str_detect(id, "amnestyID")) |>
  mutate(
    amnestyID = as.integer(str_replace(id, "amnestyID ", "")),
    ref = TRUE
  ) |>
  select(amnestyID, ref) |>
  arrange(amnestyID) |>
  filter(amnestyID %in% ids) |>
  full_join(
    tjet[["MegaBase"]][["Amnesties"]] |>
      tibble() |>
      filter(amnestyID %in% ids) |>
      select(amnestyID, sourceInformation) |>
      arrange(amnestyID) |>
      mutate(db = TRUE),
    by = "amnestyID"
  ) |>
  filter(is.na(ref) | is.na(db))
if (nrow(temp) > 0) {
  write_csv(temp, here::here("zot_missing/amnesties.csv"), na = "")
}

### reparations
ids <- read_csv(here::here("tjet_datasets/tjet_reparations.csv")) |>
  select(reparationID) |>
  arrange(reparationID) |>
  unlist(use.names = FALSE)
temp <- lookup |>
  filter(str_detect(id, "reparationID")) |>
  mutate(
    reparationID = as.integer(str_replace(id, "reparationID ", "")),
    ref = TRUE
  ) |>
  select(reparationID, ref) |>
  arrange(reparationID) |>
  filter(reparationID %in% ids) |>
  full_join(
    tjet[["MegaBase"]][["Reparations"]] |>
      tibble() |>
      filter(reparationID %in% ids) |>
      select(
        reparationID,
        basicsSources,
        operationSources,
        policySources,
        implementationSources,
        nexusSources
      ) |>
      arrange(reparationID) |>
      mutate(db = TRUE),
    by = "reparationID"
  ) |>
  filter(is.na(ref) | is.na(db))
if (nrow(temp) > 0) {
  write_csv(temp, here::here("zot_missing/reparations.csv"), na = "")
}

### TCs
ids <- read_csv(here::here("tjet_datasets/tjet_tcs.csv")) |>
  select(truthcommissionID) |>
  arrange(truthcommissionID) |>
  unlist(use.names = FALSE)
temp <- lookup |>
  filter(str_detect(id, "truthcommissionID")) |>
  mutate(
    truthcommissionID = as.integer(str_replace(id, "truthcommissionID ", "")),
    ref = TRUE
  ) |>
  select(truthcommissionID, ref) |>
  arrange(truthcommissionID) |>
  filter(truthcommissionID %in% ids) |>
  full_join(
    tjet[["MegaBase"]][["TruthCommissions"]] |>
      tibble() |>
      filter(truthcommissionID %in% ids) |>
      select(
        truthcommissionID,
        basicsSources,
        fundingSource,
        sources,
        timingSources,
        criteriaSources,
        powersSources,
        operationSources,
        testimonySources,
        reportSources,
        implementationSources
      ) |>
      arrange(truthcommissionID) |>
      mutate(db = TRUE),
    by = "truthcommissionID"
  ) |>
  filter(is.na(ref) | is.na(db))
if (nrow(temp) > 0) {
  write_csv(temp, here::here("zot_missing/tcs.csv"), na = "")
}

### vetting
ids <- read_csv(here::here("tjet_datasets/tjet_vettings.csv")) |>
  select(vettingID) |>
  arrange(vettingID) |>
  unlist(use.names = FALSE)
temp <- lookup |>
  filter(str_detect(id, "vettingID")) |>
  mutate(
    vettingID = as.integer(str_replace(id, "vettingID ", "")),
    ref = TRUE
  ) |>
  select(vettingID, ref) |>
  arrange(vettingID) |>
  filter(vettingID %in% ids) |>
  full_join(
    tjet[["MegaBase"]][["Vettings"]] |>
      tibble() |>
      filter(vettingID %in% ids) |>
      select(vettingID, sources) |>
      arrange(vettingID) |>
      mutate(db = TRUE),
    by = "vettingID"
  ) |>
  filter(is.na(ref) | is.na(db))
if (nrow(temp) > 0) {
  write_csv(temp, here::here("zot_missing/vetting.csv"), na = "")
}

### trials
ids_trials <- read_csv(here::here("tjet_datasets/tjet_trials.csv")) |>
  select(trialID) |>
  arrange(trialID) |>
  unlist(use.names = FALSE)
temp_trials <- tjet[["Prosecutions"]][["Trials"]] |>
  tibble() |>
  filter(trialID %in% ids_trials) |>
  select(trialID, nonSDsourceFirst, nonSDsources) |>
  mutate(
    nonSDsourceFirst = ifelse(nonSDsourceFirst == "\n", NA, nonSDsourceFirst),
    nonSDsources = ifelse(nonSDsources == "\n", NA, nonSDsources)
  ) |>
  arrange(trialID) |>
  mutate(db = TRUE)

## these are trials that still need to be sourced
# lookup |>
#   filter(str_detect(id, "trialID")) |>
#   mutate(trialID = as.integer(str_replace(id, "trialID ", "")),
#          ref = TRUE) |>
#   select(trialID, ref) |>
#   arrange(trialID) |>
#   filter(trialID %in% ids_trials) |>
#   full_join(temp_trials,
#             by = "trialID") |>
#   filter(is.na(ref)) |>

## these are the trials that need to be sourced but actually have source information in Airtable
miss_trials <- lookup |>
  filter(str_detect(id, "trialID")) |>
  mutate(trialID = as.integer(str_replace(id, "trialID ", "")), ref = TRUE) |>
  select(trialID, ref) |>
  arrange(trialID) |>
  filter(trialID %in% ids_trials) |>
  full_join(temp_trials, by = "trialID") |>
  filter(is.na(ref)) |>
  filter(!is.na(nonSDsourceFirst) | !is.na(nonSDsources))
if (nrow(miss_trials) > 0) {
  write_csv(miss_trials, here::here("zot_missing/trials.csv"), na = "")
}

### accused
ids_acc <- read_csv(here::here("tjet_datasets/tjet_accused.csv")) |>
  select(accusedID) |>
  arrange(accusedID) |>
  unlist(use.names = FALSE)
temp_accused <- tjet[["Prosecutions"]][["Accused"]] |>
  tibble() |>
  rename(invalid_trial = `invalid [trialID]`) |>
  select(accusedID, trialID, invalid, invalid_trial, sources) |>
  unnest(c(trialID, invalid_trial)) |>
  filter(invalid == 0 & invalid_trial == 0) |>
  rename(airtable_record_id = trialID) |>
  left_join(
    tjet[["Prosecutions"]][["Trials"]] |>
      tibble() |>
      select(airtable_record_id, trialID),
    by = "airtable_record_id"
  ) |>
  select(accusedID, trialID, sources) |>
  arrange(accusedID) |>
  filter(accusedID %in% ids_acc & trialID %in% ids_trials) |>
  left_join(temp_trials |> select(-db), by = "trialID") |>
  mutate(db = TRUE)

## trials & accused without sourcing despite source info in Airtable
miss_prosecutions <- lookup |>
  filter(str_detect(id, "accusedID")) |>
  mutate(
    accusedID = as.integer(str_replace(id, "accusedID ", "")),
    ref_acc = TRUE
  ) |>
  select(accusedID, ref_acc) |>
  arrange(accusedID) |>
  full_join(temp_accused, by = "accusedID") |>
  filter(is.na(ref_acc) & db) |>
  filter(
    !(is.na(sources) & is.na(nonSDsourceFirst) & is.na(nonSDsources)) &
      sources !=
        "Bureau of Democracy Human Rights and Labor, U.S. Department of State, Country Reports on Human Rights Practices, available at <http://www.state.gov/j/drl/rls/hrrpt/>" &
      sources !=
        "Bureau of Democracy Human Rights and Labor, U.S. Department of State, Country Reports on Human Rights Practices, available at <http://www.state.gov/j/drl/rls/hrrpt/>\n" &
      sources !=
        "Bureau of Democracy Human Rights and Labor, U.S. Department of State, Country Reports on Human Rights Practices, available at <http://www.state.gov/j/drl/rls/hrrpt/>\r\n"
  ) |>
  select(accusedID, trialID, sources, nonSDsourceFirst, nonSDsources)
# if (nrow(miss_prosecutions) > 0) {
#   write_csv(
#     miss_prosecutions,
#     here::here("zot_missing/prosecutions.csv"),
#     na = ""
#   )
# }
