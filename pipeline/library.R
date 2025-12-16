get_data <- TRUE
source(here::here("pipeline/go/zot_setup.R"))

cat("\nBacking up Zotero...\n")
file.copy(
  from = "~/Zotero/zotero.sqlite",
  # to = paste("~/Zotero/zotero.sqlite.backup", str_replace_all(today(), "-", "_"), sep = "_"),
  to = "~/Zotero/zotero.sqlite.TJETbackup",
  copy.date = TRUE,
  overwrite = TRUE
)

if (get_data) {
  cat("\nDownloading Zotero collections and saving locally...\n")
  req <- request(paste(base, "collections?format=json", sep = "")) |>
    req_headers(
      'Zotero-API-Key' = zot_key,
      'Zotero-API-Version' = "3",
      Accept = "application/json"
    )
  resps <- req |>
    req_retry(max_seconds = 60, retry_on_failure = TRUE) |>
    req_perform_iterative(
      next_req = iterate_with_link_url(rel = "next"),
      max_reqs = Inf,
      on_error = "return"
    )
  saveRDS(resps, here::here("data/zot_collections.rds"))
}

cat("\nProcessing collections...\n")
collections <- readRDS(here::here("data/zot_collections.rds")) |>
  resps_successes() |>
  resps_data(\(resp) resp_body_json(resp)) |>
  map(function(x) {
    x[["data"]][c("key", "version", "name", "parentCollection")] |>
      as_tibble() |>
      mutate(
        parentCollection = ifelse(
          is.logical(parentCollection),
          NA,
          parentCollection
        )
      )
  }) |>
  bind_rows() |>
  rename(collection = name, collection_key = key, parent_key = parentCollection)

collections <- collections |>
  left_join(
    collections |>
      select(collection_key, collection) |>
      rename(parent = collection),
    by = c("parent_key" = "collection_key")
  ) |>
  select(collection, collection_key, parent_key, parent) |>
  arrange(parent, collection)

dupes <- collections |>
  filter(
    .by = c(collection, parent_key),
    n() > 1
  ) |>
  arrange(collection)

if (nrow(dupes) > 0) {
  print(dupes, n = Inf)
  stop(
    "There are duplicate mechanism ID collections! These need to be resolved manually in Zotero first."
  )
}

### don't actually need tags on their own; getting them below from items
# if(get_data) {
#   cat("\nDownloading Zotero tags and saving locally...\n")
#   req <- request(paste(base, "tags?format=json", sep = "") ) |>
#     req_headers(
#       'Zotero-API-Key' = zot_key,
#       'Zotero-API-Version' = 3,
#       Accept = "application/json"
#     )
#
#   req |>
#     req_perform() |>
#     resp_headers()
#
#   resps <- req |>
#     req_retry(max_seconds = 60, retry_on_failure = TRUE) |>
#     req_perform_iterative(next_req = iterate_with_link_url(rel = "next"),
#                           max_reqs = Inf, on_error = "return")
#   saveRDS(resps, "tags.rds")
# }
#
# tags <- readRDS("tags.rds") |>
#   resps_successes() |>
#   resps_data(\(resp) resp_body_json(resp)) |>
#   map(function(x) {x$tag}) |>
#   tibble(tag = _) |>
#   unnest(tag) |>
#   arrange(tag)

if (get_data) {
  cat(
    "\nDownloading Zotero items locally (this will take a long time, at least 30-45 min)...\n"
  )
  req <- request(paste(base, "items?format=json", sep = "")) |>
    req_headers(
      'Zotero-API-Key' = zot_key,
      'Zotero-API-Version' = "3",
      Accept = "application/json"
    )
  # req |>
  #   req_dry_run()
  #   req_perform()
  # resp <- req |>
  #   resp_headers()
  #   resp_header("link")
  #   resp_link_url(rel = "next")
  #   resp_raw()
  #   resp_body_json()
  resps <- req |>
    req_retry(max_seconds = 60, retry_on_failure = TRUE) |>
    req_perform_iterative(
      next_req = iterate_with_link_url(rel = "next"),
      max_reqs = Inf,
      on_error = "return"
    )
  saveRDS(resps, here::here("data/zot_items.rds"))
}

cat("\nProcessing items...\n")
items <- readRDS(here::here("data/zot_items.rds")) |>
  resps_successes() |>
  resps_data(\(resp) resp_body_json(resp)) |>
  map(\(x) {
    x[["data"]]
  })

cat("\nProcessing and cleaning tags...\n")

tags <- items |>
  map(\(x) {
    x[["tags"]] |>
      map(\(z) {
        z[["tag"]]
      }) |>
      unlist() |>
      tibble(tag = _)
  }) |>
  bind_rows() |>
  distinct() |>
  arrange(tag)

cleaned_tags <- tags |>
  mutate(
    cleaned = str_replace(tag, "   ", "  "),
    cleaned = str_replace(tag, "  ", " "),
    cleaned = ifelse(
      str_detect(
        str_to_lower(cleaned),
        regex(
          "^(accusedid|amnestyid|reparationid|trialid|truthcommissionid|vettingid|leaderid)"
        )
      ),
      str_to_lower(cleaned),
      cleaned
    ),
    cleaned = str_replace(cleaned, "id ", "ID ")
  )

### these are actually not yet getting cleaned below; need to fix that!!!
cleaned_tags |>
  filter(cleaned != tag) |>
  arrange(cleaned) |>
  print(n = Inf)

dict <- read_csv(here::here("data/zot_dict.csv"))

cat("\nAre there collections and/or tags not in the dictionary? Checking...\n")

### are there new collections?
collections |>
  filter(
    !str_detect(
      collection,
      regex(
        "^(amnestyID|reparationID|trialID|truthcommissionID|vettingID)( [1-9]\\d*)$"
      )
    )
  ) |>
  full_join(
    dict |>
      select(collection, collection_key, parent_key) |>
      mutate(dict = TRUE),
    by = c("collection", "collection_key", "parent_key")
  ) |>
  filter(is.na(dict))

### are there tags not in the dictionary?
unassigned <- cleaned_tags |>
  select(cleaned) |>
  rename(tag = cleaned) |>
  filter(
    !str_detect(
      tag,
      regex(
        "^(accusedID|amnestyID|reparationID|trialID|truthcommissionID|vettingID|leaderID|legalID)( [1-9]\\d*)$"
      )
    )
  ) |>
  full_join(
    dict |>
      select(tag) |>
      mutate(dict = TRUE),
    by = "tag"
  ) |>
  filter(is.na(dict)) |>
  select(-dict)
if (nrow(unassigned) > 0) {
  warning("There are unassigned tags!")
  print(unassigned, n = Inf)
}

mechID_tags <- cleaned_tags |>
  select(cleaned) |>
  rename(tag = cleaned) |>
  filter(str_detect(
    tag,
    "^(accusedID|amnestyID|reparationID|trialID|truthcommissionID|vettingID)( [1-9]\\d*)$"
  ))

# other_tags <- cleaned_tags |>
#   select(cleaned) |>
#   rename(tag = cleaned) |>
#   filter(!str_detect(tag, "^(accusedID|amnestyID|reparationID|trialID|truthcommissionID|vettingID)( [1-9]\\d*)$"))

### assigning accusedIDs tags to respective trialIDs

read_csv(here::here("tjet_datasets/tjet_accused.csv")) |>
  select(accusedID, trialID)


cat("\nCreating new sub-collections for mechanism IDs...\n")

new_subcollections <- mechID_tags |>
  arrange(tag) |>
  mutate(
    mech = case_when(
      str_detect(tag, regex("^(amnestyID)( [1-9]\\d*)$")) ~ "amnestyID",
      str_detect(tag, regex("^(reparationID)( [1-9]\\d*)$")) ~ "reparationID",
      str_detect(tag, regex("^(trialID)( [1-9]\\d*)$")) ~ "trialID",
      str_detect(
        tag,
        regex("^(truthcommissionID)( [1-9]\\d*)$")
      ) ~ "truthcommissionID",
      str_detect(tag, regex("^(vettingID)( [1-9]\\d*)$")) ~ "vettingID",
      TRUE ~ tag
    )
  ) |>
  full_join(
    dict |>
      filter(str_detect(tag, fixed(" *"))) |>
      mutate(tag = str_replace(tag, fixed(" *"), "")) |>
      select(tag, collection_key),
    by = c("mech" = "tag")
  ) |>
  select(-mech) |>
  left_join(
    collections |>
      select(collection, parent_key) |>
      mutate(exists = TRUE),
    by = c("tag" = "collection", "collection_key" = "parent_key")
  ) |>
  filter(is.na(exists)) |>
  select(tag, collection_key) |>
  rename(name = tag, parentCollection = collection_key)

### creating missing sub-collections

responses <- new_subcollections |>
  group_split(group_id = row_number() %/% 50) |>
  as.list() |>
  map(\(df) {
    resp <- request(paste(base, "collections/", sep = "")) |>
      req_headers(
        'Zotero-API-Key' = zot_key,
        'Zotero-API-Version' = "3",
        'Zotero-Write-Token' = ids::random_id()
      ) |>
      req_body_json(
        data = df |>
          select(name, parentCollection) |>
          as.list() |>
          list_transpose(simplify = FALSE)
      ) |>
      req_method(method = "POST") |>
      req_retry(max_seconds = 60, retry_on_failure = TRUE) |>
      # req_dry_run()
      req_perform()
  })

### read updated collections again

if (nrow(new_subcollections) > 0) {
  cat("\nDownloading updated collections and saving locally...\n")
  req <- request(paste(base, "collections?format=json", sep = "")) |>
    req_headers(
      'Zotero-API-Key' = zot_key,
      'Zotero-API-Version' = "3",
      Accept = "application/json"
    )
  resps <- req |>
    req_retry(max_seconds = 60, retry_on_failure = TRUE) |>
    req_perform_iterative(
      next_req = iterate_with_link_url(rel = "next"),
      max_reqs = Inf,
      on_error = "return"
    )
  saveRDS(resps, here::here("data/zot_collections.rds"))

  collections <- readRDS(here::here("data/zot_collections.rds")) |>
    resps_successes() |>
    resps_data(\(resp) resp_body_json(resp)) |>
    map(function(x) {
      x[["data"]][c("key", "version", "name", "parentCollection")] |>
        as_tibble() |>
        mutate(
          parentCollection = ifelse(
            is.logical(parentCollection),
            NA,
            parentCollection
          )
        )
    }) |>
    bind_rows() |>
    rename(
      collection = name,
      collection_key = key,
      parent_key = parentCollection
    )
}

cat("\nCreating lookup table for TJET website...\n")
# libbase <- "https://library.transitionaljusticedata.org/?tjetdb="
lookup <- collections |>
  filter(str_detect(
    collection,
    "^(accusedID|amnestyID|reparationID|trialID|truthcommissionID|vettingID)( [1-9]\\d*)$"
  )) |>
  mutate(keys = paste(parent_key, ".", collection_key, sep = "")) |>
  select(collection, keys) |>
  arrange(collection) |>
  rename(id = collection)

cat("\nCleaning items & uploading edits...\n")

new <- items |>
  future_map(\(x) {
    tags <- x[["tags"]] |>
      unlist(use.names = FALSE)
    collection_keys <- dict |>
      filter(tag %in% tags & is.na(tag2)) |>
      select(collection_key) |>
      distinct() |>
      unlist(use.names = FALSE)
    mech_collection_keys <- collections |>
      select(collection, collection_key) |>
      filter(str_detect(
        collection,
        "^(accusedID|amnestyID|reparationID|trialID|truthcommissionID|vettingID)( [1-9]\\d*)$"
      )) |>
      filter(collection %in% tags) |>
      select(collection_key) |>
      distinct() |>
      unlist(use.names = FALSE)
    new_collections <- x[["collections"]] |>
      c(collection_keys, mech_collection_keys) |>
      unique() |>
      unlist(use.names = FALSE)
    tibble(
      key = x[["key"]],
      version = x[["version"]],
      old_collections = list(unlist(x[["collections"]])),
      collections = list(new_collections)
    )
  }) |>
  bind_rows() |>
  rowwise() |>
  mutate(update = length(collections) > length(old_collections)) |>
  ungroup() |>
  select(key, version, collections, update)

responses <- new |>
  filter(update) |>
  select(-update) |>
  group_split(group_id = row_number() %/% 50) |>
  as.list() |>
  map(\(x) {
    resp <- request(paste(base, "items/", sep = "")) |>
      req_headers(
        'Zotero-API-Key' = zot_key,
        'Zotero-API-Version' = "3"
      ) |>
      req_body_json(
        data = x |>
          select(-group_id) |>
          as.list() |>
          list_transpose(simplify = FALSE)
      ) |>
      req_method(method = "POST") |>
      req_retry(max_seconds = 60, retry_on_failure = TRUE) |>
      # req_dry_run()
      req_perform()
  })

cat("\nWriting lookup table to site generator database...\n")

con <- dbConnect(
  RMariaDB::MariaDB(),
  host = Sys.getenv("TJET_DATABASE_HOST"),
  dbname = Sys.getenv("TJET_DATABASE_NAME"),
  user = Sys.getenv("TJET_DATABASE_USER"),
  password = Sys.getenv("TJET_DATABASE_PASSWORD")
)
dbListTables(con) %>%
  sort()

dbExecute(con, "TRUNCATE TABLE mechIDcollections")
dbWriteTable(
  conn = con,
  name = "mechIDcollections",
  value = lookup,
  append = TRUE
)
dbReadTable(con, "mechIDcollections") %>%
  tibble()
dbDisconnect(con)

#### what's missing?
cat(
  "\nAnalyzing missing sourcing for TJET mechanisms and saving results locally...\n"
)

load(here::here("data", "tjet.RData"), verbose = TRUE)

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
if (nrow(miss_prosecutions) > 0) {
  write_csv(
    miss_prosecutions,
    here::here("zot_missing/prosecutions.csv"),
    na = ""
  )
}
