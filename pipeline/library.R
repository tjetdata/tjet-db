get_data <- TRUE
source(here::here("pipeline/go/zot_setup.R"))

message("Backing up Zotero...")
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

message("Processing collections...")
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
rm(dupes)

### don't actually need tags on their own; getting them below from items
# if(get_data) {
#   message("Downloading Zotero tags and saving locally...")
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
  message(
    "Downloading Zotero items locally (this will take a long time, at least 42-45 min)..."
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

message("Processing items...")
items <- readRDS(here::here("data/zot_items.rds")) |>
  resps_successes() |>
  resps_data(\(resp) resp_body_json(resp)) |>
  map(\(x) {
    x[["data"]]
  })

length(items)

message("Processing and cleaning tags...")

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

cleaned_tags |>
  filter(cleaned != tag) |>
  arrange(cleaned) |>
  print(n = Inf)

### FROM HERE >

CleanTags <- function(x) {
  if (length(x) == 0) {
    return(character(0))
  }
  tibble(tag = x) |>
    left_join(cleaned_tags, by = "tag") |>
    mutate(cleaned = if_else(is.na(cleaned), tag, cleaned)) |>
    pull(cleaned)
}

message(
  "Are there collections and/or tags not in the dictionary? Checking & updating..."
)

dict <- here::here("data/zot_dict.csv") |>
  read_csv()

# dict |>
#   filter(TJETincl == 1) |>
#   arrange(tag) |>
#   pull(tag) |>
#   write_lines("data/zot-include.txt", append = FALSE)

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
}

dict <- collections |>
  filter(
    !str_detect(
      collection,
      regex(
        "^(amnestyID|reparationID|trialID|truthcommissionID|vettingID)( [1-9]\\d*)$"
      )
    )
  ) |>
  # mutate(exist = TRUE) |>
  full_join(
    dict,
    by = c("collection", "collection_key", "parent_key", "parent")
  ) |>
  # filter(exist) |>
  select(
    parent,
    parent_key,
    collection,
    collection_key,
    tag,
    TJETincl
  ) |>
  full_join(unassigned, by = "tag") |>
  arrange(parent, collection, desc(TJETincl), tag) |>
  write_csv(here::here("data/zot_dict.csv"), na = "")

mechID_tags <- cleaned_tags |>
  select(cleaned) |>
  rename(tag = cleaned) |>
  filter(
    str_detect(
      tag,
      "^(amnestyID|reparationID|truthcommissionID|vettingID)( [1-9]\\d*)$" # trialID
    )
  )

message("Assigning accusedIDs to respective trialIDs if missing... ")
load(here::here("data/tjet.RData"))

ids <- tjet[["Prosecutions"]][["Accused"]] |>
  tibble() |>
  select(accusedID, trialID) |>
  unnest(trialID, keep_empty = TRUE) |>
  rename(airtable_record_id = trialID) |>
  left_join(
    tjet[["Prosecutions"]][["Trials"]] |>
      select(airtable_record_id, trialID),
    by = "airtable_record_id"
  ) |>
  select(accusedID, trialID)

getID <- function(x) {
  tibble(accusedID = x) |>
    left_join(ids, by = "accusedID") |>
    pull(trialID) |>
    as.character()
}

####################################################################################################
#### assigning accusedIDs to respective trialIDs may have to be done before creating new collections
####################################################################################################

message("Creating new sub-collections for mechanism IDs...")

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
      ) ~
        "truthcommissionID",
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

message("Creating lookup table for TJET website...")
# libbase <- "https://library.transitionaljusticedata.org/?tjetdb="
lookup <- collections |>
  filter(
    str_detect(
      collection,
      "^(accusedID|amnestyID|reparationID|trialID|truthcommissionID|vettingID)( [1-9]\\d*)$"
    )
  ) |>
  mutate(keys = paste(parent_key, ".", collection_key, sep = "")) |>
  select(collection, keys) |>
  arrange(collection) |>
  rename(id = collection)

message("Cleaning items & uploading edits...")

new <- items |>
  future_map(\(x) {
    # x = items[[200]]
    tags <- x[["tags"]] |>
      unlist(use.names = FALSE) |>
      CleanTags() |>
      sort()

    incl <- tags |>
      tibble(tag = _) |>
      inner_join(dict, by = "tag") |>
      reframe(
        TJETincl = sum(TJETincl, na.rm = TRUE) > 0
      ) |>
      unlist(use.names = FALSE)

    new_tags <- tags[str_detect(tags, regex("^(accusedID)( [1-9]\\d*)$"))] |>
      str_replace("accusedID ", "") |>
      as.integer() |>
      getID()
    new_tags <- unique(new_tags[!is.na(new_tags)])
    if (length(new_tags) > 0) {
      new_tags <- paste("trialID", new_tags)
    }
    new_tags <- unique(c(tags, new_tags))

    if (incl) {
      new_tags <- unique(c(new_tags, "TJETincl"))
    } else {
      new_tags <- new_tags[new_tags != "TJETincl"]
    }

    collection_keys <- dict |>
      filter(tag %in% tags) |>
      select(collection_key) |>
      filter(!is.na(collection_key)) |>
      distinct() |>
      unlist(use.names = FALSE)
    mech_collection_keys <- collections |>
      select(collection, collection_key) |>
      filter(
        str_detect(
          collection,
          "^(accusedID|amnestyID|reparationID|trialID|truthcommissionID|vettingID)( [1-9]\\d*)$"
        )
      ) |>
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
      collections = list(new_collections),
      old_tags = list(unlist(x[["tags"]])),
      tags = list(new_tags)
    )
  }) |>
  bind_rows() |>
  rowwise() |>
  mutate(
    update = length(collections) > length(old_collections) |
      length(tags) > length(old_tags),
    tags = list(
      map(tags, \(x) {
        list(tag = x)
      })
    )
  ) |>
  ungroup() |>
  filter(update)

responses <- new |>
  select(key, version, collections, tags) |>
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

responses

message("Writing lookup table to site generator database...")

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

# here::here("pipeline", "go", "zot_missing.R") |>
#   source()
