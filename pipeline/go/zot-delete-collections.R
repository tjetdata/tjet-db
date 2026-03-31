### for one-time use for deleting collections

# DELETE <userOrGroupPrefix>/collections?collectionKey=<collectionKey>,<collectionKey>,<collectionKey>
# If-Unmodified-Since-Version: <last library version>

collections |>
  filter(str_detect(collection, "trialID ")) |>
  group_split(group_id = row_number() %/% 50) |>
  as.list() |>
  map(\(x) {
    z <- x |>
      pull(collection_key) |>
      unlist() |>
      str_flatten(collapse = ",")

    request(paste(base, "collections?collectionKey=", z, sep = "")) |>
      req_headers(
        'Zotero-API-Key' = zot_key,
        'Zotero-API-Version' = "3",
        # 'If-Unmodified-Since-Version' = x[["version"]]
      ) |>
      req_method(method = "DELETE") |>
      req_retry(max_seconds = 60, retry_on_failure = TRUE) |>
      # I()
      # req_dry_run()
      req_perform()
  })
