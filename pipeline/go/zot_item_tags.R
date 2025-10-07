
stop("This script contains one-off code!")


### Argentina Ministerio

new_tags <- c(
  read_csv("~/Desktop/Zotero_work/Accused-sourcing_missing_Arg.csv") |>
  select(trialID) |> 
  distinct() |> 
  arrange(trialID) |> 
  unlist(use.names = FALSE) %>% 
  paste("trialID", .), 
read_csv("~/Desktop/Zotero_work/Accused-sourcing_missing_Arg.csv") |>
  select(accusedID) |> 
  distinct() |> 
  arrange(accusedID) |> 
  unlist(use.names = FALSE) %>% 
  paste("accusedID", .) 
) 
items[map(items, function(x) {x[["key"]]} ) %in% c("WVED52P9")] |>
  future_map(\(x) {
    tibble(key = x[["key"]],
           version = x[["version"]],
           tags = list(unlist(x[["tags"]]))
    )
  }) |>
  bind_rows() |>
  rowwise() |>
  mutate(
    tags = list(unique(c(tags, new_tags))),
    tags = list(map(tags, \(x) { list(tag = x)}))
  ) |>
  ungroup() |>
  group_split(group_id = row_number() %/% 50) |>
  as.list() |>
  map(\(x) {
    resp <- request(paste(base, "items/", sep = "")) |>
      req_headers(
      'Zotero-API-Key' = zot_key,
      'Zotero-API-Version' = "3"
      ) |>
      req_body_json(data = x |>
                      select(-group_id) |>
                      as.list() |>
                      list_transpose(simplify = FALSE)
                    ) |>
      req_method(method = "POST") |>
      req_retry(max_seconds = 60, retry_on_failure = TRUE) |>
      # req_dry_run()
      req_perform()
  })


### Chile Observatorio

new_tags <- c(
  read_csv("~/Desktop/Zotero_work/Accused-sourcing_missing_Chile.csv") |>
  select(trialID) |> 
  distinct() |> 
  arrange(trialID) |> 
  unlist(use.names = FALSE) %>% 
  paste("trialID", .), 
read_csv("~/Desktop/Zotero_work/Accused-sourcing_missing_Chile.csv") |>
  select(accusedID) |> 
  distinct() |> 
  arrange(accusedID) |> 
  unlist(use.names = FALSE) %>% 
  paste("accusedID", .) 
) 
items[map(items, function(x) {x[["key"]]} ) %in% c("8A3C248T")] |>
  future_map(\(x) {
    tibble(key = x[["key"]],
           version = x[["version"]],
           tags = list(unlist(x[["tags"]]))
    )
  }) |>
  bind_rows() |>
  rowwise() |>
  mutate(
    tags = list(unique(c(tags, new_tags))),
    tags = list(map(tags, \(x) { list(tag = x)}))
  ) |>
  ungroup() |>
  group_split(group_id = row_number() %/% 50) |>
  as.list() |>
  map(\(x) {
    resp <- request(paste(base, "items/", sep = "")) |>
      req_headers(
      'Zotero-API-Key' = zot_key,
      'Zotero-API-Version' = "3"
      ) |>
      req_body_json(data = x |>
                      select(-group_id) |>
                      as.list() |>
                      list_transpose(simplify = FALSE)
                    ) |>
      req_method(method = "POST") |>
      req_retry(max_seconds = 60, retry_on_failure = TRUE) |>
      # req_dry_run()
      req_perform()
  })


### hosID tags
to_revise <- items |>
  future_map(\(x) {
    tibble(key = x[["key"]],
           version = x[["version"]],
           tags = list(unlist(x[["tags"]]))
    )
  }) |>
  bind_rows() |>
  rowwise() |>
  filter(str_detect(str_flatten_comma(unlist(tags)), "hosID")) |>
  mutate(
    tags = list(unique(str_squish(unlist(tags)))),
    tags = list(str_replace(unlist(tags), "hosID", "leaderID")),
    tags = list(map(tags, \(x) { list(tag = x)}))
  ) |>
  ungroup()

resp <- to_revise |>
  group_split(group_id = row_number() %/% 50) |>
  as.list() |>
  map(\(x) {
    resp <- request(paste(base, "items/", sep = "")) |>
      req_headers(
      'Zotero-API-Key' = zot_key,
      'Zotero-API-Version' = "3"
      ) |>
      req_body_json(data = x |>
                      select(-group_id) |>
                      as.list() |>
                      list_transpose(simplify = FALSE)
                    ) |>
      req_method(method = "POST") |>
      req_retry(max_seconds = 60, retry_on_failure = TRUE) |>
      # req_dry_run()
      req_perform()
  })
