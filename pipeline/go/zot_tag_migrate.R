### this script was used for the initial migration from the extra field to tags
### DO NOT RUN AGAIN!

source(here::here("pipeline/go/zot_setup.R"))

### getting all items

req <- request(paste(base, "items?format=json", sep = "") ) |>
  req_headers(
    'Zotero-API-Key' = zot_key,
    'Zotero-API-Version' = 3,
    Accept = "application/json"
  )

# req |>
#   req_dry_run()
# resp <- req |>
#   req_perform()
# resp |>
#   resp_headers()
# resp |>
#   resp_header("link")
# resp |>
#   resp_link_url(rel = "next")
# resp |>
#   resp_raw()
# resp |>
#   resp_body_json()

resps <- req |>
  req_retry(max_seconds = 60, retry_on_failure = TRUE) |>
  req_perform_iterative(next_req = iterate_with_link_url(rel = "next"),
                        max_reqs = Inf, on_error = "return")
saveRDS(resps, "items.rds")

### cleaning items

new2 <- map(new, \(x) { x[["data"]]} )
new2 <- new2[map(new2, \(x) {"extra" %in% names(x)} ) |> unlist()]
new2 <- new2[map(new2, \(x) { x[["extra"]] != "" } ) |> unlist()]
new2 <- map(new2, \(x) {
  tibble(key = x[["key"]],
         version = x[["version"]],
         # collections = list(x[["collections"]]),
         extra = list(str_split_1(x[["extra"]], "\n")),
         tags = list(x[["tags"]]))
} ) |>
  bind_rows() |>
  rowwise() |>
  mutate(extra = list(extra),
         extra = case_when(
           str_detect(extra, "tex.ids= ") ~ NA,
           str_detect(extra, "googlebooksid: ") ~ NA,
           str_detect(str_to_lower(extra), "section: ") ~ NA,
           str_detect(extra, "DOI: ") ~ NA,
           str_detect(str_to_lower(extra), "publisher: ") ~ NA,
           str_detect(extra, "Published: ") ~ NA,
           str_detect(extra, "OCLC: ") ~ NA,
           str_detect(extra, "PMID: ") ~ NA,
           str_detect(extra, "Page Version ID: ") ~ NA,
           str_detect(extra, "Place: ") ~ NA,
           str_detect(extra, "Meeting Name: ") ~ NA,
           str_detect(extra, "Num Pages: ") ~ NA,
           str_detect(extra, "Number: ") ~ NA,
           str_detect(extra, "Google-Books-ID: ") ~ NA,
           str_detect(extra, "Library Catalog: ") ~ NA,
           str_detect(extra, "_eprint: ") ~ NA,
           str_detect(extra, "http://") ~ NA,
           str_detect(str_to_lower(extra), "lois") ~ NA,
           extra == "trialID:" ~ NA,
           TRUE ~ extra
         ) |>
           list(),
         extra = extra[!is.na(extra)] |>
           list(),
         extra = list(str_squish(extra)),
         extra = list(str_to_lower(extra)),
         extra = list(str_replace(extra, "id: ", "ID: ")),
         extra = list(str_replace(extra, ": ", " ")),
         extra = list(unique(extra)),
         newtags = list(c(tags, map(extra, \(x) { list(tag = x)}))),
         newtags = list(unique(newtags))
  ) |>
  filter(length(extra) > 0) |>
  ungroup() |>
  # select(key, tags, extra, newtags)
  select(key, version, newtags)

# new2[3, "newtags"] |> unlist()

# new2 |>
#   rowwise() |>
#   mutate(newtags = list(unlist(newtags)) ) |>
#   select(newtags) |>
#   unnest(newtags) |>
#   distinct() |>
#   arrange(newtags) |>
#   write_csv("~/Desktop/temp.csv")

### uploading edits
## template
# GET /items/new?itemType=book

# new2[[3, "newtags"]] |>
#   toJSON() |>
#   write_lines(file = "~/Desktop/temp.txt")

### sample

# to_upload <- new2 |>
#   filter(key == "K25RISQE") |>
#   select(newtags) |>
#   rename(tags = newtags) |>
#   unlist(recursive = FALSE)

### for all

# new2 |>
#   rename(tags = newtags) |>
#   transpose() |>
#   map(\(x) {
#     resp <- request(paste(base, "items/", x[["key"]], sep = "")) |>
#       req_headers(
#         'Zotero-API-Key' = zot_key,
#         'Zotero-API-Version' = 3,
#         'If-Unmodified-Since-Version' = x[["version"]]
#       ) |>
#       req_body_json(data = x["tags"]) |>
#       req_method(method = "PATCH") |>
#       req_retry(max_seconds = 60, retry_on_failure = TRUE) |>
#       req_perform()
#     # req_dry_run()
#     ### include response handling here
#     print(x[["key"]])
#   })
