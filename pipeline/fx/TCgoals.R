TCGoals <- function(aims) {
  db[["TruthCommissions_tcAims"]] %>% # Goals (binaries)
    left_join(db[["labels"]], by = "labelID") %>%
    select(-labelID) %>%
    # mutate(
    #   new = 1,
    #   label = str_replace_all(label, fixed(" "), "_")
    # ) %>%
    #   pivot_wider(names_from = label, values_from = new) %>%
    #   mutate(across(!truthcommissionID, ~ ifelse(is.na(.x), 0, .x)))
    filter(label %in% aims) %>%
    select(truthcommissionID) %>%
    arrange(truthcommissionID) %>%
    distinct() %>%
    unlist(use.names = FALSE)
}
