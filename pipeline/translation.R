### NEED TO CLEAN THIS UP:
### no longer a regularly run script but for ad hoc bulk translating, so not part of the pipeline anymore

### translation code is wrapped in conditional statements because it is costly
translations <- list(
  conflicts = FALSE,
  country_profiles = FALSE,
  dl_codebook = FALSE,
  labels = FALSE,
  surveysmeta = FALSE,
  surveys = FALSE
)

### packages
require(tidyverse)
require(deeplr)
require(keyring)

### build in more data integrity checks and prevent accidental
### overwriting of translations that already exist in Airtable

### setting the authorization key locally (do only once for each new key)
# keyring::key_set(service = "DeepL")

### loading & checking our database
load(here::here("data", "tjetdb.RData"), verbose = TRUE)
str(db[sort(names(db))], 1)
fr <- readRDS(file = here::here("data", "tjetdb_fr.rds"))
str(fr[sort(names(fr))], 1)
db[names(fr)] <- fr
str(db[sort(names(db))], 1)

### sample code for using the Deepl API (useful as reference)
# txt <- c("This sentence was brought to you from English to French back to English.",
#          "Transitional justice consists of polices to deal with past human rights abuses.")
# translated <- toFrench2(text = txt, source_lang = "EN", auth_key = key_get("DeepL"))
# back_trans <- toEnglish2(text = translated, source_lang = "FR", auth_key = key_get("DeepL"))
# split_text(txt, max_size_bytes = 29000, tokenize = "sentences")
# pimp(txt, source_lang = "EN", help_lang = "FR", auth_key = key_get("DeepL"))
# usage(key_get("DeepL"))

### fx
translate <- function(col) {
  ifelse(
    is.na(col),
    "",
    toFrench(
      text = col,
      source_lang = "EN",
      auth_key = key_get("DeepL")
    )
  )
  # ifelse(is.na(col), "",
  #   toEnglish(
  #     text = col,
  #     source_lang = "ES",
  #     auth_key = key_get("DeepL")
  #   )
  # )
}
usage(key_get("DeepL"))

### translation of relevant fields in tables

if (translations$conflicts) {
  # about 3 min / 2000 characters
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  db[["ConflictDyads_fr"]] <- db[["ConflictDyads"]] %>%
    select(dyad_id, conflict_id, gwno_loc, side_b, ep_start_date) %>%
    rowwise() %>%
    mutate(side_b = translate(side_b)) %>%
    ungroup()
  print(Sys.time() - start)
  cat(
    "Characters:",
    usage(key_get("DeepL"))[["character_count"]] - usage_last,
    "\n"
  )
  db[["ConflictDyads_fr"]]
}

if (translations$country_profiles) {
  # about 10 min, 812,000 characters
  ## do not translate the country field; this is used in the website structure
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  order <- names(db[["Countries"]])
  db[["Countries_fr"]] <- db[["Countries"]] %>%
    rowwise() %>% ### deeplr does not handle NAs well; this seems to be a simple work-around
    mutate(
      txt_intro = translate(txt_intro),
      auto_regime = translate(auto_regime),
      txt_regime = translate(txt_regime),
      txt_conflict = translate(txt_conflict),
      auto_conflict = translate(auto_conflict),
      txt_TJ = translate(txt_TJ),
      txt_summary = translate(txt_summary),
      txt_amnesties = translate(txt_amnesties),
      txt_domestic = translate(txt_domestic),
      txt_intl = translate(txt_intl),
      txt_foreign = translate(txt_foreign),
      txt_reparations = translate(txt_reparations),
      txt_tcs = translate(txt_tcs),
      txt_vetting = translate(txt_vetting),
      txt_un = translate(txt_un)
    ) %>%
    ungroup() %>%
    select(all_of(order))
  print(Sys.time() - start)
  cat(
    "Characters:",
    usage(key_get("DeepL"))[["character_count"]] - usage_last,
    "\n"
  )
  db[["Countries_fr"]] %>%
    select(country, txt_intro, txt_regime, txt_conflict, txt_TJ)
}

if (translations$dl_codebook) {
  # about 1.5 min / 26000 characters
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  db[["dl_tjet_codebook_fr"]] <- db[["dl_tjet_codebook"]] %>%
    rowwise() %>%
    mutate(definition = translate(definition)) %>%
    ungroup()
  print(Sys.time() - start)
  cat(
    "Characters:",
    usage(key_get("DeepL"))[["character_count"]] - usage_last,
    "\n"
  )
  db[["dl_tjet_codebook_fr"]]
}

if (translations$labels) {
  # about 0.5 min / 5000 characters
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  db[["labels_fr"]] <- db[["labels"]] %>%
    rowwise() %>%
    mutate(label = translate(label)) %>%
    ungroup()
  print(Sys.time() - start)
  cat(
    "Characters:",
    usage(key_get("DeepL"))[["character_count"]] - usage_last,
    "\n"
  )
  db[["labels_fr"]]
}

if (translations$surveysmeta) {
  # about 0.5 min / 12,000 characters
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  order <- names(db[["SurveysMeta"]])
  db[["SurveysMeta_fr"]] <- db[["SurveysMeta"]] %>%
    rowwise() %>%
    mutate(
      section_title = translate(section_title),
      text_context = translate(text_context),
      text_results = translate(text_results),
      text_methods = translate(text_methods),
      survey_design = translate(survey_design)
    ) %>%
    ungroup() %>%
    select(all_of(order))
  print(Sys.time() - start)
  cat(
    "Characters:",
    usage(key_get("DeepL"))[["character_count"]] - usage_last,
    "\n"
  )
  db[["SurveysMeta_fr"]]
}

surveytabs <- db[["SurveysMeta"]] %>%
  select(results_tables) %>%
  unlist(use.names = FALSE) %>%
  str_replace(fixed(".xlsx"), "")

if (translations$surveys) {
  # about 11 min / 67,000 characters
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  db[paste(surveytabs, "_fr", sep = "")] <- surveytabs %>%
    map(function(tab) {
      df <- db[[tab]]
      order <- names(df)
      headers <- translate(names(df)) %>%
        str_replace(fixed("Les femmes"), "Femmes") %>%
        str_replace(fixed("Les hommes"), "Hommes")
      tooltips <- translate(df[1, ])
      df <- rbind(headers, tooltips, df) %>% tibble()
      sec <- df %>%
        select(Section) %>%
        distinct() %>%
        mutate(Section_fr = ifelse(is.na(Section), "", translate(Section)))
      que <- df %>%
        select(Question) %>%
        distinct() %>%
        mutate(Question_fr = ifelse(is.na(Question), "", translate(Question)))
      df %>%
        rowwise() %>%
        mutate(Responses_fr = translate(Responses)) %>%
        ungroup() %>%
        left_join(sec, by = "Section") %>%
        left_join(que, by = "Question") %>%
        select(all_of(order), Section_fr, Question_fr, Responses_fr)
    })
  print(Sys.time() - start)
  cat(
    "Characters:",
    usage(key_get("DeepL"))[["character_count"]] - usage_last,
    "\n"
  )
}

### saving locally
str(db[sort(names(db))], 1)
save(db, file = here::here("data", "tjetdb.RData"))
to_save <- c(
  "Countries",
  "ConflictDyads",
  "dl_tjet_codebook",
  "Amnesties",
  "Reparations",
  "TruthCommissions",
  "Trials",
  "Accused",
  "Vettings",
  "SurveysMeta",
  surveytabs,
  "labels",
  "TJETmembers"
)
tabs <- paste(to_save, "_fr", sep = "")
tabs <- tabs[tabs %in% names(db)]
db[tabs] %>%
  saveRDS(file = here::here("data", "tjetdb_fr.rds"))


read_csv("~/Documents/GitHub/tjet-db/auto_text_update/new_confl.csv") |>
  rowwise() |>
  mutate(
    conflict_fr = translate(conflict),
  ) %>%
  ungroup() |>
  write_csv("~/Desktop/new_confl.csv", na = "")

read_csv("~/Documents/GitHub/tjet-db/auto_text_update/new_domestic.csv") |>
  rowwise() |>
  mutate(
    domestic_fr = translate(domestic),
  ) %>%
  ungroup() |>
  write_csv("~/Desktop/new_domestic.csv", na = "")

read_csv("~/Documents/GitHub/tjet-db/auto_text_update/new_intl.csv") |>
  rowwise() |>
  mutate(
    intl_fr = translate(intl),
  ) %>%
  ungroup() |>
  write_csv("~/Desktop/new_intl.csv", na = "")

read_csv("~/Documents/GitHub/tjet-db/auto_text_update/new_regime.csv") |>
  rowwise() |>
  mutate(
    regime_fr = translate(regime),
  ) %>%
  ungroup() |>
  write_csv("~/Desktop/new_regime.csv", na = "")

read_csv("~/Documents/GitHub/tjet-db/auto_text_update/new_summary.csv") |>
  rowwise() |>
  mutate(
    summary_fr = translate(summary),
  ) %>%
  ungroup() |>
  write_csv("~/Desktop/new_summary.csv", na = "")


read_csv("~/Desktop/Trials-for-translation.csv") |>
  rowwise() |>
  mutate(
    caseDescription_fr = translate(caseDescription),
  ) %>%
  ungroup() |>
  rename(caseDescription_translated = caseDescription) |>
  write_csv("~/Desktop/Trials.csv", na = "")

usage(key_get("DeepL"))

read_csv("~/Desktop/Individuals-translations_to_revise.csv") |>
  select(individualID, description) |>
  reframe(
    .by = description,
    individualID = list(individualID)
  ) |>
  rowwise() |>
  mutate(
    description_fr = translate(description),
  ) %>%
  ungroup() |>
  unnest(individualID) |>
  rename(description_translated = description) |>
  write_csv("~/Desktop/Individuals-new.csv", na = "")


### for translating focus country reports
# usage(key_get("DeepL"))
# base <- "~/Documents/GitHub/tjetFI/quarto-site-generator/_focus"
# base %>%
#   dir()
# here::here(base, "CotedIvoire.qmd") %>%
#   read_file() %>%
#   translate() %>%
#   write_file(file = here::here(base, "CotedIvoire-fr.qmd"))

read_csv("~/Desktop/temp.csv") |>
  rowwise() |>
  mutate(
    summary_fr = translate(summary),
    amnesties_fr = translate(amnesties),
    domestic_fr = translate(domestic),
    intl_fr = translate(intl),
    foreign_fr = translate(foreign),
    reparations_fr = translate(reparations),
    tcs_fr = translate(tcs),
    vetting_fr = translate(vetting)
  ) %>%
  ungroup() |>
  mutate(
    summary_fr = str_replace(summary_fr, ", la TJET ", ", TJET "),
    summary_fr = str_replace(summary_fr, ", le TJET ", ", TJET "),
    domestic_fr = str_replace(domestic_fr, "La TJET ", "TJET "),
  ) |>
  write_csv("~/Desktop/new.csv", na = "")


"auto_text_update/new_confl.csv" |>
  read_csv() |>
  rowwise() |>
  mutate(
    conflict_fr = translate(conflict)
  ) %>%
  ungroup() |>
  mutate(
    conflict_fr = str_replace(conflict_fr, ", la TJET ", ", TJET "),
    conflict_fr = str_replace(conflict_fr, ", le TJET ", ", TJET ")
  ) |>
  write_csv("auto_text_update/new_confl.csv", na = "")

"auto_text_update/new_summary.csv" |>
  read_csv() |>
  rowwise() |>
  mutate(
    summary_fr = translate(summary)
  ) %>%
  ungroup() |>
  mutate(
    summary_fr = str_replace(summary_fr, ", la TJET ", ", TJET "),
    summary_fr = str_replace(summary_fr, ", le TJET ", ", TJET ")
  ) |>
  write_csv("auto_text_update/new_summary.csv", na = "")

"auto_text_update/new_domestic.csv" |>
  read_csv() |>
  rowwise() |>
  mutate(
    domestic_fr = translate(domestic)
  ) %>%
  ungroup() |>
  mutate(
    domestic_fr = str_replace(domestic_fr, ", la TJET ", ", TJET "),
    domestic_fr = str_replace(domestic_fr, ", le TJET ", ", TJET "),
    domestic_fr = str_replace(domestic_fr, "La TJET ", "TJET "),
    domestic_fr = str_replace(domestic_fr, "Le TJET ", "TJET ")
  ) |>
  write_csv("auto_text_update/new_domestic.csv", na = "")

"auto_text_update/new_foreign.csv" |>
  read_csv() |>
  rowwise() |>
  mutate(
    foreign_fr = translate(foreign)
  ) %>%
  ungroup() |>
  write_csv("auto_text_update/new_foreign.csv", na = "")

"auto_text_update/new_intl.csv" |>
  read_csv() |>
  rowwise() |>
  mutate(
    intl_fr = translate(intl)
  ) %>%
  ungroup() |>
  write_csv("auto_text_update/new_intl.csv", na = "")

"auto_text_update/new_reparations.csv" |>
  read_csv() |>
  rowwise() |>
  mutate(
    reparations_fr = translate(reparations)
  ) %>%
  ungroup() |>
  mutate(
    reparations_fr = str_replace(reparations_fr, ", la TJET ", ", TJET "),
    reparations_fr = str_replace(reparations_fr, ", le TJET ", ", TJET "),
    reparations_fr = str_replace(reparations_fr, "La TJET ", "TJET "),
    reparations_fr = str_replace(reparations_fr, "Le TJET ", "TJET ")
  ) |>
  write_csv("auto_text_update/new_reparations.csv", na = "")

"auto_text_update/new_tcs.csv" |>
  read_csv() |>
  rowwise() |>
  mutate(
    tcs_fr = translate(tcs)
  ) %>%
  ungroup() |>
  mutate(
    tcs_fr = str_replace(tcs_fr, "; la TJET ", "; TJET "),
    tcs_fr = str_replace(tcs_fr, "; le TJET ", "; TJET "),
  ) |>
  write_csv("auto_text_update/new_tcs.csv", na = "")

"auto_text_update/new_vetting.csv" |>
  read_csv() |>
  rowwise() |>
  mutate(
    vetting_fr = translate(vetting)
  ) %>%
  ungroup() |>
  mutate(
    vetting_fr = str_replace(vetting_fr, "; la TJET ", "; TJET "),
    vetting_fr = str_replace(vetting_fr, "; le TJET ", "; TJET "),
    vetting_fr = str_replace(
      vetting_fr,
      "politique de vérification des antécédents",
      "politique de filtrage"
    ),
    vetting_fr = str_replace(
      vetting_fr,
      "politiques de vérification des antécédents",
      "politiques de filtrage"
    ),
    vetting_fr = str_replace(
      vetting_fr,
      "politique de contrôle",
      "politique de filtrage"
    ),
    vetting_fr = str_replace(
      vetting_fr,
      "politiques de contrôle",
      "politiques de filtrage"
    ),
  ) |>
  write_csv("auto_text_update/new_vetting.csv", na = "")

"auto_text_update/new_un.csv" |>
  read_csv() |>
  rowwise() |>
  mutate(
    un_fr = translate(un)
  ) %>%
  ungroup() |>
  write_csv("auto_text_update/new_un.csv", na = "")
