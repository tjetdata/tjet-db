### packages
require(tidyverse)
require(deeplr)
require(keyring)

### should build in more data integrity checks and prevent accidental 
### overwriting of translations that already exist in Airtable

### translation code is wrapped in a conditional statements because it's costly
translations <- list(
  overwrite = FALSE, 
  country_profiles = FALSE, 
  country_auto = FALSE, 
  conflicts = FALSE,
  tjet_bios = FALSE, 
  amnesties = FALSE,
  reparations = FALSE, 
  tcs = FALSE, 
  trials = FALSE,
  accused = FALSE,
  vetting = FALSE,
  investigations = FALSE,
  surveysmeta = FALSE,
  surveys = FALSE, 
  labels = FALSE, 
  dl_codebook = FALSE)

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
# translated <- toFrench2(text = txt, source_lang = "EN",
#                         auth_key = key_get("DeepL"))
# back_trans <- toEnglish2(text = translated, source_lang = "FR",
#                          auth_key = key_get("DeepL"))
# split_text(txt, max_size_bytes = 29000, tokenize = "sentences")
# pimp(txt, source_lang = "EN", help_lang = "FR", auth_key = key_get("DeepL"))
# usage(key_get("DeepL"))

### fx
translate <- function(col) {
  ifelse(is.na(col), "", 
         toFrench(text = col,
                  source_lang = "EN", 
                  auth_key = key_get("DeepL")))
}
usage(key_get("DeepL"))

### translation of relevant fields in tables

if(translations$country_profiles) { # about 10 min, 812,000 characters
  ## do not translate the country field; this is used in the website structure
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  order <- names(db[["Countries"]])
  db[["Countries_fr"]] <- db[["Countries"]] %>% 
    rowwise() %>% ### deeplr does not handle NAs well; this seems to be a simple work-around
    mutate(txt_intro = translate(txt_intro), 
           txt_regime = translate(txt_regime), 
           txt_conflict = translate(txt_conflict), 
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
  cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
}
db[["Countries_fr"]] %>%
  select(country, txt_intro, txt_regime, txt_conflict, txt_TJ)

if(translations$country_auto) { # about 2.5 min, 54,000 characters
  ## do not translate the country field; this is used in the website structure
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  db[["auto_fr"]] <- db[["Countries"]] %>% 
    select(country, country_case, ccode, ccode_case, beg, end, auto_regime, auto_conflict) %>%
    rowwise() %>% ### deeplr does not handle NAs well; this seems to be a simple work-around
    mutate(auto_regime = translate(auto_regime), 
           auto_conflict = translate(auto_conflict)
           ) %>%
    ungroup()
  print(Sys.time() - start)
  cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
}
db[["auto_fr"]] %>%
  select(country, auto_regime, auto_conflict)

if(translations$labels) { # about 0.5 min / 5000 characters 
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  db[["labels_fr"]] <- db[["labels"]] %>% 
    rowwise() %>%
    mutate(label = translate(label)) %>%
    ungroup()
  print(Sys.time() - start)
  cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
}
db[["labels_fr"]]

if(translations$dl_codebook) { # about 1.5 min / 26000 characters 
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  db[["dl_tjet_codebook_fr"]] <- db[["dl_tjet_codebook"]] %>% 
    rowwise() %>%
    mutate(definition = translate(definition)) %>%
    ungroup()
  print(Sys.time() - start)
  cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
}
db[["dl_tjet_codebook_fr"]]

if(translations$conflicts) { # about 3 min / 2000 characters 
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  db[["ConflictDyads_fr"]] <- db[["ConflictDyads"]] %>% 
    select(dyad_id, conflict_id, gwno_loc, side_b, ep_start_date) %>% 
    rowwise() %>%
    mutate(side_b = translate(side_b)) %>%
    ungroup()
  print(Sys.time() - start)
  cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
}
db[["ConflictDyads_fr"]] 

# if(translations$tjet_bios) {
#   usage_last <- usage(key_get("DeepL"))[["character_count"]]
#   start <- Sys.time()
#   order <- names(db[["TJETmembers"]])
#   db[["TJETmembers_fr"]] <- db[["TJETmembers"]] %>% 
#     rowwise() %>%
#     mutate(bio_text = translate(bio_text)) %>%
#     ungroup() %>%
#     select(all_of(order))
#   print(Sys.time() - start)
#   cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
# }
# db[["TJETmembers_fr"]] %>%
#   select(last_name, given_name, bio_text)

if(translations$amnesties) { # about 5 min / 124,000 characters  
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  db[["Amnesties_fr"]] <- db[["Amnesties"]] %>% 
    select(amnestyID, mechanismDescription, whoWasAmnestied) %>%
    rowwise() %>%
    mutate(mechanismDescription = translate(mechanismDescription)) %>%
    ungroup()
  print(Sys.time() - start)
  cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
}
db[["Amnesties_fr"]] 

if(translations$reparations) { # about 0.5 min  / 8000 characters  
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  db[["Reparations_fr"]] <- db[["Reparations"]] %>% 
    select(reparationID, officialName_en) %>%
    rowwise() %>%
    mutate(officialName_en = translate(officialName_en)) %>%
    ungroup()
  print(Sys.time() - start)
  cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
}
db[["Reparations_fr"]]

if(translations$tcs) { # about 0.5 min / 8000 characters  
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  db[["TruthCommissions_fr"]] <- db[["TruthCommissions"]] %>% 
    select(truthcommissionID, officialName_en) %>%
    rowwise() %>%
    mutate(officialName_en = translate(officialName_en)) %>%
    ungroup()
  print(Sys.time() - start)
  cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
}
db[["TruthCommissions_fr"]]

if(translations$trials) { # about 30 min / 646,000 characters  
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  db[["Trials_fr"]] <- db[["Trials"]] %>% 
    select(trialID, caseDescription) %>%
    rowwise() %>%
    mutate(caseDescription = translate(caseDescription)) %>%
    ungroup()
  print(Sys.time() - start)
  cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
}
db[["Trials_fr"]]

if(translations$accused) { # about 56 min / 471,000 characters 
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  if(translations$overwrite) {
    db[["Accused_fr"]] <- db[["Accused"]] %>% 
      select(accusedID, nameOrDesc) %>%
      rowwise() %>%
      mutate(nameOrDesc = translate(nameOrDesc)) %>%
      ungroup()
  } else {
    db[["Accused_fr"]] <- db[["Accused"]] %>% 
      select(accusedID, nameOrDesc) %>% 
      rename("nameOrDesc_en" = "nameOrDesc") %>% 
      left_join(fr[["Accused_fr"]] %>% 
                  select(accusedID, nameOrDesc), 
                by = "accusedID") %>% 
      rowwise() %>%
      mutate(nameOrDesc = ifelse(is.na(nameOrDesc), 
                                 translate(nameOrDesc_en), 
                                 nameOrDesc)) %>%
      ungroup() %>% 
      select(-nameOrDesc_en)
  }
  print(Sys.time() - start)
  cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
}
db[["Accused_fr"]]

if(translations$vetting) { # about 0.5 min / 8,000 characters  
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  db[["Vettings_fr"]] <- db[["Vettings"]] %>% 
    select(vettingID, policyName) %>%
    rowwise() %>%
    mutate(policyName = translate(policyName)) %>%
    ungroup()
  print(Sys.time() - start)
  cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
}
db[["Vettings_fr"]]

if(translations$investigations) { # about 0.5 min / 4000 characters  
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  db[["Investigations_fr"]] <- db[["Investigations"]] %>% 
    select(ccode_cow, beg, end, mandate) %>%
    rowwise() %>%
    mutate(mandate_en = mandate, 
           mandate = translate(mandate)) %>%
    ungroup()
  print(Sys.time() - start)
  cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
}
db[["Investigations_fr"]]

if(translations$surveysmeta) { # about 0.5 min / 12,000 characters  
  usage_last <- usage(key_get("DeepL"))[["character_count"]]
  start <- Sys.time()
  order <- names(db[["SurveysMeta"]])
  db[["SurveysMeta_fr"]] <- db[["SurveysMeta"]] %>%
    rowwise() %>%
    mutate(section_title = translate(section_title),
           text_context = translate(text_context),
           text_results = translate(text_results),
           text_methods = translate(text_methods), 
           survey_design = translate(survey_design)
           ) %>%
    ungroup() %>%
    select(all_of(order))
  print(Sys.time() - start)
  cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
}
db[["SurveysMeta_fr"]]

surveytabs <- db[["SurveysMeta"]] %>% 
  select(results_tables) %>% 
  unlist(use.names = FALSE) %>% 
  str_replace(fixed(".xlsx"), "")

if(translations$surveys) { # about 11 min / 67,000 characters
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
  cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
}

# if(translations$focus) { 
#   usage_last <- usage(key_get("DeepL"))[["character_count"]]
#   start <- Sys.time()
#   read_file("focus/SriLanka.qmd") %>% 
#     translate() %>% 
#     write_file("focus/SriLanka-fr.qmd")
#   print(Sys.time() - start)
#   cat("Characters:", usage(key_get("DeepL"))[["character_count"]] - usage_last, "\n")
# }

### saving locally 
str(db[sort(names(db))], 1)
save(db, file = here::here("data", "tjetdb.RData"))

to_save <- c("Countries", "ConflictDyads", "dl_tjet_codebook", "Amnesties", 
             "Reparations", "TruthCommissions", "Trials", "Accused", "Vettings", 
             "SurveysMeta", surveytabs, "labels", "TJETmembers", "auto") 
tabs <- paste(to_save, "_fr", sep = "")
tabs <- tabs[tabs %in% names(db)]
db[tabs] %>% 
  saveRDS(file = here::here("data", "tjetdb_fr.rds"))

### NEED TO DELETE TABLES FROM THIS OBJECT THAT ARE NO LONGER NEEDED WITH THE NEW DB SETUP
### COULD ALSO INTEGRATE THE REMAINING FR TABLES IN THE SAME WAY

# read_csv("~/Desktop/temp.csv") %>% 
#   rowwise() %>%
#   mutate(domestic_fr = translate(domestic)) %>%
#   ungroup() %>% 
#   write_csv("~/Desktop/new.csv")
