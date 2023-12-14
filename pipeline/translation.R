### packages
require(tidyverse)
require(deeplr)
require(keyring)

### translation code is wrapped in a conditional statements because it's costly
translate <- list(
  country_profiles = FALSE, 
  conflicts = FALSE,
  tjet_bios = FALSE, 
  amnesties = FALSE,
  reparations = FALSE, 
  tcs = FALSE, 
  trials = FALSE,
  accused = FALSE,
  vetting = FALSE,
  surveysmeta = FALSE,
  surveys = FALSE, 
  labels = FALSE)

### setting the authorization key locally (do only once for each new key)
# keyring::key_set(service = "DeepL") 

### loading & checking our database
load(here::here("data", "tjetdb.RData"), verbose = TRUE)
fr <- readRDS(file = here::here("data", "tjetdb_fr.rds"))
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

if(translate$country_profiles) { # about 5 min, 425,000 characters
  ## do not translate the country field; this is used in the website structure
  start <- Sys.time()
  order <- names(db[["Countries"]])
  db[["Countries_fr"]] <- db[["Countries"]] %>% 
    rowwise() %>% ### deeplr does not handle NAs well; this seems to be a simple work-around
    mutate(txt_intro = translate(txt_intro), 
           txt_regime = translate(txt_regime), 
           txt_conflict = translate(txt_conflict), 
           txt_TJ = translate(txt_TJ)) %>%
    ungroup() %>%
    select(all_of(order))
  Sys.time() - start
  usage(key_get("DeepL"))
}
db[["Countries_fr"]] %>%
  select(country, txt_intro, txt_regime, txt_conflict, txt_TJ)

if(translate$labels) { # about 0.5 min / 5000 characters 
  start <- Sys.time()
  db[["labels_fr"]] <- db[["labels"]] %>% 
    rowwise() %>%
    mutate(label = translate(label)) %>%
    ungroup()
  Sys.time() - start
  usage(key_get("DeepL"))
}
db[["labels_fr"]]

if(translate$conflicts) { # about 3 min / 2000 characters 
  start <- Sys.time()
  db[["ConflictDyads_fr"]] <- db[["ConflictDyads"]] %>% 
    select(dyad_id, conflict_id, side_b) %>% 
    rowwise() %>%
    mutate(side_b = translate(side_b)) %>%
    ungroup()
  Sys.time() - start
  usage(key_get("DeepL"))
}
db[["ConflictDyads_fr"]]

if(translate$tjet_bios) {
  start <- Sys.time()
  order <- names(db[["TJETmembers"]])
  db[["TJETmembers_fr"]] <- db[["TJETmembers"]] %>% 
    rowwise() %>%
    mutate(bio_text = translate(bio_text)) %>%
    ungroup() %>%
    select(all_of(order))
  Sys.time() - start
  usage(key_get("DeepL"))
}
db[["TJETmembers_fr"]] %>%
  select(last_name, given_name, bio_text)

if(translate$amnesties) { # about 8.5 min / 170,000 characters  
  start <- Sys.time()
  db[["Amnesties_fr"]] <- db[["Amnesties"]] %>% 
    select(amnestyID, mechanismDescription, whoWasAmnestied) %>%
    rowwise() %>%
    mutate(mechanismDescription = translate(mechanismDescription)) %>%
    ungroup()
  Sys.time() - start
  usage(key_get("DeepL"))
}
db[["Amnesties_fr"]] 

if(translate$reparations) { # about 0.5 min  / 9000 characters  
  start <- Sys.time()
  db[["Reparations_fr"]] <- db[["Reparations"]] %>% 
    select(reparationID, officialName_en) %>%
    rowwise() %>%
    mutate(officialName_en = translate(officialName_en)) %>%
    ungroup()
  Sys.time() - start
  usage(key_get("DeepL"))
}
db[["Reparations_fr"]]

if(translate$tcs) { # about 0.5 min / 12,000 characters  
  start <- Sys.time()
  db[["TruthCommissions_fr"]] <- db[["TruthCommissions"]] %>% 
    select(truthcommissionID, officialName_en) %>%
    rowwise() %>%
    mutate(officialName_en = translate(officialName_en)) %>%
    ungroup()
  Sys.time() - start
  usage(key_get("DeepL"))
}
db[["TruthCommissions_fr"]]

if(translate$trials) { # about 29 min / 650,000 characters  
  start <- Sys.time()
  db[["Trials_fr"]] <- db[["Trials"]] %>% 
    select(trialID, caseDescription) %>%
    rowwise() %>%
    mutate(caseDescription = translate(caseDescription)) %>%
    ungroup()
  Sys.time() - start
  usage(key_get("DeepL"))
}
db[["Trials_fr"]]

if(translate$accused) { # about 56 min / 470,000 characters 
  start <- Sys.time()
  db[["Accused_fr"]] <- db[["Accused"]] %>% 
    select(accusedID, nameOrDesc) %>%
    rowwise() %>%
    mutate(nameOrDesc = translate(nameOrDesc)) %>%
    ungroup()
  Sys.time() - start
  usage(key_get("DeepL"))
}
db[["Accused_fr"]]

if(translate$vetting) { # about 0.5 min / 9,000 characters  
  start <- Sys.time()
  db[["Vettings_fr"]] <- db[["Vettings"]] %>% 
    select(vettingID, policyName) %>%
    rowwise() %>%
    mutate(policyName = translate(policyName)) %>%
    ungroup()
  Sys.time() - start
  usage(key_get("DeepL"))
}
db[["Vettings_fr"]]

if(translate$surveysmeta) { # about 0.5 min / 12,000 characters  
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
  Sys.time() - start
  usage(key_get("DeepL"))
}
db[["SurveysMeta_fr"]]

surveytabs <- db[["SurveysMeta"]] %>% 
  select(results_tables) %>% 
  unlist(use.names = FALSE) %>% 
  str_replace(fixed(".xlsx"), "")

if(translate$surveys) { # about 14 min / 67,000 characters  
  start <- Sys.time()
  db[paste(surveytabs, "_fr", sep = "")] <- surveytabs %>% 
    map(function(tab) {
      df <- db[[tab]]
      order <- names(df)
      headers <- translate(names(df))
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
  Sys.time() - start
  usage(key_get("DeepL"))
}

### saving locally 
save(db, file = here::here("data", "tjetdb.RData"))

to_save <- c("Countries", "Amnesties", "Reparations", "TruthCommissions", 
             "Trials", "Accused", "Vettings", "SurveysMeta", surveytabs, 
             "labels", "TJETmembers") 
tabs <- paste(to_save, "_fr", sep = "")
tabs <- tabs[tabs %in% names(db)]
db[tabs] %>% 
  saveRDS(file = here::here("data", "tjetdb_fr.rds"))
