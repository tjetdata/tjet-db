### packages
require(tidyverse)
require(deeplr)
require(keyring)

### use this for setting the authorization key locally
### needs to be done only once for each new key
# keyring::key_set(service = "DeepL") 

### sample code for using the Deepl API
### no longer needed but useful as reference
# txt <- c("This sentence was brought to you from 
#          English to French back to English.",
#          "Transitional justice consists of polices 
#          to deal with past human rights abuses.")
# translated <- toFrench2(text = txt, 
#                         source_lang = "EN",
#                         auth_key = key_get("DeepL"))
# back_trans <- toEnglish2(text = translated, 
#                          source_lang = "FR",
#                          auth_key = key_get("DeepL"))
# split_text(txt, max_size_bytes = 29000, tokenize = "sentences")
# pimp2(txt, source_lang = "EN", help_lang = "FR", auth_key = key_get("DeepL"))
# usage2(key_get("DeepL"))

### loading & checking our database
load(here::here("data", "tjetdb.RData"), verbose = TRUE)
# str(db, 1)

### this sample code translates the Countries table
### for now, it translates each table column individually 
### but this can be done much more compactly if needed
### the code is wrapped in a conditional statement because it's costly
go_ahead <- FALSE
translate <- function(col) {
  toFrench2(
    text = col,
    source_lang = "EN", 
    auth_key = key_get("DeepL"))
  
}
if(go_ahead) {
  ## do not translate the country field; this is used in the website structure
  start <- Sys.time()
  order <- names(db[["Countries"]])
  db[["fr_Countries"]] <- db[["Countries"]] %>% 
    rowwise() %>% ### deeplr does not handle NAs well; this seems to be a work-around
    mutate(txt_intro = ifelse(is.na(txt_intro), "", translate(txt_intro)), 
           txt_regime = ifelse(is.na(txt_regime), "", translate(txt_regime)), 
           txt_conflict = ifelse(is.na(txt_conflict), "", translate(txt_conflict)), 
           txt_TJ = ifelse(is.na(txt_TJ), "", translate(txt_TJ))) %>%
    ungroup() %>%
    select(all_of(order))
  Sys.time() - start
}

### saving locally 
save(db, file = here::here("data", "tjetdb.RData"))
