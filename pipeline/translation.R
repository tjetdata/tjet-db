require(keyring)
require(deeplr)

### use this for setting the authorization key locally
### needs to be done only once for each new key
# keyring::key_set(service = "DeepL") 

txt <- c("This sentence was brought to you from English to French back to English.", 
         "Transitional justice consists of polices to deal with past human rights abuses.")

# translated <- toFrench2(text = txt, 
#                         source_lang = "EN",
#                         auth_key = key_get("DeepL"))
# back_trans <- toEnglish2(text = translated, 
#                          source_lang = "FR",
#                          auth_key = key_get("DeepL"))

# split_text(txt, max_size_bytes = 29000, tokenize = "sentences")
# pimp2(txt, source_lang = "EN", help_lang = "FR", auth_key = key_get("DeepL"))
# usage2(key_get("DeepL"))


### actual 

load(here::here("data", "tjetdb.RData"), verbose = TRUE)
# str(db, 1)
db$fr_Countries <- db$Countries 

db$fr_Countries$country <- toFrench2(text = db$fr_Countries$country,
                                     source_lang = "EN", 
                                     auth_key = key_get("DeepL"))

db$fr_Countries$txt_intro[!is.na(db$fr_Countries$txt_intro)] <- toFrench2(
  text = db$fr_Countries$txt_intro[!is.na(db$fr_Countries$txt_intro)],
  source_lang = "EN", 
  auth_key = key_get("DeepL"))

db$fr_Countries$txt_regime[!is.na(db$fr_Countries$txt_regime)] <- toFrench2(
  text = db$fr_Countries$txt_regime[!is.na(db$fr_Countries$txt_regime)],
  source_lang = "EN", 
  auth_key = key_get("DeepL"))

db$fr_Countries$txt_conflict[!is.na(db$fr_Countries$txt_conflict)] <- toFrench2(
  text = db$fr_Countries$txt_conflict[!is.na(db$fr_Countries$txt_conflict)],
  source_lang = "EN", 
  auth_key = key_get("DeepL"))

db$fr_Countries$txt_TJ[!is.na(db$fr_Countries$txt_TJ)] <- toFrench2(
  text = db$fr_Countries$txt_TJ[!is.na(db$fr_Countries$txt_TJ)],
  source_lang = "EN", 
  auth_key = key_get("DeepL"))

sample <- db$fr_Countries
save(sample, file = here::here("data", "fr_sample.RData"))

save(db, file = here::here("data", "tjetdb.RData"))
