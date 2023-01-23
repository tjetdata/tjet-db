library(keyring)
library(deeplr)

### use this for setting the authorization key locally
### needs to be done only once for each new key
# keyring::key_set(service = "DeepL") 

txt <- c("This sentence was brought to you from English to French back to English.", 
         "Transitional justice consists of polices to deal with past human rights abuses.")

translated <- toFrench2(text = txt, 
                        source_lang = "EN",
                        auth_key = key_get("DeepL"))

back_trans <- toEnglish2(text = translated, 
                         source_lang = "FR",
                         auth_key = key_get("DeepL"))

# split_text(txt, max_size_bytes = 29000, tokenize = "sentences")

pimp2(txt, source_lang = "EN", help_lang = "FR", auth_key = key_get("DeepL"))

usage2(key_get("DeepL"))
