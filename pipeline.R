
### loading key packages
# library(dotenv)
library(tidyverse)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("fixed", "stringr")

# readRenviron(".env")

### only use the scripts below if you know what you're doing
### there is potential for damage to the production database
### caution: the translation script is potentially expensive

### downloading all data from Airtable MegaBase
source(here::here("pipeline", "downloads.R"))
### processing raw data into production database & creating the TJET datasets 
source(here::here("pipeline", "processing.R"))
### translating necessary fields into French (only use as needed) 
source(here::here("pipeline", "translation.R"))
### writing the production database to the Cloudways server
source(here::here("pipeline", "sql.R"))

### one-offs NEED TO ADD EXPLANATORY COMMENTS
## transitions 
## conflicts 
## pipeline/profiles

