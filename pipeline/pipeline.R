
### loading key packages
library(tidyverse)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("fixed", "stringr")

### only use the scripts below if you know what you're doing
### there is potential for damage to the production database
### caution: the translation script is potentially expensive

### downloading all data from Airtable MegaBase
source(here::here("pipeline", "downloads.R"))
### getting the production database ready 
source(here::here("pipeline", "processing.R"))
### creating the TJET analyses dataset 
source(here::here("pipeline", "datasets.R"))
### translating necessary field into French
source(here::here("pipeline", "translation.R"))
### writing the production database to the Cloudways server
source(here::here("pipeline", "sql.R"))
