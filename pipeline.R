# remotes::install_github('matthewjrogers/rairtable', ref = 'dev')
library(tidyverse)
library(rairtable)
library(RSQLite)

### the api key needs to be set only once, and is saved to the local R environment
### if it needs to be set again, replace string below accordingly
# set_airtable_api_key("ACTUAL_API_KEY_GOES_HERE", install = TRUE)
# readRenviron("~/.Renviron")
# Sys.getenv("AIRTABLE_API_KEY")

### download data from Airtable 
base_id <- "appHsoHAemKITZgMF"
to_download <- c("Amnesties" = "amnesties_table_full", 
  "Trials" = "trials_table_full", 
  # "Accused" = "accusd_table_full", 
  "Truth Commissions" = "tcs_table_full", 
  "Reparations" = "reparations_table_full", 
  "Countries" = "countries_table",
  "Transitions" = "cy_table",
  "metadata" = "metadata_table")
tjet <- lapply(names(to_download)[8], function(table) {
  airtable(table, 'appHsoHAemKITZgMF', view = to_download[table]) %>%
    read_airtable(id_to_col = TRUE)
})

# amnesty <- read_airtable(amnesty, id_to_col = TRUE)
# sort(names(amnesty))
# class(amnesty$whoWasAmnestied[1:10])
# class(amnesty[1:10, c("whoWasAmnestied", "whatCrimes")])
