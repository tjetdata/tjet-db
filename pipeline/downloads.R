# remotes::install_github('matthewjrogers/rairtable', ref = 'dev')
### the development version handles multi-select fields without error
library(tidyverse)
library(rairtable)
library(RSQLite)
library(here)

### the api key needs to be set only once, is saved to the local R environment
### if it needs to be set again, replace string and un-comment below to run once
# set_airtable_api_key("ACTUAL_API_KEY_GOES_HERE", install = TRUE)
# readRenviron("~/.Renviron")
# Sys.getenv("AIRTABLE_API_KEY")

### setup
base_id <- "appHsoHAemKITZgMF"
names(to_download) <- to_download <- c(
  "Amnesties",
  "Trials",
  "Accused",
  "TruthCommissions",
  "Reparations",
  "Vettings",
  "CountryYears",
  "Countries",
  "Transitions",
  "Conflicts",
  "Dyads",
  "select_options",
  "metadata"
)
pkeys <- c(
  "Amnesties" = "amnestyID",
  "Trials" = "trialID",
  "Accused" = "accusedID",
  "TruthCommissions" = "truthcommissionID",
  "Reparations" = "reparationID",
  "Vettings" = "vettingID",
  "CountryYears" = "countryyearID",
  "Countries" = "ccode",
  "Transitions" = "transitionID",
  "Conflicts" = "conflict_id",
  "Dyads" = "dyad_id"
)

### download data from Airtable
tjet <- lapply(to_download, function(table) {
  cat("Downloading", table, "\n")
  airtable(table, base_id) %>%
    read_airtable(id_to_col = TRUE)
})
tjet$Accused$trialID <- as.integer(tjet$Accused$trialID)
save(tjet, file = here("data/tjet.RData"))
