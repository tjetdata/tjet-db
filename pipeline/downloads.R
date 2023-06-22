# remotes::install_github('matthewjrogers/rairtable', ref = 'dev')
### the development version handles multi-select fields without error
require(tidyverse)
require(rairtable)
require(here)

### the api key needs to be set only once, is saved to the local R environment
### if it needs to be set again, replace string and un-comment below to run once
# set_airtable_api_key("ACTUAL_AIRTABLE_API_PAT_GOES_HERE", install = TRUE)
# readRenviron("~/.Renviron")
# Sys.getenv("AIRTABLE_API_KEY")

to_download <- 
  list(
    "appHsoHAemKITZgMF" = c(
      "Amnesties", "TruthCommissions", "Reparations", "Vettings",
      "Experts", "NGOs", "Legal", "Transitions", "ConflictDyadSpells",
      "amnesties_challenges", "Mallinder", "Rozic", # "ICDB", 
      "BalkanInsight_comparison", "vetting_comparison", 
      "Countries", "Conflicts", "Dyads", "UCDPcountries",
      "select_options", "metadata", "TJETmembers"),
    "appF8HAH7SN7C09cU" = c("Trials", "Accused", "CourtLevels", 
                            "Countries", "Transitions", # "ConflictDyadSpells",
                            "Conflicts", "Dyads", "metadata")
  )

### still need to ensure that the countries and conflicts tables are synced between the two bases
### actually this may become a non-issue shortly

tjet <- map(names(to_download), function(base_id) {
  cat("base ID:", base_id, "\n")
  names(tab) <- tab <- to_download[[base_id]] 
  lapply(tab, function(df) {
    cat("Downloading", df, "\n")
    airtable(df, base_id) %>%
      read_airtable(id_to_col = TRUE)
  })
})
names(to_download) <- names(tjet) <- c("MegaBase", "Prosecutions")
save(tjet, to_download, file = here("data", "tjet.RData"))
