# remotes::install_github('matthewjrogers/rairtable', ref = 'dev')
### the development version handles multi-select fields without error

## packages
require(tidyverse)
require(rairtable)

### the api key needs to be set only once, to save it to the local environment
### if it needs to be set again, replace the string and un-comment to run once

# set_airtable_api_key("MY_KEY_HERE", install = TRUE)
# readRenviron("~/.Renviron")
# Sys.getenv("AIRTABLE_API_KEY")

### this list specifies the Airtable bases and tables to download via API
to_download <-
  list(
    "appHsoHAemKITZgMF" = c(
      "Amnesties", "Reparations", "TruthCommissions", "Vettings", "Countries",
      "Transitions", "ConflictDyadSpells", "Conflicts", "Dyads", 
      "select_options", "metadata", "UCDPcountries", "TJETmembers", "Experts",
      "NGOs", "Legal", "ICDB", "Mallinder", "Rozic", "Challenges",
      "VettingComparison", "BIcomparison", "ICC", "Investigations",
      "AdHocHybrid", "SurveysMeta", "Ethiopia", "TJETversions", 
      "Southey", "Filipa", "CorrectionsCandidates"
    ),
    "appF8HAH7SN7C09cU" = c(
      "Trials", "Accused", "CourtLevels", "Countries", "Conflicts", "Dyads", 
      "metadata", "ArgCausas", "ArgAccused", "ArgCLs", "ArgCharges", 
      "BIcomparison", "BIcaseIDlink", "TI", "newTI"
    )
  )

### the countries and conflicts tables are currently not synced between the
### two bases but were the same when first added; the only differences are the
### linked table fields; we could build in a check to ensure the data in these
### tables are consistent but ideally all tables would be in the same base

### downloading data via Airtable API and saving the raw data locally

tjet <- map(names(to_download), function(base_id) {
  cat("base ID:", base_id, "\n")
  names(tab) <- tab <- to_download[[base_id]]
  lapply(tab, function(df) {
    Sys.sleep(1)
    cat("Downloading", df, "\n")
    airtable(df, base_id) %>%
      read_airtable(id_to_col = TRUE) %>%
      return()
  }) %>%
    return()
})
names(to_download) <- names(tjet) <- c("MegaBase", "Prosecutions")
tjet[["db_timestamp"]] <- Sys.time()
save(tjet, to_download, file = here::here("data", "tjet.RData"))

map(tjet[["MegaBase"]][["SurveysMeta"]][["results_tables"]], function(x) {
  download.file(
    url = x$url,
    destfile = here::here("data", "downloads", x$filename)
  )
})
