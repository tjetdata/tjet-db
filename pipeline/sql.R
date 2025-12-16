### packages
require(tidyverse)
require(RMariaDB)

### use this for setting the authorization key locally
### needs to be done only once for each new key
# keyring::key_set(service = "TJETdb", username = "fckdtuwsqu")

### loading the tables to write to the database
load(here::here("data", "tjetdb.RData"), verbose = TRUE)
db_fr <- readRDS(file = here::here("data", "tjetdb_fr.rds"))
str(db_fr[sort(names(db_fr))], 1)
db <- c(db, db_fr[names(db_fr)[!names(db_fr) %in% names(db)]])
str(db[sort(names(db))], 1)

fr <- names(db)[str_detect(names(db), "_fr")]
surveytabs <- db[["SurveysMeta"]] %>%
  select(results_tables) %>%
  unlist(use.names = FALSE) %>%
  str_replace(fixed(".xlsx"), "_fr")
fr <- fr[!fr %in% surveytabs] %>%
  print()

db[["labels"]] <- db[["labels"]] %>%
  rename(label_en = label) %>%
  full_join(db_fr[["labels_fr"]], by = "labelID") %>%
  rename(label_fr = label)

db[["ConflictDyads"]] <- db[["ConflictDyads"]] %>%
  rename(side_b_en = side_b) %>%
  left_join(
    db_fr[["ConflictDyads_fr"]],
    by = c("dyad_id", "conflict_id", "gwno_loc", "ep_start_date")
  ) %>%
  rename(side_b_fr = side_b) %>%
  select(
    dyad_id,
    conflict_id,
    location,
    gwno_loc,
    side_b_en,
    side_b_fr,
    ep_years,
    intensity,
    ep_start_date,
    ep_start_year,
    ep_end_year,
    confl_start,
    confl_end
  )

db[["dl_tjet_codebook"]] <- db[["dl_tjet_codebook"]] %>%
  rename(definition_en = definition) %>%
  left_join(
    db_fr[["dl_tjet_codebook_fr"]] %>%
      select(col_name, definition),
    by = "col_name"
  ) %>%
  rename(definition_fr = definition) %>%
  select(
    col_name,
    definition_en,
    definition_fr,
    source,
    source_description,
    source_url,
    tjet_version
  )

db[["SurveysMeta"]] <- db[["SurveysMeta"]] %>%
  rename(
    section_title_en = section_title,
    text_context_en = text_context,
    text_results_en = text_results,
    text_methods_en = text_methods,
    survey_design_en = survey_design
  ) %>%
  left_join(
    db_fr[["SurveysMeta_fr"]] %>%
      select(
        country,
        year,
        date_start,
        date_end,
        section_title,
        text_context,
        text_results,
        text_methods,
        survey_design
      ),
    by = c("country", "year", "date_start", "date_end")
  ) %>%
  rename(
    section_title_fr = section_title,
    text_context_fr = text_context,
    text_results_fr = text_results,
    text_methods_fr = text_methods,
    survey_design_fr = survey_design
  )

tabs <- c(
  "Accused",
  "AccusedCodebook",
  "Amnesties",
  "AmnestiesCodebook",
  "Amnesties_whoWasAmnestied",
  "ConflictDyads",
  "Countries",
  "CountryYears",
  "CourtLevels",
  "CourtLevelsCodebook",
  "dl_tjet_codebook",
  "dl_tjet_cy",
  "fields_meta",
  "ICC",
  "ICCcodebook",
  "ICCaccused",
  "ICCaccusedCodebook",
  "Investigations",
  "InvestigationsCodebook",
  "labels",
  "Reparations",
  "ReparationsCodebook",
  "Reparations_collectiveReparationsEligibility",
  "Reparations_individualReparationsEligible",
  "SurveysMeta",
  "TJETversions",
  "Transitions",
  "Trials",
  "TrialsCodebook",
  "TruthCommissions",
  "TruthCommissionsCodebook",
  "Vettings",
  "VettingsCodebook",
  "Vettings_targetingPositionSought"
) %>%
  print()

### two different ways of establishing the same database connection
### (note that cloudways requires the local IP address to be added)
con <- dbConnect(
  RMariaDB::MariaDB(),
  host = "159.203.34.223",
  dbname = "fckdtuwsqu",
  user = "fckdtuwsqu",
  password = keyring::key_get("TJETdb")
)
# con <- dbConnect(odbc::odbc(),
#   Driver = "mysql",
#   Server = "159.203.34.223", Port = "3306",
#   UID = "fckdtuwsqu",
#   PWD = rstudioapi::askForPassword("Database password:"),
#   Database = "fckdtuwsqu", timeout = 10
# )

### list of all tables in database
dbListTables(con) %>%
  sort()

### writing & reading specific table
# dbWriteTable(
#   conn = con,
#   name = "dl_tjet_cy",
#   value = db[["dl_tjet_cy"]],
#   overwrite = TRUE
# )
# dbReadTable(con, "dl_tjet_cy") %>%
#   tibble()

### write all tables to the database
### (this overwrites existing tables by first truncating and then appending)
map(tabs, function(table_name) {
  print(table_name)
  dbExecute(con, paste("TRUNCATE TABLE ", table_name, sep = ""))
  dbWriteTable(
    conn = con,
    name = table_name,
    value = db[[table_name]],
    append = TRUE
  )
})

### write survey data
# surveytabs %>%
#   map(function(table_name) {
#     print(table_name)
#     dbWriteTable(
#       conn = con,
#       name = table_name,
#       value = db[[table_name]],
#       overwrite = TRUE
#     )
#   })

### translations
translations <- dbReadTable(con, "Translations") %>% tibble()
newtrans <- translations %>%
  rename(Translation_exists = Translation) %>%
  full_join(db[["translations"]], by = "Source") %>%
  filter(is.na(Translation_exists)) %>%
  select(id, Source, Translation)
if (nrow(newtrans) > 0) {
  dbWriteTable(
    conn = con,
    name = "Translations",
    value = newtrans,
    append = TRUE
  )
}

### timestamp
dbExecute(con, "TRUNCATE TABLE tjet_timestamp")
dbWriteTable(
  conn = con,
  name = "tjet_timestamp",
  value = db[["db_timestamp"]],
  append = TRUE
)

### rankings (not fully integrated yet, just for trying out on the site for now)
# rankings <- readRDS(here::here("data", "rankings.rds"))
# dbWriteTable(
#   conn = con,
#   name = "rankings",
#   value = rankings,
#   overwrite = TRUE
# )
# dbReadTable(con, "rankings") %>%
#   tibble()

### when done always disconnect
dbDisconnect(con)

### other useful SQL functions
# dbListFields(con, "Countries")
# dbGetQuery(con, "SELECT * FROM Countries")
# result <- dbSendQuery(con, "SELECT * FROM Countries")
# dbFetch(result)
# dbClearResult(result)

### the script just writes the tables to the database
### but does not set the PKs and FKs
### for the production database this is not necessary because it is read-only
