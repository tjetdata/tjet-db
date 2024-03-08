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

tabs <- c(
  "Accused", # "Accused_fr", 
  "Amnesties", # "Amnesties_fr", 
  "Amnesties_whoWasAmnestied", "codebook", "ConflictDyads", "ConflictDyads_fr", 
  "Countries", # "Countries_fr", 
  "CountryYears", "CourtLevels", 
  "dl_tjet_codebook", "dl_tjet_codebook_fr", "dl_tjet_cy", "fields_meta", "ICC", 
  "ICCaccused", "Investigations", "labels", "labels_fr", "Reparations", 
  # "Reparations_fr", 
  "Reparations_collectiveReparationsEligibility",
  "Reparations_individualReparationsEligible", "SurveysMeta", 
  # "SurveysMeta_fr", 
  "Transitions", "Trials", # "Trials_fr", 
  "TruthCommissions", # "TruthCommissions_fr", 
  "Vettings", # "Vettings_fr", 
  "Vettings_targetingAffiliation"
  ) %>% 
  print()

### two different ways of establishing the same database connection
### (note that cloudways requires the local IP address to be added)
con <- dbConnect(RMariaDB::MariaDB(),
                 host = "159.203.34.223",
                 dbname = "fckdtuwsqu",
                 user = "fckdtuwsqu",
                 password = keyring::key_get("TJETdb"))
# con <- dbConnect(odbc::odbc(), Driver="mysql", 
#                   Server = "159.203.34.223", Port = "3306", 
#                   UID = "fckdtuwsqu", 
#                   PWD = rstudioapi::askForPassword("Database password:"), 
#                   Database = "fckdtuwsqu", timeout = 10)

### list of all tables in database
dbListTables(con) %>% 
  sort()

### reading specific table
# dbWriteTable(conn = con,
#              name = "Countries_fr",
#              value = db[["Countries_fr"]],
#              overwrite = TRUE)
# dbReadTable(con, "Countries_fr") %>% tibble()

### write all tables to the database 
### (this overwrites existing tables by first truncating and then appending)
map(tabs, function(table_name) {
  print(table_name)
  dbExecute(con, paste("TRUNCATE TABLE ", table_name, sep = ""))
  dbWriteTable(conn = con,
               name = table_name,
               value = db[[table_name]],
               append = TRUE)
})

### write survey data 
surveytabs %>%
  map(function(table_name) {
    print(table_name)
    dbWriteTable(conn = con,
                 name = table_name,
                 value = db[[table_name]],
                 overwrite = TRUE)
  })

dbExecute(con, "TRUNCATE TABLE tjet_timestamp")
dbWriteTable(conn = con,
             name = "tjet_timestamp",
             value = db[["db_timestamp"]],
             append = TRUE)

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
