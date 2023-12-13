### packages
require(tidyverse)
require(RMariaDB)

### use this for setting the authorization key locally
### needs to be done only once for each new key
# keyring::key_set(service = "TJETdb", username = "fckdtuwsqu")

### loading the tables to write to the database
load(here::here("data", "tjetdb.RData"), verbose = TRUE)
fr <- readRDS(file = here::here("data", "tjetdb_fr.rds"))
str(fr[sort(names(fr))], 1)
db <- c(db, fr[names(fr)[!names(fr) %in% names(db)]])
str(db[sort(names(db))], 1)
fr <- names(db)[str_detect(names(db), "_fr")]

tabs <- c(fr, 
  "Accused", "Amnesties", "Amnesties_whoWasAmnestied", "codebook", 
  "ConflictDyads", "Countries", "CountryYears", "CourtLevels", "dl_tjet_cy", 
  "dl_tjet_codebook", "labels", "Reparations", 
  "Reparations_collectiveReparationsEligibility",
  "Reparations_individualReparationsEligible", "SurveysMeta", "Transitions",
  "Trials", "TruthCommissions", "Vettings", "Vettings_targetingAffiliation"
  ) %>% sort()

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
# dbReadTable(con, "fr_Countries") %>% tibble() 

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
# db[["SurveysMeta"]] %>% 
#   select(results_tables) %>% 
#   unlist(use.names = FALSE) %>% 
#   str_replace(fixed(".xlsx"), "") %>% 
#   map(function(table_name) {
#     print(table_name)
#     dbWriteTable(conn = con,
#                  name = table_name,
#                  value = db[[table_name]],
#                  overwrite = TRUE)
#   })

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
