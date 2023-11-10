### packages
require(tidyverse)
require(RMariaDB)

### use this for setting the authorization key locally
### needs to be done only once for each new key
# keyring::key_set(service = "TJETdb", username = "fckdtuwsqu")

### loading the tables to write to the database
load(here::here("data", "tjetdb.RData"), verbose = TRUE)
### for checking the loaded data
# str(db, 1)

### two different ways of establishing the same database connection
### (note that cloudways requires the local IP address to be added)
con <- dbConnect(RMariaDB::MariaDB(),
                 host = "159.203.34.223",
                 dbname = "fckdtuwsqu",
                 user = "fckdtuwsqu",
                 # password = rstudioapi::askForPassword("Database password:"))
                 password = keyring::key_get("TJETdb"))
# con <- dbConnect(odbc::odbc(), Driver="mysql", 
#                   Server = "159.203.34.223", Port = "3306", 
#                   UID = "fckdtuwsqu", 
#                   PWD = rstudioapi::askForPassword("Database password:"), 
#                   Database = "fckdtuwsqu", timeout = 10)

tabs <- c("Accused", "Amnesties", "Amnesties_whoWasAmnestied", "codebook", 
          "ConflictDyads", "Countries", "CountryYears", "CourtLevels", 
          "fr_Countries", "labels", "Reparations", 
          "Reparations_collectiveReparationsEligibility", 
          "Reparations_individualReparationsEligible", "SurveysMeta", 
          "Transitions", "Trials", "TruthCommissions", 
          "Uganda_2005_descriptives", "Vettings", 
          "Vettings_targetingAffiliation")

### write all tables to the database (this overwrites existing tables)
map(tabs, function(table_name) {
  print(table_name)
  dbWriteTable(conn = con, 
               name = table_name, 
               value = db[[table_name]], 
               overwrite = TRUE)
})

### list of all tables in database
dbListTables(con)
### reading specific table
dbReadTable(con, "fr_Countries")
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
