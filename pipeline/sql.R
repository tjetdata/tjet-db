require(here)
require(tidyverse)
require(RMariaDB)
require(keyring)

### use this for setting the authorization key locally
### needs to be done only once for each new key
# keyring::key_set(service = "TJETdb", 
#                  username = "fckdtuwsqu")

load(here("data", "tjetdb.RData"), verbose = TRUE)
# str(db, 1)

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "159.203.34.223",
                 dbname = "fckdtuwsqu",
                 user = "fckdtuwsqu",
                 password = rstudioapi::askForPassword("Database password:"))
                 # password = key_get("TJETdb"))

# con <- dbConnect(odbc::odbc(), Driver="mysql", 
#                   Server = "159.203.34.223", Port = "3306", 
#                   UID = "fckdtuwsqu", 
#                   PWD = rstudioapi::askForPassword("Database password:"), 
#                   Database = "fckdtuwsqu", timeout = 10)

dbWriteTable(con, "Dictionary", db[["dictionary"]], overwrite = TRUE)
dbWriteTable(con, "Countries", db[["Countries"]], overwrite = TRUE)
dbWriteTable(con, "CountryYears", db[["CountryYears"]], overwrite = TRUE)
dbWriteTable(con, "ConflictDyads", db[["ConflictDyads"]], overwrite = TRUE)
dbWriteTable(con, "Trials", db[["Trials"]], overwrite = TRUE)
dbWriteTable(con, "Accused", db[["Accused"]], overwrite = TRUE)
dbWriteTable(con, "Amnesties", db[["Amnesties"]], overwrite = TRUE)
dbWriteTable(con, "TruthCommissions", db[["TruthCommissions"]], overwrite = TRUE)
dbWriteTable(con, "Reparations", db[["Reparations"]], overwrite = TRUE)

### there are other tables to add
dbListTables(con)
dbReadTable(con, "ConflictDyads")
dbDisconnect(con)

### SQL functions
# dbListFields(con, "Countries")
# dbReadTable(con, "CountryYears")
# dbGetQuery(con, "SELECT * FROM Countries")
# result <- dbSendQuery(con, "SELECT * FROM Countries")
# dbFetch(result)
# dbClearResult(result)

### write all tables
# map(names(db), function(table_name) {
#   print(table_name)
#   dbWriteTable(con, table_name, db[[table_name]])
# })

### TO DO
## - set all PKs and FKs in DB 
