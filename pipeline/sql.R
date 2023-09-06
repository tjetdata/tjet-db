require(tidyverse)
require(RMariaDB)

### use this for setting the authorization key locally
### needs to be done only once for each new key
# keyring::key_set(service = "TJETdb", username = "fckdtuwsqu")

load(here::here("data", "tjetdb.RData"), verbose = TRUE)
# str(db, 1)

# con <- dbConnect(odbc::odbc(), Driver="mysql", 
#                   Server = "159.203.34.223", Port = "3306", 
#                   UID = "fckdtuwsqu", 
#                   PWD = rstudioapi::askForPassword("Database password:"), 
#                   Database = "fckdtuwsqu", timeout = 10)
con <- dbConnect(RMariaDB::MariaDB(),
                 host = "159.203.34.223",
                 dbname = "fckdtuwsqu",
                 user = "fckdtuwsqu",
                 password = rstudioapi::askForPassword("Database password:"))
                 # password = keyring::key_get("TJETdb"))

### write all tables
map(names(db), function(table_name) {
  print(table_name)
  dbWriteTable(con, table_name, db[[table_name]], overwrite = TRUE)
})

### there are other tables to add
dbListTables(con)
dbReadTable(con, "ConflictDyads")
dbReadTable(con, "fr_Countries")
dbDisconnect(con)

### other SQL functions
# dbListFields(con, "Countries")
# dbReadTable(con, "CountryYears")
# dbGetQuery(con, "SELECT * FROM Countries")
# result <- dbSendQuery(con, "SELECT * FROM Countries")
# dbFetch(result)
# dbClearResult(result)

### TO DO
## - set all PKs and FKs in DB 
