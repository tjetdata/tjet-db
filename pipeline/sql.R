require(here)
require(tidyverse)
require(RMariaDB)
require(keyring)

### use this for setting the authorization key locally
### needs to be done only once for each new key
# keyring::key_set(service = "TJETdb", 
#                  username = "fckdtuwsqu")

load(here("data", "tjetdb.RData"), verbose = TRUE)


## file.remove("tjet.db")

# conn <- dbConnect(RSQLite::SQLite(), "../data/tjet.db")
conn <- dbConnect(RMariaDB::MariaDB(),
                  host = "159.203.34.223",
                  dbname = "fckdtuwsqu", 
                  user = "fckdtuwsqu", 
                  password = key_get("TJETdb"))




## write tables
map(names(db), function(table_name) {
  print(table_name)
  dbWriteTable(conn, table_name, db[[table_name]])
})
dbListTables(conn)
# dbListFields(conn, "Countries")
# dbReadTable(conn, "CountryYears")
# dbGetQuery(conn, "SELECT * FROM Countries")
# result <- dbSendQuery(conn, "SELECT * FROM Countries")
# dbFetch(result)
# dbClearResult(result)
dbDisconnect(conn)

### TO DO
## - set all PKs and FKs in DB 
