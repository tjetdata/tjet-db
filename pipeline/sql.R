require(here)
require(tidyverse)
require(RMariaDB)
require(keyring)

### use this for setting the authorization key locally
### needs to be done only once for each new key
# keyring::key_set(service = "TJETdb", 
#                  username = "fckdtuwsqu")

load(here("data", "tjetdb.RData"), verbose = TRUE)

names(db)
db$dictionary # %>% print(n = Inf)

conn <- dbConnect(RMariaDB::MariaDB(),
                  host = "159.203.34.223",
                  dbname = "fckdtuwsqu", 
                  user = "fckdtuwsqu", 
                  password = key_get("TJETdb"))

dbWriteTable(conn, "Dictionary", db[["dictionary"]])
dbWriteTable(conn, "Countries", db[["Countries"]], overwrite = TRUE)
dbWriteTable(conn, "CountryYears", db[["CountryYears"]], overwrite = TRUE)

dbReadTable(conn, "CountryYears")

dbDisconnect(conn)

### SQL functions
# dbListTables(conn)
# dbListFields(conn, "Countries")
# dbReadTable(conn, "CountryYears")
# dbGetQuery(conn, "SELECT * FROM Countries")
# result <- dbSendQuery(conn, "SELECT * FROM Countries")
# dbFetch(result)
# dbClearResult(result)

### write all tables
# map(names(db), function(table_name) {
#   print(table_name)
#   dbWriteTable(conn, table_name, db[[table_name]])
# })

### TO DO
## - set all PKs and FKs in DB 
