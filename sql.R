### create SQLite DB
load("tjetdb.RData")
## file.remove("tjet.db")

conn <- dbConnect(RSQLite::SQLite(), "tjet.db")

## write tables
map(names(db), function(table_name) {
  print(table_name)
  dbWriteTable(conn, table_name, db[[table_name]])
})
dbListTables(conn)
# dbListFields(conn, "Countries")
# dbReadTable(conn, "Countries")
# dbGetQuery(conn, "SELECT * FROM Countries")
# result <- dbSendQuery(conn, "SELECT * FROM Countries")
# dbFetch(result)
# dbClearResult(result)
dbDisconnect(conn)

### TO DO
## - turn multi-select fields into dummies for data downloads (needs a consistent naming scheme)
## - set all PKs and FKs in DB 
