### create SQLite DB
load("../data/tjetdb.RData")
## file.remove("tjet.db")

conn <- dbConnect(RSQLite::SQLite(), "../data/tjet.db")

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
## - set all PKs and FKs in DB 
