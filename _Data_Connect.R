library(DBI);library(RSQLite)

con = dbConnect(SQLite(), "PlayerRecords.sqlite")
dbListTables(con)
play_data = dbGetQuery(con,"SELECT * FROM  Player_Detail")
