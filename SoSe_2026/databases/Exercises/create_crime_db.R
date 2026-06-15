library(DBI)
library(RSQLite)

base_path <- here::here("SoSe_2026/databases/Exercises")
db_path <- file.path(base_path, "crime_db.sqlite")

# ESTABLISH CONNECTION
con <- dbConnect(drv = RSQLite::SQLite(), db_path)

# WRITE DATA
crime_df <- read_rds(file.path(base_path, "Crime_Large.rds"))
dbWriteTable(conn = con, name = "tbl_crime", value = crime_df)
dbDisconnect(con)
