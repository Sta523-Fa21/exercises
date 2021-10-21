library(dplyr)

# Create flights.sqlite

db = DBI::dbConnect(
  RSQLite::SQLite(), 
  here::here("class_2021_10_21/flights.sqlite")
)

dplyr::copy_to(
  db, nycflights13::flights, 
  name = "flights", 
  temporary = FALSE
)

dplyr::copy_to(
    db, nycflights13::planes, 
  name = "planes", 
  temporary = FALSE
)

DBI::dbDisconnect(db)

#