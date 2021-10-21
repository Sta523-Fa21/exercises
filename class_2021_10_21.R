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


## Exercise 1

db = DBI::dbConnect(
  RSQLite::SQLite(), 
  here::here("class_2021_10_21/flights.sqlite")
)

data(flights, package = "nycflights13")
data(planes, package = "nycflights13")

tidyquery::query(
  "select sum(seats) from flights natural left join planes"
)

tidyquery::show_dplyr(
  "select sum(seats) from flights natural left join planes"
)

tidyquery::query(
  "select sum(seats) from flights left join planes using (tailnum)" 
)



