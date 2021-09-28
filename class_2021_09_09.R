library(dplyr)
library(magrittr)
library(nycflights13)

# Demo 1

## How many flights to Los Angeles (LAX) did each of the legacy carriers (AA, UA, DL or US) 
## have in May from JFK, and what was their average duration?

flights %>%
  filter(dest == "LAX", origin == "JFK") %>%
  filter(carrier %in% c("AA","UA","UL","US")) %>%
  filter(month == 5) %>%
  group_by(carrier) %>%
  summarize(
    n = n(),
    avg_dur = mean(air_time, na.rm=TRUE)
  )

# Demo 2

## Which date should you fly on if you want to have the lowest possible average departure 
## delay? What about arrival delay?