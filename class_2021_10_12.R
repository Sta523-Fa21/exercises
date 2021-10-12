library(rvest)
library(tidyverse)

## Getting location from a denny's restaurant page

url = "http://www2.stat.duke.edu/~cr173/data/dennys/locations.dennys.com/NC/BATTLEBORO/246171.html"

read_html(url) %>%
  html_elements(".Core-address meta[itemprop=\"latitude\"]") %>%
  html_attr("content") %>%
  as.numeric()


# Create dirs

dir.create(here::here("data/dennys/"), recursive = TRUE, showWarnings = FALSE)

download.file(
  url = url,
  destfile = here::here("data/dennys/", basename(url)),
  quiet = TRUE
)


## For LQ

### Only hotel locations in the US

pi
letters
LETTERS

state.name
state.abb



### Denny's API

url = paste0(
  "https://nomnom-prod-api.dennys.com/restaurants/near",
  "?lat=35.994&long=-78.8986&radius=1000&limit=100", 
  "&nomnom=calendars&nomnom_calendars_from=20211011&nomnom_calendars_to=20211019&nomnom_exclude_extref=999"
)

z = jsonlite::read_json(url) %>%
  {tibble::tibble(data = .$restaurants)} %>%
  unnest_wider(data)

View(z)

