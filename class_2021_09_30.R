library(rvest)
library(tidyverse)

## Example

url = "https://www.rottentomatoes.com/"

polite::bow(url)

page = read_html(url)


movies = tibble::tibble(
  title = page %>%
    html_elements(".ordered-layout__list--score:nth-child(1) .clamp-1") %>%
    html_text(),
  
  rating = page %>%
    html_elements(".ordered-layout__list--score:nth-child(1) .dynamic-text-list__tomatometer-group") %>%
    html_text2() %>%
    str_remove("%$") %>%
    as.numeric() %>%
    {. / 100},
  
  freshness = page %>%
    html_elements(".ordered-layout__list--score:nth-child(1) .icon--tiny") %>%
    html_attr("class") %>%
    str_remove_all("icon |icon--tiny |icon__") %>%
    str_replace("_", " ") %>%
    str_to_title(),
  
  url = page %>%
    html_elements(".ordered-layout__list--score:nth-child(1) .dynamic-text-list__tomatometer-group") %>%
    html_attr("href") %>%
    paste0(url, .)
)

movies
