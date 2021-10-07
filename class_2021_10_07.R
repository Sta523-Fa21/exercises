library(tidyverse)

## Demo 1

#### Books

url = "https://www.anapioficeandfire.com/api/books?pageSize=50"

books = jsonlite::read_json(url)

books %>%
  tibble(data = .) %>%
  unnest_wider(data) %>%
  View()

#### Houses

house_url = "https://www.anapioficeandfire.com/api/houses?pageSize=50&page=22"

houses = jsonlite::read_json(house_url)

houses %>%
  tibble(data = .) %>%
  unnest_wider(data) %>%
  View()


#### Pagination

full = list()
page = 1
house_url = "https://www.anapioficeandfire.com/api/houses"

repeat {
  cat("Grabbing page: ", page, "\n")
  houses = jsonlite::read_json(
    paste0(
      house_url,
      "?pageSize=50&page=", page,
      "&hasDiedOut=True"
    )
  )
  
  if (length(houses) == 0)
    break
  
  full = c(full, houses)
  page = page + 1
}

full %>%
  tibble(data = .) %>%
  unnest_wider(data) %>%
  View()


### httr2

library(httr2)


resp = request("https://www.anapioficeandfire.com/api/characters") %>%
  req_url_query(page = 1, pageSize = 50) %>%
  #req_dry_run()
  req_perform()

resp %>% 
  resp_body_json() %>%
  View()

resp %>%
  resp_header("link")


get_link = function(resp) {
  resp %>%
    resp_header("link") %>%
    str_match_all("<(.*?)>; rel=\"([a-zA-Z ]+)\"") %>%
    .[[1]] %>%
    { setNames(.[,2], .[,3]) } %>%
    as.list()
}

get_link(resp)[["next"]]



resp = request("https://www.anapioficeandfire.com/api/characters") %>%
  req_url_query(page = 1, pageSize = 50) %>%
  req_perform()
page = 1
full = list()

repeat {
  cat("Grabbing page: ", page, "\n")
  full = c(full, resp_body_json(resp))
  
  links = get_link(resp)
  
  if (is.null(links[["next"]]))
    break
  
  resp = request(links[["next"]]) %>%
    req_perform()
  
  page = page + 1
}

full %>%
  tibble(data = .) %>%
  unnest_wider(data) %>%
  View()


#### GitHub Example

r  = request("https://api.github.com/gists") %>%
  req_headers(
    Authorization = paste("token", Sys.getenv("GITHUB_PAT"))
  ) %>%
  req_body_json( list(
    description = "Testing 1,2,3,...",
    files = list(
      "test.R" = list(
        content = "print('hello world')\n"
      )
    ),
    public = TRUE
  ) ) %>%
  req_perform()

r %>%
  resp_body_json() %>%
  View()
