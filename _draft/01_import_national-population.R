
# NATIONAL POPULATION DATA ------------------------------------------------


library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)

# URL
api_url  <- "https://api.stlouisfed.org/fred/"

# Path
req_type <- "release/series"

# Query keys
api_key_fred <- getOption("api_key_fred")
tags <- paste("population", "annual", "nsa", "nation")
rid = "234"

url <- parse_url(api_url)
url$path <- paste0(url$path, req_type)
url$query <- list(release_id = rid,
                  api_key = api_key_fred,
                  tag_names = tags,
                  file_type = "json")

fred_qry <- url %>%
  build_url() %>%
  gsub("%3B|%20", ";", .)

pop_meta <- list()
n <- 1

repeat {
  h <- html_text(read_html(fred_qry))
  h_series <- fromJSON(h)[["seriess"]]
  pop_meta[[n]] <- as_tibble(h_series)
  
  if (nrow(pop_meta[[n]]) != 1000) break
  
  url$query <- list(release_id = rid,
                    api_key = api_key_fred,
                    tag_names = tags,
                    offset = 1000 * n,
                    file_type = "json")
  
  fred_qry <- url %>%
    build_url() %>%
    gsub("%3B|%20", ";", .)
    # gsub("world;bank", "world%20bank", .)
  
  n <- n + 1
}

pop_meta <- pop_meta %>% bind_rows()

pop_meta <- pop_meta %>%
  filter(str_detect(title, "^Population, Total"))

sids <- pop_meta$id

# Add new path
req_type <- "series/observations"
url <- parse_url(api_url)
url$path  <- paste0(url$path, req_type)

pop_data <- sids %>%
  map(~{
    url$query <- list(series_id = .x,
                      api_key = api_key_fred,
                      file_type = "json")
    
    fred_qry <- gsub("%3B|%20", ";", build_url(url))
    h <- html_text(read_html(fred_qry))
    h_observations <- fromJSON(h)[["observations"]]
    as_tibble(h_observations)
  })

names(pop_data) <- sids
pop_data <- pop_data %>% bind_rows(.id = "Nation")

save(pop_data, file = "~/R/my_blog/_draft/pop_data.RData")
