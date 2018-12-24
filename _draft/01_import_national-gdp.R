
# NATIONAL GDP DATA -------------------------------------------------------


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
tags <- paste("nation", "gdp", "nsa", "annual")
rid <- "234"

url <- parse_url(api_url)
url$path <- paste0(url$path, req_type)
url$query <- list(release_id = rid,
                  api_key = api_key_fred,
                  tag_names = tags,
                  file_type = "json")

fred_qry <- url %>%
  build_url() %>%
  gsub("%3B|%20", ";", .)

nation_meta <- fred_qry %>%
  read_html() %>%
  html_text() %>%
  fromJSON() %>%
  .[["seriess"]] %>%
  as_tibble()

gdp_meta <- nation_meta %>%
  filter(grepl("Gross Domestic Product", title))

gdp_meta <- gdp_meta %>%
  filter(!grepl("Per Capita", title))

sids <- gdp_meta$id

# Add new path
req_type <- "series/observations"
url <- parse_url(api_url)
url$path  <- paste0(url$path, req_type)

gdp_data <- sids %>%
  map(~{
    url$query <- list(series_id = .x,
                      api_key = api_key_fred,
                      file_type = "json")
    
    fred_qry <- gsub("%3B|%20", ";", build_url(url))
    h <- html_text(read_html(fred_qry))
    h_observations <- fromJSON(h)[["observations"]]
    as_tibble(h_observations)
  })

names(gdp_data) <- sids

gdp_data <- gdp_data %>% bind_rows(.id = "Nation")

save(gdp_data, file = "~/R/my_blog/_draft/gdp_data.RData")
