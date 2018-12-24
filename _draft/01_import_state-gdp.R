
# STATE GDP DATA ----------------------------------------------------------


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
tags <- paste("state", "gsp", "gdp", "nsa", "annual", sep = ";")
rid <- "140"

url <- parse_url(api_url)
url$path  <- paste0(url$path, req_type)
url$query <- list(release_id = rid,
                  api_key = api_key_fred,
                  tag_names = tags,
                  file_type = "json")

fred_qry <- build_url(url) %>% gsub("%3B", ";", .)


state_meta <- list()
n <- 1

repeat {
  h <- html_text(read_html(fred_qry))
  h_series <- fromJSON(h)[["seriess"]]
  state_meta[[n]] <- as_tibble(h_series)
  
  if (nrow(state_meta[[n]]) != 1000) break
  
  url$query <- list(release_id = "140",
                    api_key = api_key_fred,
                    tag_names = tags,
                    offset = 1000 * n,
                    file_type = "json")
  
  fred_qry <- build_url(url) %>% gsub("%3B", ";", .)
  n <- n + 1
}

state_meta <- state_meta %>% bind_rows()

gsp_meta <- state_meta %>%
  filter(str_detect(title, "^Total Gross Domestic Product .*"))

# Add new path
req_type <- "series/observations"
url <- parse_url(api_url)
url$path  <- paste0(url$path, req_type)

# Get series id for series_id
sids <- gsp_meta$id

gsp_data <- sids %>%
  map(~{
    url$query <- list(series_id = .x,
                      api_key = api_key_fred,
                      file_type = "json")
    
    fred_qry <- gsub("%3B", ";", build_url(url))
    h <- html_text(read_html(fred_qry))
    h_observations <- fromJSON(h)[["observations"]]
    as_tibble(h_observations)
  })

names(gsp_data) <- sids
gsp_data <- gsp_data %>% bind_rows(.id = "State")

save(gsp_data, file = "~/R/my_blog/_draft/gsp_data.RData")
