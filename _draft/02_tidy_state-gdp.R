library(tidyverse)

load("~/R/my_blog/_draft/gsp_data.RData")

# Meaningful state names with lookup table
state_lookup <- tibble(state_abb = state.abb,
                       state_name = state.name)
state_lookup <- state_lookup %>%
  add_row(state_abb = "DC", state_name = "Washington DC")

gsp_data <- gsp_data %>%
  mutate(state_abb = str_sub(State, 1L, 2L)) %>%
  left_join(state_lookup, by = "state_abb")

# Convert columns to correct class and correct value
gsp_data <- gsp_data %>%
  mutate_at(vars(realtime_start, realtime_end, date), as.Date) %>%
  mutate_at(vars(value), as.double) %>%
  mutate(value = value * 10^6)

save(gsp_data, file = "~/R/my_blog/_draft/gsp_data.RData")
