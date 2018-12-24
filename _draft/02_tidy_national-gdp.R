library(tidyverse)

load("~/R/my_blog/_draft/gdp_data.RData")

gdp_data <- gdp_data %>%
  mutate_at(vars(realtime_start, realtime_end, date), as.Date) %>%
  mutate_at(vars(value), as.double)

gdp_match_tbl <- gdp_meta %>%
  select(id, title) %>%
  mutate(title = str_replace(title, "^Gross Domestic Product for ", ""))

gdp_data <- gdp_data %>%
  mutate(Nation2 = str_sub(Nation, 7L, 9L)) %>%
  left_join(gdp_match_tbl, by = c("Nation" = "id"))

save(gdp_data, file = "~/R/my_blog/_draft/gdp_data.RData")
