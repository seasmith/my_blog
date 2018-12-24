library(tidyverse)

 load("~/R/my_blog/_draft/pop_data.RData")

pop_data <- pop_data %>%
  mutate_at(vars(realtime_start, realtime_end, date), as.Date) %>%
  mutate_at(vars(value), as.integer)

pop_match_tbl <- pop_meta %>%
  select(id, title) %>%
  mutate(title = str_replace(title, "^Population, Total for ", ""))

pop_data <- pop_data %>%
  mutate(Nation2 = str_sub(Nation, 7L, 9L)) %>%
  left_join(pop_match_tbl, by = c("Nation" = "id"))

save(pop_data, file = "~/R/my_blog/_draft/pop_data.RData")
