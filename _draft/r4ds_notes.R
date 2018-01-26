# * Duration = the lubridate difftime object
#                you get from substracting
#                two dates/datetimes.

# How old is Hadley?
h_age <- today() - ymd(19791014)
h_age
as.duration(h_age)

# * Periods = durations that account for "human"
#               time, like days and months,
#               regardless of such things as
#               timezones.

one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
one_pm

one_pm + days(1)

seconds(15)
#> [1] "15S"
minutes(10)
#> [1] "10M 0S"
hours(c(12, 24))
#> [1] "12H 0M 0S" "24H 0M 0S"

# * Interval = a duration with a starting point;
#                because it makes the duration
#                precise (think LEAP YEARS!).

years(1) / days(1) # WHAT IF WE GOT 366!
#> estimate only: convert to intervals for accuracy
#> [1] 365

# Solved here:
next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)
#> [1] 365

library(tidyverse)
library(lubridate)
library(ggridges)
library(nycflights13)

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

# 16.3.3
#
# Density plot of all dep_time as a single day
#   and facet by month with ggridges.
# 
flights_dt %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  ggplot() +
  geom_density_ridges(aes(dep_hour, factor(month(dep_time))), alpha = 0.7) +
  scale_y_discrete(labels = month.abb) +
  facet_grid(origin ~ .)



same_hour <- hou_cr_dt %>%
  mutate(Date = update(Date, yday = 1),
         Date = update(Date, year = 2017))

same_hour_summ <- same_hour %>%
  group_by(Date, `Offense Type`) %>%
  summarize(n_offenses = sum(n_offenses, na.rm = TRUE)) %>%
  ungroup()


splines_hour <- same_hour_summ %>%
  # mutate(`Offense Type` = factor(`Offense Type`, sort(unique(`Offense Type`)), ordered = TRUE)) %>%
  split(.$`Offense Type`) %>%
  map2(seq_along(.), ~{
    
    this_level <- levels(.x$`Offense Type`)[.y]
    ggplot(filter(.x, `Offense Type` == this_level)) +
      geom_xspline(aes(Date, n_offenses), color = picked_colors[.y], size = 0.7) +
      # geom_area(aes(Date, n_offenses), fill = picked_colors[.y], stat = "xspline") +
      scale_y_continuous(labels = comma, expand = expand_scale()) +
      scale_x_datetime(NULL, date_labels = "%I %p", expand = expand_scale()) +
      labs(title = paste0(this_level, "\n")) +
      guides(fill = FALSE) +
      theme_dk()
    
  })


# Create a row per n_offense
u2 <- same_hour %>%
  group_by(Date, `Offense Type`) %>%
  summarize(n_offenses = sum(n_offenses, na.rm = TRUE)) %>%
  split(.$`Offense Type`) %>%
  map(~split(.x, .x$Date)) %>%
  modify_depth(2, ~.x[rep(1, .x$n_offenses), ])

u2 <- u2 %>%
  map(bind_rows) %>%
  bind_rows() %>%
  ungroup()

u2 %>%
  mutate(`Offense Type` = factor(`Offense Type`, rev(unique(`Offense Type`)), ordered = TRUE)) %>%
  ggplot() +
  geom_density_ridges(aes(Date, `Offense Type`)) +
  scale_y_discrete(NULL) +
  scale_x_datetime(NULL, date_labels = "%H") +
  theme(axis.text.y = element_text(angle = -90))

counts <- u2 %>%
  mutate(`Offense Type` = factor(`Offense Type`, sort(unique(`Offense Type`)), ordered = TRUE)) %>%
  split(.$`Offense Type`) %>%
  map2(seq_along(.), ~{
    this_level <- levels(.x$`Offense Type`)[.y]
    ggplot(filter(.x, `Offense Type` == this_level)) +
      geom_bar(aes(Date, group = `Offense Type`), fill = picked_colors[.y]) +
      # geom_bar(aes(Date, group = `Offense Type`, fill = ..count..)) +
      # scale_fill_viridis_c(option = "C", begin = 0.2, end = 1) +
      scale_y_continuous(labels = comma, expand = expand_scale()) +
      scale_x_datetime(NULL, date_labels = "%I %p", expand = expand_scale()) +
      labs(title = paste0(this_level, "\n")) +
      guides(fill = FALSE) +
      theme_dk()
  })


x <- multi_plot(counts, gb,,FALSE)
ggsave("bars_twitter.png", plot = x, width = 5.55556, height = 16.66667, dpi = 100)
