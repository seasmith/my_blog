library(tidyverse)
library(sf)
library(rmapshaper)

load("~/R/misc/maps/counties_2014_adjusted.RData")

# -- Default weighting
counties_2014_adjusted_05  <- ms_simplify(counties_2014_adjusted, keep = 0.05)
counties_2014_adjusted_025 <- ms_simplify(counties_2014_adjusted, keep = 0.025)
counties_2014_adjusted_01  <- ms_simplify(counties_2014_adjusted, keep = 0.01)
counties_2014_adjusted_001 <- ms_simplify(counties_2014_adjusted, keep = 0.001)

# -- weighting = 1
counties_2014_adjusted_05_1  <- ms_simplify(counties_2014_adjusted, keep = 0.05,  weighting = 1)
counties_2014_adjusted_025_1 <- ms_simplify(counties_2014_adjusted, keep = 0.025, weighting = 1)
counties_2014_adjusted_01_1  <- ms_simplify(counties_2014_adjusted, keep = 0.01,  weighting = 1)
counties_2014_adjusted_001_1 <- ms_simplify(counties_2014_adjusted, keep = 0.001, weighting = 1)

# -- weighting = 0.4
counties_2014_adjusted_05_04  <- ms_simplify(counties_2014_adjusted, keep = 0.05,  weighting = 0.4)
counties_2014_adjusted_025_04 <- ms_simplify(counties_2014_adjusted, keep = 0.025, weighting = 0.4)
counties_2014_adjusted_01_04  <- ms_simplify(counties_2014_adjusted, keep = 0.01,  weighting = 0.4)
counties_2014_adjusted_001_04 <- ms_simplify(counties_2014_adjusted, keep = 0.001, weighting = 0.4)


# -- weighting = 0.7 (default)
weighting_07 <- list(counties_2014_adjusted_05,
                     counties_2014_adjusted_025,
                     counties_2014_adjusted_01,
                     counties_2014_adjusted_001) %>%
  map(~filter(.x, STATE == "TX"))

names(weighting_07) <- c("0.05", "0.025", "0.01", "0.001")

weighting_07 <- weighting_07 %>%
  map2(names(.), ~mutate(.x, keep = .y, weighting = "0.7")) %>%
  reduce(rbind)


# -- weighting = 1.0
weighting_1 <- list(counties_2014_adjusted_05_1,
                     counties_2014_adjusted_025_1,
                     counties_2014_adjusted_01_1,
                    counties_2014_adjusted_001_1) %>%
  map(~filter(.x, STATE == "TX"))

names(weighting_1) <- c("0.05", "0.025", "0.01", "0.001")

weighting_1 <- weighting_1 %>%
  map2(names(.), ~mutate(.x, keep = .y, weighting = "1.0")) %>%
  reduce(rbind)


# -- weighting = 0.4
weighting_04 <- list(counties_2014_adjusted_05_04,
                     counties_2014_adjusted_025_04,
                     counties_2014_adjusted_01_04,
                     counties_2014_adjusted_001_04) %>%
  map(~filter(.x, STATE == "TX"))

names(weighting_04) <- c("0.05", "0.025", "0.01", "0.001")

weighting_04 <- weighting_04 %>%
  map2(names(.), ~mutate(.x, keep = .y, weighting = "0.4")) %>%
  reduce(rbind)




weighting_07 %>%
  ggplot() +
  geom_sf() +
  coord_sf(datum = NA) +
  facet_grid(. ~ keep)


# -- Plot all
weightings <- list(weighting_1,
                   weighting_07,
                   weighting_04) %>%
  reduce(rbind)

weightings <- weightings %>%
  mutate(weighting = factor(weighting, levels = rev(c("0.4", "0.7", "1.0")), ordered = TRUE),
         keep      = factor(keep, levels = rev(c("0.001", "0.01", "0.025", "0.05")), ordered = TRUE))

weightings %>%
  ggplot() +
  geom_sf() +
  coord_sf(datum = NA) +
  # scale_x_continuous(expand = expand_scale()) +
  # scale_y_continuous(expand = expand_scale()) +
  facet_grid(weighting ~ keep)
