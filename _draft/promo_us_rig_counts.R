load("~/R/misc/oil/US/rig_counts/rc_master.RData")
purp00 <- "#bdadb8"
purp0 <- "#a6819c"
purp1 <- "#814b72"
purp2 <- "#5a344f"
purp3 <- "#361f2f"

# Summary basins per week
rcm <- rc_master %>%
  filter(Country == "UNITED STATES") %>%
  filter(!is.na(RigCount)) %>%
  group_by(Basin, PublishDate) %>%
  summarize(RigCount = sum(RigCount, na.rm = TRUE)) %>%
  arrange(RigCount)

rcm_current <- rcm %>%
  filter(PublishDate == key_dates[["this_week"]])

rcm_current <- rcm_current %>%
  ungroup() %>%
  add_row(Basin = "Fayetteville", PublishDate = key_dates[["this_week"]], RigCount = 0)

rcm <- rcm %>%
  filter(row_number() == 1 | row_number() == n()) %>%
  ungroup()

# Get levels to rank basins
basin_levels <- rcm_current %>%
  arrange(desc(RigCount)) %>%
  pull(Basin) %>%
  unique()

# Rank basins
rcm <- rcm %>%
  mutate(rank = as.integer(factor(Basin, levels = rev(basin_levels), ordered = TRUE)))

# Convert to wide format
rcm <- rcm %>%
  select(-PublishDate) %>%
  group_by(Basin) %>%
  mutate(cat = if_else(RigCount == max(RigCount), "max", "min")) %>%
  ungroup() %>%
  spread(cat, RigCount)

rcm <- rcm %>%
  inner_join(rcm_current, "Basin")

rcm_label <- tibble(x = 267, y = 7.5, label = "= current rig count")

rcm %>%
  ggplot() +
  geom_rect(aes(ymin = rank + 0.25, ymax = rank - 0.25, xmin = min, xmax = max, fill = "1")) +
  geom_rect(aes(ymin = rank + 0.4, ymax = rank - 0.4, xmin = RigCount - 2.5, xmax = RigCount + 2.5), fill = purp3) +
  geom_text(aes(x, y, label = label), rcm_label, nudge_x = 115) +
  geom_rect(aes(ymin = y + 0.25, ymax = y - 0.25, xmin = x - 2.5, xmax = x + 2.5), rcm_label, fill = purp3) +
  scale_y_continuous(breaks = seq_along(basin_levels),
                     labels = rev(basin_levels),
                     expand = expand_scale()) +
  scale_x_continuous(expand = expand_scale(),
                     labels = comma) +
  scale_fill_manual(values = purp0) +
  labs(title = paste0("Room to grow"),
       subtitle = paste0("Rig count ranges per basin", " (",
                         pretty_date(min(rc_master$PublishDate)), " to ",
                         pretty_date(max(rc_master$PublishDate)), ")"),
       caption = paste0("Source: Baker Hughes"),
       x = NULL, y = NULL) +
  guides(fill = FALSE) +
  theme(plot.title = element_text(hjust = -0.85),
        plot.subtitle = element_text(size = 12, hjust = 0.96, margin = margin(7, 0, 5, 0, "pt")),
        plot.caption = element_text(size = 10, color = "gray50", margin = margin(5, 0, 0, 0, "pt")),
        axis.title.x = element_text(size = 14, hjust = 0.3),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        panel.grid.major.y = element_blank())
