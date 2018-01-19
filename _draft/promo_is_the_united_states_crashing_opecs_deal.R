
x <- seq.int(1986, 2017, 1)
x[c(FALSE, TRUE)] <- ""

prod_ridge <- prod %>%
  filter(date <= end_date) %>%
  filter(Name == "United States") %>%
  select(date, production) %>%
  dplyr::union(other_us_prod) %>%
  filter(date >= minest_date) %>%
  mutate(year = year(date)) %>%
  ggplot() +
  geom_density_ridges(aes(production, factor(year)), fill = "#c9b3c3", height = 1) +
  labs(subtitle = paste0("Production",
                         "\nThousand Barrels Per Day"),
       x = NULL, y = NULL) +
  theme(plot.subtitle = element_text(size = 11),
        plot.margin = unit(c(0, 0, 0, 0), "pt"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11)) +
  scale_x_continuous(labels = comma)


# --- Price ridge
price_ridge <- oil_price %>%
  filter(between(date, minest_date, end_date)) %>%
  mutate(year = year(date)) %>%
  ggplot() +
  geom_density_ridges(aes(price, factor(year)), fill = "#66947f") +
  scale_y_discrete(labels = x) +
  labs(subtitle = paste0("Price",
                         "\nDollars ($) Per Barrel"),
       x = NULL, y = NULL) +
  theme(plot.subtitle = element_text(size = 11, margin = margin(b = 8, unit = "pt")),
        plot.margin = unit(c(0, 0, 0, 0), "pt"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))


gb <- ggplot() +
  geom_blank() +
  labs(title = "US Production Follows The Money",
       subtitle = paste0("$50+ oil spurred US production to heights not seen since the mid-1980's."),
       x = NULL, y = "Year",
       caption = paste0("Source: EIA and FRED")) +
  theme(plot.caption = element_text(size = 10, color = "gray50", margin = margin(10)))

# Grobify
gb_grob <- ggplotGrob(gb)

gA <- ggplotGrob(price_ridge)
gB <- ggplotGrob(prod_ridge)

maxWidth = unit.pmin(gA$widths[2:5], gB$widths[2:5])

gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)


# Add grobs
gb_grob$grobs[[which(gb_grob$layout$name == "panel")]] <- gb_grob$grobs[[which(gb_grob$layout$name == "panel")]] %>%
  addGrob(arrangeGrob(gA,gB,
                      layout_matrix = rbind(c(1, 2)),
                      widths = unit(c(215, 215), "pt")
  ))

grid.newpage()
grid.draw(gb_grob)


# Animation ---------------------------------------------------------------


td <- tempdir()

post_cut_tweened <- post_cut %>%
  arrange(date) %>%
  mutate(group = factor(group)) %>%
  split(.$date) %>%
  tween_states(1, 0.7, "cubic-in-out", 200)

map_chr(unique(post_cut_tweened$date),
        ~{
          d <- filter(post_cut_tweened, date == .x)
          gg <- ggplot(d, aes(frame = .frame)) +
            geom_line(aes(date, init_change, color = group)) +
            scale_x_date(limits = c(as.Date("2016-09-01"), as.Date("2017-09-01"))) +
            guides(color = FALSE)
          
          fil <- file.path(td, sprintf("%04d.png", as.integer(.x)))
          ggsave(fil, gg, , units = "in", width = 6, height = 3.5)
          fil
          }) %>%
  map(image_read) %>%
  map(image_scale, geometry = "675x") %>%
  image_join() %>%
  image_animate() %>%
  image_write("post_cut.gif")


# North Sea ---------------------------------------------------------------

ns <- prod %>%
  filter((Name == "United Kingdom" | Name == "Norway") & date <= as.Date("2017-09-01")) %>%
  group_by(date) %>%
  summarize(production = sum(production, na.rm = TRUE),
            Name = "North Sea")

prod %>%
  filter((Name == "United Kingdom" | Name == "Norway") & date <= as.Date("2017-09-01")) %>%
  select(date, production, Name) %>%
  dplyr::union(ns) %>%
  ggplot() +
  geom_line(aes(date, production, group = Name)) +
  labs(x = NULL, y = NULL,
       title = paste0("UK and Norway Oil Production"),
       subtitle = paste0("Thousand Barrels Per Day"),
       caption = paste0("Source: US Energy Information Administration")) +
  theme(plot.caption = element_text(size = 11, color = "gray50"))



# Top 10 Producers --------------------------------------------------------

top_10_prod <- prod %>%
  select(Name, date, production) %>%
  filter(!is.na(production)) %>%
  filter(date == end_date) %>%
  arrange(desc(production, date)) %>%
  slice(1:10) %>%
  rename(current = production) %>%
  select(-date)

top_10_prod_names <- top_10_prod %>%
  pull(Name)

top_10_prod

highest_lowest <- prod %>%
  select(Name, date, production) %>%
  filter(Name %in% top_10_prod_names) %>%
  filter(between(date, start_date, end_date)) %>%
  group_by(Name) %>%
  filter(production == max(production) | production == min(production)) %>%
  group_by(Name, production) %>%
  filter(date == max(date)) %>%
  group_by(Name) %>%
  mutate(cat = if_else(production == max(production), "max", "min")) %>%
  ungroup()

highest_lowest_wide <- highest_lowest %>%
  select(-date) %>%
  spread(cat, production)

blob <- inner_join(top_10_prod, highest_lowest_wide, "Name") %>%
  mutate(Name = if_else(grepl("Iran", Name), "Iran", Name)) %>%
  mutate(Name = if_else(grepl("Russian", Name), "Russia", Name)) %>%
  mutate(rank = dplyr::min_rank(current))

blob_label <- blob %>%
  filter(Name == "United States") %>%
  select(Name, current, rank)

swing <- blob %>%
  mutate(diff = max - current) %>%
  summarize(diff = sum(diff, na.rm = TRUE)) %>%
  pull(diff)

blob %>%
  ggplot() +
  geom_rect(aes(ymin = rank + 0.25, ymax = rank - 0.25, xmin = min, xmax = max, fill = factor(current))) +
  geom_rect(aes(ymin = rank + 0.3, ymax = rank - 0.3, xmin = current - 40, xmax = current + 40), fill = clrs$red) +
  geom_text(aes(x = current, y = rank - 0.85, label = "Current production"), blob_label, family = "Open Sans") +
  geom_curve(aes(x = current - 11, xend = current - 11, y = rank - 0.3, yend = rank - 0.75),
             blob_label, curvature = 0) +
  scale_y_continuous(labels = blob$Name,
                     breaks = 10:1,
                     expand = expand_scale()) +
  scale_x_continuous(expand = expand_scale(),
                     labels = comma) +
  scale_fill_manual(values = colorRampPalette(c(RColorBrewer::brewer.pal(9, "Blues")[c(8)]))(10)) +
  labs(title = paste0("Room to grow"),
       subtitle = paste0("Production ranges for top oil producers (September 2007 - September 2017)"),
       caption = paste0("Source: US EIA"),
       x = paste0("Thousand Barrels Per Day"),
       y = NULL) +
  guides(fill = FALSE) +
  theme(plot.title = element_text(hjust = -0.47),
        plot.subtitle = element_text(size = 12, hjust = 1.95),
        plot.caption = element_text(size = 11, color = "gray50", margin = margin(-11, 0, 0, 0, "pt")),
        axis.title.x = element_text(size = 14, hjust = 0.3),
        axis.text.y = element_text(size = 11, margin = margin(0, 0, 0, 0, "pt")),
        axis.text.x = element_text(size = 11))



# Groups ------------------------------------------------------------------

north_america <- c("United States", "Canada", "Mexico")
soviet <- c("Russian Federation", "Kazakhstan")
other_brics <- c("Brazil", "India", "China", "South Africa")

group_prod <- prod %>%
  select(Name, date, production) %>%
  mutate(group = case_when(
    Name %in% opec          ~ "OPEC",
    Name %in% north_america ~ "North America",
    Name %in% soviet        ~ "FSU",
    Name %in% other_brics   ~ "Other BRICS",
    TRUE                    ~ "All Others"
  ))

group_current <- group_prod %>%
  filter(date == end_date) %>%
  group_by(group) %>%
  summarize(production = sum(production, na.rm = TRUE))

