library(tidyverse)
library(extrafont); loadfonts("win", quiet = TRUE)
library(magick)
library(ggalt)

# http://popstats.unhcr.org/en/asylum_seekers

f <- "C:/Users/Luke/Downloads/unhcr_popstats_export_asylum_seekers_2018_06_17_005947.csv"

asy <- read_csv(f, skip = 3L)

asy_sum <- asy %>%
  group_by(Year, `Country / territory of asylum/residence`, Origin) %>%
  summarize(n = sum(`Applied during year`, na.rm = TRUE)) %>%
  mutate(Asylum = `Country / territory of asylum/residence`)

asy_sum %>%
  ggplot() +
  geom_line(aes(Year, n, color = Asylum, group = Asylum)) +
  scale_color_discrete("Country of Asylum") +
  scale_y_continuous(NULL, labels = scales::comma) +
  scale_x_continuous(NULL) +
  facet_wrap(vars(Origin)) +
  labs(title = "Asylum Applications by Central Americans", 
       caption = "Source: UNHCR") +
  theme_classic(base_family = "Open Sans", base_size = 13) +
  theme(legend.position = "top",
        legend.direction = "horizontal")

f2 <- "C:/Users/Luke/Downloads/unhcr_popstats_export_asylum_seekers_2018_06_17_074206.csv"

asy2 <- read_csv(f2, skip = 3L)

asy2 <- asy2 %>%
  rename(Applied = `Applied during year`,
         Decisions = `Total decisions`,
         Pending_End = `Total pending end-year`)

asy2_summ <- asy2 %>%
  {
    x <- .
  x %>%
    group_by(Year) %>%
    summarize_at(vars(Applied, Decisions, Pending_End), sum, na.rm = TRUE) %>%
    mutate(Origin = "Total") %>%
    bind_rows(x %>% group_by(Origin, Year) %>% summarize_at(vars(Applied, Decisions, Pending_End), sum, na.rm = TRUE))
  }

asy2_summ %>%
  filter(Origin == "Total") %>%
  ggplot() +
  geom_line(aes(Year, Pending_End, color = Origin)) +
  scale_y_continuous(NULL, labels = scales::comma) +
  scale_x_continuous(NULL, breaks = seq(2000, 2016, by = 2)) +
  scale_color_discrete(guide = FALSE) +
  theme_classic()
  



asy2_summ %>%
  filter(Origin != "Total") %>%
  ggplot() +
  geom_line(aes(Year, Decisions, color = Origin)) +
  scale_y_log10(NULL, labels = scales::comma) +
  scale_x_continuous(NULL, breaks = seq(2000, 2016, by = 2)) +
  scale_color_discrete(guide = FALSE) +
  theme_classic()

asy2_summ2 <- asy2_summ %>%
  filter(Origin %in% c("El Salvador", "Guatemala", "Honduras", "Mexico", "Total")) %>%
  group_by(Year, CA = grepl("Salv|Guat|Hond|Mexi", Origin)) %>%
  summarize_at(vars(Applied, Decisions, Pending_End), sum, na.rm = TRUE) %>%
  arrange(CA) %>%
  mutate(pct_Applied = Applied / lag(Applied),
         pct_Decisions = Decisions / lag(Decisions),
         pct_Pending_End = Pending_End / lag(Pending_End))

ls <- 0.9
fs <- 16
asy_plots <- vector("list", 3L)

asy_plots[[1]] <- asy2_summ2 %>%
  filter(CA == FALSE) %>%
  select(Year, Applied:Pending_End) %>%
  gather(... = -Year) %>%
  mutate(key = if_else(key == "Pending_End", "Pending (Year End)", key)) %>%
  mutate(key = if_else(key == "Applications", "Applications", key)) %>%
  ggplot() +
  geom_xspline(aes(Year, value, color = key), size = ls) +
  scale_x_continuous(NULL) +
  scale_y_continuous(NULL, labels = scales::comma) +
  scale_color_discrete("Asylum Status") +
  labs(title = "Total") +
  theme_classic(base_family = "Lato", base_size = fs)

asy_plots[[2]] <- asy2_summ2 %>%
  filter(CA == TRUE) %>%
  select(Year, Applied:Pending_End) %>%
  gather(... = -Year) %>%
  ggplot() +
  geom_xspline(aes(Year, value, color = key), size = ls) +
  scale_x_continuous(NULL) +
  scale_y_continuous(NULL, labels = scales::comma) +
  labs(title = "Latin American*") +
  theme_classic(base_family = "Lato", base_size = fs)

asy_plots[[3]] <- asy2_summ2 %>%
  filter(CA == TRUE) %>%
  select(Year, pct_Applied:pct_Pending_End) %>%
  gather(... = -Year) %>%
  ggplot() +
  geom_xspline(aes(Year, value, color = key), size = ls) +
  scale_x_continuous(NULL) +
  scale_y_continuous(NULL, labels = scales::percent) +
  labs(title = "Percent Latin American") +
  theme_classic(base_family = "Lato", base_size = fs)

p <- lemon::grid_arrange_shared_legend(asy_plots[[1]], asy_plots[[2]], asy_plots[[3]], nrow = 1)

ggsave(f3 <-"C:/Users/Luke/Downloads/asylum.png", p, width = 12, height = 6, dpi = 300)

x <- image_read(f3)

x <- x %>%
  image_resize("850x420")

header <- image_blank(850, 40, "white") %>%
  image_annotate("Asylum Seekers To The United States",
                 location = geometry_point(5, 5),
                 color = "black", font = "Lato", size = 22) 

cap <- image_blank(850, 23, "white")
   
header %>%
  image_join(x, cap) %>%
  image_append(stack = TRUE) %>%
  image_annotate("Source: UNHCR",
                 location = geometry_point(5, 448),
                 color = "gray30", font = "Lato", size = 12) %>%
  image_annotate("*Latin American countries include: El Salvador, Guatemala, Honduras, and Mexico",
                 location = geometry_point(5, 462),
                 color = "gray30", font = "Lato", size = 12) %>%
  image_write(f3)



p2 <- asy2_summ %>%
  filter(Origin %in% c("El Salvador", "Guatemala", "Honduras", "Mexico")) %>%
  gather(... = -c(Year, Origin)) %>%
  mutate(key = case_when(
    key == "Applied" ~ "New Applications",
    key == "Decisions" ~ "Decisions",
    key == "Pending_End" ~ "Pending Applications (End of Year)"
  )) %>%
  ggplot() +
  geom_line(aes(Year, value, color = Origin, group = Origin)) +
  scale_x_continuous(NULL, breaks = seq(2000, 2016, by = 2)) +
  scale_y_continuous(NULL, labels = scales::comma) +
  scale_color_discrete(guide = FALSE) +
  labs(title = "Asylum Seekers From Latin America\nTo The United States",
       caption = "Source: UNHCR") +
  facet_wrap(vars(key), nrow = 3, scales = "free_y") +
  theme_classic(base_family = "Lato", base_size = fs) +
  theme(legend.position = "top", legend.direction = "horizontal")

ggsave(f4 <-"C:/Users/Luke/Downloads/asylum2.png", p2, width = 6, height = 12, dpi = 300)

x2 <- image_read(f4)
