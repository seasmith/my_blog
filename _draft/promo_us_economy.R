
# Load Dependencies -------------------------------------------------------


library(tidyverse)
library(lubridate)
library(httr)
library(rvest)
library(jsonlite)
library(extrafont)
loadfonts("win", T)
library(grid)
library(magick)

index <- function(x) {
  min_x <- unique(min(x))
  max_x <- unique(max(x))
  vapply(X = x,
         FUN = function(value) (value - min_x) / (max_x - min_x),
         FUN.VALUE = numeric(1))
}

# Import ------------------------------------------------------------------

# What we will be importing: series ids
vars <- c("CPIAUCSL", "A191RL1Q225SBEA",
          "INDPRO", "DGS10", "DEXUSEU", 
          "UNRATE", "PAYEMS", "IC4WSA")

# URL
api_url  <- "https://api.stlouisfed.org/fred/"

# Path
req_type <- "series/observations"

# Query keys
api_key_fred <- getOption("api_key_fred")

# Prep for loop through series ids
url <- parse_url(api_url)
url$path  <- paste0(url$path, req_type)

# Loop
urls <- vars %>%
  map(~{
    
    # Inflation (CPI) is special
    if (.x == "CPIAUCSL") {
      .y$query <- list(series_id = .x,
                       api_key = api_key_fred,
                       file_type = "json",
                       frequency = "q",
                       units = "pc1")  # We need percent change from year ago
    } else {
      .y$query <- list(series_id = .x,
                       api_key = api_key_fred,
                       file_type = "json",
                       frequency = "q")
    }
    
    .y
    build_url(.y)
    
  }, .y = url)

# Ge the list of data frames (ldf)
ldf <- urls %>%
  map(read_html) %>%
  map(html_text) %>%
  map(fromJSON) %>%
  map(~as_tibble(.x$observations))



# Tidy --------------------------------------------------------------------

# Assign names and bind data frames with `.id = "series"` (vars)
names(ldf) <- vars
ldf <- bind_rows(ldf, .id = "series")

# Get only what we need, ftmp
ldf <- ldf %>%
  select(-realtime_start:-realtime_end) %>%  # don't care about these columns
  mutate(value = as.double(value),  # care about the type here
         date = as.Date(date))  # and here


# Plot Prep ---------------------------------------------------------------

bh_with_name <- 15.6  # barheight with name
bh_without_name <- 16.4  # barheight without name (title = NULL)


# Theme
theme_g10 <- function (base_size = 11, base_family = "Open Sans", base_line_size = base_size/22,
                       base_rect_size = base_size/22)
{
  half_line <- base_size/2
  theme(line = element_line(colour = "gray50", size = base_line_size,
                            linetype = 1, lineend = "butt"),
        rect = element_rect(fill = "gray10",
                            colour = "gray10", size = base_rect_size, linetype = 1),
        text = element_text(family = base_family, face = "plain",
                            colour = "gray90", size = base_size, lineheight = 0.9,
                            hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                            debug = FALSE),
        axis.line = element_blank(), axis.line.x = NULL,
        axis.line.y = NULL,
        axis.text = element_text(size = rel(0.9),
                                 colour = "gray90"),
        axis.text.x = element_text(margin = margin(t = 0.8 * half_line/4), vjust = 0.5),
        axis.text.x.top = element_text(margin = margin(b = 0.8 * half_line/4), vjust = 0),
        axis.text.y = element_text(margin = margin(r = 0.8 * half_line/4), hjust = 1),
        axis.text.y.right = element_text(margin = margin(l = 0.8 * half_line/4), hjust = 0),
        axis.ticks = element_line(colour = "gray50"),
        axis.ticks.length = unit(half_line/2, "pt"),
        axis.title.x = element_text(margin = margin(t = half_line/1.5), vjust = 1),
        axis.title.x.top = element_text(margin = margin(b = half_line/1.5), vjust = 0),
        axis.title.y = element_text(angle = 90, margin = margin(r = half_line/1.5), vjust = 1),
        axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line/1.5), vjust = 0),
        legend.background = element_rect(),
        legend.spacing = unit(1, "lines"),
        legend.spacing.x = NULL,
        legend.spacing.y = NULL,
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "lines"),
        legend.key = element_rect(fill = "gray10", colour = "gray90"),
        legend.key.size = unit(1.2, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = rel(0.9)),
        legend.text.align = NULL,
        legend.title = element_text(hjust = 0),
        legend.title.align = NULL,
        legend.position = "right",
        legend.direction = NULL,
        legend.justification = "center",
        legend.box = NULL,
        legend.box.margin = margin(0, 0, 0, 0, "lines"),
        legend.box.background = element_blank(),
        legend.box.spacing = unit(0.5, "lines"),
        panel.background = element_rect(),
        panel.border = element_blank(),
        panel.grid = element_line(),
        panel.grid.minor = NULL,
        panel.spacing = unit(half_line/4, "lines"),
        panel.spacing.x = NULL,
        panel.spacing.y = NULL,
        panel.ontop = FALSE,
        strip.background = element_rect(),
        strip.text = element_text(size = rel(0.9),
                                  margin = margin(half_line, half_line, half_line, half_line)),
        strip.text.x = NULL,
        strip.text.y = element_text(angle = -90),
        strip.placement = "inside",
        strip.placement.x = NULL,
        strip.placement.y = NULL,
        strip.switch.pad.grid = unit(0.1, "cm"),
        strip.switch.pad.wrap = unit(0.1, "cm"),
        plot.background = element_rect(),
        plot.title = element_text(size = rel(1.4), hjust = 0, vjust = 0.5, margin = margin(b = half_line * 1.4)),
        plot.subtitle = element_text(size = rel(1.1), hjust = 0, vjust = 0.5, margin = margin(b = half_line * 1.1)),
        plot.caption = element_text(size = rel(0.7), color = "gray50",
                                    hjust = 1, vjust = 1, margin = margin(t = half_line * 0.7)),
        plot.margin = margin(half_line/2, half_line/2, half_line/2, half_line/2),
        complete = TRUE)
}

# Last date with value will be the end date
max_date <- ldf %>%
  group_by(series) %>%
  filter(!is.na(value)) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  pull(date) %>%
  unique() %>%
  sort()

# Pick a starting point
min_date <- max_date - years(16)

# Start from there
pdf <- ldf %>%
  filter(date <= max_date)

# Factor order
series_order <- c("A191RL1Q225SBEA", "DGS10",
                  "INDPRO", "PAYEMS", "DEXUSEU",
                  "CPIAUCSL", "IC4WSA", "UNRATE")

series_order2 <- c("A191RL1Q225SBEA", "IC4WSA", "UNRATE",
                   "DGS10", "INDPRO", "PAYEMS", "DEXUSEU",
                   "CPIAUCSL")

# Maybe keep these
rm_series <- c("A191RL1Q225SBEA", "PAYEMS")

# Actual names help
nms <- c("Real GDP", "10-Year Treasury", "Industrial Production",
         "Payroll Employment", "US/Euro", "Inflation",
         "Initial Claims", "Unemployment")

nms2 <- c("Real GDP", "Initial Claims", "Unemployment",
          "10-Year Treasury", "Industrial Production",
          "Payroll Employment", "US/Euro", "Inflation")

rm_nms <- c("Real GDP", "Payroll Employment")

# These look like good width and height
w <- 850 #846
h <- 380 #363


#   -----------------------------------------------------------------------
# Long date range ---------------------------------------------------------
#   -----------------------------------------------------------------------

# UNMODIFIED --------------------------------------------------------------

long_plot <- pdf %>%
  filter(date >= min_date) %>%
  group_by(series) %>%
  arrange(date) %>%
  mutate(Index = index(value)) %>%
  ungroup() %>%
  mutate(series = factor(series, levels = rev(series_order), ordered = TRUE)) %>%
  ggplot() + geom_raster(aes(date, series, fill = Index), interpolate = TRUE) +
  scale_fill_viridis_c(option = "D") +
  scale_y_discrete(NULL, expand = expand_scale(), labels = rev(nms)) +
  scale_x_date(NULL, expand = expand_scale()) +
  guides(fill = guide_colorbar(title = NULL, raster = FALSE, ticks = FALSE,
                               barwidth = 1, barheight = 16.4)) +
  labs(title = "Indexed Economic Indicators") +
  theme_g10() +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank())

lp_grob <- ggplotGrob(long_plot)

lp_grob$layout$l[which(lp_grob$layout$name == "title")] <- 1

# grid.newpage()
# grid.draw(lp_grob)
ggsave("_plots/econ_indicators1.png", lp_grob, width = w / 100, height = h / 100, dpi = 100)

base_plot <- image_read("_plots/econ_indicators1.png")
base_title <- image_blank(850, 50, "gray10") %>%
  image_annotate("How did the events of the housing recession play out?",
                 location = geometry_point(-1, 0),
                 font = "Open Sans", color = "gray90", size = 33)
base_caption <- image_blank(850, 20, "gray10") %>%
  image_annotate("Source: Federal Reserve Economic Data",
                 font = "Open Sans", color = "gray50", size = 12) %>%
  image_annotate("Luke Smith (@lksmth)", location = geometry_point(715, 0),
                 font = "Open Sans", color = "springgreen", size = 13)

base_plot %>%
  image_join(base_title,
             .,
             base_caption) %>%
  image_append(stack = TRUE) %>%
  image_join(image_blank(3, 450, "gray10"), .) %>%
  image_append() %>%
  image_write("_plots/housing_recession.png")



# MODIFIED DATA -----------------------------------------------------------
## Change unemployment and initial claims to peak early

long_plot2 <- pdf %>%
  filter(date >= min_date) %>%
  mutate(value = if_else(series == "UNRATE", 100 - value, value),
         value = if_else(series == "IC4WSA", -value, value)) %>%
  group_by(series) %>%
  arrange(date) %>%
  mutate(Index = index(value)) %>%
  ungroup() %>%
  mutate(series = factor(series, levels = rev(series_order2), ordered = TRUE)) %>%
  ggplot() + geom_raster(aes(date, series, fill = Index), interpolate = TRUE) +
  scale_fill_viridis_c() +
  scale_y_discrete(NULL, expand = expand_scale(), labels = rev(nms2)) +
  scale_x_date(NULL, expand = expand_scale()) +
  guides(fill = guide_colorbar(title = NULL, raster = FALSE, ticks = FALSE,
                               barwidth = 1, barheight = 16.4)) +
  labs(title = "Indexed Economic Indicators") +
  theme_g10() +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank())

lp_grob2 <- ggplotGrob(long_plot2)

lp_grob2$layout$l[which(lp_grob2$layout$name == "title")] <- 1

# grid.newpage()
# grid.draw(lp_grob2)
ggsave("_plots/econ_indicators2.png", lp_grob2, width = w / 100, height = h / 100, dpi = 100)

base_plot <- image_read("_plots/econ_indicators2.png")
base_title <- image_blank(850, 50, "gray10") %>%
  image_annotate("How did the events of the housing recession play out?",
                 location = geometry_point(-1, 0),
                 font = "Open Sans", color = "gray90", size = 33)
base_caption <- image_blank(850, 20, "gray10") %>%
  image_annotate("Source: Federal Reserve Economic Data",
                 font = "Open Sans", color = "gray50", size = 12) %>%
  image_annotate("Luke Smith (@lksmth)", location = geometry_point(715, 0),
                 font = "Open Sans", color = "springgreen", size = 13)

base_plot %>%
  image_join(base_title,
             .,
             base_caption) %>%
  image_append(stack = TRUE) %>%
  image_join(image_blank(3, 450, "gray10"), .) %>%
  image_append() %>%
  image_write("_plots/housing_recession2.png")




#   -----------------------------------------------------------------------
# Short date range --------------------------------------------------------
#   -----------------------------------------------------------------------

# UNMODIFIED DATA ---------------------------------------------------------
# Pick a difference end date to remove recent highs/lows

short_plot <- pdf %>%
  filter(between(date, as.Date("2005-01-01"), as.Date("2012-01-01"))) %>%
  group_by(series) %>%
  arrange(date) %>%
  mutate(Index = index(value)) %>%
  ungroup() %>%
  mutate(series = factor(series, levels = rev(series_order), ordered = TRUE)) %>%
  ggplot() + geom_raster(aes(date, series, fill = Index), interpolate = TRUE) +
  scale_fill_viridis_c() +
  scale_y_discrete(NULL, expand = expand_scale(), labels = rev(nms)) +
  scale_x_date(NULL, expand = expand_scale()) +
  guides(fill = guide_colorbar(title = NULL, raster = FALSE, ticks = FALSE,
                               barwidth = 1, barheight = 16.4)) +
  labs(title = "Indexed Economic Indicators") +
  theme_g10() +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank())

sp_grob <- ggplotGrob(short_plot)

sp_grob$layout$l[which(sp_grob$layout$name == "title")] <- 1

# grid.newpage()
# grid.draw(sp_grob)
ggsave("_plots/econ_indicators3.png", sp_grob, width = w / 100, height = h / 100, dpi = 100)

base_plot <- image_read("_plots/econ_indicators3.png")
base_title <- image_blank(850, 50, "gray10") %>%
  image_annotate("How did the events of the housing recession play out?",
                 location = geometry_point(-1, 0),
                 font = "Open Sans", color = "gray90", size = 33)
base_caption <- image_blank(850, 20, "gray10") %>%
  image_annotate("Source: Federal Reserve Economic Data",
                 font = "Open Sans", color = "gray50", size = 12) %>%
  image_annotate("Luke Smith (@lksmth)", location = geometry_point(715, 0),
                 font = "Open Sans", color = "springgreen", size = 13)

base_plot %>%
  image_join(base_title,
             .,
             base_caption) %>%
  image_append(stack = TRUE) %>%
  image_join(image_blank(3, 450, "gray10"), .) %>%
  image_append() %>%
  image_write("_plots/housing_recession3.png")



# MODIFIED DATA -----------------------------------------------------------
# Change unemployment and initial claims to peak early

short_plot2 <- pdf %>%
  filter(between(date, as.Date("2005-01-01"), as.Date("2012-01-01"))) %>%
  mutate(value = if_else(series == "UNRATE", 100 - value, value),
         value = if_else(series == "IC4WSA", -value, value)) %>%
  group_by(series) %>%
  arrange(date) %>%
  mutate(Index = index(value)) %>%
  ungroup() %>%
  mutate(series = factor(series, levels = rev(series_order2), ordered = TRUE)) %>%
  # filter(!series %in% rm_series) %>%
  ggplot() + geom_raster(aes(date, series, fill = Index), interpolate = TRUE) +
  scale_fill_viridis_c() +
  scale_y_discrete(NULL, expand = expand_scale(), labels = rev(nms2)) +
  scale_x_date(NULL, expand = expand_scale()) +
  guides(fill = guide_colorbar(title = NULL, raster = FALSE, ticks = FALSE,
                               barwidth = 1, barheight = 16.4)) +
  labs(title = "Indexed Economic Indicators") +
  theme_g10() +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank())

sp_grob2 <- ggplotGrob(short_plot2)

sp_grob2$layout$l[which(sp_grob2$layout$name == "title")] <- 1

# grid.newpage()
# grid.draw(sp_grob2)
ggsave("_plots/econ_indicators4.png", sp_grob2, width = w / 100, height = h / 100, dpi = 100)

base_plot <- image_read("_plots/econ_indicators4.png")
base_title <- image_blank(850, 50, "gray10") %>%
  image_annotate("How did the events of the housing recession play out?",
                 location = geometry_point(-1, 0),
                 font = "Open Sans", color = "gray90", size = 33)
base_caption <- image_blank(850, 20, "gray10") %>%
  image_annotate("Source: Federal Reserve Economic Data",
                 font = "Open Sans", color = "gray50", size = 12) %>%
  image_annotate("Luke Smith (@lksmth)", location = geometry_point(715, 0),
                 font = "Open Sans", color = "springgreen", size = 13)

base_plot %>%
  image_join(base_title,
             .,
             base_caption) %>%
  image_append(stack = TRUE) %>%
  image_join(image_blank(3, 450, "gray10"), .) %>%
  image_append() %>%
  image_write("_plots/housing_recession4.png")
