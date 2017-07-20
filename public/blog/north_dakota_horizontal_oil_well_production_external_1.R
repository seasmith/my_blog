library(ggplot2)

load("~/R/cum_percent.RData")
load("~/R/ind_percent.RData")

ggsave("~/R/percents.png",
       grid.arrange(cum_percent, ind_percent, ncol = 2),
       width = 8,
       height = 4)

file.remove("~/R/my_blog/content/blog/north_dakota_horizontal_oil_well_production_files/figure-html/Thumbs.db")
file.remove("~/R/my_blog/public/blog/north_dakota_horizontal_oil_well_production_files/figure-html/Thumbs.db")
file.copy("~/R/percents.png", "north_dakota_horizontal_oil_well_production_files/figure-html/percents.png")
