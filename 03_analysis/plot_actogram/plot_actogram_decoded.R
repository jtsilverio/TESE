library(ggplot2)
library(maptools)
library(dplyr)
library(lubridate)
library(data.table)
library(scales)
library(egg)
source("03_analysis/plot_actogram/stat-bar-tile-etho.R") # From ggetho
source("03_analysis/plot_actogram/stat-tile-etho.R") # From ggetho

# Read Data
tuco_plot = readRDS("01_data/rds/tuco_preprocessed.rds")
tuco_decoded = readRDS("01_data/rds/tuco_decoded.rds")
tuco_plot = merge.data.table(tuco_plot, tuco_decoded, by = c("ID","datetime"))

# Separate Date and Time columns
tuco_plot[,time := round(hour(datetime)*60 + minute(datetime) + second(datetime)/60, 4)]
tuco_plot[,date := lubridate::date(datetime)]

# Transform Lux into 0/1
tuco_plot[,lux := ifelse(lux >= 2, 1, 0)]
tuco_plot[,lux := ifelse(is.na(tuco_plot$lux), 0, lux)]

# Transformation function to invert datetime into ggplot
# From: 
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
    a <- as.trans(a)
    b <- as.trans(b)
    
    name <- paste(a$name, b$name, sep = "-")
    
    trans <- function(x) a$trans(b$trans(x))
    inv <- function(x) b$inverse(a$inverse(x))
    
    trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
}
rev_date <- c_trans("reverse", "date")

# Actograms Activity Bar -------------------------------------------------------

actograms =
    ggplot(data = tuco_plot[state %in% c("2","3")], aes(x = time, y = date)) +
    scale_x_continuous(limits = c(0, 1440), breaks = c(0,360,720,1080,1440), labels = c(0,6,12,18,24)) +
    scale_y_continuous(trans = rev_date) +
    geom_bar_tile(mapping = aes(height = lux), fill = "orange", alpha = 0.2, width = 5) +
    geom_bar_tile(mapping = aes(height = vedba, fill = state,colour = state), width = 1) +
    #stat_bar_tile(data = tuco_plot[state == 2],fill = "blue" ,mapping = aes(z = vedba), width = 1) +
    #stat_bar_tile_etho(data = tuco_plot[state == 3],fill = "red" ,mapping = aes(z = vedba), width = 1) +
    facet_wrap(~ID, scales = "free_y", ncol = 3) +
    xlab("") +
    ylab("") + 
    theme_article() +
    theme(panel.grid.major.y = element_line(color = "grey95")) +
    theme(legend.position = "none")
actograms 

sexlabels = unique(tuco_plot %>% select(ID, sex)) %>% mutate(sex = if_else(sex == "m", "♂", "♀"))
actograms = actograms + geom_text(x = Inf, y = Inf, 
                                  aes(label = sex, color = sex), 
                                  data = sexlabels, vjust = 1.2, hjust = 1.2, 
                                  fontface = "bold", size = 5, family = "Arial Unicode MS") + 
    theme(legend.position = "none")

ggsave(filename = "actograms.png", path = "04_figures/",plot = actograms, device = "png", dpi = 132, width = 210, height = 290, units = "mm")

# Actograms Activity Heatmap ---------------------------------------------------

# actograms_heatmap =
#     ggplot(data = tuco_plot, aes(x = time, y = date)) +
#     scale_x_continuous(limits = c(0, 1440), breaks = c(0,360,720,1080,1440), labels = c(0,6,12,18,24)) +
#     scale_y_continuous(trans = rev_date) +
#     #geom_bar_tile(aes(height = lux), width = 5, fill = "orange", alpha = 0.9) +
#     #geom_bar_tile(aes(height = vedba), width = 1) +
#     stat_tile_etho(mapping = aes(z = vedba), width = 1) +
#     facet_wrap(facets = vars(ID), scales = "free_y", ncol = 3) +
#     xlab("VeDBA") +
#     ylab("") + 
#     theme_article() +
#     theme(panel.grid.major.y = element_line(color = "grey95")) +
#     theme(legend.position = "bottom")
# 
# sexlabels = unique(tuco_plot %>% select(ID, sex)) %>% mutate(sex = if_else(sex == "m", "♂", "♀"))
# actograms_heatmap = actograms_heatmap + geom_text(x = Inf, y = Inf, 
#                                                   aes(label = sex, color = sex), 
#                                                   data = sexlabels, vjust = 1.2, hjust = 1.2, 
#                                                   fontface = "bold", size = 5, family = "Arial Unicode MS")
# 
# ggsave(filename = "actograms_heatmap.png", path = "04_figures/",plot = actograms_heatmap, device = "png", dpi = 132, width = 210, height = 290, units = "mm")

# Actograms Temp Heatmap -------------------------------------------------------

# actograms_temp =
#     ggplot(data = tuco_plot, aes(x = time, y = date)) +
#     scale_x_continuous(limits = c(0, 1440), breaks = c(0,360,720,1080,1440), labels = c(0,6,12,18,24)) +
#     scale_y_continuous(trans = rev_date) +
#     #geom_bar_tile(aes(height = lux), width = 5, fill = "orange", alpha = 0.9) +
#     #geom_bar_tile(aes(height = vedba), width = 1) +
#     stat_tile_etho(mapping = aes(z = temp), width = 1) +
#     facet_wrap(facets = vars(ID), scales = "free_y", ncol = 3) +
#     xlab("Collar Temp.") +
#     ylab("") + 
#     theme_article() +
#     theme(panel.grid.major.y = element_line(color = "grey95")) +
#     theme(legend.position = "bottom")
# 
# sexlabels = unique(tuco_plot %>% select(ID, sex)) %>% mutate(sex = if_else(sex == "m", "♂", "♀"))
# actograms_temp = actograms_temp + geom_text(x = Inf, y = Inf, 
#                                             aes(label = sex, color = sex), 
#                                             data = sexlabels, vjust = 1.2, hjust = 1.2, 
#                                             fontface = "bold", size = 5, family = "Arial Unicode MS")
# 
# ggsave(filename = "actograms_temp.png", path = "04_figures/",plot = actograms_temp, device = "png", dpi = 132, width = 210, height = 290, units = "mm")
