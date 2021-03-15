library(ggplot2)
library(maptools)
library(dplyr)
library(lubridate)
library(data.table)

source("functions/stat-bar-tile-etho.R")

tuco_plot = readRDS("data/rds/tuco_01hz_vedba_lux.rds")

tuco_plot[,time := round(hour(datetime) + minute(datetime)/60 + second(datetime)/3600, 4)]
tuco_plot[,date := lubridate::date(datetime)]
tuco_plot[,lux := ifelse(lux >= 3.4, 1, 0)]
tuco_plot[,lux := ifelse(is.na(tuco_plot$lux), 0, lux)]
tuco_plot$ID =  factor(tuco_plot$ID)

c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
    a <- as.trans(a)
    b <- as.trans(b)
    
    name <- paste(a$name, b$name, sep = "-")
    
    trans <- function(x) a$trans(b$trans(x))
    inv <- function(x) b$inverse(a$inverse(x))
    
    trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
}

rev_date <- c_trans("reverse", "date")

width = resolution(tuco_plot$time, F) + 0.008

actograms =
    ggplot(data = tuco_plot, aes(x = time, y = date)) +
    scale_x_continuous(limits = c(0,24), breaks = c(0,6,12,18,24)) +
    scale_y_continuous(trans = rev_date) +
    geom_bar_tile(aes(height = lux), width = width, fill = "orange", alpha = 0.5) +
    geom_bar_tile(aes(height = vedba), width = width) +
    facet_wrap(facets = vars(ID), scales = "free_y", ncol = 3) +
    xlab("") +
    ylab("") + 
    theme_bw()

sexlabels = unique(tuco_plot %>% select(ID, sex)) %>% mutate(sex = if_else(sex == "m", "♂", "♀"))
actograms = actograms + geom_text(x = Inf, y = Inf, 
                                  aes(label = sex, color = sex), 
                                  data = sexlabels, vjust = 1.2, hjust = 1.2, 
                                  fontface = "bold", size = 5, family = "Arial Unicode MS") + 
    theme(legend.position = "none")

ggsave(filename = "04_plots/actograms.png",plot = actograms, device = "png", dpi = 320, width = 210, height = 290, units = "mm")

