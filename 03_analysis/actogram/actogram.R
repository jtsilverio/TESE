library(momentuHMM)
library(patchwork)
require(egg)
library(dplyr)
library(showtext)
library(lubridate)
library(scales)
source("03_analysis/actogram/stat-bar-tile-etho.R") # From ggetho
source("03_analysis/actogram/stat-tile-etho.R") # From ggetho 
source(file = "03_analysis/actogram/function_actogram.R")
tuco = readRDS("01_data/activity_processed/tuco_processed.rds")
m2 = readRDS("03_analysis/hmm/m2.rds")

decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("Low","Medium","High"))

# Set Theme ----------------------------------------------------------
font_add_google("Roboto Condensed", "roboto")
showtext_auto()

tuco_pal = c("Low" = "#66C2A5",
             "Medium" = "#5a69af",
             "High" = "#c25b67",
             "General Activity" = "#393939")

theme_tuco =
    egg::theme_article() +
    theme(text = element_text(size = 14,
                              family = "roboto")
    )

theme_set(theme_tuco)

# Transformation function to invert datetime in ggplot
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
    a <- scales::as.trans(a)
    b <- scales::as.trans(b)
    
    name <- paste(a$name, b$name, sep = "-")
    
    trans <- function(x) a$trans(b$trans(x))
    inv <- function(x) b$inverse(a$inverse(x))
    
    scales::trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
}
rev_date <- c_trans("reverse", "date")

# Calculate Sunrise and Sunset Times -------------------------------------------
anillaco = matrix(c(-66.95, -28.8), nrow = 1) 
sunriset = tuco %>% dplyr::select(ID, season, datetime) %>%
    group_by(ID, season) %>% 
    summarise(datetime = median(datetime))

sunriset$dawn = maptools::crepuscule(crds = anillaco,
                                     dateTime = sunriset$datetime,
                                     solarDep = 6, 
                                     direction = "dawn", 
                                     POSIXct.out=TRUE)$day_frac  * 1440

sunriset$dusk = maptools::crepuscule(crds = anillaco,
                                     dateTime = sunriset$datetime,
                                     solarDep = 6,
                                     direction = "dusk",
                                     POSIXct.out=TRUE)$day_frac  * 1440


actograms_lux = tuco %>% 
    filter(ID %in% c("JUL16","JUL17","JUL18","JUL20","JUL21","JUL23","OCT01","OCT10","FEV01","FEV02","FEV03","FEV05","FEV06")) %>%
    mutate(date = date(datetime), 
           aboveground = as.numeric(aboveground)) %>% 
    mutate(aboveground = if_else(is.na(aboveground), 0, aboveground)) %>% 
    ggplot(aes(x = time, y = date, group = ID)) +
    geom_vline(data = sunriset,
              aes(xintercept = dawn), color = "grey60", linetype = 2, size = 0.5) +
    geom_vline(data = sunriset,
              aes(xintercept = dusk),  color = "grey60", linetype = 2, size = 0.5) +
    geom_bar_tile(mapping = aes(height = aboveground), fill = "orange", alpha = 0.9, width = 5) +
    scale_x_continuous(limits = c(0, 1440), breaks = c(0,360,720,1080,1440), labels = c(0,6,12,18,24)) +
    scale_y_continuous(trans = rev_date) + 
    facet_wrap(~ID, scales = "free_y", ncol = 3) +
    xlab("") +
    ylab("") + 
    #theme_article() +
    theme(panel.grid.major.y = element_line(color = "grey95"))

actograms_vedba = plot_actogram(tuco, plot_days = 5)
actograms_high = plot_actogram(tuco, height = "High", plot_days = 5)
actograms_medium = plot_actogram(tuco, height = "Medium", plot_days = 5)
actograms_low = plot_actogram(tuco, height = "Low", plot_days = 5)


ggsave(filename = "04_figures/actograms/actograms_vedba.jpeg", plot = actograms_vedba, device = "jpeg", dpi = 132, width = 210, height = 290, units = "mm", bg = "white")
ggsave(filename = "04_figures/actograms/actograms_high.jpeg",  plot = actograms_high, device = "jpeg", dpi = 132, width = 210, height = 290, units = "mm", bg = "white")
ggsave(filename = "04_figures/actograms/actograms_medium.jpeg",plot = actograms_medium, device = "jpeg", dpi = 132, width = 210, height = 290, units = "mm", bg = "white")
ggsave(filename = "04_figures/actograms/actograms_low.jpeg",plot = actograms_low, device = "jpeg", dpi = 132, width = 210, height = 290, units = "mm", bg = "white")
ggsave(filename = "04_figures/actograms/actograms_lux.jpeg",plot = actograms_lux, device = "jpeg", dpi = 132, width = 210, height = 290, units = "mm", bg = "white")
