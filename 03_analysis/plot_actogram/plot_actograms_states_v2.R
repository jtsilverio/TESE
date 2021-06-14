library(ggplot2)
library(maptools)
library(dplyr)
library(lubridate)
library(data.table)
library(scales)
library(egg)
library(data.table)
library(momentuHMM)
source("03_analysis/plot_actogram/stat-bar-tile-etho.R") # From ggetho
source("03_analysis/plot_actogram/stat-tile-etho.R") # From ggetho

# Read Data
tuco = readRDS("01_data/activity_processed/tuco_preprocessed.rds")
m2 = readRDS("03_analysis/hmm/m2.rds") # modelo com estação
decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("rest","medium","high"))

# number of days to show on actogram
ndays = 5
tuco = tuco[day_number <= ndays]

# Fill animals that have less than 'ndays'
tuco_to_fill = tuco[, .(to_fill = max(day_number) < ndays, max = max(day_number)), by = ID]
tuco_to_fill = droplevels(tuco_to_fill[tuco_to_fill$to_fill])
tuco_to_fill$max_datetime = tuco[ID %in% tuco_to_fill$ID][, .(max_datetime = max(datetime)), by = ID]$max_datetime

# Create sequence of dates to fill
seq_date = vector("list") # Initialize a empty list
for (i in tuco_to_fill$ID) {
    max_datetime = tuco_to_fill[ID == i]$max_datetime
    fill_to = max_datetime + days(ndays - tuco_to_fill[ID == i]$max)
    seq_date[[i]] = seq(from = max_datetime, to = fill_to, by = "1 min")
}

max_nrows = max(sapply(seq_date, length, simplify = T))
seq_date = lapply(seq_date, 
                  function(x){
                      if(length(x) < max_nrows){
                          return(c(x,rep(NA,max_nrows - length(x))))
                      }else{
                          return(x)
                      }
                  }
)

# Merge new sequence of datetimes to original dataset
seq_date = setDT(seq_date) 
seq_date = melt(seq_date, measure.vars = colnames(seq_date), variable.name = "ID", value.name = "datetime")
tuco = data.table::merge.data.table(tuco, seq_date, by = c("ID","datetime"), all = T)
tuco[is.na(tuco$vedba)]$vedba = 0

# Separate Date and Time columns
tuco[,time := round(hour(datetime)*60 + minute(datetime) + second(datetime)/60, 4)]
tuco[,date := lubridate::date(datetime)]

# Transform Lux into 0/1
tuco[,lux := ifelse(lux >= 2, 1, 0)]
tuco[,lux := ifelse(is.na(tuco$lux), 0, lux)]

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

# Actograms Activity Bar Medium ------------------------------------------------
actograms =
    ggplot(data = tuco[state %in% c("medium")], aes(x = time, y = date)) +
    scale_x_continuous(limits = c(0, 1440), breaks = c(0,360,720,1080,1440), labels = c(0,6,12,18,24)) +
    scale_y_continuous(trans = rev_date) +
    geom_bar_tile(data = tuco, mapping = aes(height = lux), fill = "orange", alpha = 0.4, width = 5) +
    geom_bar_tile(mapping = aes(height = as.numeric(state)), width = 1) +
    facet_wrap(~ID, scales = "free_y", ncol = 3) +
    xlab("") +
    ylab("") + 
    theme_article() +
    theme(panel.grid.major.y = element_line(color = "grey95")) +
    theme(legend.position = "none") +
    ggtitle("Medium Activity Only")

sexlabels = unique(tuco %>% select(ID, sex)) %>% mutate(sex = if_else(sex == "m", "♂", "♀"))
actograms = actograms + geom_text(x = Inf, y = Inf, 
                                  aes(label = sex), 
                                  data = sexlabels, vjust = 1.2, hjust = 1.2, 
                                  size = 3, family = "Arial Unicode MS") + 
    theme(legend.position = "none")

ggsave(filename = "actograms_states_medium_v2.png", path = "04_figures/actograms/",plot = actograms, device = "png", dpi = 132, width = 210, height = 290, units = "mm")

# Actograms Activity Bar HIGH --------------------------------------------------
actograms =
    ggplot(data = tuco[state %in% c("high")], aes(x = time, y = date)) +
    scale_x_continuous(limits = c(0, 1440), breaks = c(0,360,720,1080,1440), labels = c(0,6,12,18,24)) +
    scale_y_continuous(trans = rev_date) +
    geom_bar_tile(data = tuco, mapping = aes(height = lux), fill = "orange", alpha = 0.4, width = 5) +
    geom_bar_tile(mapping = aes(height = as.numeric(state)), width = 1) +
    facet_wrap(~ID, scales = "free_y", ncol = 3) +
    xlab("") +
    ylab("") + 
    theme_article() +
    theme(panel.grid.major.y = element_line(color = "grey95")) +
    theme(legend.position = "none") +
    ggtitle("High Activity Only")

sexlabels = unique(tuco %>% select(ID, sex)) %>% mutate(sex = if_else(sex == "m", "♂", "♀"))
actograms = actograms + geom_text(x = Inf, y = Inf, 
                                  aes(label = sex), 
                                  data = sexlabels, vjust = 1.2, hjust = 1.2, 
                                  size = 3, family = "Arial Unicode MS") + 
    theme(legend.position = "none")

ggsave(filename = "actograms_states_high_v2.png", path = "04_figures/actograms/",plot = actograms, device = "png", dpi = 132, width = 210, height = 290, units = "mm")

# Actograms Activity Bar REST --------------------------------------------------
actograms =
    ggplot(data = tuco[state %in% c("rest")], aes(x = time, y = date)) +
    scale_x_continuous(limits = c(0, 1440), breaks = c(0,360,720,1080,1440), labels = c(0,6,12,18,24)) +
    scale_y_continuous(trans = rev_date) +
    geom_bar_tile(data = tuco, mapping = aes(height = lux), fill = "orange", alpha = 0.4, width = 5) +
    geom_bar_tile(mapping = aes(height = as.numeric(state)), width = 1) +
    facet_wrap(~ID, scales = "free_y", ncol = 3) +
    xlab("") +
    ylab("") + 
    theme_article() +
    theme(panel.grid.major.y = element_line(color = "grey95")) +
    theme(legend.position = "none") +
    ggtitle("Rest Bouts Only")

sexlabels = unique(tuco %>% select(ID, sex)) %>% mutate(sex = if_else(sex == "m", "♂", "♀"))
actograms = actograms + geom_text(x = Inf, y = Inf, 
                                  aes(label = sex), 
                                  data = sexlabels, vjust = 1.2, hjust = 1.2, 
                                  size = 3, family = "Arial Unicode MS") + 
    theme(legend.position = "none")

ggsave(filename = "actograms_states_rest_v2.png", path = "04_figures/actograms/",plot = actograms, device = "png", dpi = 132, width = 210, height = 290, units = "mm")