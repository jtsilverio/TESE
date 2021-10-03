library(dplyr)
library(data.table)
library(momentuHMM)
library(ggplot2)
library(dplR)
library(egg)
library(patchwork)
source("03_analysis/actogram/function_actogram.R")
source("03_analysis/ggplot_theme/ggplot_theme.R")

# Read Data ---------------------------------------------------------------
tuco = readRDS("01_data/activity_processed/tuco_processed.rds")
tuco.metadata = fread("01_data/animals/animal_metadata.csv")

# Read Model --------------------------------------------------------------
m2 = readRDS("03_analysis/hmm/m2.rds") # modelo com 

# Viterbi State Decoding --------------------------------------------------
decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("Rest","Medium","High"))

# Select one tuco
tuco_selected = tuco %>% filter(ID == "OCT09")
remove_facet = theme(strip.text.x = element_blank(),
                     plot.title = element_text(size=12))

# Actograms --------------------------------------------------------------------
actograms_vedba = plot_actogram(tuco_selected, plot_days = 5) + 
    remove_facet +
    ggtitle("VeDBA")

actograms_medium = plot_actogram(tuco_selected, plot_days = 5, height = "Medium") +
    remove_facet +
    ggtitle("Medium Activity State")
    
actograms_high = plot_actogram(tuco_selected, plot_days = 5, height = "High") +
    remove_facet +
    ggtitle("High Activity State")

# Time Series ------------------------------------------------------------------
tuco_selected = tuco_selected[day_number <= 5]   
ts_vedba = ggplot(tuco_selected) +
    geom_point(aes(datetime, vedba, color = state), size = 0.3) + 
    xlab("") +
    ylab("VeDBA") +
    theme_article() +
    scale_x_datetime(date_breaks = "1 day") +
    scale_color_manual(values = tuco_pal[1:3], labels = c("Rest", "Medium", "High")) +
    labs(color = "State") +
    guides(colour = guide_legend(override.aes = list(size=1.5)))

# Organize Plot ----------------------------------------------------------------
actogram_results = (actograms_vedba + actograms_medium + actograms_high) / ts_vedba + 
    plot_layout( heights = c(3,1)) + plot_annotation(tag_levels = 'A') & 
    theme(plot.tag.position = c(0, 1),
          plot.tag = element_text(hjust = 0, vjust = 0))
actogram_results

ggsave(filename = "04_figures/actograms/actograms_results.png", 
       plot = actogram_results,
       device = "png",
       dpi = 132,
       width = 250,
       height = 125,
       units = "mm")
