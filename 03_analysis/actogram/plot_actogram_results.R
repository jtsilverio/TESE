library(momentuHMM)
library(patchwork)
require(egg)
library(dplyr)
source(file = "03_analysis/plot_actogram/function_plot_actogram.R")
tuco = readRDS("01_data/activity_processed/tuco_processed.rds")
m2 = readRDS("03_analysis/hmm/m2.rds")
color_pal = c(rest = "#DDAA33", medium = "#BB5566", high = "#004488")

decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("rest","medium","high"))
tuco_selected = tuco %>% filter(ID == "OCT09")
remove_facet = theme(strip.text.x = element_blank(),
                     plot.title = element_text(size=12))

# Actograms --------------------------------------------------------------------
actograms_vedba = plot_actogram(tuco_selected, plot_days = 5) + 
    remove_facet +
    ggtitle("VeDBA")

actograms_medium = plot_actogram(tuco_selected, plot_days = 5, height = "medium") +
    remove_facet +
    ggtitle("Medium Activity State")
    
actograms_high = plot_actogram(tuco_selected, plot_days = 5, height = "high") +
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
    scale_color_manual(values = color_pal, labels = c("Rest", "Medium", "High")) +
    labs(color = "State") +
    guides(colour = guide_legend(override.aes = list(size=1.5)))

# Organize Plot ----------------------------------------------------------------
actogram_results = (actograms_vedba + 
    actograms_medium + 
    actograms_high) /
    ts_vedba + 
    #ts_medium + 
    #ts_high + 
    plot_layout( heights = c(3,1)) + plot_annotation(tag_levels = 'A')& 
    theme(plot.tag.position = c(0, 1),
          plot.tag = element_text(hjust = 0, vjust = 0))
actogram_results

ggsave(filename = "04_figures/actograms/actograms_results.png", 
       plot = actogram_results,
       device = "png",
       dpi = 132,
       width = 250,
       height = 125,
       #height = 290,
       units = "mm")
