library(momentuHMM)
library(patchwork)
require(egg)
library(dplyr)
source(file = "03_analysis/plot_actogram/function_plot_actogram.R")
tuco = readRDS("01_data/activity_processed/tuco_processed.rds")
m2 = readRDS("03_analysis/hmm/m2.rds")

decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("Rest","Medium","High"))

debug(plot_actogram)
actograms_vedba = plot_actogram(tuco)
actograms_high = plot_actogram(tuco, height = "High")
actograms_medium = plot_actogram(tuco, height = "Medium")

ggsave(filename = "04_figures/actograms/actograms_vedba.png", plot = actograms_vedba, device = "png", dpi = 132, width = 210, height = 290, units = "mm")
ggsave(filename = "04_figures/actograms/actograms_high.png",  plot = actograms_high, device = "png", dpi = 132, width = 210, height = 290, units = "mm")
ggsave(filename = "04_figures/actograms/actograms_medium.png",plot = actograms_medium, device = "png", dpi = 132, width = 210, height = 290, units = "mm")