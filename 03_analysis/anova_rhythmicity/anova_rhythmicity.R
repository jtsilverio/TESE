library(dplyr)
library(ggplot2)
library(gghalves)

acf_peaks = readRDS("03_analysis/ACF/acf_peaks.rds")

# Rhythimicity
aov_ri = aov(acf_peaks, formula = acf~state)
summary(aov_ri)
TukeyHSD(aov_ri)

# Plot
acf_dist_plot = ggplot(acf_peaks,
                       aes(x = state,
                           y = acf,
                           color = state)) +
    gghalves::geom_half_boxplot() +
    gghalves::geom_half_point(transformation = position_jitter(width = 0.05, height = 0)) +
    scale_y_continuous(n.breaks = 8) +
    ylab("Rhythmicity Index (RI)") +
    xlab("") +
    geom_text(data = data.frame(state = "Medium", acf = 0.3), aes(state, acf), label = "*", color = "grey60", size = 5)
acf_dist_plot
