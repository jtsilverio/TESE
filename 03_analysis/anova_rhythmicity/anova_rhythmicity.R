library(dplyr)
library(ggplot2)
library(momentuHMM)

tuco = readRDS("01_data/activity_processed/tuco_processed.rds")

# Read Models ------------------------------------------------------------------
m2 = readRDS("03_analysis/hmm/m2.rds") # modelo com 

# Viterbi Decoding -------------------------------------------------------------
decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("rest","medium","high"))


# Rhythimicity
aov_ri = aov(acf_peaks, formula = acf~state)
summary(aov_ri)
TukeyHSD(aov_ri)

# Estimated Period
