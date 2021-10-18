library(dplyr)
library(ggplot2)
library(momentuHMM)

tuco = readRDS("01_data/activity_processed/tuco_processed.rds")

# Read Models ------------------------------------------------------------------
m2 = readRDS("03_analysis/hmm/m2.rds") # modelo com 

# Viterbi Decoding -------------------------------------------------------------
decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("rest","medium","high"))

# Calculate Daylenght ----------------------------------------------------------
anillaco = matrix(c(-66.95, -28.8), nrow = 1) 
daylength = tuco[, .(datetime = median(datetime)), by = ID]

daylength$dawn = maptools::crepuscule(crds = anillaco,
                                      dateTime = daylength$datetime,
                                      solarDep = 6,
                                      direction = "dawn",
                                      POSIXct.out=TRUE)$day_frac  * 1440

daylength$dusk = maptools::crepuscule(crds = anillaco,
                                      dateTime = daylength$datetime,
                                      solarDep = 6,
                                      direction = "dusk",
                                      POSIXct.out=TRUE)$day_frac  * 1440

daylength = daylength[, .(daylength = dusk - dawn), by = ID]
tuco = left_join(tuco, daylength, by = "ID")

# Calculate Time in States -----------------------------------------------------
time_in_state = tuco %>% 
    group_by(ID, season, daylength, daytime, state) %>% 
    summarise(n = n()) %>% 
    group_by(ID, season, state) %>% 
    summarise(diurnality = (sum(n[daytime])/daylength)/
                  (sum(n[daytime])/daylength + sum(n[!daytime])/(1440-daylength))) %>% 
    unique() %>% 
    ungroup()

# ANOVA TESTS ------------------------------------------------------------------

# AOV HIGH
aov_high = aov(data = time_in_state %>% filter(state == "high"), formula = diurnality~season)
summary(aov_high)
#plot(aov_high)
TukeyHSD(aov_high)

# AOV MEDIUM
aov_medium = aov(data = time_in_state %>% filter(state == "medium"), formula = diurnality~season)
summary(aov_medium)
#plot(aov_medium)
TukeyHSD(aov_medium)

# AOV REST
aov_rest = aov(data = time_in_state %>% filter(state == "rest"), formula = diurnality~season)
summary(aov_rest)
#plot(aov_rest)
TukeyHSD(aov_rest)
