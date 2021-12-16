library(dplyr)
library(ggplot2)
library(momentuHMM)
library(data.table)

tuco = readRDS("01_data/activity_processed/tuco_processed.rds")
tuco.metadata = fread("01_data/animals/animal_metadata.csv")

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

# Calculate aboveground activity percentage
tuco_luximeter = tuco.metadata %>% filter(lux, recaptured, collar_recovered) %>% dplyr::select(ID)    

# calculate percentage of daylenght seen per animal
time_aboveground = tuco %>% 
    filter(ID %in% tuco_luximeter$ID) %>% 
    group_by(ID, season, date = lubridate::date(datetime)) %>% 
    summarise(daylength = sum(daytime),
              time_aboveground = sum(aboveground, na.rm = T) * 5,
              perc_aboveground = time_aboveground/daylength) %>% 
    group_by(ID, season) %>% 
    summarise(mean_t_aboveground = mean(time_aboveground),
              sd_t_aboveground = sd(time_aboveground),
              mean_p_aboveground = mean(perc_aboveground),
              sd_p_aboveground = sd(perc_aboveground),
              season = unique(season),
              daylength = median(daylength)) %>% 
    ungroup()

# ANOVA TESTS ------------------------------------------------------------------

# AOV TIME
aov_surface_time = aov(data = time_aboveground, formula = mean_t_aboveground~season)
summary(aov_surface_time)
#plot(aov_surface_time)
TukeyHSD(aov_surface_time)

# AOV PERC
aov_surface_time = aov(data = time_aboveground, formula = mean_p_aboveground~season)
summary(aov_surface_time)
#plot(aov_surface_time)
TukeyHSD(aov_surface_time)
