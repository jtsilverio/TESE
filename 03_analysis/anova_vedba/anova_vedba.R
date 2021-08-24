library(dplyr)
library(ggplot2)
tuco = readRDS("01_data/activity_processed/tuco_processed.rds")

 # Daily Mean VeDBA ANOVA ------------------------------------------------------
vedba_daily = tuco %>% 
    group_by(ID, season, date = lubridate::date(datetime)) %>% 
    summarise(vedba = mean(vedba)) %>% 
    group_by(ID, season) %>% 
    summarise(vedba = mean(vedba))

# AOV
aov_vedba_daily = aov(data = vedba_daily[], formula = vedba~season)
summary(aov_vedba_daily)
TukeyHSD(aov_vedba_daily) # compare groups

# Check group means
vedba_daily %>% 
    group_by(season) %>% 
    summarise(mean(vedba), sd(vedba))

# Daytime Mean VeDBA Anova ----------------------------------------------------------
vedba_daytime = tuco %>%
    group_by(ID, season, date = lubridate::date(datetime)) %>% 
    summarise(vedba = mean(vedba[daytime])) %>% 
    group_by(ID, season) %>% 
    summarise(vedba = mean(vedba))

# AOV
aov_vedba_daytime = aov(data = vedba_daytime, formula = vedba~season)
summary(aov_vedba_daytime)
TukeyHSD(aov_vedba_daytime) # compare groups

# Check Group Means
mean_daytime_vedba = vedba_daytime %>% 
    group_by(season) %>% 
    summarise(mean(vedba), sd(vedba))