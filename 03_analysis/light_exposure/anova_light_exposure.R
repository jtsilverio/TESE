# Calculate Above ground bouts of activity
source("../03_analysis/light_exposure/function_bouts.R")

# calculate percentage of daylenght seen per animal
time_aboveground_daily = tuco %>% 
    group_by(ID, season, date = lubridate::date(datetime)) %>% 
    summarise(time_aboveground = sum(aboveground)*5/median(daylength)) #%>% 
    #group_by(ID, season) %>% 
    #na.omit() %>% 
    #summarise(time_aboveground = mean(time_aboveground),
    #          season = unique(season))

tuco_bouts = tuco %>%
    group_by(ID, season, date = lubridate::date(datetime)) %>% 
    do(bouts(.$aboveground))

# Calculate mean number of bouts per season 
bouts_number = tuco_bouts %>% 
    na.omit() %>% 
    group_by(ID, season) %>% 
    summarise(mean_bouts = mean(number))

bouts_duration  = tuco_bouts %>% 
    na.omit() %>% 
    group_by(ID, season) %>% 
    summarise(mean_duration = mean(duration)) 


# ANOVA TESTS ------------------------------------------------------------------

# AOV DAILY PERCENTEGE OF LIGHT EXPOSURE
aov_perc_light = aov(data = time_aboveground_daily, formula = time_aboveground~season)
summary(aov_perc_light)
#plot(aov_perc_light)
TukeyHSD(aov_perc_light)

# AOV BOUTS NUMBER
aov_medium = aov(data = bouts_number, formula = mean_bouts~season)
summary(aov_medium)
#plot(aov_medium)
TukeyHSD(aov_medium)

# AOV BOUTS DURATION
aov_rest = aov(data = bouts_duration, formula = mean_duration~season)
summary(aov_rest)
#plot(aov_rest)
TukeyHSD(aov_rest)

