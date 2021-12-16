# Calculate Above ground bouts of activity
#source("../03_analysis/light_exposure/function_bouts.R")
tuco = readRDS("01_data/activity_processed/tuco_processed.rds")
tuco = readRDS("01_data/animals/animal_metadata.csv")

# list of animals that had luximeter
tuco_luximeter = tuco.metadata %>% filter(lux, recaptured, collar_recovered) %>% dplyr::select(ID)    

# calculate percentage of daylenght seen per animal
time_aboveground_daily = tuco %>% 
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

# AOV DAILY TIME OF LIGHT EXPOSURE
aov_time_light = aov(data = time_aboveground_daily, formula = mean_t_aboveground~season)
summary(aov_time_light)
TukeyHSD(aov_time_light)

# AOV DAILY PERCENTEGE OF LIGHT EXPOSURE
aov_perc_light = aov(data = time_aboveground_daily, formula = mean_p_aboveground~season)
summary(aov_perc_light)
TukeyHSD(aov_perc_light)

 
# tuco_bouts = tuco %>%
#     group_by(ID, season, date = lubridate::date(datetime)) %>% 
#     do(bouts(.$aboveground))
# 
# # Calculate mean number of bouts per season 
# bouts_number = tuco_bouts %>% 
#     na.omit() %>% 
#     group_by(ID, season) %>% 
#     summarise(mean_bouts = mean(number))
# 
# bouts_duration  = tuco_bouts %>% 
#     na.omit() %>% 
#     group_by(ID, season) %>% 
#     summarise(mean_duration = mean(duration)) 


# # AOV BOUTS NUMBER
# aov_medium = aov(data = bouts_number, formula = mean_bouts~season)
# summary(aov_medium)
# #plot(aov_medium)
# TukeyHSD(aov_medium)
# 
# # AOV BOUTS DURATION
# aov_rest = aov(data = bouts_duration, formula = mean_duration~season)
# summary(aov_rest)
# #plot(aov_rest)
# TukeyHSD(aov_rest)

