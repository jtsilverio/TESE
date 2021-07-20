tuco = readRDS("../01_data/activity_processed/tuco_processed.rds")

 # Daily Mean VeDBA ANOVA ------------------------------------------------------
vedba_daily = tuco %>% 
    group_by(ID, season, date = lubridate::date(datetime)) %>% 
    summarise(vedba = mean(vedba)) %>% 
    group_by(ID, season) %>% 
    summarise(vedba = mean(vedba))

# AOV HIGH ---------------------------------------------------------------------
aov_vedba_daily = aov(data = vedba_daily[], formula = vedba~season)
summary(aov_vedba_daily)
TukeyHSD(aov_vedba_daily) # compare groups

# Check group means
vedba_daily %>% 
    group_by(season) %>% 
    summarise(mean(vedba), sd(vedba))

# Plot Distribution
ggplot(data = vedba_daily, aes(x = season, y = vedba, color = season)) +
    gghalves::geom_half_boxplot(nudge = 0.05, outlier.color = NA) +
    gghalves::geom_half_point() +
    geom_text(data = data.frame(season = "July", ypos = 0.16), aes(season, ypos), label = "*", size = 6, color = "black") +
    xlab("") +
    ylab("Daily Mean(VeDBA)")

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

# Plot DIstribution
ggplot(data = vedba_daytime, aes(x = season, y = vedba, color = season)) +
    gghalves::geom_half_boxplot(nudge = 0.05, outlier.color = NA) +
    gghalves::geom_half_point() +
    xlab("") +
    ylab("Daytime Mean(VeDBA)")

