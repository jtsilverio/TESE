tuco_luximeter = tuco.metadata %>% filter(lux, recaptured, collar_recovered) %>% dplyr::select(ID)    

aboveground_activity = tuco %>%
    group_by(ID, date = lubridate::date(datetime), season) %>% 
    mutate(daylength = sum(daytime)) %>%
    filter(ID %in% tuco_luximeter$ID, daytime == T) %>%
    group_by(ID, date = lubridate::date(datetime), season, state) %>% 
    summarise(daylength = median(daylength),
              time = sum(aboveground),
              perc   = sum(aboveground)/daylength) %>% 
    group_by(season, ID, state) %>%
    summarise(time = mean(time),
              perc   = mean(perc),
              daylength = median(daylength))
    

ggplot(aboveground_activity, aes(season, time, color = state)) +
    gghalves::geom_half_point() +
    gghalves::geom_half_boxplot(nudge = 0.05, outlier.alpha = 0) +
    ylab("Time of Activity Aboveground On Daytime") +
    xlab("") +
    scale_color_manual(values = tuco_pal[1:3]) +
    theme(legend.position = "none") +
    facet_wrap(~state)

ggplot(aboveground_activity, aes(state, time, color = state)) +
    gghalves::geom_half_point() +
    gghalves::geom_half_boxplot(nudge = 0.05, outlier.alpha = 0) +
    ylab("Time of Activity Aboveground") +
    xlab("") +
    scale_color_manual(values = tuco_pal[1:3]) +
    theme(legend.position = "none") + 
    scale_y_continuous(limits = c(0,80))


# -----

ggplot(aboveground_activity, aes(season, perc, color = state)) +
    gghalves::geom_half_point() +
    gghalves::geom_half_boxplot(nudge = 0.05, outlier.alpha = 0) +
    ylab("Perc of Activity Aboveground") +
    xlab("Behavioral State") +
    scale_color_manual(values = tuco_pal[1:3]) +
    theme(legend.position = "none") +
    facet_wrap(~state) 

ggplot(aboveground_activity, aes(state, perc, color = state)) +
    gghalves::geom_half_point() +
    gghalves::geom_half_boxplot(nudge = 0.05, outlier.alpha = 0) +
    ylab("Perc of Activity Aboveground") +
    xlab("Behavioral State") +
    scale_color_manual(values = tuco_pal[1:3]) +
    theme(legend.position = "none") +
    scale_y_continuous(limits = c(0,0.15))




