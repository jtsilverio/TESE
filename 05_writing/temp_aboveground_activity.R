tuco_luximeter = tuco.metadata %>% filter(lux, recaptured, collar_recovered) %>% dplyr::select(ID)    

aboveground_activity = tuco %>% 
    filter(ID %in% tuco_luximeter$ID) %>% 
    group_by(ID, date(datetime), season, state) %>% 
    summarise(aboveground_time = sum(aboveground),
              n = n(),
              aboveground_perc = aboveground_time/n()) %>%
    mutate(aboveground_time = ifelse(is.na(aboveground_time), 0, aboveground_time),
           aboveground_perc = ifelse(is.na(aboveground_perc), 0, aboveground_perc)) %>% 
    group_by(ID, season, state) %>% 
    summarise(aboveground_time = mean(aboveground_time),
              aboveground_perc = mean(aboveground_perc))



ggplot(aboveground_activity, aes(state, aboveground_perc, color = state)) +
    gghalves::geom_half_point() +
    gghalves::geom_half_boxplot(nudge = 0.05, outlier.alpha = 0) +
    ylab("Percentage of Activity Aboveground") +
    xlab("Behavioral State") +
    scale_color_manual(values = tuco_pal[1:3])

ggplot(aboveground_activity, aes(season, (aboveground_perc), color = state)) +
    gghalves::geom_half_point() +
    gghalves::geom_half_boxplot(nudge = 0.05, outlier.alpha = 0) +
    facet_wrap(~state) +
    scale_x_discrete(labels = c("Jul","Oct","Feb")) +
    scale_color_manual(values = tuco_pal[1:3]) +
    ylab("Percentage of Activity Aboveground") +
    xlab("") +
    theme(legend.position = "none")

# ---

aboveground_activity %>% 
    dplyr::arrange(desc(aboveground_perc))
