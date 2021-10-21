diurnality_vedba = tuco %>% 
    group_by(ID, season, daylength, daytime) %>%
    summarise(vedba_sum = sum(vedba)) %>% 
    group_by(ID, season) %>% 
    summarise(diurnality = (vedba_sum[daytime]/daylength)/
                  (vedba_sum[daytime]/daylength + vedba_sum[!daytime]/(1440 - daylength)) ) %>% 
    unique()
diurnality_vedba$state = "General Activity"


diurnality2 = tuco %>% 
    group_by(ID, date(datetime), season) %>% 
    mutate(daylength = sum(daytime)) %>% 
    group_by(ID, date(datetime), season, state) %>% 
    summarise(daylength = median(daylength), nighttime = sum(!daytime), daytime = sum(daytime),
    diurnality = (daytime/daylength)/
                  (daytime/daylength + nighttime/(1440 - daylength))) %>% 
    group_by(ID, season, state) %>% 
    summarise(diurnality = mean(diurnality))

  
    ggplot(diurnality2,
           aes(x = season,
               y = diurnality,
               color = state)) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey90") +
        gghalves::geom_half_boxplot(nudge = 0.05,
                                    outlier.color = NA) +
        gghalves::geom_half_point() +
        scale_color_manual(values = tuco_pal) +
        scale_x_discrete(labels = c("Mar","Jul","Oct","Feb")) +
        scale_y_continuous(limits = c(0,1))+
        facet_wrap(~state, ncol = 4) +
        xlab("") +
        ylab("Diurnality Index (DI)") +
        theme(legend.position = "none")
    