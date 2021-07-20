#- calcular duracao do dia para cada animal
#- dividir as porcentagens pela duracao do dia

# Calculate Sunrise and Sunset Times
# TODO: Take the median date first and only calculate crepuscules for that date.
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

time_in_state = tuco %>% 
    group_by(ID,
             season,
             daylength,
             daytime,
             state) %>% 
    summarise(n = n()) %>% 
    group_by(ID, season, state) %>% 
    summarise(diurnality = (n[daytime]/daylength)/
                (n[daytime]/daylength + n[!daytime]/(1440-daylength))) %>% 
    unique() %>% 
    ungroup()

# BARRAS INDIVIDUAIS -----------------------------------------------------------
ggplot(time_in_state, aes(diurnality, ID, fill = state, group = season)) +
    geom_bar(
             stat = "identity") +
    geom_text(aes(label = round(diurnality, 2)), 
              position = position_fill(vjust = 0.1), 
              color = "white", size = 2.8) +
    scale_fill_manual(values = tuco_pal) +
    scale_x_continuous(expand = c(0.02, 0)) +
    scale_y_discrete(limits = rev, expand = c(0.05, 0)) +
    facet_grid(season~state,
               scales = "free",
               space = "free_y") +
    xlab("Diurnality") +
    ylab("") #+
    #theme(panel.border = element_rect(colour = NA, fill=NA),
    #      axis.ticks.y = element_blank())

# BOXPLOT ----------------------------------------------------------------------
ggplot(time_in_state,
                  aes(x = season,
                      y = diurnality,
                      color = state)) +
    gghalves::geom_half_boxplot(nudge = 0.05,
                                outlier.color = NA) +
    gghalves::geom_half_point() +
    scale_color_manual(values = tuco_pal) +
    scale_x_discrete(labels = c("Mar","Jul","Oct","Feb")) +
    scale_y_continuous(limits = c(0,1))+
    facet_wrap(~state) +
    xlab("") +
    ylab("Diurnality") +
    theme(legend.position = "none")

# BARRAS SAZONAIS --------------------------------------------------------------
mean_time_in_state = time_in_state %>% 
    group_by(season, state) %>%
    summarise(mean_diurnality = mean(diurnality))

ggplot(mean_time_in_state, aes(season, mean_diurnality, fill = state)) +
    geom_bar(
        stat = "identity") +
    geom_text(aes(label = round(mean_diurnality, 2)), 
              position = position_fill(vjust = 0.1), 
              color = "white", size = 2.8) +
    scale_fill_manual(values = tuco_pal) +
    #scale_x_continuous(expand = c(0.02, 0)) +
    #scale_y_discrete() +
    facet_grid(~state,
               scales = "free",
               space = "free_y") +
    xlab("Diurnality") +
    ylab("") #+
#theme(panel.border = element_rect(colour = NA, fill=NA),
#      axis.ticks.y = element_blank())




# ---------------------------------------------------------------------------
# calcula media de porcentagem por dia e noite

# Calculate Sunrise and Sunset Times
# TODO: Take the median date first and only calculate crepuscules for that date.
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

time_in_state = tuco %>% # Could have been calculated using momentuHMM::TimeInState
    group_by(ID,
             season,
             date = lubridate::date(datetime),
             daylength,
             daytime) %>% 
    summarise(p_rest = sum(state == "rest")/n(),
              p_medium = sum(state == "medium")/n(),
              p_high = sum(state == "high")/n()) %>% 
    group_by(ID,
             season,
             daytime) %>% 
    summarise(rest = mean(p_rest),
              medium = mean(p_medium),
              high = mean(p_high)) %>% 
    tidyr::pivot_longer(cols = c("rest","medium","high"),
                        values_to = "perc",
                        names_to = "state")


ggplot(time_in_state, aes(perc, ID, fill = state, group = season)) +
    geom_bar(position = "stack",
             stat = "identity") +
    geom_text(aes(label = round(perc, 2)), 
              position = position_fill(vjust = 0.5), 
              color = "white", size = 2.8) +
    scale_fill_manual(values = tuco_pal) +
    scale_x_continuous(expand = c(0.02, 0)) +
    scale_y_discrete(limits = rev, expand = c(0.05, 0)) +
    facet_grid(season~daytime,
               scales = "free",
               space = "free_y") +
    xlab("Percentege of Time in State") +
    ylab("") +
    theme(panel.border = element_rect(colour = NA, fill=NA),
          axis.ticks.y = element_blank())

ggplot(time_in_state,
                  aes(x = daytime,
                      y = perc,
                      color = state)) +
    gghalves::geom_half_boxplot(nudge = 0.05,
                                outlier.color = NA) +
    gghalves::geom_half_point() +
    scale_color_manual(values = tuco_pal) +
    scale_x_discrete(labels = c("Night","Day")) +
    scale_y_continuous(limits = c(0,1))+
    facet_grid(state~season, scales = "free_x") +
    xlab("") +
    ylab("Time in State (%)") +
    theme(legend.position = "none") 



# ---------------------------------------------------------------------------
# calcula media de porcentagem por dia e noite

# Calculate Sunrise and Sunset Times
# TODO: Take the median date first and only calculate crepuscules for that date.
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

total_state = tuco %>% # Could have been calculated using momentuHMM::TimeInState
    group_by(ID,
             season,
             date = lubridate::date(datetime),
             daylength) %>% 
    transmute(t_rest = sum(state == "rest"),
              t_medium = sum(state == "medium"),
              t_high = sum(state == "high")) %>% 
    unique()


day_state = tuco %>% # Could have been calculated using momentuHMM::TimeInState
    filter(daytime) %>% 
    group_by(ID,
             season,
             date = lubridate::date(datetime),
             daylength) %>% 
    transmute(day_rest = sum(state == "rest")/daylength,
              day_medium = sum(state == "medium")/daylength,
              day_high = sum(state == "high")/daylength) %>% 
    unique()

df = left_join(total_state, day_state, by = c("ID", "date","season","daylength"))

df = df %>% 
    group_by(ID,season,date,daytime) %>% 
    summarise(rest = day_rest/t_rest,
              medium = day_medium/t_medium,
              high = day_high/t_high) %>% 
    group_by(ID, season, daytime) %>% 
    summarise(rest = mean(rest),
              medium = mean(medium),
              high = mean(high))

df = df %>% tidyr::pivot_longer(cols = c("rest","medium","high"), names_to = "state", values_to = "perc")

ggplot(df, aes(perc, ID, fill = state, group = season)) +
    geom_bar(position = "stack",
             stat = "identity") +
    geom_text(aes(label = round(perc, 2)), 
              position = position_fill(vjust = 0.5), 
              color = "white", size = 2.8) +
    scale_fill_manual(values = tuco_pal) +
    scale_x_continuous(expand = c(0.02, 0)) +
    scale_y_discrete(limits = rev, expand = c(0.05, 0)) +
    facet_grid(season~daytime,
               scales = "free",
               space = "free_y") +
    xlab("Percentege of Time in State") +
    ylab("") +
    theme(panel.border = element_rect(colour = NA, fill=NA),
          axis.ticks.y = element_blank())

ggplot(df,
       aes(x = daytime,
           y = perc,
           color = state)) +
    gghalves::geom_half_boxplot(nudge = 0.05,
                                outlier.color = NA) +
    gghalves::geom_half_point() +
    scale_color_manual(values = tuco_pal) +
    scale_x_discrete(labels = c("Night","Day")) +
    scale_y_continuous(limits = c(0,1))+
    facet_grid(state~season, scales = "free_x") +
    xlab("") +
    ylab("Time in State (%)") +
    theme(legend.position = "none") 


