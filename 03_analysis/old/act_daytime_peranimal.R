## Atividade na fase clara

sunrise = sunriset(anillaco_coord, as_datetime(tuco$date, tz = "America/Argentina/La_Rioja"), direction="sunrise") * 24
sunset  = sunriset(anillaco_coord, as_datetime(tuco$date, tz = "America/Argentina/La_Rioja"), direction="sunset") * 24
dawn    = crepuscule(anillaco_coord, as_datetime(tuco$date, tz = "America/Argentina/La_Rioja"), direction="dawn", solarDep=6) * 24
dusk    = crepuscule(anillaco_coord, as_datetime(tuco$date, tz = "America/Argentina/La_Rioja"), direction="dusk", solarDep=6) * 24

#################
# PLOTS POR TEMPO
#################
act_perc = tuco %>% 
    mutate(daytime = ifelse(time > dawn & time < dusk, T, F)) %>% 
    group_by(id, sex, season, date) %>% 
    mutate(active = ifelse(odba > mean(odba), T, F)) 

act_perc_day = act_perc %>% 
    group_by(id, sex, season, date) %>% 
    summarise(sum_act_day = sum(active[daytime]),
              sum_act_night = sum(active[!daytime]),
              daylength = sum(daytime),
              nightlength = sum(!daytime),
              perc_act = (sum_act_day/daylength)/(sum_act_day/daylength + sum_act_night/nightlength))

act_perc_id = act_perc %>% 
    group_by(id, sex, season) %>% 
    summarise(date = median(date),
                sum_act_day = sum(active[daytime]),
              sum_act_night = sum(active[!daytime]),
              daylength = sum(daytime),
              nightlength = sum(!daytime),
              perc_act = (sum_act_day/daylength)/(sum_act_day/daylength + sum_act_night/nightlength))

act_perc_day$season = factor(act_perc_day$season, levels = c("autumn", "winter", "spring", "summer"))
act_perc_id$season = factor(act_perc_id$season, levels = c("autumn", "winter", "spring", "summer"))

a = ggplot(act_perc_day, aes(x = season, y = perc_act)) +
    geom_point(aes(color = sex), position = position_jitterdodge()) +
    geom_boxplot(aes(fill = sex), alpha = 0.8) +
    ylab("% de tempo de atividade durante o dia") +
    xlab("")

b = ggplot(act_perc_id, aes(x = season, y = perc_act)) +
    geom_point(aes(color = sex), position = position_jitterdodge()) +
    geom_boxplot(aes(fill = sex), alpha = 0.8) +
    ylab("% de tempo de atividade durante o dia") +
    xlab("")

cowplot::plot_grid(plotlist = list(a,b), ncol = 2)

###################
# Plots por % ODBA
##################
sum_odba = tuco %>% 
    mutate(daytime = ifelse(time > dawn & time < dusk, T, F)) 

sum_odba_day = sum_odba%>% 
    group_by(id, sex, season, date) %>% 
    summarise(sum_odba_day = sum(odba[daytime]),
              sum_odba_night = sum(odba[!daytime]),
              daylength = sum(daytime),
              nightlength = sum(!daytime),
              perc_odba = (sum_odba_day/daylength)/(sum_odba_day/daylength + sum_odba_night/nightlength)) 

sum_odba_id = sum_odba%>% 
    group_by(id, sex, season) %>% 
    summarise(sum_odba_day = sum(odba[daytime]),
              sum_odba_night = sum(odba[!daytime]),
              daylength = sum(daytime),
              nightlength = sum(!daytime),
              perc_odba = (sum_odba_day/daylength)/(sum_odba_day/daylength + sum_odba_night/nightlength)) 

sum_odba_day$season = factor(sum_odba_day$season, levels = c("autumn", "winter", "spring", "summer"))
sum_odba_id$season = factor(sum_odba_id$season, levels = c("autumn", "winter", "spring", "summer"))

a = ggplot(sum_odba_day, aes(x = season, y = perc_odba)) +
    geom_point(aes(color = sex), position = position_jitterdodge()) +
    geom_boxplot(aes(fill = sex), alpha = 0.4) +
    ylab("% do ODBA na fase clara") +
    xlab("")

b = ggplot(sum_odba_id, aes(x = season, y = perc_odba)) +
    geom_point(aes(color = sex), position = position_jitterdodge()) +
    geom_boxplot(aes(fill = sex), alpha = 0.4) +
    ylab("% do ODBA na fase clara") +
    xlab("")
cowplot::plot_grid(plotlist = list(a,b), ncol = 2)