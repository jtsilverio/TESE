## Atividade na fase clara
```{r}
sunrise = sunriset(anillaco_coord, as_datetime(tuco$date, tz = "America/Argentina/La_Rioja"), direction="sunrise") * 24
sunset  = sunriset(anillaco_coord, as_datetime(tuco$date, tz = "America/Argentina/La_Rioja"), direction="sunset") * 24
dawn    = crepuscule(anillaco_coord, as_datetime(tuco$date, tz = "America/Argentina/La_Rioja"), direction="dawn", solarDep=6) * 24
dusk    = crepuscule(anillaco_coord, as_datetime(tuco$date, tz = "America/Argentina/La_Rioja"), direction="dusk", solarDep=6) * 24

# MINUTOS
act_perc = tuco %>% 
    mutate(daytime = ifelse(time > dawn & time < dusk, T, F)) %>% 
    group_by(id, sex, season, date) %>% 
    mutate(active = ifelse(odba > mean(odba) & daytime, T, F)) %>% 
    summarise(sum_act = sum(active), daylength = sum(daytime), perc_act = sum_act/daylength) 
act_perc$season = factor(act_perc$season, levels = c("autumn", "winter", "spring", "summer"))

a = ggplot(act_perc, aes(x = season, y = perc_act)) +
    geom_point(aes(color = sex),position = position_jitterdodge()) +
    geom_boxplot(aes(fill = sex), alpha = 0.8) +
    ylab("(Tempo de atividade claro)/(Duração do dia)") +
    xlab("")

b = ggplot(act_perc, aes(x = daylength * 5, y = sum_act * 5, color = sex)) +
    geom_smooth(method = "lm", alpha = 0.2) +
    geom_point() +
    ylab("Tempo de atividade no claro (min)") +
    xlab("Duração da fase clara (min)")

# ODBA
odba_day = tuco %>% 
    mutate(daytime = ifelse(time > dawn & time < dusk, T, F)) %>% 
    group_by(id, sex, season, date) %>% 
    summarise(sum_odba_day = sum(odba[daytime]),
              sum_odba_night = sum(odba[!daytime]),
              daylength = sum(daytime),
              nightlength = sum(!daytime),
              perc_odba = (sum_odba_day/daylength)/(sum_odba_day/daylength + sum_odba_night/nightlength)) 
odba_day$season = factor(odba_day$season, levels = c("autumn", "winter", "spring", "summer"))

# SUM(ODBA) NA FASE CLARA
c = ggplot(odba_day, aes(x = season, y = sum_odba_day)) +
    geom_point(aes(color = sex), position = position_jitterdodge()) +
    geom_boxplot(aes(fill = sex), alpha = 0.4) +
    ylab("Sum(ODBA) na fase clara") +
    xlab("")

d = ggplot(odba_day, aes(x = daylength * 5, y = sum_odba_day, color = sex)) +
    geom_smooth(method = "lm", alpha = 0.2) +
    geom_point() +
    ylab("sum(ODBA) na fase clara") +
    xlab("Duração do fase clara (min)")

# SUM(ODBA) NAS 24H
e = ggplot(odba_day, aes(x = season, y = sum_odba_day + sum_odba_night)) +
    geom_point(aes(color = sex), position = position_jitterdodge()) +
    geom_boxplot(aes(fill = sex), alpha = 0.4) +
    ylab("Sum(ODBA) em 24h") +
    xlab("")

f = ggplot(odba_day, aes(x = daylength * 5, y = sum_odba_day + sum_odba_night, color = sex)) +
    geom_smooth(method = "lm", alpha = 0.2) +
    geom_point() +
    ylab("sum(ODBA) em 24h") +
    xlab("Duração do fase clara (min)")

# Porcentagem ODBA
g = ggplot(odba_day, aes(x = season, y = perc_odba)) +
    geom_point(aes(color = sex),position = position_jitterdodge()) +
    geom_boxplot(aes(fill = sex), alpha = 0.8) +
    ylab("% Atividade no claro") +
    xlab("")

h = ggplot(odba_day, aes(x = daylength * 5, y = perc_odba, color = sex)) +
    geom_smooth(method = "lm", alpha = 0.2) +
    geom_point() +
    ylab("% Atividade no claro") +
    xlab("Duração do claro")

cowplot::plot_grid(plotlist = list(a,b,c,d,e,f,g,h), nrow = 4)
```