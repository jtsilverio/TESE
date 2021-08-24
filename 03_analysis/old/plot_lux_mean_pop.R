library(ggplot2)
library(cowplot)
library(egg)
source("read_treated.R") # READ DATA

# CALCULATE ONOFFSETS
onoffsets_lux = tuco %>% 
  group_by(id, season, sex, weight_cap, rep_state) %>% 
  do(onoffset(timestamp = .$timestamp, activity = .$lux, sensor = "lightlogger", format = "long")) %>% 
  ungroup() %>% 
  droplevels()

# GEOM LIST FOR PLOTTING SUNRISE AND SUNSET LINES
daylenght_background = 
  list(
    #geom_ribbon(data = anillaco_sunriset, aes(ymin = dawn, ymax = dusk ), fill = "#FFEBCD", alpha = 0.6),
    #geom_ribbon(data = anillaco_sunriset, aes(ymin = sunrise, ymax = sunset), fill = "#FAFAD2", alpha = 1),
    geom_line(data = anillaco_sunriset, aes(x = date, y = sunrise), linetype = "longdash", alpha = 0.3),
    geom_line(data = anillaco_sunriset, aes(x = date, y = sunset), linetype = "longdash", alpha = 0.3),
    geom_line(data = anillaco_sunriset, aes(x = date, y = dawn), linetype = "dashed", alpha = 0.3),
    geom_line(data = anillaco_sunriset, aes(x = date, y = dusk), linetype = "dashed", alpha = 0.3)
  ) 

# BRUTO
graph_bruto = 
  ggplot(data = onoffsets_lux, aes(x = date)) + 
  daylenght_background +
  geom_point(mapping = aes(y = time, color = type, shape= type), alpha = 0.5) +
  scale_y_continuous(limits = c(0,24), breaks = 1:24) +
  scale_x_date(breaks = "month", date_labels = "%b\n%Y") +
  egg::theme_article() + 
  xlab(" ") +
  ylab("Hora do Dia") +
  ggtitle("Onsets e offsets brutos (todos os pontos de todos os animais)")
graph_bruto

#####################
# POPULACIONAL MEDIAS

## MEAN PER DAY
onoffsets_lux_mean1 = onoffsets_lux %>% na.omit() %>% 
  group_by(date, type) %>% 
  transmute(mean = mean(time), sd = sd(time))

graph_day1 =
  ggplot(data = onoffsets_lux_mean1, aes(x = date)) + 
  daylenght_background +
  geom_point(mapping = aes(y = mean, color = type ,shape = type), size = 2) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.1,) +
  scale_y_continuous(limits = c(0,24), breaks = seq(from = 2, to = 24, by = 2)) +
  scale_x_date(breaks = "month", date_labels = "%b") +
  xlab(" ") +
  ylab("Hora do Dia") +
  egg::theme_article() 

# mean per week
onoffsets_lux_mean7 = onoffsets_lux %>% na.omit() %>% 
  group_by(date = floor_date(date, unit = "week"), type) %>% 
  transmute(mean = mean(time), sd = sd(time)) %>% unique()

graph_day7 =
  ggplot(data = onoffsets_lux_mean7, aes(x = date)) + 
  daylenght_background +
  geom_point(mapping = aes(y = mean, color = type,shape = type), size = 2) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width=.2, alpha = 0.3, position = position_dodge(width=10)) +
  scale_y_continuous(limits = c(0,24), breaks = seq(from = 2, to = 24, by = 2)) +
  scale_x_date(breaks = "month", date_labels = "%b") +
  egg::theme_article() + 
  xlab(" ") +
  ylab("Hora do Dia")

# mean per season
onoffsets_lux_mean_season = onoffsets_lux %>% na.omit() %>% 
  group_by(date = floor_date(date, unit = "3 months"), season, type) %>% 
  transmute(mean = mean(time), sd = sd(time)) %>% unique()

graph_season =
  ggplot(data = onoffsets_lux_mean_season, aes(x = date)) + 
  daylenght_background +
  geom_point(mapping = aes(y = mean, color = type,shape = type), size = 2) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width=.2, alpha = 0.3, position = position_dodge(width=10)) +
  scale_y_continuous(limits = c(0,24), breaks = seq(from = 2, to = 24, by = 2)) +
  scale_x_date(breaks = "month", date_labels = "%b") +
  egg::theme_article() + 
  xlab(" ") +
  ylab("Hora do Dia")

ggarrange(
  plots = list(graph_day1 + theme(legend.position="none"), graph_day7, graph_season + theme(legend.position="none")),  
  labels = c('A', 'B', "C"), 
  nrow = 3, 
  ncol = 1)

ggsave(filename = "graph_lux_mean.png", plot = graph_means, path = "graphs", device = "png", dpi = 320, width = 297, height = 210, units = "mm")
