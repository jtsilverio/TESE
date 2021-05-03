library(ggplot2)
library(dplyr)
library(lubridate)
library(egg)
library(RColorBrewer)
source("functions/read_treated.R")
source("functions/anillaco_daylength.R")

# READ DATA
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

# Calculate mean per animal 
tuco_mean_id = 
  onoffsets_lux %>%
  group_by(id, type) %>% 
  summarise(mean = mean(time), date = median(date), sd = sd(time)) %>% droplevels()

# PLOT
pallete = colorRampPalette(brewer.pal(n = 12, name = "Paired"))
graph_mean_id =
  ggplot(data = tuco_mean_id, mapping = aes(x = date)) +
  daylenght_background +
  geom_errorbar(mapping = aes(ymin = mean -sd, ymax = mean +sd), alpha = 0.2) +
  geom_point(mapping = aes(y = mean, color = id, shape = type), size = 1.5) +
  scale_y_continuous(limits = c(0,24), breaks = 1:24) +
  scale_x_date(breaks = "month", date_labels = "%b") +
  scale_color_manual(values = pallete(13)) +
  theme_article() +
  xlab(" ") +
  ylab("Hora do Dia")
graph_mean_id

ggsave(filename = "graphs/img/graph_means_id_lux.png", plot = graph_mean_id, device = "png", dpi = 300, width = 297, height = 210, units = "mm")

