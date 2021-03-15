library(ggplot2)
library(maptools)
library(dplyr)
library(lubridate)
library(tidyr)
source("functions/read_treated.R") # read data
source("functions/onoffset.R")
source("functions/anillaco_daylength.R") # calcucale sunrisets for a year

# calculate onoffsets
onoffsets_lux = tuco %>% 
	group_by(id, season, sex, weight_cap, rep_state) %>% 
	do(onoffset(timestamp = .$timestamp, activity = .$lux, sensor = "lightlogger", format = "long")) %>% 
	ungroup() %>% 
	droplevels()

# Join sunrise and sunset times into onoffset dataset
onoffsets_lux_sunriset = left_join(x = onoffsets_lux, y = anillaco_sunriset[c(2:5,8)])

#create geoms for sunrise and sunset lines
background_season = 
	list(
		#geom_ribbon(data = anillaco_sunriset, aes(ymin = dawn, ymax = dusk ), fill = "#FFEBCD", alpha = 0.6),
		#geom_ribbon(data = anillaco_sunriset, aes(ymin = sunrise, ymax = sunset), fill = "#FAFAD2", alpha = 1),
		geom_line(data = onoffsets_lux_sunriset, aes(x = date, y = sunrise), linetype = "longdash", alpha = 0.3),
		geom_line(data = onoffsets_lux_sunriset, aes(x = date, y = sunset), linetype = "longdash", alpha = 0.3),
		geom_line(data = onoffsets_lux_sunriset, aes(x = date, y = dawn), linetype = "dashed", alpha = 0.3),
		geom_line(data = onoffsets_lux_sunriset, aes(x = date, y = dusk), linetype = "dashed", alpha = 0.3)
	) 

# PLOTS

# ONOFFSETS PER SEASON
ggplot(data = onoffsets_lux_sunriset) +
	background_season +
	geom_point(mapping = aes(x = date, y = time, color = type), size = 1.5) +
	scale_y_continuous(limits = c(0,24), breaks = seq(from = 2, to = 24, by = 2)) +
	scale_x_date(breaks = "4 day", date_labels = "%b\n%d") +
	facet_wrap(vars(season), scales = "free") +
	xlab("") +
	ylab("Hora do dia") +
	theme_article() 

# ONOFFSETES PER SEASON/REPSTATE
ggplot(data = onoffsets_lux_sunriset) +
	background_season +
	geom_point(mapping = aes(x = date, y = time, color = type), size = 1.5) +
	scale_y_continuous(limits = c(0,24), breaks = seq(from = 2, to = 24, by = 2)) +
	scale_x_date(breaks = "2 day", date_labels = "%b\n%d") +
	facet_wrap(season~rep_state, scales = "free", ncol = 2, ) +
	xlab("") +
	ylab("Hora do dia") +
	theme_article() 
