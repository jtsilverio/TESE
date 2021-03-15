library(ggplot2)
library(maptools)
library(dplyr)
library(tidyr)
library(egg)
library(RColorBrewer)
source("functions/read_treated.R")
source("functions/anillaco_daylength.R")
source("functions/onoffset.R")

onoffsets_lux = tuco %>% 
	group_by(id, season, sex, weight_cap, rep_state) %>% 
	do(onoffset(timestamp = .$timestamp, activity = .$lux, sensor = "lightlogger", format = "long")) %>% 
	ungroup() %>% 
	droplevels()
onoffsets_lux = left_join(x = onoffsets_lux, y = anillaco_sunriset[c(2:5,8)])

onoffsets_lux_wide = tuco %>%
	group_by(id, season, sex, weight_cap, rep_state) %>% 
	do(onoffset(timestamp = .$timestamp, activity = .$lux, sensor = "lightlogger", format = "wide")) %>% 
	ungroup() %>% 
	droplevels()


# GEOM LIST FOR PLOTTING
year_background = 
	list(
		#geom_ribbon(data = anillaco_sunriset, aes(ymin = dawn, ymax = dusk ), fill = "#FFEBCD", alpha = 0.6),
		#geom_ribbon(data = anillaco_sunriset, aes(ymin = sunrise, ymax = sunset), fill = "#FAFAD2", alpha = 1),
		geom_line(data = anillaco_sunriset, aes(x = date, y = sunrise), linetype = "longdash", alpha = 0.3),
		geom_line(data = anillaco_sunriset, aes(x = date, y = sunset), linetype = "longdash", alpha = 0.3),
		geom_line(data = anillaco_sunriset, aes(x = date, y = dawn), linetype = "dashed", alpha = 0.3),
		geom_line(data = anillaco_sunriset, aes(x = date, y = dusk), linetype = "dashed", alpha = 0.3)
	) 

season_background = 
	list(
		#geom_ribbon(data = anillaco_sunriset, aes(ymin = dawn, ymax = dusk ), fill = "#FFEBCD", alpha = 0.6),
		#geom_ribbon(data = anillaco_sunriset, aes(ymin = sunrise, ymax = sunset), fill = "#FAFAD2", alpha = 1),
		geom_line(data = onoffsets_lux, aes(x = date, y = sunrise), linetype = "longdash", alpha = 0.3),
		geom_line(data = onoffsets_lux, aes(x = date, y = sunset), linetype = "longdash", alpha = 0.3),
		geom_line(data = onoffsets_lux, aes(x = date, y = dawn), linetype = "dashed", alpha = 0.3),
		geom_line(data = onoffsets_lux, aes(x = date, y = dusk), linetype = "dashed", alpha = 0.3)
	) 

# mean per season
onoffsets_lux_mean_season = onoffsets_lux %>% na.omit() %>% 
	group_by(season, type) %>% 
	summarise(date = median(date), mean = mean(time), sd = sd(time))

graph_year =
	ggplot(data = onoffsets_lux_mean_season, aes(x = date)) + 
	year_background +
	geom_point(data = onoffsets_lux, mapping = aes(y = time, shape = type, color = type), size = 2, alpha = 0.5) + 
	scale_y_continuous(limits = c(0,24), breaks = seq(from = 5, to = 24, by = 2)) +
	scale_x_date(breaks = "month", date_labels = "%b") +
	theme_article() + 
	xlab(" ") +
	ylab("Hora do Dia")

graph_season =
	ggplot(data = onoffsets_lux_mean_season, aes(x = date, shape = type, color = type)) +
	season_background +
	geom_point(data = onoffsets_lux, mapping = aes(x = date, y = time, shape = type), size = 2,  color = "grey90", alpha = 0.5) +
	geom_errorbar(aes(x = date, ymin = mean - sd, ymax = mean + sd), width=0.2, alpha = 0.8) +
	geom_point(aes(x = date, y = mean, color = type,shape = type), size = 2) +
	scale_y_continuous(limits = c(0,24), breaks = seq(from = 2, to = 24, by = 2)) +
	scale_x_date(breaks = "2 day", date_labels = "%b\n%d") +
	facet_wrap(vars(season), scales = "free", ncol = 3) +
	xlab("") +
	ylab("Hora do dia") +
	theme_article() 

extreme_onset = 
	onoffsets_lux[onoffsets_lux$type == "onset",] %>% na.omit() %>% 
	group_by(id, season, type) %>% 
	summarise(date = date[which.min(time)], time = min(time)) 

extreme_offset = 
	onoffsets_lux[onoffsets_lux$type == "offset",] %>% na.omit() %>% 
	group_by(id, season, type) %>% 
	summarise(date = date[which.max(time)], time = max(time)) 

onoffset_extreme_season = bind_rows(extreme_onset, extreme_offset)
	
pallete = colorRampPalette(brewer.pal(n = 12, name = "Paired"))
graph_exterme_id =
	ggplot(data = onoffset_extreme_season) +
	geom_point(data = onoffsets_lux, mapping = aes(x = date, y = time, shape = type), size = 2, color = "grey90", alpha = 0.5) +
	season_background +
	geom_point(aes(x = date, y = time, shape = type, color = id), size = 2) +	
	geom_point(aes(x = date, y = time, shape = type, color = id), size = 2) +
	scale_y_continuous(limits = c(0,24), breaks = 1:24) +
	#scale_x_date(breaks = "month", date_labels = "%b") +
	scale_color_manual(values = pallete(13)) +
	facet_wrap(vars(season), scales = "free", ncol = 3) +
	theme_article() +
	xlab(" ") +
	ylab("Hora do Dia")

ggarrange(graph_year, graph_season, graph_exterme_id, widths = c(1.5,2), nrow = 3)


########

onoffsets_lux_mean = onoffsets_lux %>% 
	group_by(season, type, sex) %>% 
	summarise(date = median(date), mean = mean(time), sd = sd(time)) %>% ungroup()

ggplot(data = onoffsets_lux_mean, aes(x = season, color = sex, shape = type, )) +
	geom_point(data = onoffsets_lux, aes(x = season, y = time, color = sex, shape = type), alpha = 0.2, size = 1.5, position = position_jitterdodge()) +
	geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.3, alpha = 0.5, position = position_dodge(width=0.8)) +
	geom_point(aes(y = mean), size = 2, position=position_dodge(width=0.8)) +
	scale_y_continuous(limits = c(0,24), breaks = seq(from = 2, to = 24, by = 2)) +
	#geom_rect(data = onoffsets_lux, aes(x = season, y = ))
	facet_wrap(vars(sex)) +
	xlab("") +
	ylab("Hora do dia") +
	theme_article() 
