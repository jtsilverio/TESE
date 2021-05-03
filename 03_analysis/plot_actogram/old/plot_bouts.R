library(ggplot2)
library(maptools)
library(dplyr)
library(lubridate)
library(tidyr)
library(egg)
source("functions/read_treated.R") # read data
source("functions/onoffset.R")
source("functions/anillaco_daylength.R") # calcucale sunrisets for a year
source("functions/bouts.R")

# Calculate Above ground bouts of activity
bouts = tuco %>%
	group_by(id, date, season, sex, weight_cap, rep_state) %>% 
	do(bouts(.))

# Join bouts dataset with sunrise, sunset and daylength data
bouts = left_join(bouts, anillaco_sunriset)

bouts$sex =  factor(bouts$sex, levels = c("m", "f"))
bouts$rep_state = factor(bouts$rep_state, levels = c("M", "NR", "P", "L"))
bouts$season = factor(bouts$season, levels = c("autumn", "winter", "spring", "summer"))

# Calculate mean duration per season 
bouts_duration_sex = bouts %>% 
	na.omit() %>% 
	group_by(season, sex) %>% 
	summarise(mean.duration = mean(duration), sd.duration = sd(duration), min.duration = min(duration), max.duration = max(duration)) %>% 
	ungroup()

ggplot(data = bouts_duration_sex) +
	geom_bar(aes(x = season, y = mean.duration, fill = sex), stat = "identity", position = position_dodge2(preserve = "single")) +
	geom_errorbar(aes(x = season, ymin = mean.duration - sd.duration, ymax = mean.duration + sd.duration),  position = position_dodge2(preserve = "single")) +
	xlab("") +
	ylab("Tempo Médio de duração de cada bout \n de atividade de superfície (min)") +
	theme_article() 

# Calculate mean number of bouts per season 
bouts_number = bouts %>% na.omit() %>% 
	group_by(id, sex, season) %>% 
	summarise(n = max(number)) %>% 
	group_by(sex, season) %>% 
	summarise(bouts.mean = mean(n), bouts.sd = sd(n), n = n()) %>% 
	ungroup()

ggplot(data = bouts_number) +
	geom_bar(aes(x = season, y = bouts.mean, fill =  sex), stat = "identity", position = position_dodge2(preserve = "single")) +
	geom_errorbar(aes(x = season, ymin = bouts.mean - bouts.sd, ymax = bouts.mean + bouts.sd),  position = position_dodge2(preserve = "single")) +
	xlab("") +
	ylab("Número médio de bouts") +
	theme_article()


# calculate percentage of daylenght seen per animal
daylight_seen = bouts %>% na.omit() %>% 
	group_by(id, sex, rep_state, season, date) %>% 
	summarise(exposure = sum(duration)/60, daylength = mean(daylength), perc = (exposure/daylength))

ggplot(data = daylight_seen) +
	geom_bar(aes(x = date, y = perc, fill = rep_state), stat = "identity", position = position_dodge2(preserve = "single")) +
	#scale_y_continuous(limits = c(0,3), breaks  = seq(from = 0, to = 3, by = 0.5)) +
	xlab("") +
	ylab("Porcentagem de exposição à luz") +
	theme_article() + 
	facet_wrap(vars(id), scales = "free_x")