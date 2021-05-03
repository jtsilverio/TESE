library(ggplot2)
library(maptools)
library(dplyr)
library(lubridate)
library(tidyr)
source("functions/read_treated.R") #create tuco dataset
source("functions/onoffset.R")
source("functions/anillaco_daylength.R")

##################################
## Calculate onsets and offsets ##
##################################

# Calculate Onoffsets
onoffsets_lux = tuco %>% 
  group_by(id, season, sex, weight_cap, rep_state) %>% 
  do(onoffset(timestamp = .$timestamp, activity = .$lux, sensor = "lightlogger", format = "long")) %>% 
  ungroup() %>% 
  droplevels()

# Transform onoffset times relative to sunrise and sunset
onoffsets_lux_relative = 
  onoffsets_lux %>% mutate(
  time = case_when(
    type == "onset"  ~ (time - (sunriset(anillaco_coord, as.POSIXct(.$date), direction="sunrise") * 24)),
    type == "offset" ~ (time - (sunriset(anillaco_coord, as.POSIXct(.$date), direction="sunset") * 24))
  )
) 

###############
## PLOTS
#############

# POPULATION
graph_dist_pop = 
  ggplot(data = onoffsets_lux_relative, mapping = aes(x = time, fill = type)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey80") +
  geom_histogram(binwidth = 0.25) +
  facet_grid(~type) +
  theme_article() +
  ylab("Frequência") +
  xlab("Tempo em relação aos crepúsculos (horas)") + theme(legend.position="none")
graph_dist_pop 

# SEX
graph_dist_sex = 
  ggplot(data = onoffsets_lux_relative, mapping = aes(x = time, fill = type)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey80") +
  geom_histogram(binwidth = 0.25) +
  scale_y_continuous(limits = c(0,16), breaks = 0:16) +
  facet_grid(sex~type) +
  theme_article() +
  ylab("Frequência") +
  xlab("Tempo em relação aos crepúsculos (horas)") + theme(legend.position="none")
graph_dist_sex 

# REP STATE
# n = onoffsets_lux %>% group_by(sex, season) %>% summarise(n = n())
graph_dist_state = 
  ggplot(data = onoffsets_lux_relative, mapping = aes(x = time, fill = type)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey80") +
  geom_histogram(binwidth = 0.25) +
  scale_y_continuous(limits = c(0,7), breaks = 0:7) +
  facet_grid(rep_state~type) +
  theme_article() +
  ylab("Frequência") +
  xlab("Tempo em relação aos crepúsculos (horas)") + theme(legend.position="none")
graph_dist_state