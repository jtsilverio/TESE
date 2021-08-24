library(momentuHMM)
library(data.table)
library(zoo)
library(maptools)
library(cowplot)
library(egg)
library(ggplot2)
library(dplyr)
source("functions/DBA.R")
source("functions/add_sex_season.R")

# prepare data - momentuhmm -----------------------------------------------
tuco.prep = momentuHMM::prepData(tuco, coordNames = NULL, dataNames = c("vedba"))
summary(tuco.prep, dataNames = "vedba")
#plot(tuco.prep, dataNames = "vedba")

# fit HMM -----------------------------------------------------------------
# chute de alguns parametros iniciais
# para acharmos os minimos globais ainda é necessário testar outros parâmetros inicias
#par = c(0.1, 0.3, 0.8, 0.2, 0.5, 2, 0.0003, 0.0003, 0.0003)
par = c(0.01, 0.1, 0.4, 0.01, 0.1, 0.5, 0.0003, 0.0003, 0.0003)

(tuco.fit = fitHMM(data = tuco.prep, 
                   nbStates = 3, 
                   dist = list(vedba = "gamma"), 
                   Par0 = list(vedba = par)))

# decode states -----------------------------------------------------------
tuco$state = as.factor(viterbi(tuco.fit))


# plots -------------------------------------------------------------------
plot(tuco.fit, breaks = 100, plotCI = T, ask = F)




# actograms ---------------------------------------------------------------
source("functions/stat-bar-tile-etho.R")

tuco[,time := data.table::hour(datetime) + data.table::minute(datetime)/60 + data.table::second(datetime)/3600]
tuco[,date := lubridate::date(datetime)]

ggplot(data = tuco[ID == "FEV05"], aes(x = time, y = reorder(date, desc(date), tz = "America/Argentina/La_Rioja"))) +
    scale_x_continuous(limits = c(0,24), breaks = c(0,6,12,18,24)) +
    #scale_y_date(date_labels = "%d") +
    geom_bar_tile(aes(height = vedba)) +
    facet_wrap(facets = vars(ID), scales = "free_y", ncol = 3) +
    xlab("") +
    ylab("") + 
    theme_bw()

