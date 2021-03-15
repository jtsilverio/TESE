library(momentuHMM)
library(data.table)
library(zoo)
library(maptools)
library(cowplot)
library(egg)
library(ggplot2)


# Read Data
tuco = readRDS("data/activity/tuco_subset.rds")
names(tuco)[1] = "ID"
#tuco = tuco[ID == "OCT09"]
#tuco$ID = as.factor(tuco$ID)
tuco = tuco[ID %in% c("OCT09","JUL16")]
#tuco = droplevels(tuco)

# Calculate Dynamic acceleration
tuco[, c("Xd", "Yd" , "Zd") := DBA(X, Y, Z), by = ID]
# Calculate VeDBA
tuco[, vedba := sqrt(Xd^2 + Yd^2 + Zd^2)]
# Remove X,Y,Z Columns
tuco[, c("X","Y","Z", "Xd","Yd","Zd") := NULL]

# Downsample with mean each 10 sec
tuco_acc[, vedba := rollapply(x = Xd, n = nrows, FUN = sd, fill = NA), by = id]
tuco.prep = na.omit(tuco.prep)

# Prep Data
tuco.prep = prepData(tuco, coordNames = NULL, dataNames = c("vedba"))
#plot(tuco.prep, dataNames = "vedba", breaks = 20, ask = F)
summary(tuco.prep, dataNames = "vedba")

# Ajuste do Modelo
#par = c(0.1, 0.3, 0.8, 0.2, 0.5, 2, 0.0003, 0.0003, 0.0003)
par = c(0.01, 0.1, 0.4, 0.01, 0.1, 0.5, 0.0003, 0.0003, 0.0003)

(tuco.fit = fitHMM(data = tuco.prep, 
       nbStates = 3, 
       dist = list(vedba = "gamma"), 
       Par0 = list(vedba = par)))

plot(tuco.fit, breaks = 100, plotCI = T, ask = F, sepAnimals = F, plotTracks = F)
momentuHMM::plotPR(tuco.fit) # Plota Residuos
momentuHMM::plotStates(tuco.fit) # PLota Estados

# Decodificando os estados no dataframe original
tuco$state = as.factor(viterbi(tuco.fit))

# TS com cores dos estados
ggplot(tuco, aes(1:length(vedba), vedba, fill = state)) +
        geom_col() +
        scale_x_continuous(breaks = seq(1, length(tuco$vedba), 8640)) +
        theme_article() +
        theme(panel.grid.major.x = element_line(colour = "grey", linetype = "dotted")) +
        xlab("")
