library(readr)
library(dplyr)
library(ggplot2)
library(egg)

#### READ REP. STATE DATA
tuco_rep = read_delim(file = "reproductivity_data/rep_data.csv", delim = ";") #Read csv
tuco_rep = tuco_rep %>% select(id, season, sex, rep_state) # Select only useful columns
tuco_rep$rep_state = factor(tuco_rep$rep_state, levels = c("NR", "P", "L")) # Convert Rep.state variable to Factor
tuco_rep$season = factor(tuco_rep$season, levels = c("autumn", "winter", "spring", "summer")) # Convert season variable to Factor

#### Female Graphs
tuco_females = tuco_rep %>% filter(sex == "F", rep_state != "?") 
graph_females = ggplot(data = tuco_females) +
	geom_bar(mapping = aes(x = season, fill = rep_state), position = "fill") +
	xlab("") +
	ylab("Porcentagem") +
	theme_article()
graph_females

#### Number of Youngs
tuco_young = tuco_rep %>% filter(sex == "Y") 
graph_youngs = ggplot(data = tuco_young) +
	geom_bar(mapping = aes(x = season)) +
	scale_x_discrete(drop=FALSE) +
	xlab("") +
	ylab("Número de indivíduos jovens") +
	theme_article()
graph_youngs
