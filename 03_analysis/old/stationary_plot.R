debug(plotStationary)
momentuHMM::plotStationary(m2)

plotStationary(m2)
timeInStates(m2, by = "sex")
stationary(m2, )

momentuHMM::

color_pal = c(rest = "#E6C000", medium = "#2F3D70", high = "#D04E59")

stat_prob = stationary(m3, covs = data.frame(season = c("March","July","October","February")))[[1]]
stat_prob = as.data.frame(stat_prob)
stat_prob$season = c("March","July","October","February")
library(tidyr)
stat_prob = pivot_longer(stat_prob, cols = 1:3, names_to = "state", values_to = "prob")
stat_prob$season = factor(stat_prob$season, levels = c("March","July","October","February"))

ggplot(stat_prob, aes(x = season, y = prob, group = state, color = state)) +
    geom_point(size = 3) +
    geom_line(linetype = 2, size = 0.2) +
    xlab("") +
    ylab("Stationary Distribution") +
    scale_color_manual(values = color_pal) +
    theme_article() +
    theme(panel.grid.major.y = element_line(color = "grey95")) +
    scale_y_continuous(limits = c(0,1))
    
plotStationary(m2, plotCI = T)

1/(1 + exp(-(log(pr[, state]/(1 - pr[, state])) - 
qnorm(1 - (1 - 0.95)/2) * (1/(pr[, state] - pr[, state]^2)) * se)))

momentuHMM::CIreal(m2)
se <- 
t(apply(dN, 1, function(x) suppressWarnings(sqrt(x %*% m2$mod$Sigma %*% x))))


uci[, state] <- 1/(1 + exp(-(log(pr[, state]/(1 - 
                                                  pr[, state])) + qnorm(1 - (1 - alpha)/2) * (1/(pr[, 
                                                                                                    state] - pr[, state]^2)) * se)))




dN <- t(apply(desMat, 1, function(x) numDeriv::grad(get_stat, 
                                                    model$mod$estimate[gamInd[unique(c(model$conditions$betaCons))]], 
                                                    covs = matrix(x, 1), nbStates = nbStates, 
                                                    i = state, betaRef = model$conditions$betaRef, 
                                                    betaCons = model$conditions$betaCons, workBounds = model$conditions$workBounds$beta, 
                                                    mixture = mix, ref = ref)))

tmpSig <- Sigma[gamInd[unique(c(model$conditions$betaCons))], 
                gamInd[unique(c(model$conditions$betaCons))]]


m2$mod$Sigma

                        