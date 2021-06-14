vedba_par = lapply(tuco_hmm, function(m){m$mle$vedba})

get_curves = function(ID){
    mean = vedba_par[[ID]][1]
    sd   = vedba_par[[ID]][2]
    x = seq(0,0.8, by = 0.0001)
    
    return(
        data.frame(
            ID = rep(ID, length(x)),
            x = x,
            rest = dgamma(x, shape = (mean/sd)**2, rate = mean/(sd**2)),
        )
    )
}

curves = lapply(names(vedba_par), get_curves)
curves = rbindlist(curves)

mean = curves %>% 
    group_by(x) %>% 
    summarise(y = mean(rest))


ggplot(curves) + 
    geom_line(aes(x, rest, group = ID), color = "grey70") +
    geom_line(data = mean, aes(x, y)) +
    scale_x_continuous(limits = c(0,0.8))

hist(tuco$vedba[tuco$ID == "MAR01"])
momentuHMM::plot.momentuHMM()
debug(plotHistMVN)
library(momentuHMM)
plot(tuco_hmm[["FEV01"]])