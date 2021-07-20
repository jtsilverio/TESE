########################
library(overlap)

s = (tuco[tuco$season == "February"])
s$high = F
s[s$state == "high"]$high = T
s$time = lubridate::hour(s$datetime)*60 + lubridate::minute(s$datetime) 
s$radians = s$time * ((2*pi)/1440)
times = s[s$high]$radians

a = densityPlot(times, rug = T, extend = F, adjust = 1, n.grid = 500)
str(a)
##################

library(activity)

#s = (tuco)
s$radians = s$time * ((2*pi)/1440)
times = s[s$high]$radians

mod = fitact(s[s$high]$radians, bw = 30)
plot(mod, data = c("both"), centre = "day", yunit = "density")

df = data.frame(mod@pdf)

library(ggplot2)
ggplot(df) +
    #geom_histogram(aes(x,y), binwidth = 60) +
    geom_histogram(data = data.frame(mod@data), aes(mod.data, y = ..density..), color = "white", alpha = 0.1) +
    #geom_ribbon(aes(x, ymin = lcl, ymax = ucl), fill = "grey70") +
    geom_density(aes(x, y), stat = "identity", size = 1) +
    scale_x_continuous(limits = c(0,2*pi), breaks = c(0, pi/2, pi, 3/4*2*pi, 2*pi), labels = c(0,6,12,18,24)) +
    geom_rug(data = data.frame(mod@data), aes(mod.data), alpha = 0.02)


ggplot(s[s$above]) +
    #geom_histogram(aes(x = time, y = ..density.., fill = season), 
    #               binwidth = 60) +
    geom_density(aes(x = time, fill = season), 
                 bw = 45, kernel = "e", size = 0.5, color = "grey50") +
    #stat_ecdf(aes(x = time, color = season)) +
    #ggridges::geom_density_ridges(aes(x = time, y = ID))
    #facet_wrap(~season) +
    geom_vline(xintercept = 1440/2, linetype = 3) +
    scale_x_continuous(limits = c(0,1440), expand = c(0,0), breaks = c(0, 360,720, 1080, 1440), labels = c(0,6,12,18,24)) +
    scale_fill_viridis_d()
