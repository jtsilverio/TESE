epanechnikov = function(x, bw=1) {
    #' bw = standard deviation of the kernel
    #' The support of the kernel will be -sqrt(5)bw tp sqrt(5)bw
    x = x / bw
    res = rep(0, length(x))
    res[abs(x) < sqrt(5)] = 3/4/sqrt(5) * (1 - x[abs(x) < sqrt(5)]^2/5) / bw
    return(res)
    
}

state_density_epa = function(state_seq, period, bw=600) {
    #' Epanechnikov KDE of time points where the system is in a specific state
    #' If the period is too big (> 10 min) we only have a single state per 10 min interval
    #' Period of the data (i.e. inverse of sample frequency) shall be given in seconds
    #' hence this returns effectively a histogram
    #' (This is for the default binwidth of 600 seconds)
    nr_states = 3
    bw = bw / period
    window = epanechnikov(-as.integer(bw*sqrt(5)):as.integer(bw*sqrt(5)), bw)
    sw = length(window)  # support width
    densities = matrix(0, nr_states, length(state_seq) + sw - 1)
    for (i in 1:length(state_seq)){
        densities[state_seq[i],i:(i+sw-1)] = densities[state_seq[i],i:(i+sw-1)] + window
    }
    #densities = densities / sapply(1:nr_states, function(s){ sum(state_seq == s) })
    # for low sampling rate this normalisation is better for later use in calculating 
    # entropy and similarity because we want to treat it as a discrete distribution then
    densities = densities / rowSums(densities)
    return(densities)
}

ggplot(s[s$high]) +
    geom_histogram(aes(time), bw = 60, kernel = "e")


debug(state_density_epa)
d = state_density_epa(s, 60, 3600)
densities_df = data.frame(t(d))

length(s)
nrow(densities_df)

ggplot(densities_df) +
    geom_density(aes(x = 1:nrow(densities_df), X3))

###############

d = density(as.numeric(s), bw = 0.1)

plot(1:length(s), d$y)

d <- density(faithful$eruptions, bw = "sj")
d
plot(d)


install.packages("ash")
install.packages("KernSmooth")
ash::bin2(vector(1:length(s), s))
plot(1:length(s), KernSmooth::bkde(s)$y)
KernSmooth::bkde(s, bandwidth = 60)


########################
library(overlap)

s = (tuco[tuco$ID == "MAR02"])
s$high = F
s[s$state == "high"]$high = T
s$time = lubridate::hour(s$datetime)*60 + lubridate::minute(s$datetime) 
s$radians = s$time * ((2*pi)/1440)
times = s[s$high]$radians

densityPlot(times, rug = T, extend = F, adjust = 1, n.grid = 500)
str(a)
##################

library(activity)

s = (tuco)
s$radians = s$time * ((2*pi)/1440)
times = s[s$high]$radians

mod = fitact(s[s$high]$radians, bw = 45)
plot(mod, data = c("both"), centre = "day", yunit = "density")

df = data.frame(mod@pdf)

ggplot(df) +
    #geom_histogram(aes(x,y), binwidth = 60) +
    geom_histogram(data = data.frame(mod@data), aes(mod.data, y = ..density..), color = "white", alpha = 0.1) +
    geom_ribbon(aes(x, ymin = lcl, ymax = ucl), fill = "grey70") +
    geom_density(aes(x, y), stat = "identity", size = 1) +
    scale_x_continuous(limits = c(0,2*pi), breaks = c(0, pi/2, pi, 3/4*2*pi, 2*pi), labels = c(0,6,12,18,24)) +
    theme_article() +
    geom_rug(data = data.frame(mod@data), aes(mod.data), alpha = 0.02)


    #facet_wrap(~season)

####

ggplot(s[s$above]) +
    #geom_histogram(aes(x = time, y = ..density.., fill = season), 
    #               binwidth = 60) +
    geom_density(aes(x = time, fill = season), 
                 bw = 45, kernel = "e", size = 0.5, color = "grey50") +
    #stat_ecdf(aes(x = time, color = season)) +
    #ggridges::geom_density_ridges(aes(x = time, y = ID))
    facet_wrap(~season) +
    geom_vline(xintercept = 1440/2, linetype = 3) +
    scale_x_continuous(limits = c(0,1440), expand = c(0,0), breaks = c(0, 360,720, 1080, 1440), labels = c(0,6,12,18,24)) +
    theme_article() +
    scale_fill_viridis_d()
