library(activity)
get_pdf = function(x){
    m = fitact(x$radians, adj = 0.3)
    data = data.frame(m@data)
    pdf  = data.frame(m@pdf)
    return(list(data, pdf))
}


#################
## STATES
tuco_fit = tuco %>% 
    dplyr::select(ID, sex, season, time, state) %>% 
    mutate(radians = time * ((2*pi)/1440)) %>% 
    tidyr::nest(cols = -c(season, state)) %>% 
    mutate(fit = purrr::map(cols, get_pdf)) %>% 
    dplyr::select(-cols)

kernel_data = tuco_fit %>% 
    mutate(data = purrr::map(fit, function(x) x[[1]])) %>% 
    select(-fit) %>% 
    tidyr::unnest(data)

kernel_pdf = tuco_fit %>% 
    mutate(pdf = purrr::map(fit, function(x) x[[2]])) %>% 
    select(-fit) %>% 
    tidyr::unnest(pdf)
    
ggplot() +
    geom_histogram(data = kernel_data, aes(x = m.data,
                                           y = ..density..,
                                           fill = state),
                   color = "white",
                   alpha = 0.2,
                   bins = 24) +
    #geom_ribbon(aes(x, ymin = lcl, ymax = ucl), fill = "grey70") +
    geom_density(data = kernel_pdf,
                 aes(x, y, color = state),
                 stat = "identity",
                 size = 0.5) +
    scale_x_continuous(limits = c(0,2*pi),
                       breaks = c(0, pi/2, pi, 3/4*2*pi, 2*pi),
                       labels = c(0,6,12,18,24)) +
    facet_grid(season~state) +
    scale_color_manual(values = tuco_pal) +
    scale_fill_manual(values = tuco_pal) +
    theme(legend.position = "none")+
    xlab("Time (h)") +
    ylab("Density")


################################################################################
## LUX

# BY ID
tuco_lux_fit = tuco %>% 
    dplyr::select(ID, sex, season, time, aboveground) %>% 
    filter(aboveground == TRUE) %>% 
    mutate(radians = time * ((2*pi)/1440)) %>% 
    tidyr::nest(cols = -c(ID)) %>% 
    mutate(fit = purrr::map(cols, get_pdf)) %>% 
    dplyr::select(-cols)

kernel_data = tuco_lux_fit %>% 
    mutate(data = purrr::map(fit, function(x) x[[1]])) %>% 
    select(-fit) %>% 
    tidyr::unnest(data)

kernel_pdf = tuco_lux_fit %>% 
    mutate(pdf = purrr::map(fit, function(x) x[[2]])) %>% 
    select(-fit) %>% 
    tidyr::unnest(pdf)

ggplot() +
    geom_histogram(data = kernel_data, aes(x = m.data,
                                           y = ..density..),
                   color = "white",
                   alpha = 0.2,
                   bins = 24,
                   fill = "orange") +
    #geom_ribbon(aes(x, ymin = lcl, ymax = ucl), fill = "grey70") +
    geom_density(data = kernel_pdf,
                 aes(x, y),
                 stat = "identity",
                 size = 0.8,
                 color = "orange") +
    scale_x_continuous(limits = c(0,2*pi),
                       breaks = c(0, pi/2, pi, 3/4*2*pi, 2*pi),
                       labels = c(0,6,12,18,24)) +
    facet_wrap(vars(ID)) +
    scale_color_manual(values = tuco_pal) +
    scale_fill_manual(values = tuco_pal) +
    theme(legend.position = "none")+
    xlab("Time (h)") +
    ylab("Density")


# BY SEASON
tuco_lux_fit = tuco %>% 
    dplyr::select(ID, sex, season, time, aboveground) %>% 
    filter(aboveground == TRUE) %>% 
    mutate(radians = time * ((2*pi)/1440)) %>% 
    tidyr::nest(cols = -c(season)) %>% 
    mutate(fit = purrr::map(cols, get_pdf)) %>% 
    dplyr::select(-cols)

kernel_data = tuco_lux_fit %>% 
    mutate(data = purrr::map(fit, function(x) x[[1]])) %>% 
    select(-fit) %>% 
    tidyr::unnest(data)

kernel_pdf = tuco_lux_fit %>% 
    mutate(pdf = purrr::map(fit, function(x) x[[2]])) %>% 
    select(-fit) %>% 
    tidyr::unnest(pdf)

kernel_pdf = kernel_pdf %>%
    mutate(season = factor(season, levels = c("March", "July", "October", "February")))

kernel_data = kernel_data %>%
    dplyr::add_row(season = "March", m.data = NA) %>% 
    mutate(season = factor(season, levels = c("March", "July", "October", "February")))

ggplot() +
    geom_vline(data = sunriset_season, 
               aes(xintercept = dawn * ((2*pi)/1440)), 
               linetype = "dotted", 
               color = "grey70") +
    geom_vline(data = sunriset_season, 
               aes(xintercept = dusk * ((2*pi)/1440)), 
               linetype = "dotted", 
               color = "grey70") +
    geom_histogram(data = kernel_data, aes(x = m.data,
                                           y = ..density..),
                   color = "white",
                   alpha = 0.2,
                   bins = 24,
                   fill = "orange") +
    #geom_ribbon(aes(x, ymin = lcl, ymax = ucl), fill = "grey70") +
    geom_density(data = kernel_pdf,
                 aes(x, y),
                 stat = "identity",
                 size = 0.8,
                 color = "orange") +
    scale_x_continuous(limits = c(0,2*pi),
                       breaks = c(0, pi/2, pi, 3/4*2*pi, 2*pi),
                       labels = c(0,6,12,18,24)) +
    facet_wrap(vars(season)) +
    theme(legend.position = "none")+
    xlab("Time (h)") +
    ylab("Density")





#######################
# library(overlap)
# 
# s = tuco %>% filter(season == "February" & state == "High")
# #s$high = F
# s[s$state == "high"]$high = T
# s$time = lubridate::hour(s$datetime)*60 + lubridate::minute(s$datetime)
# s$radians = s$time * ((2*pi)/1440)
# #times = s[s$high]$radians
# 
# a = densityPlot(s$radians, rug = T, extend = F, adjust = 1, n.grid = 500,)
# str(a)
#################
