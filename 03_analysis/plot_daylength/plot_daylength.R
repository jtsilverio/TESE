require(ggplot2)
require(maptools)
source("03_analysis/tuco_theme.R")

 # Anillaco's Coordinates
anillaco = matrix(c(-66.95, -28.8), nrow = 1)

# Sequence of dates to calculate daylenghts of
anillaco_seq <- seq(from=as.POSIXct("2019-01-01", tz="America/Argentina/La_Rioja"), length.out=400, by="days")

# Calculate Sunrise and Sunset Times
#sunrise = sunriset(anillaco, anillaco_seq, direction="sunrise", POSIXct.out=TRUE)$day_frac * 24
#sunset  = sunriset(anillaco, anillaco_seq, direction="sunset", POSIXct.out=TRUE)$day_frac  * 24
dawn    = crepuscule(crds = anillaco, dateTime = anillaco_seq,
                     solarDep = 6, direction = "dawn", POSIXct.out=TRUE)$day_frac  * 24
dusk    = crepuscule(crds = anillaco, dateTime = anillaco_seq,
                     solarDep = 6, direction = "dusk", POSIXct.out=TRUE)$day_frac  * 24

daylength_df = data.frame(date = anillaco_seq, daylength = dusk - dawn)

plot_daylength = ggplot(daylength_df) +
    geom_line(aes(date, daylength), size = 1, color = "gold2") +
    ylab("Daylength (h)") +
    xlab("") +
    theme(panel.grid.major.y = element_line(color = "grey85", linetype = 3))

ggsave("04_figures/appendix/plot_daylength.png", plot_daylength, "png", width = 9, height = 4.5)
