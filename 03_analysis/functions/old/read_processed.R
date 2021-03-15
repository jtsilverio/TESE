require(dplyr)
require(lubridate)
require(ggplot2)
require(tibble)

###################
## Read CSV Data ##
###################
tuco = as_tibble(read.csv(file = "../data/activity/tuco_5min.csv", colClasses = c("factor", "integer", "character", "numeric", "numeric", "numeric", "factor","factor","factor", "numeric")))
tuco$timestamp = ymd_hms(tuco$timestamp, tz = "America/Argentina/La_Rioja") # coverting timestap to POSIXct

# Creating new date and time variables for plotting
tuco = tuco %>% 
    mutate(date = as_date(tuco$timestamp), .after = 2) %>% 
    mutate(time = round(lubridate::hour(tuco$timestamp) + lubridate::minute(tuco$timestamp)/60 + lubridate::second(tuco$timestamp)/3600, digits = 2), .after = 3)

## Reordering the 'id' levels
tuco$id = paste0("#",tuco$id)
tuco$id = factor(x = tuco$id, 
                      levels =  c("#MAR01", "#MAR02", 
                                  "#JUL15", "#JUL16", "#JUL17", "#JUL18", "#JUL19", "#JUL20", "#JUL21", "#JUL23",
                                  "#OCT01", "#OCT08", "#OCT09", "#OCT10", "#OCT13", "#OCT14",
                                  "#FEV01", "#FEV02", "#FEV03", "#FEV05", "#FEV06"))

