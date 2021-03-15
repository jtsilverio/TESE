library(data.table)
library(zoo)
library(momentuHMM)

source("functions/add_sex_season.R")
source("functions/DBA.R")

# read data ---------------------------------------------------------------
#tuco = readRDS("data/activity/tuco_subset.rds")
#tuco = readRDS("data/activity/tuco_subset_8animals_3days_season_sex.rds")
tuco = readRDS("data/activity/tuco_10hz_raw.rds")


# Calculate Dynamic acceleration ------------------------------------------
tuco[, c("Xd", "Yd" , "Zd") := DBA(X, Y, Z), by = ID]
tuco[, c("X","Y","Z") := NULL]
tuco[, vedba := sqrt(Xd^2 + Yd^2 + Zd^2)]
tuco[, c("Xd","Yd","Zd") := NULL]

# downsample - 10sec mean  (0.1Hz) ----------------------------------------
tuco[, vedba := rollapply(data = vedba, FUN = mean, width = 100, by = 100, fill = NA), by = ID]
tuco = na.omit(tuco)

# Reduce data size
tuco = tuco[day_number <= 3]

# add sex and season
tuco = add.sex.season(tuco)

# save
saveRDS(tuco, "data/activity/tuco_01hz_3days_sexseason.rds")


# prepare data - momentuhmm -----------------------------------------------
tuco = momentuHMM::prepData(tuco, 
                            coordNames = NULL, 
                            dataNames = c("vedba"), 
                            covNames = c("sex", "season"))

summary(tuco, dataNames = "vedba")
#plot(tuco.prep, dataNames = "vedba")

saveRDS(tuco, "data/hmm/tuco_prep_01hz_3days_sexseason.rds")

