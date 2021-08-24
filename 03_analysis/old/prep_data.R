# -------------------------------------------------------------------------
# Script to prepare the data for a HMM
# This script reads all raw activity data from the data folder
# It calculate VEDBA, filter the number of days and add sex and season
# -------------------------------------------------------------------------
library(data.table)
library(zoo)
library(momentuHMM)
source("functions/add_sex_season.R")

# Read data ---------------------------------------------------------------
tuco = readRDS("data/activity/tuco_10hz_vedba.rds.rds")
tuco_lux = read.csv("data/activity/")

# 10sec mean (0.1Hz) ------------------------------------------------------
tuco[, vedba := rollapply(data = vedba, FUN = mean, width = 100, by = 100, fill = NA), by = ID]
tuco = na.omit(tuco)

# Reduce data size
tuco = tuco[day_number <= 5]

# Add sex and season to the dataset
tuco = add.sex.season(tuco)

# Prepare data - MomentuHMM -----------------------------------------------
tuco = momentuHMM::prepData(tuco, 
                            coordNames = NULL, 
                            dataNames = c("vedba"), 
                            covNames = c("sex", "season"))

summary(tuco, dataNames = "vedba")

#saveRDS(tuco, "data/hmm/tuco_prep_01hz_3days_sexseason.rds")