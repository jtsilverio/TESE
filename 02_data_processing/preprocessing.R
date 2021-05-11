library(data.table)
library(dplyr)
library(zoo)
library(tidyr)
library(lubridate)
library(xts)

dba = function(X, Y, Z, time_window = 4, hz =  10){
    #' Dynamic Body Acceleration
    #' 
    #' The Dynamic Body Acceleration in each axis is
    #' the device's acceleration discounting the effects of gravity
    #' This is done by (1) Calculating the effect of gravity using a
    #' movig average over 4 seconds, know as the static acceleration.
    #' (2) The static acceleration id discounted from the raw acceleration to obtain de Dynamic Acceleration of each axis.

    X = X - frollmean(x = X,
                      n = hz * time_window,
                      align = "left")
    Y = Y - frollmean(x = Y,
                      n = hz * time_window,
                      align = "left")
    Z = Z - frollmean(x = Z,
                      n = hz * time_window,
                      align = "left")
    
    return(data.table("Xd" = X,"Yd" = Y,"Zd" = Z))
}

vedba = function(X, Y, Z){
    #' Vectorial Dynamic Body Acceleration
    #' 
    #' The vectorial sum of the dynamic acceleration of the three axis.
    
    return(sqrt(X^2 + Y^2 + Z^2))
}

add_sex_season = function(data){
    #' Add a column for sex and season into a dataframe

    animals = fread("01_data/animals/animal_metadata.csv")
    animals[, weight_cap := NULL]
    animals[, acc := NULL]
    animals[, lux := NULL]
    animals[, sex := as.factor(sex)]
    animals[, season := as.factor(season)] 
    
    data = left_join(data, animals, by = "ID")
    data[,season := factor(season, levels = c("March", "July", "October", "February"))]
    
    return(data)
}

downsample = function(data, window = 600, FUN = median){
    #' Downsample the 10Hz data to a lower frequency.
    #' 
    #' Get split data, aggregate VeDBA and Temp by FUN with a non-overlapping window
    #' Return a new data frame with smaller size

    id_apply = function(df){
        endpoints = c(0, seq(window + 1, length(df$vedba), by = window))
        vedba_aggregated = data.table(vedba = period.apply(df$vedba, endpoints, FUN, na.rm = T))
        temp_aggregated = data.table(temp = period.apply(df$temp, endpoints, FUN, na.rm = T))
        endpoints[1] = 1
        
        df = df[endpoints,]
        df[,vedba := vedba_aggregated]
        df[,temp := temp_aggregated]
        
        return(df)
    }
    
    data_split = split(data, data$ID)
    data_split = lapply(data_split, id_apply)
    return(rbindlist(data_split))
}

join_acc_lux= function(acc, lux){
    # Match acc data to lux
    lux$datetime = ceiling_date(lux$datetime, "5 min")
    joint_data = left_join(acc, lux) 
    
    # As the luximeters have 1 minute sampling time but only record the maximum value each 5 minutes
    # joint_data[,lux := nafill(x = lux, type = "nocb"), by = "ID"]
    # joint_data[,lux := nafill(x = lux, type = "locf"), by = "ID"] 
    
    return(joint_data)
}

# Do not run on source().
################################################################################
if (sys.nframe() == 0){
    source("02_data_processing/read_acc.R")

# subset animals ----------------------------------------------------------
    tuco_subset = tuco_acc[ID %in% c("FEV02","MAR02","JUL16", "OCT08")]
    tuco_subset = tuco_acc[day_number <= 4]
    tz(tuco_subset$datetime) = "America/Argentina/La_Rioja"
    saveRDS(tuco_subset, "01_data/activity_processed/tuco_10hz_smooth_subset.rds")
    rm(tuco_subset)
    
    # Calculate Dynamic acceleration ------------------------------------------
    tuco_acc[, c("Xd", "Yd" , "Zd") := dba(X, Y, Z), by = ID]
    tuco_acc[, c("X","Y","Z") := NULL]
    # tuco_acc = na.omit(tuco_acc)
    
    # Calculate VeDBA ---------------------------------------------------------
    tuco_acc[, vedba := vedba(Xd, Yd, Zd)]
    tuco_acc[, c("Xd","Yd","Zd") := NULL]

    # Downsample --------------------------------------------------------------
    tuco_acc = downsample(tuco_acc)
    
    # Join Acc and Lux --------------------------------------------------------
    source("02_data_processing/read_lux.R")
    tuco = join_acc_lux(tuco_acc, tuco_lux)
    tuco = add_sex_season(tuco)
    rm(tuco_acc, tuco_lux, i, files, dir.data)
    setcolorder(tuco, c(1,7,6,2,3,4,5))
    tuco[,ID := factor(ID, levels = c("MAR01", "MAR02",
                                      "JUL15", "JUL16", "JUL17", "JUL18", "JUL19", "JUL20", "JUL21", "JUL23",
                                      "OCT01", "OCT08", "OCT09", "OCT10", "OCT13", "OCT14",
                                      "FEV01", "FEV02", "FEV03", "FEV05", "FEV06"))]
    saveRDS(tuco, "01_data/activity_processed/tuco_preprocessed.rds")
    gc()
}
