library(data.table)
library(dplyr)
library(zoo)
library(tidyr)
library(lubridate)

dba = function(X, Y, Z, time_window, hz =  10){
    #' Dynamic Body Acceleration
    #'
    #' The raw values of acceleration in each axis are the result of the combination of 
    #' static acceleration (due to gravity) and dynamic acceleration (due to movement).
    #' 
    #' To approximate DBA,
    #' (1) static acceleration is first approximated by 
    #' smoothing the obtained acceleration using 2-sec running means,
    #' according to Halsey et al (2009)
    #' Halsey LG, Shepard ELC, Quintana F, Laich AG, Green JA, Wilson RP.
    #' The relationship between oxygen consumption and body acceleration in 
    #' a range of species. Comp Biochem Physiol A Mol Integr Physiol. 2009; 152: 197–202.
    #' PMID: 18854225
    #' (2) dynamic acceleration is calculated by subtracting the static 
    #' acceleration from the raw acceleration
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
    #' Vector sum of 3d dynamic accelaration data
    #' 
    #' Aka Euclidean norm.
    #'   
    #' Qasem L, Cardew A, Wilson A, Griffiths I, Halsey LG, Shepard ELC, et al.
    #' Tri-axial dynamic acceleration as a proxy for animal energy expenditure;
    #' should we be summing values or calculating the vector?.
    #' PLoS One. 2012; 7. PMID: 22363576
    return(sqrt(X^2 + Y^2 + Z^2))
}

add_sex_season = function(data){
    # Add a column for sex and season into a dataframe
    animals = fread("01_data/animals/animal_metadata.csv")
    animals[,rep_state := NULL]
    animals[,weight_cap := NULL]
    data = left_join(data, animals, by = "ID")
    
    return(data)
}

downsample = function(data, time_window = 100){
    # Downsample the 10Hz data to a lower frequency.
    # time_window: time in seconds to use for downsample
    data[, vedba := rollapply(data = vedba, FUN = mean, width = time_window, by = time_window, fill = NA, align = "left"), by = ID]
    data = na.omit(data)
    
    return(data)
}

join_acc_lux= function(acc, lux){
    # Match acc data to lux
    lux$datetime = ceiling_date(lux$datetime, "5 min")
    joint_data = left_join(acc, lux) 

    # As the luximeters have 1 minute sampling time but only record the maximum value each 5 minutes
    joint_data[,lux := nafill(x = lux, type = "nocb"), by = "ID"]
    joint_data[,lux := nafill(x = lux, type = "locf"), by = "ID"] 
    
    return(joint_data)
}


# when the file is sourced, execution will end here 
################################################################################
if (sys.nframe() == 0){
    tuco = readRDS("01_data/rds/tuco_acc_raw.rds")
    
    # subset animals ----------------------------------------------------------
    tuco_subset = tuco[ID %in% c("FEV02","MAR02","JUL16", "OCT08")]
    tuco_subset = tuco[day_number <= 4]
    tz(tuco_subset$datetime) = "America/Argentina/La_Rioja"
    saveRDS(tuco_subset, "01_data/rds/tuco_smooth_subset.rds")
    rm(tuco_subset)
    
    # Calculate Dynamic acceleration ------------------------------------------
    tuco[, c("Xd", "Yd" , "Zd") := dba(X, Y, Z, time_window = 4), by = ID]
    tuco[, c("X","Y","Z") := NULL]
    tuco = na.omit(tuco)
    
    # Calculate VeDBA ---------------------------------------------------------
    tuco[, vedba := vedba(Xd, Yd, Zd)]
    tuco[, c("Xd","Yd","Zd") := NULL]
    saveRDS(tuco, file = "01_data/rds/tuco_10hz_vedba.rds")
    
    # Downsample --------------------------------------------------------------
    tuco = downsample(tuco)

    # Join Acc and Lux --------------------------------------------------------
    tuco_lux = readRDS("01_data/rds/tuco_lux_raw.rds")
    tuco = join_acc_lux(tuco, tuco_lux)
    tuco = add_sex_season(tuco)
    saveRDS(tuco, "01_data/rds/tuco_01hz_preprocessed.rds")
}
