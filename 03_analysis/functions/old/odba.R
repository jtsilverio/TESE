## Calculate ODBA
## Be sure to run subtract_gravity before calculating ODBA
## 
## ARGUMENTS
## - data = dataframe containing the recorded acceleration in the X,Y and Z columns. Also needed a Timestamp ## columns with time of each record
## - time.window = time window in minutes in which the ODBA will be averaged. Default is 5 minutes
## - hz = frequency of the recorded data. Default is 10Hz
## 
## RETURNS
## - A dataframe containing the columns id, timestamp and odba summarized within the given time window

odba = function(Xd, Yd, Zd) {
    # Calculate de ODBA by summing the aboluste values of each axis
    odba = abs(data$Xd) + abs(data$Yd) + abs(data$Zd)
    
    # Retorna dataframe
    return(odba)
}
