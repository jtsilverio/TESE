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

mean_odba = function(datetime, x, y, z, mean_window) {
    # Calculate de ODBA by summing the aboluste values of each axis
    odba = abs(x) + abs(y) + abs(z)
    
    # Average the calculated ODBA to the given window of time
    index.begin = seq(from = 1, to = length(data), by = mean_window*hz)
    index.end = seq(from = time_window*hz, to = length(odba), by = mean_window*hz)
    
    odba_mean = rep(NA, times = length(index.end))
    for(i in 1:(length(index.end))){
        odba_mean[i] = FUN(odba[index.begin[i]:index.end[i]])
    }
    
    # Cria um novo dataframe com os vetores do Timestamp e ao ODBA calculado.
    index.time = index.end[1:(length(index.end))]
    odba_mean = data.frame("datetime" = data$Timestamp[index.time], odba_mean)
    
    # Retorna dataframe
    return(data)
}
