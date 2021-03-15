## Calculate VeDBA
## Be sure to run subtract_gravity before calculating VeDBA 
##
## ARGUMENTS
## - data = dataframe containing the recorded acceleration in the X,Y and Z columns. Also needed a Timestamp ## columns with time of each record
## - time.window = time window in minutes in which the VeDBA will be averaged. Default is 5 minutes
## - hz = frequency of the recorded data. Default is 10Hz
## 
## RETURNS
## - A dataframe containing the columns id, timestamp and odba summarized within the given time window

vedba = function(Xd, Yd, Zd) {
    # Soma Vetorial dos valores das coordenadas
    vedba = sqrt((Xd)^2 + (Yd)^2 + (Zd)^2)
    
    # Retorna dataframe
    return(vedba)
}
