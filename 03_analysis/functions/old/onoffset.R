onoffset = function(datetime, activity, sensor = "NULL", decimal_time = TRUE, format = "long", mav.window = 72, cog = TRUE){
    library(data.table)
    library(lubridate)
    library(tidyr)
    source("functions/onoffset_ace.R")
    source("functions/onoffset_cog.R")
    source("functions/onoffset_lux.R")

    if(is.null(sensor)){
        stop("Argumento 'sensor' deve ser uma das opções: 'acelerometer', 'lightlogger'")
    }else{
        if(sensor == "accelerometer"){
            if(cog){
                onoffset_cog(datetime = datetime, activity = activity, mav.window = mav.window, format = format)
            }else{
                onoffset_ace(datetime = datetime, activity = activity, mav.window = mav.window, format = format)
            }
        }else{
            if(sensor == "lightlogger"){
                onoffset_lux(datetime = datetime, activity = activity, decimal_time = decimal_time, format = format)
            }else{
                stop("Argumento 'sensor' deve ser uma das opções: 'acelerometer', 'lightlogger'")
            }
        }
    }
}
