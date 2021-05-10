# FROM CHRONOSHOP:
# The onset of activity is calculated for each cycle by going 0.5 cycle back in time from the CoG. 
# The onset is defined as the first moment the activity exceeds the average activity in the current cycle. 
# In order to avoid premature onsets, a running mean is fit into the data. 
# The default length of the running mean is 10 bins. 
# The offset is found in the same way, but then by going 0.5 cycle forward from the CoG and moving back in time to find 
# the last moment the activity was above the average activity value for the current cycle.

# COG
cog = function(time, activity){
    library(circular)
    
    cog.linear = mean(time[activity > mean(activity, na.rm = T)], na.rm = T)
    cog.circular = mean.circular(circular(time[activity > mean(activity, na.rm = T)], units = "hour"), na.rm = T)
    cog.circular = ifelse(cog.circular < 0, 24 + cog.circular, cog.circular)
    
    cog = data.frame(cog.linear, cog.circular)
    
    return(cog$cog.circular)
}

# ONSET/OFFSET
find_onoffset = function(time, activity){
    cog = cog(time, activity)
    if(is.na(cog)){
        onoffset = data.frame(cog = 0, onset = 0, offset = 0)
        return(onoffset)
    }
    cog.index = which.min(abs(time - cog))
    
    activity = na.omit(activity)
    vector.length = length(activity)
    mean.activity = mean(activity, na.rm = T)
    
    onset = 0
    i = cog.index
    while (i > 0) { 
        if (activity[i] < mean.activity) {
            onset = time[i + 1]
            break
        }
        i = i - 1
    }
    
    offset = 24
    i = cog.index
    while (i < vector.length) { 
        if (activity[i] < mean.activity) {
            offset = time[i - 1]
            break
        }
        i = i + 1
    }
    onoffset = data.frame(cog, onset, offset)
    
    return(onoffset)
}

# WRAP FUNCTION
onoffset_cog = function(datetime, activity, mav.window = 72, format = "long"){
    library(data.table)
    library(tidyr)
    
    # Combina dados de entrada em um dataframe, o que facilita a manipulação.
    df = data.frame(datetime, activity)
    df$date = as_date(df$datetime)
    df$time = lubridate::hour(df$datetime) + lubridate::minute(df$datetime)/60 + lubridate::second(df$datetime)/3600 # converting time to decimals numbers

    # smooth data
    if (mav.window != 0){
        df$activity = frollmean(df$activity, n = mav.window, align = "center")
    }
    
    #df = na.omit(df)
    onoffset.cog = df %>% 
        group_by(date) %>% 
        summarise(find_onoffset(time = time, activity = activity))
    
    if(format == "long"){
        onoffset.cog = pivot_longer(data = onoffset.cog, 
                                    cols = c("onset","offset", "cog"), 
                                    names_to = "type", 
                                    names_ptypes = list(type = factor(levels = c("onset","offset","cog"))), 
                                    values_to = "time")
    }
    
    return(onoffset.cog)  
}
