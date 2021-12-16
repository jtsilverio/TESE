bouts = function(aboveground){
    # aboveground = ifelse(test = data$lux >= 2, yes = TRUE, no = FALSE)
    number = integer()
    duration = integer()
    
    if (!anyNA(aboveground)){
        n = 0 # counter for bout number
        d = 0 # counter for bout duration
        for (i in 1:length(aboveground)) {
            if (aboveground[i] == T){
                if (aboveground[i-1] == T){
                    d = d + 1
                }else{
                    n = n + 1
                    d = d + 1
                }
            }else{
                if(i > 1){
                    if(aboveground[i-1] == T){
                        number[n] = n
                        duration[n] = d
                        d = 0
                    }
                }
            }
        }
        bout = data.frame(number, duration)
        return(bout)
    }
    else{
        bout = data.frame(number = 0, duration = NA)
        return(bout)	
    }
}