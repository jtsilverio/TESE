# Code forked and modified from https://onlinelibrary.wiley.com/doi/full/10.1002/ece3.4786
delta = function(data){
    require(data.table)
    
    data_shift = shift(data, n=1, type = "lag", fill = 0)
    delta = data - data_shift
    
    # Sum deltas in the delta_time window
    delta = sum(abs(delta), na.rm = T)

    return(delta)
}

