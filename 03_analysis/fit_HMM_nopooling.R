library(momentuHMM)
library(data.table)
vedba_Par0 = readRDS("01_data/models/vedbaPar0.rds")
tuco = readRDS("01_data/rds/tuco_preprocessed.rds")

# Create a named list from the dataframe and prep HMM data -----------------
tuco = tuco %>% 
    split(.$ID) %>% 
    lapply(., function(x){
    prepData(x, 
             coordNames = NULL, 
             covNames = c("sex","season"))})

vedba_Par0 = lapply(vedba_Par0, function(x){
    mean0 = unname(sort(x["mean",]))
    sd0 = unname(sort(x["sd",]))
    if(dim(x)[1] == 3){
        zeromass0 = unname(sort(x["zeromass",]))
        return(c(mean0, sd0, zeromass0))
    }else{
        return(c(mean0, sd0))
    }
})

# Fit Models --------------------------------------------------------------
stateNames <- c("rest","medium", "high")
fitHMM_list= function(index){
    tryCatch(
        {m = fitHMM(data = tuco[[index]], 
                    formula = ~1,
                    nbStates = 3, 
                    dist = list(vedba = "gamma"), 
                    Par0 = list(vedba = vedba_Par0[[index]]),
                    stateNames =  stateNames)},
        error = function(e){return(e)})
}
m = lapply(names(vedba_Par0), fitHMM_list)

# Global Decoding ---------------------------------------------------------
states = lapply(m, viterbi)
tuco_decoded = rbindlist(lapply(seq_along(tuco), function(i){as.data.table(cbind(tuco[[i]][c("ID","datetime")], state = states[[i]]))}))
tuco_decoded[,state := as.factor(state)]

saveRDS(tuco_decoded, "01_data/rds/tuco_decoded.rds")