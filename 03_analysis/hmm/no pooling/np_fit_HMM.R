library(momentuHMM)
library(data.table)
library(dplyr)
library(ggplot2)

vedba_Par0 = readRDS("01_data/hmm/vedbaPar0_np.rds")
tuco = readRDS("01_data/activity_processed/tuco_preprocessed.rds")

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
stateNames <- c("rest","medium","high")
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
tuco_hmm = lapply(names(vedba_Par0), fitHMM_list)
names(tuco_hmm) = names(vedba_Par0)
saveRDS(tuco_hmm, "01_data/hmm/tuco_hmm_np.rds")

# plot estimated means ----------------------------------------------------
dist_means = lapply(tuco_hmm, function(m){as.data.table(sort(m$mle$vedba["mean",]), keep.rownames = "mean")})
names(dist_means) = names(tuco)
dist_means = rbindlist(dist_means, idcol = "ID")
names(dist_means) = c("ID","state","mean")

ggplot(data = dist_means) +
    geom_histogram(aes(mean, fill = state), binwidth = 0.01) +
    facet_wrap(vars(state), scales = "free_x")

# Global Decoding ---------------------------------------------------------
states = lapply(tuco_hmm, viterbi)
tuco_decoded = rbindlist(lapply(seq_along(tuco), function(i){as.data.table(cbind(tuco[[i]][c("ID","datetime")], state = states[[i]]))}))
tuco_decoded[,state := as.factor(state)]

saveRDS(tuco_decoded, "01_data/hmm/tuco_states_np.rds")
