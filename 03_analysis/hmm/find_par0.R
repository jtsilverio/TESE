#' In any maximum likelihood estimation, such as the fitHMM used here, the initial
#' parameters are important to try to avoid local minimums in the likelihhod profile.
#' 
#' The momentuHMM documention has a good discussion on how to find good starting values:
#' [McClintock, B.T. and Michelot, T. (2018)](https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-starting-values.pdf)
library(momentuHMM)
library(data.table)
library(dplyr)
library(parallel)

start_script = Sys.time()
tuco = readRDS("01_data/rds/tuco_preprocessed.rds")
tuco$vedba[tuco$vedba >= 0 & tuco$vedba <= 0.000001] = 0.00001
#tuco = tuco[ID %in% c("MAR01","MAR02","FEV01")]
#tuco = droplevels.data.frame(tuco)

# Delete unused columns
tuco$datetime = NULL
tuco$day_number =  NULL
tuco$lux = NULL
tuco$temp =  NULL

# State names
stateNames <- c("rest","medium", "high")

# Prep data
tuco = tuco %>% 
    split(.$ID)

tuco = lapply(tuco, function(x){
    prepData(x, 
             coordNames = NULL, 
             covNames = c("sex","season"))})

apply_fitHMM = function(animal_data){
    message(c("ID:",levels(animal_data$ID)[1]))
    
    # Calculate initial means using k-means
    tuco_kmeans = kmeans(animal_data$vedba, 3, 50)
    centers = sort(tuco_kmeans$centers)
    
    # Randomize initial parameters
    niter = 100
    allPar0 = lapply(as.list(1:niter), function(x){
        vedba_mean0 = runif(3, min = centers - centers/2, max = centers + centers/2)
        vedba_sd0 = runif(3, min = c(0.01, 0.05, 0.3), max = c(0.05, 0.3, 0.8))
        
        return(c(vedba_mean0, vedba_sd0))
    })
    
    # Prepare parallel processing
    ncores = detectCores() - 1
    cl = makeCluster(getOption("cl.cores", ncores))
    clusterExport(cl,list("tuco","fitHMM","stateNames"))
    
    allm_parallel = parLapply(cl = cl, allPar0, fun = function(Par0){
        tryCatch(
            {m = fitHMM(data = animal_data, 
                        formula = ~1,
                        nbStates = 3, 
                        dist = list(vedba = "gamma"), 
                        Par0 = list(vedba = Par0),
                        stateNames =  stateNames,
                        retryFits = 2)},
            error = function(e){return(NA)}
        )
    })
    stopCluster(cl)
    
    return(allm_parallel)
}
models = lapply(tuco, apply_fitHMM)


# Extract best initial parameters -----------------------------------------

get_bestmodel = function(model_list){
    get_nll = function(m){
        tryCatch(
            m$mod$minimum,
            error = function(e){return(NA)}
        )
    }
    bm_index = which.min(unlist(lapply(model_list, get_nll)))
    return(model_list[[bm_index]])
}

best_models = lapply(models, get_bestmodel)
vedba_par0 = lapply(best_models, function(m){m$mle$vedba})
#lapply(best_models, function(m){plot(m)})

end_script = Sys.time()
total_time = end_script - start_script
Sys.time() - start_script
