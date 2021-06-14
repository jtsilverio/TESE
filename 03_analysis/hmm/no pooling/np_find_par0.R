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
tuco = readRDS("01_data/activity_processed/tuco_preprocessed.rds")
set.seed(53)
niter = 100
retryfits = 2

# Delete unused columns
tuco$datetime = NULL
tuco$day_number =  NULL
tuco$lux = NULL
tuco$temp =  NULL

# State names
stateNames <- c("rest","medium", "high")

# Prep data
tuco = tuco %>% 
    split(.$ID) %>% 
    lapply(., function(x){
        prepData(x, 
                 coordNames = NULL, 
                 covNames = c("sex","season"))})

apply_fitHMM = function(animal_data){
    message(c("ID:",levels(animal_data$ID)[1]))
    has_zero = any(animal_data$vedba == 0)
    
    # Calculate initial means using k-means
    tuco_kmeans = kmeans(animal_data$vedba, 3, 50)
    centers = sort(tuco_kmeans$centers)
    
    # Randomize initial parameters
    allPar0 = lapply(as.list(1:niter), function(x){
        vedba_mean0 = runif(3, min = centers - centers/2, max = centers + centers/2)
        vedba_sd0 = runif(3, min = c(0.01, 0.05, 0.3), max = c(0.05, 0.3, 0.8))
        
        if(has_zero){
            prop_zero = length(which(animal_data$vedba==0))/nrow(animal_data)
            vedba_zeromass0 = runif(3, min = 0, max = prop_zero)
            return(c(vedba_mean0, vedba_sd0, vedba_zeromass0))
        }else{
            return(c(vedba_mean0, vedba_sd0))
        }
    })
    
    # Prepare parallel processing
    ncores = detectCores() - 1
    cl = makeCluster(getOption("cl.cores", ncores))
    clusterExport(cl,list("tuco","fitHMM","stateNames","retryfits"))
    
    allm_parallel = parLapply(cl = cl, allPar0, fun = function(Par0){
        tryCatch(
            {m = fitHMM(data = animal_data, 
                        formula = ~1,
                        nbStates = 3, 
                        dist = list(vedba = "gamma"), 
                        Par0 = list(vedba = Par0),
                        stateNames =  stateNames,
                        retryFits = retryfits)},
            error = function(e){return(e)}
        )
    })
    stopCluster(cl)
    
    return(allm_parallel)
}

models = lapply(tuco, apply_fitHMM)

# Extract Best model parameters ------------------------------------------------

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
saveRDS(vedba_par0, "01_data/hmm/vedbaPar0.rds")

# Check Plots
# lapply(best_models, function(m){plot(m)})

Sys.time() - start_script