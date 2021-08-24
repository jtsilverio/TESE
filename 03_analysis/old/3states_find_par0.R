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

# Delete unused columns
tuco$datetime = NULL
tuco$day_number =  NULL
tuco$lux = NULL

# State names
stateNames <- c("rest","low", "high")

# Prep data
tuco = prepData(tuco, 
                coordNames = NULL, 
                dataNames = c("vedba"))

# Prepare parallel processing
ncores = detectCores() - 1
cl = makeCluster(getOption("cl.cores", ncores))
clusterExport(cl,list("tuco","fitHMM","stateNames"))
niter = 50

# Randomize initial parameters
allPar0 = lapply(as.list(1:niter), function(x){
    vedba_mean0 = runif(3, min = c(0.01, 0.05, 0.3), max = c(0.05, 0.3, 0.8))
    vedba_sd0 = runif(3, min = c(0.01, 0.05, 0.3), max = c(0.05, 0.3, 0.8))
    vedba_zeromass = runif(3, max = c(0.0001, 0.000001, 0.00000001), min = c(0.00001, 0.0000001, 0.000000001))
    
    Par0 = c(vedba_mean0, vedba_sd0, vedba_zeromass)
    
    return(Par0)
})

# Fit models
allm_parallel = parLapply(cl = cl, X = allPar0, fun = function(par0){
    m = fitHMM(data = tuco, 
               formula = ~ 1,
               nbStates = 3, 
               dist = list(vedba = "gamma"), 
               Par0 = list(vedba = par0),
               stateNames =  stateNames,
               retryFits = 2) # it runs 150 times (50 * 3)
})

# Extract parameters
Sys.time() - start_script
all_nllk = unlist(lapply(allm_parallel, function(m){m$mod$minimum}))
best_m = which.min(all_nllk)
allm_parallel[[best_m]]

#' BEST MODELS PARAMETERS
#' ----------------------
#' Value of the maximum log-likelihood: 1969287 
#' 
#' vedba parameters:
#' -----------------
#'                 rest          low         high
#' mean     0.021690036 1.324671e-01 4.045975e-01
#' sd       0.015031764 7.506273e-02 1.391908e-01
#' zeromass 0.001605849 5.279177e-10 1.124205e-09
#' 
#' Regression coeffs for the transition probabilities:
#' ---------------------------------------------------
#'                1 -> 2   1 -> 3    2 -> 1    2 -> 3    3 -> 1
#' (Intercept) -2.136027 -6.37334 -1.992111 -2.992227 -6.143034
#'                3 -> 2
#' (Intercept) -2.825302
#' 
#' Transition probability matrix:
#' ------------------------------
#'            rest        low        high
#' rest 0.89299292 0.10548323 0.001523849
#' low  0.11495802 0.84275620 0.042285782
#' high 0.00202404 0.05585882 0.942117142
#' 
#' Initial distribution:
#' ---------------------
#'         rest          low         high 
#' 5.096022e-01 4.903978e-01 2.026476e-11 
