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
tuco = readRDS("01_data/rds/tuco_01hz_preprocessed.rds")

# Delete unused columns
tuco$datetime = NULL
tuco$day_number =  NULL
tuco$lux = NULL

# state names
stateNames <- c("rest","active")

# prep data
tuco = prepData(tuco, 
                coordNames = NULL, 
                dataNames = c("vedba"))

# prepare parallel processing
ncores = detectCores() - 1
cl = makeCluster(getOption("cl.cores", ncores))
clusterExport(cl,list("tuco","fitHMM","stateNames"))
niter = 50

# Randomize initial parameters
allPar0 = lapply(as.list(1:niter), function(x){
    vedba_mean0 = runif(2, min = c(0.01, 0.25), max = c(0.25, 0.8))
    vedba_sd0 = runif(2, min = c(0.01, 0.15), max = c(0.15, 0.5))
    vedba_zeromass = runif(2, max = c(0.001, 0.00001), min = c(0.00001, 0.0000001))
    
    Par0 = c(vedba_mean0, vedba_sd0, vedba_zeromass)
    
    return(Par0)
})

# Fit models
allm_parallel = parLapply(cl = cl, X = allPar0, fun = function(par0){
    m = fitHMM(data = tuco, 
               formula = ~ 1,
               nbStates = 2, 
               dist = list(vedba = "gamma"), 
               Par0 = list(vedba = par0),
               stateNames =  stateNames,
               retryFits = 2) # it runs 150 times (50 * 3)
})

Sys.time() - start_script

# extract best par0 -------------------------------------------------------
all_nllk = unlist(lapply(allm_parallel, function(m){m$mod$minimum}))
best_m = which.min(all_nllk)
allm_parallel[[best_m]]

# BEST MODEL
# Value of the maximum log-likelihood: 1704629 
# 
# vedba parameters:
# -----------------
#                 rest       active
# mean     0.039563939 2.907706e-01
# sd       0.036110078 1.776693e-01
# zeromass 0.001203833 3.109470e-11
# 
# Regression coeffs for the transition probabilities:
# ---------------------------------------------------
#               1 -> 2    2 -> 1
# (Intercept) -3.44615 -3.405001
# 
# Transition probability matrix:
# ------------------------------
#              rest     active
# rest   0.96911611 0.03088389
# active 0.03213953 0.96786047
# 
# Initial distribution:
# ---------------------
#      rest    active 
# 0.6120755 0.3879245 
