library(momentuHMM)
library(data.table)
library(dplyr)
library(future) # for parallel processing
plan(multisession) # for parallel processing. If parallelism is not wanted use "sequential".

start_script = Sys.time()

# Choose Data to run
tuco = readRDS("01_data/rds/tuco_01hz_preprocessed.rds")

# Delete unused columns
tuco$time = hour(tuco$datetime)
tuco$datetime = NULL
tuco$day_number =  NULL

tuco = prepData(tuco, 
                coordNames = NULL, 
                dataNames = c("vedba"), 
                covNames = c("sex", "season"))

# Global Parameters -------------------------------------------------------
stateNames <- c("rest", "active")
retryFits_m1 = 10
retryFits = 5


# Model 01: Empty Model ---------------------------------------------------
start_model = Sys.time()
message(paste0("Model 01 Start Time: ", start_model))

# How can I get better start values?
# to read: https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-starting-values.pdf
# Model was run 150 times, the parameter values can be seen in find_start_par.R
# vedba parameters:
# -----------------
#                 rest       active
# mean     0.039563939 2.907706e-01
# sd       0.036110078 1.776693e-01
# zeromass 0.001203833 3.109470e-11

par0_m1 = c(0.039563939, 0.036110078, 0.001203833, 
            2.907706e-01, 1.776693e-01, 3.109470e-11) 
m1 = fitHMM(data = tuco, 
            formula = ~ 1,
            nbStates = 2, 
            dist = list(vedba = "gamma"), 
            Par0 = list(vedba = par0_m1),
            stateNames =  stateNames,
            retryFits = retryFits_m1)

Sys.time() - start_model


##
## BLOCK 1
##

# Model 02: ~sex ----------------------------------------------------------
start_model = Sys.time()
message(paste0("M1, M2, M3 Start Time: ", start_model))

formula = ~sex
par0_m2 = getPar0(model = m1, formula = formula)
m2 %<-% fitHMM(data = tuco, 
               formula = formula,
               nbStates = 2, 
               dist = list(vedba = "gamma"), 
               Par0 = par0_m2$Par,
               #beta0 = par0_m2$beta,
               stateNames = stateNames,
               retryFits = retryFits)

# Model 03: ~season -------------------------------------------------------
formula = ~season
par0_m3 = getPar0(model = m1, formula = formula)
m3 %<-% fitHMM(data = tuco, 
               formula = formula,
               nbStates = 2, 
               dist = list(vedba = "gamma"), 
               Par0 = par0_m3$Par,
               #beta0 = par0_m3$beta,
               stateNames = stateNames,
               retryFits = retryFits)


# Model 04: ~ time --------------------------------------------------------
formula = ~ cosinor(time, period = 24)
par0_m4 = getPar0(model = m1, formula = formula)
m4 %<-% fitHMM(data = tuco,
               formula = formula,
               nbStates = 2,
               dist = list(vedba = "gamma"),
               Par0 = par0_m4$Par,
               #beta0 = par0_m4$beta,
               stateNames = stateNames,
               retryFits = retryFits)



# Wait for promises -------------------------------------------------------
f_m2 <- futureOf(m2)
f_m3 <- futureOf(m3)
f_m4 <- futureOf(m4)
message("Waiting for m2, m3 and m4 to be resolved ...\n")
while (!(resolved(f_m2) & resolved(f_m3) & resolved(f_m4))) {
    Sys.sleep(0.2)
}
Sys.time() - start_model
message("\n\n\n")
invisible(resolve(list(m2, m3, m4)))


##
## BLOCK 2
##


start_model = Sys.time()
message(paste0("M5, M6, M7, M8. Start Time: ", start_model))

# Model 05: ~ sex + time --------------------------------------------------------
formula = ~ sex + cosinor(time, period = 24)
par0_m5 = getPar0(model = m2, formula = formula)
m5 %<-% fitHMM(data = tuco,
               formula = formula,
               nbStates = 2,
               dist = list(vedba = "gamma"),
               stateNames = stateNames,
               Par0 = par0_m5$Par,
               #beta0 = par0_m5$beta,
               retryFits = retryFits)

# Model 07: ~ season + time --------------------------------------------------------
formula = ~ season + cosinor(time, period = 24)
par0_m7 = getPar0(model = m3, formula = formula)
m7 %<-% fitHMM(data = tuco,
               formula = formula,
               nbStates = 2,
               dist = list(vedba = "gamma"),
               stateNames = stateNames,
               Par0 = par0_m7$Par,
               #beta0 = par0_m7$beta,
               retryFits = retryFits)

# Model 06: ~ sex * time --------------------------------------------------------
formula = ~ sex * cosinor(time, period = 24)
par0_m6 = getPar0(model = m2, formula = formula)
m6 %<-% fitHMM(data = tuco,
               formula = formula,
               nbStates = 2,
               dist = list(vedba = "gamma"),
               stateNames = stateNames,
               Par0 = par0_m6$Par,
               #beta0 = par0_m6$beta,
               retryFits = retryFits)

# Model 08: ~ season * time --------------------------------------------------------
formula = ~ season * cosinor(time, period = 24)
par0_m8 = getPar0(model = m3, formula = formula)
m8 %<-% fitHMM(data = tuco,
               formula = formula,
               nbStates = 2,
               dist = list(vedba = "gamma"),
               stateNames = stateNames,
               Par0 = par0_m8$Par,
               #beta0 = par0_m8$beta,
               retryFits = retryFits)


# wait for promises -------------------------------------------------------
f_m5 <- futureOf(m5)
f_m6 <- futureOf(m6)
f_m7 <- futureOf(m7)
f_m8 <- futureOf(m8)
message("Waiting for m5, m6, m7, m8, m9 and m10 to be resolved ...\n")
while (!(resolved(f_m5) & resolved(f_m6) & resolved(f_m7) & resolved(f_m8) & resolved(f_m9) & resolved(f_m10))) {
    Sys.sleep(0.2)
}
message("\n\n\n")
Sys.time() - start_model

invisible(resolve(list(m5, m6, m7, m8)))





##
## BLOCK 4
##

# # Model 11: ~season + sex + time ------------------------------------------
# start_model = Sys.time()
# message(paste0("M11, M12, M13, M14 Start Time: ", start_model))
# 
# formula = ~season + sex + cosinor(time, period = 24)
# par0_m11 = getPar0(model = m1, formula = formula)
# m11 %<-% fitHMM(data = tuco,
#                 formula = formula,
#                 nbStates = 2,
#                 dist = list(vedba = "gamma"),
#                 #beta0 = par0_m11$beta,
#                 Par0 = par0_m11$Par)


# Model 12: ~(season + sex) * time ------------------------------------------
formula = ~(season + sex) * cosinor(time, period = 24)
par0_m12 = getPar0(model = m1, formula = formula)
m12 %<-% fitHMM(data = tuco,
                formula = formula,
                nbStates = 2,
                dist = list(vedba = "gamma"),
                Par0 = par0_m12$Par)
#beta0 = par0_m12$beta)


# Model 13: ~season * sex + time ------------------------------------------
# formula = ~season * sex + cosinor(time, period = 24)
# par0_m13 = getPar0(model = m1, formula = formula)
# m13 %<-% fitHMM(data = tuco,
#                 formula = formula,
#                 nbStates = 2,
#                 dist = list(vedba = "gamma"),
#                 Par0 = par0_m13$Par)
# #beta0 = par0_m13$beta)

# Model 14: ~season * sex * time ------------------------------------------
formula = ~season * cosinor(time, period = 24) * sex
par0_m14 = getPar0(model = m1, formula = formula)
m14 %<-% fitHMM(data = tuco,
                formula = formula,
                nbStates = 2,
                dist = list(vedba = "gamma"),
                Par0 = par0_m14$Par)
#beta0 = par0_m14$beta)

# wait for promises -------------------------------------------------------
f_m11 <- futureOf(m11)
f_m12 <- futureOf(m12)
f_m13 <- futureOf(m13)
f_m14 <- futureOf(m14)
message("Waiting for m11, m12, m13 and m14 to be resolved ...\n")
while (!(resolved(f_m11) & resolved(f_m12) & resolved(f_m13) & resolved(f_m14))) {
    Sys.sleep(0.2)
}
message("\n\n\n")
Sys.time() - start_time
invisible(resolve(list(m11, m12, m13, m14)))






# AICs --------------------------------------------------------------------
# -------------------------------------------------------------------------
AIC(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14)
message("\n----------------------\n","Total running time:\n")
(running_time = Sys.time() - start_script)
