library(momentuHMM)
library(data.table)

start_script = Sys.time()
tuco = readRDS("data/activity/tuco_01hz_vedba.rds")
#tuco = tuco[day_number == 1]
tuco$datetime = NULL
tuco$day_number =  NULL

tuco = prepData(tuco, 
                coordNames = NULL, 
                dataNames = c("vedba"), 
                covNames = c("sex", "season"))

# 3 state HMM models ------------------------------------------------------
stateNames <- c("rest","low", "high")


# Model 01: No Covariates -------------------------------------------------
start_time = Sys.time()
cat(paste0("Model 01 Start Time: ", start_time, "\n"))

par0 = c(0.019, 0.11, 0.36, 0.01, 0.05, 0.1, 0.0001, 0.0001, 0.0001)
(m1_3s = fitHMM(data = tuco, 
                formula = ~ 1,
                nbStates = 3, 
                dist = list(vedba = "gamma"), 
                Par0 = list(vedba = par0),
                stateNames =  stateNames))

cat("Model 01 Running Time: ", Sys.time() - start_time, "\n------\n\n\n")


# Model 02: ~season -------------------------------------------------------
start_time = Sys.time()
cat(paste0("Model 02 Start Time:\n", start_time, "\n"))

par0 = getPar0(model = m1_3s, formula = ~season)
(m2_3s = fitHMM(data = tuco, 
                formula = ~ season,
                nbStates = 3, 
                dist = list(vedba = "gamma"), 
                Par0 = par0$Par,
                stateNames = stateNames))

cat("Model 02 Running Time: ", Sys.time() - start_time, "\n------\n\n\n")


# Model 03: ~sex ----------------------------------------------------------
start_time = Sys.time()
cat(paste0("Model 03 Start Time: ", start_time, "\n"))

par0 = getPar0(model = m2_3s, formula = ~sex)
(m3_3s = fitHMM(data = tuco, 
                formula = ~ sex,
                nbStates = 3, 
                dist = list(vedba = "gamma"), 
                Par0 = par0$Par,
                stateNames = stateNames))

cat("Model 03 Running Time: ", Sys.time() - start_time, "\n------\n\n\n")


# Model 04: ~sex + season -------------------------------------------------
start_time = Sys.time()
cat(paste0("Model 04 Start Time: ", start_time, "\n"))

par0 = getPar0(model = m3_3s, formula = ~sex+season)
(m4_3s = fitHMM(data = tuco, 
                formula = ~ sex+season,
                nbStates = 3, 
                dist = list(vedba = "gamma"), 
                Par0 = par0$Par,
                stateNames = stateNames))

cat("Model 04 Running Time: ", Sys.time() - start_time, "\n------\n\n\n")


# Model 05: state dependent ~sex + season ---------------------------------
start_time = Sys.time()
cat(paste0("Model 05 Start Time: ", start_time, "\n"))

# formulas for parameters of state-dependent observation distributions
DM <- list(vedba = list(mean = ~ sex + season,
                        sd = ~ 1,
                        zeromass = ~1))

Par0 <- getPar0(model = m4_3s, formula = ~sex + season, DM = DM)
(m5_3s <- fitHMM(data = tuco, 
                 nbStates = 3, 
                 dist = list(vedba = "gamma"), 
                 Par0 = Par0$Par, 
                 #beta0 = Par0$beta, 
                 DM = DM, 
                 formula = ~ sex+season,
                 stateNames = stateNames))

cat("Model 05 Running Time: ", Sys.time() - start_time, "\n------\n")
cat("Total Running TIme: ", Sys.time() - start_script, "\n------\n\n\n")


# # Model 05: ~sex*season ---------------------------------------------------
# start_time = Sys.time()
# cat(paste0("-----\nModel 05 Start Time:", start_time))
# 
# par0 = getPar0(model = m3_3s, formula = ~sex*season)
# (m5_3s = fitHMM(data = tuco, 
#                 formula = ~ sex*season,
#                 nbStates = 3, 
#                 dist = list(vedba = "gamma"), 
#                 Par0 = par0$Par))
# 
# cat("Model 05 Running Time:", Sys.time() - start_time, "\n------\n\n\n")

# AICs --------------------------------------------------------------------

AIC(m1_3s, m2_3s, m3_3s, m4_3s, m5_3s)