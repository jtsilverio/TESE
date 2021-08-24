# # 2 states ----------------------------------------------------------------
# start_time = Sys.time()
# cat(paste0("-----\nModel 01 Start Time:", start_time))
# 
# par0 = c(0.019, 0.36, 0.01, 0.1, 0.0001, 0.0001)
# (m0_2s = fitHMM(data = tuco, 
#                 formula = ~ 1,
#                 nbStates = 2, 
#                 dist = list(vedba = "gamma"), 
#                 Par0 = list(vedba = par0)))
# cat("Model 01 Running Time:", Sys.time() - start_time, "\n------\n\n\n")
# 
# 
# start_time = Sys.time()
# cat(paste0("-----\nModel 01 Start Time:", start_time))
# par0 = getPar0(model = m0_2s, formula = ~season)
# (m1_2s = fitHMM(data = tuco, 
#                 formula = ~ season,
#                 nbStates = 2, 
#                 dist = list(vedba = "gamma"), 
#                 Par0 = list(vedba = par0$Par$vedba)))
# 
# 
# Sys.time()
# par0 = getPar0(model = m1_2s, formula = ~sex)
# (m2_2s = fitHMM(data = tuco, 
#                 formula = ~ sex,
#                 nbStates = 2, 
#                 dist = list(vedba = "gamma"), 
#                 Par0 = list(vedba = par0$Par$vedba)))
# 
# 
# Sys.time()
# par0 = getPar0(model = m2_2s, formula = ~sex+season)
# (m3_2s = fitHMM(data = tuco, 
#                 formula = ~ sex+season,
#                 nbStates = 2, 
#                 dist = list(vedba = "gamma"), 
#                 Par0 = list(vedba = par0$Par$vedba)))
# 
# Sys.time()
# par0 = getPar0(model = m3_2s, formula = ~sex+season)
# (m4_2s = fitHMM(data = tuco, 
#                 formula = ~ sex+season,
#                 nbStates = 2, 
#                 dist = list(vedba = "gamma"), 
#                 Par0 = list(vedba = par0$Par$vedba)))
# Sys.time()
# par0 = getPar0(model = m3_2s, formula = ~sex*season)
# (m4_2s = fitHMM(data = tuco, 
#                 formula = ~ sex*season,
#                 nbStates = 2, 
#                 dist = list(vedba = "gamma"), 
#                 Par0 = list(vedba = par0$Par$vedba)))
