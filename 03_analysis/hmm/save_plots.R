library(momentuHMM)
load(file = "models/all_models.RData")

# PRes --------------------------------------------------------------------
# -------------------------------------------------------------------------
png(filename = "figures/fits/hist/m1.png", width = 1200, height = 1000)
plot(m1, ask = F)
dev.off()

png(filename = "figures/fits/hist/m2.png", width = 1200, height = 1000)
plot(m2, ask = F)
dev.off()

png(filename = "figures/fits/hist/m3.png", width = 1200, height = 1000)
plot(m3, ask = F)
dev.off()


png(filename = "figures/fits/hist/m4.png", width = 1200, height = 1000)
plot(m4, ask = F)
dev.off()


png(filename = "figures/fits/hist/m5.png", width = 1200, height = 1000)
plot(m5, ask = F)
dev.off()


png(filename = "figures/fits/hist/m6.png", width = 1200, height = 1000)
plot(m6, ask = F)
dev.off()


png(filename = "figures/fits/hist/m7.png", width = 1200, height = 1000)
plot(m7, ask = F)
dev.off()


png(filename = "figures/fits/hist/m8.png", width = 1200, height = 1000)
plot(m8, ask = F)
dev.off()


png(filename = "figures/fits/hist/m9.png", width = 1200, height = 1000)
plot(m9, ask = F)
dev.off()


png(filename = "figures/fits/hist/m10.png", width = 1200, height = 1000)
plot(m10, ask = F)
dev.off()


png(filename = "figures/fits/hist/m11.png", width = 1200, height = 1000)
plot(m11, ask = F)
dev.off()


png(filename = "figures/fits/hist/m12.png", width = 1200, height = 1000)
plot(m12, ask = F)
dev.off()


png(filename = "figures/fits/hist/m13.png", width = 1200, height = 1000)
plot(m13, ask = F)
dev.off()


png(filename = "figures/fits/hist/m14.png", width = 1200, height = 1000)
plot(m14, ask = F)
dev.off()




# PRes --------------------------------------------------------------------
# -------------------------------------------------------------------------
png(filename = "figures/fits/PR/m1PR.png", width = 1200, height = 1000)
plotPR(m1, lag.max = 10000, ncores = 6)
dev.off()

png(filename = "figures/fits/PR/m2PR.png", width = 1200, height = 1000)
plotPR(m2, lag.max = 10000, ncores = 6)
dev.off()

png(filename = "figures/fits/PR/m3PR.png", width = 1200, height = 1000)
plotPR(m3, lag.max = 10000, ncores = 6)
dev.off()


png(filename = "figures/fits/PR/m4PR.png", width = 1200, height = 1000)
plotPR(m4, lag.max = 10000, ncores = 6)
dev.off()


png(filename = "figures/fits/PR/m5PR.png", width = 1200, height = 1000)
plotPR(m5, lag.max = 10000, ncores = 6)
dev.off()


png(filename = "figures/fits/PR/m6PR.png", width = 1200, height = 1000)
plotPR(m6, lag.max = 10000, ncores = 6)
dev.off()


png(filename = "figures/fits/PR/m7PR.png", width = 1200, height = 1000)
plotPR(m7, lag.max = 10000, ncores = 6)
dev.off()


png(filename = "figures/fits/PR/m8PR.png", width = 1200, height = 1000)
plotPR(m8, lag.max = 10000, ncores = 6)
dev.off()


png(filename = "figures/fits/PR/m9PR.png", width = 1200, height = 1000)
plotPR(m9, lag.max = 10000, ncores = 6)
dev.off()


png(filename = "figures/fits/PR/m10PR.png", width = 1200, height = 1000)
plotPR(m10, lag.max = 10000, ncores = 6)
dev.off()


png(filename = "figures/fits/PR/m11PR.png", width = 1200, height = 1000)
plotPR(m11, lag.max = 10000, ncores = 6)
dev.off()


png(filename = "figures/fits/PR/m12PR.png", width = 1200, height = 1000)
plotPR(m12, lag.max = 10000, ncores = 6)
dev.off()


png(filename = "figures/fits/PR/m13PR.png", width = 1200, height = 1000)
plotPR(m13, lag.max = 10000, ncores = 6)
dev.off()


png(filename = "figures/fits/PR/m14PR.png", width = 1200, height = 1000)
plotPR(m14, lag.max = 10000, ncores = 6)
dev.off()
