pr = momentuHMM::pseudoRes(m1)
qqnorm(y = pr$vedbaRes)
qqline(y = pr$vedbaRes, col = 2)

nmodels = list(m1, m2, m3, m4, m1_2s)
par(mfrow= c(length(nmodels)/2, 2))
for (i in 1:length(nmodels)) {
    pr = pseudoRes(nmodels[[i]])
    qqnorm(y = pr$vedbaRes)
    qqline(y = pr$vedbaRes, col = 2)
}
par(mfrow = c(1, 1))
