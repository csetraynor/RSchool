#Dynamic Prediction of Survival Probabilities

#joint model in R
#check path .libPaths()
library(lme4)
library(nlme)
library(JM)

data(pbc2.id, package = 'JM')
data(pbc2, package = 'JM')

head(pbc2.id)
head(pbc2)
pbc2.id$Pro <- with(pbc2.id, factor(prothrombin >= 10 & prothrombin <= 13, 
                                    labels = c("Abnormal", "Normal")))

pbc2$Pro <- rep(pbc2.id$Pro, tapply(pbc2$id, pbc2$id, length))

lmeFitBsp.pbc <- lme(fixed = log(serBilir) ~ bs(year, 4,  Boundary.knots = c(0,15)),
  random = list(
    id = pdDiag(form = ~ bs(year, 4, Boundary.knots = c(0,15)))),
  data = pbc2
)

coxFit.pbc <- coxph(Surv(years, status2) ~ drug + Pro,
                    data = pbc2.id, x = TRUE)

jointFitBsp.psb <- jointModel(lmeFitBsp.pbc, coxFit.pbc, timeVar = "year", method = "piecewise-PH-aGH")

set.seed(123)

survPrbs <- survfitJM(jointFitBsp.psb, newdata = pbc2[pbc2$id == 2,])
survPrbs
survPrbsEB <- survfitJM(jointFitBsp.psb, newdata = pbc2[pbc2$id == 2,], simulate = FALSE)
survPrbsEB

survPrbs2 <- survfitJM(jointFitBsp.psb, newdata = pbc2[pbc2$id == 2,], last.time = "years")
survPrbs2

survfitJM(jointFitBsp.psb, newdata = pbc2[pbc2$id == 2,], survTimes = c(14.5, 15),last.time = "years")

plot(survPrbs, lty = c(1:2, 3, 3), conf.int = TRUE)

ND <- pbc2[pbc2$id == 2,]
survPreds <- vector("list", nrow(ND))
for(i in 1:nrow(ND)){
  set.seed(123)
  survPreds[[i]] <- survfitJM(jointFitBsp.psb, newdata = ND[1:i,])
}

#plot the updated survival curves
par(mfrow = c(2,2), oma = c(0,2,0,2))

for(i in c(1,3,5,7)){
  plot(survPreds[[i]], estimator = "median", conf.int = TRUE, include.y = TRUE, main = paste("Follow-up time: ",
                                                                                             round(survPreds[[i]]$last.time, 1)))
}
mtext("log serum bilirubin", side = 2, line = -1, outer = TRUE)
mtext("Survival Probability", side = 4, line = -1, outer = TRUE)
