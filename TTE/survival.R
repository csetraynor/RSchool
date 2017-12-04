#check path .libPaths()
library(lme4)
library(nlme)
library(JM)

#load data
data(pbc2.id, package = 'JM')

pbc2.id$status2 <- as.numeric(pbc2.id$status != 'alive')

coxFit <- coxph(Surv( years, status2) ~ drug +age+ sex, data = pbc2.id)
summary(coxFit)

#time dependent vars
data(prothro, package = 'JM')

tdCox.pro <- coxph(Surv(start, stop, event)~ pro+treat, data = prothro)
summary(tdCox.pro)
