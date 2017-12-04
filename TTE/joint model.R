#joint model in R
#check path .libPaths()
library(lme4)
library(nlme)
library(JM)

#function lme from nlme

data(aids, package = 'JM')

head(aids)

#time dependent Cox

td.Cox <- coxph(Surv(start, stop, event) ~ drug + CD4,  data = aids)
summary(td.Cox)

#create dataset for Cox fit

aids.id <- aids[!duplicated(aids$patient),]

#this code ilustrates how to fit the joint model

lmeFit.aids <- lme(CD4 ~ obstime + obstime:drug, random = ~ obstime|patient, data=aids)
coxFit.aids <- coxph(Surv(Time, death)~ drug, data= aids.id, x= TRUE, model = TRUE)
jointFit.aids <- jointModel(lmeFit.aids, coxFit.aids, timeVar = "obstime", method = "piecewise-PH-aGH")

summary(jointFit.aids)

#lets a diagonal cov matrix pdDiag() in the random argument of lme(), which it must be taken into account in the High Dimensional problem.

#opimisation

jointFit.aids <- jointModel(lmeFit.aids, coxFit.aids, timeVar = "obstime", method = "piecewise-PH-aGH", 
                            iter.EM = 80, tol3 = 1e-09, numeriDeriv = "cd", eps.Hes = 1e-04)

