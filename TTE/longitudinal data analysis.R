#check path .libPaths()
library(lme4)
library(nlme)
library(JM)

#function lme from nlme

data(aids, package = 'JM')

#random intercepts model
aids[aids$patient %in% c(1,2), c('patient','CD4', 'obstime')]

lmeFit.int <-  lme(CD4 ~ obstime, random =~1 | patient, data = aids)
summary(lmeFit.int)

margCov.int <- getVarCov(lmeFit.int, individuals = 12, type = 'marginal')
margCov.int
cov2cor(margCov.int[[1]])

#random intercepts and random slopes model
lmeFit.slp <-  lme(CD4 ~ obstime, random =~ obstime | patient, data = aids)
summary(lmeFit.slp)
margCov.slp <- getVarCov(lmeFit.slp, individuals = 12, type = 'marginal')
margCov.slp
cov2cor(margCov.slp[[1]])
