#How to do a forest plot in ggplot
library(plyr)
library(ggplot2) 

#------------load data----------------#example
d <- read.csv('E:/Thesis/BPHrepsnew/Dataset/nm7_first_treatment_time_to_first_event_dataset.csv', header = T, na='.')
#------------------------------------

#add here your base events # example #

library(ggplot2)

findrelrisk <- function(nBase, obsBase, nVar, obsVar){ 
  relrisk =  (obsVar/nVar) / (obsBase/nBase)
  alpha <- 0.05 #select you prefered alpha 
  z <- qnorm(1-(alpha/2))
  
  lbound = round(log(exp(relrisk) + (-z * sqrt ( ( (nVar - obsVar)/( nVar*obsVar) ) + ( nBase - obsBase) / (nBase * obsBase) ) )),4)
  ubound = round(log(exp(relrisk) + (z * sqrt ( ( (nVar - obsVar)/( nVar*obsVar) ) + ( nBase - obsBase) / (nBase * obsBase) ) )),4) 
    
output <- c(relrisk,lbound,ubound)
return(output)
}

findrelrisk(100, 50, 50, 25)

findrelrisk2 <- function(nBase, obsBase,...){
  VarList <- list(...)
  
  
  relrisk =  (obsVar/nVar) / (obsBase/nBase)
  alpha <- 0.05 #select you prefered alpha 
  z <- qnorm(1-(alpha/2))
  
  lbound = round(log(exp(relrisk) + (-z * sqrt ( ( (nVar - obsVar)/( nVar*obsVar) ) + ( nBase - obsBase) / (nBase * obsBase) ) )),4)
  ubound = round(log(exp(relrisk) + (z * sqrt ( ( (nVar - obsVar)/( nVar*obsVar) ) + ( nBase - obsBase) / (nBase * obsBase) ) )),4) 
}

#roc.plot2 <- function(...,x.var="fpr",y.var="tpr",x.lab=x.var,y.lab=y.var,ident=TRUE){
 # fitList <- list(...)
  
  


df <- ddply(t, .(ARM),
                     summarise, 
                     E = sum(DVX) ,
                     N = length(unique(ID)),
                     INCIDENCE = round((E / N),4),
                     RELATIVE_RISK = log(round(rr(INCIDENCE, incidenceBase),4)),
                     LOWER = round(((RELATIVE_RISK) + (-z * sqrt ( ( (N - E)/( N*E) ) + ( numberBase - eventBase) / (numberBase * eventBase) ) )),4),
                     UPPER = round(((RELATIVE_RISK) + (z * sqrt ( ( (N - E)/( N*E) ) + ( numberBase - eventBase) / (numberBase * eventBase) ) )),4) )

#plot script

fp <- ggplot(data=df, aes(x=ARM, y=RELATIVE_RISK, ymin=LOWER, ymax=UPPER)) +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  geom_pointrange() + 
  scale_x_discrete()+
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  labs(y="Log RR (95% CI)", x="Categories under study", title="Forest plot" )+
  theme_bw()
  #facet_wrap(~Symptoms, labeller = label_both)# use if need to facet
print(fp)