####Simulation HW1########
####   Sophee     ########
#### 11/5/2018    ########

library(ks)
library(dplyr)
library(EnvStats)

#-------------------------Data Preperation---------------------------------#
#I calculate average cost and average return in the original excel, and save to a csv file
drill <- read.csv("C:/Users/Sophe/Desktop/FALL/Fall3/SimulationandRiskAnalysis/Project/HW1/Drilling Cost_1.csv")

#subset only to look year 1990 to 2007
drill1 <- subset(drill, drill$Date>1990 & drill$Date<2007)

#change the percent return from factor to numeric values
drill1$Return.Crude.Oil <- as.numeric(as.character(drill1$Return.Crude.Oil))
drill1$Return.Natural.Gas <- as.numeric(as.character(drill1$Return.Natural.Gas))
drill1$Return.Dry.Well <- as.numeric(as.character(drill1$Return.Dry.Well))
drill1$Average.Return <- as.numeric(as.character(drill1$Average.Return))

#histogram for average cost and average return
hist(drill1$Average.Cost,main = "Cost Distribution",xlab="Cost")
hist(drill1$Average.Return,main = "Arithmetic Change in Cost Distribution",xlab="Arithmetic Return")

#-------------------------Kernel Density---------------------------------#

#Kernel Estimation for 2006 to 2012 based on historical data from 1990 to 2006
Density.R <- density(drill1$Average.Return,bw="SJ-ste")
Density.R

Est.R <- rkde(fhat=kde(drill1$Average.Return, h=0.04457), n=10000)
hist(Est.R, breaks=50, main='Estimated 2006 to 2012 Return Value Distribution', xlab='Return')

#qq-plot -- not sure which one to look at
qqnorm(drill1$Average.Return)


#-------------------------Simulation---------------------------------#
#Simulation1: Kernel Density - should I use triangular dist. for r2 and r3?
#seems wrong, cannot assume mean is mode
#but cannot use normal dist. as well, b/c don't know std
#need discussion

n = 10000
cost_k <- rep(0,n)
class(cost_k)

for (i in 1:n){
  
  r1 <- rkde(fhat=kde(drill1$Average.Return, h=0.04457), n=6)
  r2 <- rtri(n=3, mode = -0.0917, min = -0.22, max = -0.07)
  r3 <- rtri(n=4, mode = 0.05, min = 0.02, max = 0.06)

  P <- drill1$Average.Cost[16]
  
  r <- append(r1,append(r2,r3))
  
  for (j in 1:13){
    P <- P*(1+r[j])
  }

  cost_k[i] <- P
}

mean(cost_k)
median(cost_k)
hist(cost_k, breaks=50, main='2019 Cost Prediction Distribution', xlab='Cost')
abline(v = drill1$Average.Cost[16], col="red", lwd=2)
mtext("2006 Cost", at=drill1$Average.Cost[16], col="red")

#Simulation2: normal distribution
n = 10000
cost_n <- rep(0,n)
class(cost_n)

for (i in 1:n){
  
  r1 <- rnorm(n=6, mean = mean(drill1$Average.Return), sd = sd(drill1$Average.Return))
  r2 <- rtri(n=3, mode = -0.0917, min = -0.22, max = -0.07)
  r3 <- rtri(n=4, mode = 0.05, min = 0.02, max = 0.06)
  
  P <- drill1$Average.Cost[16]
  
  r <- append(r1,append(r2,r3))
  
  for (j in 1:13){
    P <- P*(1+r[j])
  }
  
  cost_n[i] <- P
}

mean(cost_n)
median(cost_n)
hist(cost_n, breaks=50, main='2019 Cost Prediction Distribution', xlab='Cost')
abline(v = drill1$Average.Cost[16], col="red", lwd=2)
mtext("2006 Cost", at=drill1$Average.Cost[16], col="red")






#----------------------------------Draft-------------------------------------#
P2006 <- drill1$Average.Cost[17]
P2007 <- P2006*(1+r)
hist(P2007)
P2008 <- P2007*(1+r)
P2009 <- P2008*(1+r)
P2010 <- P2009*(1+r)
P2011 <- P2010*(1+r)
P06_12 <- (P2006+P2007+P2008+P2009+P2010+P2011)/6
hist(P06_12)

