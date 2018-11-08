#--------------------------#

#         JCP HW 1         #

#        Simulation        #

#                          #

#--------------------------#

# Needed Libraries for Analysis #

library(graphics)

#install.packages("ks")

library(ks)

# install.packages("xlsx")

# library("xlsx")

# install.packages("readxl") # CRAN version

library(readxl)

library(dplyr)

#install.packages("EnvStats") # CRAN version

library(EnvStats)

library(ggplot2)

#set up working directory

setwd("C:/Users/Sophe/Desktop/FALL/Fall3/SimulationandRiskAnalysis/Project/HW1/")

#import data from the XLSX 

drill = read_excel("Analysis_Data.xlsx", sheet = 2, skip = 2)

#shortens data set to relivent years 

drill1 <- drill %>% filter(Date >= "1991-06-01") %>% filter(Date <= "2006-07-01")

#change character to a numeric 

drill1$Return.Crude.Oil = (as.numeric(drill1$`Arithmetic Return - Crude Oil`))

drill1$Return.Natural.Gas = (as.numeric(drill1$`Arithmetic Return - Natural Gas`))

drill1$Return.Dry.Well = (as.numeric(drill1$`Arithmetic Return - Dry Well`))

#creates average for the cost 

drill1$Average.Cost = ((drill1$`U.S. Nominal Cost per Crude Oil Well Drilled (Thousand Dollars per Well)`+
                          
                          drill1$`U.S. Nominal Cost per Natural Gas Well Drilled (Thousand Dollars per Well)` +
                          
                          drill1$`U.S. Nominal Cost per Dry Well Drilled (Thousand Dollars per Well)`)/3)

#histogram for average cost and average return

hist(c(drill1$Return.Crude.Oil, drill1$Return.Natural.Gas, drill1$Return.Dry.Well), 
     
     main = "Arithmetic Change in Cost Distribution",xlab="Arithmetic Return", col = "#6699FF", breaks = 24)

# Basic histogram

x = as.data.frame(c(drill1$Return.Crude.Oil, drill1$Return.Natural.Gas, drill1$Return.Dry.Well))

ggplot(x, aes(x=c(drill1$Return.Crude.Oil, drill1$Return.Natural.Gas, drill1$Return.Dry.Well))) +
  
  geom_histogram(fill="#6699FF", color="black", binwidth=.075)+
  
  #  geom_histogram(fill="#6699FF", color="black")+
  
  labs(title="Arithmetic Change in Cost Distribution",x="Arithmetic Return", y = "Frequency")+
  
  theme_minimal() + theme(title = element_text(size = 18), axis.title.x = element_text(size =14), axis.title.y = element_text(size =14))

#-------------------------Kernel Density---------------------------------#

#Kernel Estimation for 2006 to 2012 based on historical data from 1991 to 2006

set.seed(8888)

Density.R <- density(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well),bw="SJ-ste")

Density.R

Est.R <- rkde(fhat=kde(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well), h=0.07935), n=10000)

hist(Est.R, breaks=50, main='Estimated 2006 to 2012 Return Value Distribution', xlab='Return')

Est.Hist = as.data.frame(Est.R)

ggplot(Est.Hist, aes(x=Est.R)) +
  
  #geom_histogram(fill="#6699FF", color="black", binwidth=.075)+
  
  geom_histogram(fill="#6699FF", color="black")+
  
  labs(title="Estimated 2006 to 2012 Return Value Distribution",x="Return", y = "Frequency")+
  
  theme_minimal() + theme(title = element_text(size = 18), axis.title.x = element_text(size =14), axis.title.y = element_text(size =14))

#qq-plot -- not sure which one to look at

qqnorm(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well), col = "#6699FF", main="QQ Plot Historical Data")

qqline(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well), col = "2")

#Shapiro-Wilk normality test p-value = 0.8041

shapiro.test(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well))

summary(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well))

kurtosis(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well))

#-------------------------Simulation---------------------------------#

##############Simulation1: Kernel Density ####################################### 

#should I use triangular dist. for r2 and r3?

#seems wrong, cannot assume mean is mode

#but cannot use normal dist. as well, b/c don't know std

#need discussion

n = 500000

cost_k <- rep(0,n)

class(cost_k)

for (i in 1:n){
  
  
  
  r1 <- rkde(fhat=kde(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well),
                      
                      h=0.07935), n=6)
  
  r2 <- rtri(n=3, mode = -0.0917, min = -0.22, max = -0.07)
  
  r3 <- rtri(n=4, mode = 0.05, min = 0.02, max = 0.06)
  
  P <- drill1$Average.Cost[16]
  
  
  
  r <- append(r1,append(r2,r3)) # give the rate of return from years 2006 to 2019, 13 years
  
  
  
  for (j in 1:13){
    
    P <- P*(1+r[j]) # give the cost prediction for 2019 
    
  }
  
  cost_k[i] <- P # outputs cost prediciton for 2019 to vector "cost_k"
  
}

summary(cost_k) #summary statistics for cost_k 

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 

#367.4  2577.8  3476.5  3769.7  4634.9 27030.9 

quantile(cost_k, probs = c(0.05, 0.95))

#    5%      95% 

#  1643.848 6893.007

quantile(cost_k, probs = c(0.01, 0.99))

#    1%      99% 

#  1187.543 9006.787 

hist(cost_k, breaks=100, main='2019 Cost Prediction Distribution KD', xlab='Cost', col = "#6699FF")

abline(v = drill1$Average.Cost[16], col="red", lwd=2)

mtext("2006 Cost", at=drill1$Average.Cost[16], col="red")

#-------------------------Simulation---------------------------------#

########Simulation2: normal distribution###############################

n = 500000

cost_n <- rep(0,n)

class(cost_n)

for (i in 1:n){
  
  
  
  r1 <- rnorm(n=6, mean = mean(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well)), 
              
              sd = sd(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well)))
  
  r2 <- rtri(n=3, mode = -0.0917, min = -0.22, max = -0.07)
  
  r3 <- rtri(n=4, mode = 0.05, min = 0.02, max = 0.06)
  
  
  
  P <- drill1$Average.Cost[16]
  
  
  
  r <- append(r1,append(r2,r3))
  
  
  
  for (j in 1:13){
    
    P <- P*(1+r[j])
    
  }
  
  
  
  cost_n[i] <- P
  
}

summary(cost_n) #summary statistics for cost_n 

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 

#292.2  2670.8  3528.6  3770.5  4600.2 16280.9 

quantile(cost_n, probs = c(0.05, 0.95))

#   5%      95% 

#  1748.740 6618.248 

quantile(cost_n, probs = c(0.01, 0.99))

#   1%      99% 

#  1270.605 8434.950 

hist(cost_n, breaks=100, main='2019 Cost Prediction Distribution N', xlab='Cost', col = "#6699FF")

abline(v = drill1$Average.Cost[16], col="red", lwd=2)

mtext("2006 Cost", at=drill1$Average.Cost[16], col="red")