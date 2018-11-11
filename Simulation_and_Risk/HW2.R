  ####Simulation HW1########
####   Sophee     ########
#### 11/5/2018    ########
####  ........    ########
####Simulation HW2########
####   Chris      ########
#### 11/12/2018   ########


library(ks)
library(dplyr)
library(EnvStats)
library(triangle)
library(readxl)
library(rlist)
  
#-------------------------Data Preperation---------------------------------#
#I calculate average cost and average return in the original excel, and save to a csv file
drill <- read.csv("C:/Users/Jerry/Documents/MSA18/Simulation_Risk_Analysis/HW/Drilling Cost_1.csv")

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




# Year "0" Costs ----------------------------------------------------------

n_sims <- 50
lease_costs <- rnorm(n = n_sims, mean = 600, sd = 50) * 960
seismic_costs <- rnorm(n = n_sims, mean = 3, sd = 0.35) * 43000
completion_costs <- rnorm(n=n_sims, mean = 390000, sd=50000)
overhead_costs <- rtriangle(n=n_sims, a = 172000, b = 279500, c=215000)

# Production Risk ---------------------------------------------------------


Value.r <- rep(0,n_sims)
R <- matrix(data=cbind(1,0.64, 0.64, 1), nrow=2)
U <- t(chol(R))

Initial <- 1000

standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}


# Create correlated values of initial production costs and rates of decline
for(j in 1:n_sims){
  # why take log on the mean and sd?
  initial_production <- rlnorm(n = n_sims, meanlog = log(420), sdlog = log(120))
  rate_of_decline <- runif(n = n_sims, min = 0.15, max = 32 )
  
  Both.r <- cbind(standardize(rate_of_decline), standardize(initial_production))
  SB.r <- U %*% t(Both.r)
  SB.r <- t(SB.r)
  
  final.SB.r <- cbind(destandardize(SB.r[,1], rate_of_decline), destandardize(SB.r[,2], initial_production))
  
  Pt.B <- Initial*Perc.B
  Pt.S <- Initial*Perc.S
  for(i in 1:30){
    Pt.B <- Pt.B*(1 + final.SB.r[i,2])
    Pt.S <- Pt.S*(1 + final.SB.r[i,1])
  }
}

hist(Value.r, breaks=50, main='30 Year Value Distribution', xlab='Final Value')
abline(v = 1000, col="red", lwd=2)
mtext("Initial Inv.", at=1000, col="red")



# revenue -----------------------------------------------------------------

oil_pred <- read_excel("C:/Users/Jerry/Documents/MSA18/Simulation_Risk_Analysis/HW/Analysis_Data.xlsx")
View(oil_pred)
colnames(oil_pred) <- oil_pred[2,]
oil_pred <- oil_pred[c(3:nrow(oil_pred)),]

rev <- list()
for (i in 1:nrow(oil_pred)) {
  for(j in 1:n_sims){
    oil_price <- rtriangle(n = n_sims, a = oil_pred[i, 2], b = oil_pred[i, 1], c = oil_pred[i, 3])
    # production <- 
    nri <- rnorm(n = n_sims, mean = 0.75, sd = 0.02)
    rev_adj <- oil_price*production*nri*0.954
    # rev <- list.append(rev, rev_adj)
  }
}



# operating expense -------------------------------------------------------

for (i in 1:nrow(oil_pred)) {
  for(j in 1:n_sims){
    operating_cost <- rnorm(n = n_sims, mean = 2.25, sd = 0.3)
    total_operating_cost <- operating_cost*production
  }
}


# NPV ---------------------------------------------------------------------


