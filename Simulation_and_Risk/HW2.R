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




# Year "0" Costs ----------------------------------------------------------


n_sims <- 50

#  leased acres per well - Normally Distributed
#  Each acre costs us $960 (this will be a recurring annual cost if we find a "WET" well)
lease_costs <- rnorm(n = 50, mean = 600, sd = 50) * 960

# Number of seismic sections per well, Normally Distributed
# Each section (per well) costs us $43,000
seismic_costs <- rnorm(n = 50, mean = 3, sd = 0.35) * 43000

# These costs will only be incurred IF the well is "WET"
# So, "DRY" wells will not incur this cost
# Normally distributed
completion_costs <- rnorm(n=50, mean = 390000, sd=50000)

# These costs always occur year 0, and recur only if the well is "WET"
# So, "DRY" wells only have this cost for year 0
# It remains constant throughout a well's lifetime
# Triangular distribution
overhead_costs <- rtriangle(n=n_sims, a = 172000, b = 279500, c=215000)



# Production Risk ---------------------------------------------------------

# We must simulate the oil production rate, described with
# 1. Initial production  (lognormally distributed)
# 2. Decline Rate (uniform distribution, b/w 15% and 32%)
#
# These values are correlated (0.64), so we will use a correlation matrix to bend the random initial

n_wells <- 50 # the number of wells we are considering

Value.r <- rep(0, n_wells)
R <- matrix(data=cbind(1, 0.64, 0.64, 1), nrow=2)
U <- t(chol(R))


standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

# Placeholder for the final correlated values of initial produciton and rate of decline
final.ip <-  rep(0, n_wells)
final.rod <- rep(0, n_wells)

# Create correlated values of initial production costs and rates of decline

# Underlying lognormal distribution should have mean ~ 6 and sd ~0.28
# TODO fix sdlog - does not produce dist with sd of 0.28
initial_production <- rlnorm(n = n_wells, meanlog = 6, sdlog = 0.28)
rate_of_decline <- runif(n = n_wells, min = 15, max = 32 )

# Standardize the vectors and "bend" them to correlate
Both.r <- cbind(standardize(rate_of_decline), standardize(initial_production))
SB.r <- U %*% t(Both.r)
SB.r <- t(SB.r)

# Unstandardize the vectors
final.ip  <- destandardize(SB.r[,2], initial_production)
final.rod <- destandardize(SB.r[,1], rate_of_decline) / 100

# Any negative rates corresond to a "dry" well
final.ip <- ifelse(final.ip < 0, 0, final.ip)




# Check the correlation, if desired - my initial tests showed it was approx. 0.64
# cor(final.ip, final.rod)

# Now calculate estimated production rates for 30 years using the calculated
# initial production and yearly decline rate

# These matrices represent the production of oil in barrels per day
# Each row is one well
# Each column is the year (col 1 == year 1)
bopd_year_beg <- matrix(0L, nrow=n_wells, ncol = 30)
bopd_year_end <- matrix(0L, nrow=n_wells, ncol = 30)

# This matrix represents the total volume of oil produced by each well (row) in that year (columns)
yearly_volume <- matrix(0L, nrow=n_wells, ncol = 30)

# We know the initial production rates
bopd_year_beg[,1] <- final.ip

# This could maybe be optimized using matrix math, but for loops will have to do for now
for (year in seq(1:30)) {
  for (well_num in seq(1:n_wells)) {
    if (year != 1) {
      bopd_year_beg[well_num, year] = bopd_year_end[well_num, year-1]
    }
    bopd_year_end[well_num, year] = (1 - final.rod[well_num])*bopd_year_beg[well_num, year]
    yearly_volume[well_num, year] = 365 * (bopd_year_beg[well_num, year] + bopd_year_end[well_num, year])/2
  }
}


plot(yearly_volume[6,])



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

