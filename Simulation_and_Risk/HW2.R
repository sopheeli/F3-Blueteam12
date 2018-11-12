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
  
# Needed Libraries for Analysis #

library(graphics)

# install.packages("xlsx")

# library("xlsx")

# install.packages("readxl") # CRAN version

library(readxl)

library(dplyr)

#install.packages("EnvStats") # CRAN version

library(EnvStats)

library(ggplot2)

#set up working directory

setwd("C:/Users/Jerry/Documents/MSA18/Simulation_Risk_Analysis/HW/")

# simulation times 
n_sims <- 10000
n = 10000

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


# Year "0" Costs ----------------------------------------------------------

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

n_wells <- n_sims # the number of wells we are considering
n_years <- length(seq(2019,2050)) # the number of years

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

View(final.ip)


# Check the correlation, if desired - my initial tests showed it was approx. 0.64
# cor(final.ip, final.rod)

# Now calculate estimated production rates for 30 years using the calculated
# initial production and yearly decline rate

# These matrices represent the production of oil in barrels per day
# Each row is one well
# Each column is the year (col 1 == year 1)
bopd_year_beg <- matrix(0L, nrow=n_wells, ncol = n_years)
bopd_year_end <- matrix(0L, nrow=n_wells, ncol = n_years)

# This matrix represents the total volume of oil produced by each well (row) in that year (columns)
yearly_volume <- matrix(0L, nrow=n_wells, ncol = n_years)

# We know the initial production rates
bopd_year_beg[,1] <- final.ip

# This could maybe be optimized using matrix math, but for loops will have to do for now
for (year in seq(1:n_years)) {
for (well_num in seq(1:n_wells)) {
  if (year != 1) {
    bopd_year_beg[well_num, year] = bopd_year_end[well_num, year-1]
  }
  bopd_year_end[well_num, year] = (1 - final.rod[well_num])*bopd_year_beg[well_num, year]
  yearly_volume[well_num, year] = 365 * (bopd_year_beg[well_num, year] + bopd_year_end[well_num, year])/2
}
}

View(yearly_volume)
plot(yearly_volume[6,])


# revenue -----------------------------------------------------------------

oil_pred <- read_excel("C:/Users/Jerry/Documents/MSA18/Simulation_Risk_Analysis/HW/Analysis_Data.xlsx")
View(oil_pred)
colnames(oil_pred) <- oil_pred[2,]
oil_pred <- oil_pred[c(3:nrow(oil_pred)),]
oil_pred <- data.frame(oil_pred)
oil_pred$High.Oil.Price <- as.numeric(oil_pred$High.Oil.Price)
oil_pred$Low.Oil.Price <- as.numeric(oil_pred$Low.Oil.Price)
oil_pred$AEO2018.Reference <- as.numeric(oil_pred$AEO2018.Reference)

rev <- c()
for (i in 1:n_years) {
    oil_price <- rtriangle(n = n_sims, a = oil_pred[i, 3], b = oil_pred[i, 2], c = oil_pred[i, 4])
    production <- yearly_volume[, i]
    nri <- rnorm(n = n_sims, mean = 0.75, sd = 0.02)
    rev_adj <- oil_price*production*nri*0.954
    rev <- cbind(rev, rev_adj)
}

colnames(rev) <- seq(2019, 2050)

# operating expense -------------------------------------------------------

operating_cost <- c()
for (i in 1:n_years) {
  cost <- rnorm(n = n_sims, mean = 2.25, sd = 0.3)
  production <- yearly_volume[, i]
  total_operating_cost <- cost*production
  operating_cost <- cbind(operating_cost, total_operating_cost)
}

# NPV ---------------------------------------------------------------------

sum <- 0
for (i in 1:n_years) {
  sum <- sum + (rev[, i] - operating_cost[, i] - lease_costs[i] - overhead_costs[i]) / (1.1^i)
}
NPV <- -(cost_n + seismic_costs + completion_costs) + sum
NPV
