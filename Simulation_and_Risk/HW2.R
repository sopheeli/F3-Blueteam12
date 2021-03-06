  ####Simulation HW1########
####   Sophee     ########
#### 11/5/2018    ########
####  ........    ########
####Simulation HW2########
####   Chris      ########
#### 11/12/2018   ########

library(ks)
library(triangle)
library(graphics)
library(readxl)
library(dplyr)
library(EnvStats)
library(ggplot2)
library(readr)

#set up working directory
#setwd("C:/Users/Jerry/Documents/MSA18/Simulation_Risk_Analysis/HW/")
setwd("C:/Users/Christopher/Documents/IAA/Fall 3/simulation")
  
  
# number of single well simulations
n_sims <- 500000
n = 500000
n_wells <- n_sims # the number of wells we are considering
n_years <- length(seq(2019,2050)) # the number of years
set.seed(8888)

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


x = as.data.frame(c(drill1$Return.Crude.Oil, drill1$Return.Natural.Gas, drill1$Return.Dry.Well))

#-------------------------Kernel Density---------------------------------#

#Kernel Estimation for 2006 to 2012 based on historical data from 1991 to 2006

Density.R <- density(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well),bw="SJ-ste")

Density.R

Est.R <- rkde(fhat=kde(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well), h=0.07935), n=10000)

Est.Hist = as.data.frame(Est.R)


#-------------------------Simulation---------------------------------#

##############Simulation1: Kernel Density ####################################### 

cost_k <- rep(0,n)

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

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 

#367.4  2577.8  3476.5  3769.7  4634.9 27030.9 

#    5%      95% 
#  1643.848 6893.007

#    1%      99% 
#  1187.543 9006.787 

#-------------------------Simulation---------------------------------#

########Simulation2: normal distribution###############################

cost_n <- rep(0,n)


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

# Year "0" Costs ----------------------------------------------------------

#  leased acres per well - Normally Distributed
#  Each acre costs us $960 (this will be a recurring annual cost if we find a "WET" well)
lease_costs <- rnorm(n = n_sims, mean = 600, sd = 50) * 960

# Number of seismic sections per well, Normally Distributed
# Each section (per well) costs us $43,000
seismic_costs <- rnorm(n = n_sims, mean = 3, sd = 0.35) * 43000

# These costs will only be incurred IF the well is "WET"
# So, "DRY" wells will not incur this cost
# Normally distributed
completion_costs <- rnorm(n=n_sims, mean = 390000, sd=50000)

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

# Create correlated values of initial production costs and rates of decline
# Underlying lognormal distribution should have mean ~ 6 and sd ~0.28
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


# Calculate Net Present Revenue (NPR) -----------------------------------------------------------------

# Jerry's access

#oil_pred <- read_excel("C:/Users/Jerry/Documents/MSA18/Simulation_Risk_Analysis/HW/Analysis_Data.xlsx")
#oil_pred <- oil_pred[c(3:nrow(oil_pred)),]
#oil_pred <- data.frame(oil_pred)
colnames(oil_pred) <- c("year", "high_price", "low_price", "aeo_ref")

column_names <- c("year", "high_price", "low_price", "aeo_ref")
# # Chris' access 
file_path <- "C:/Users/Christopher/Documents/IAA/Fall 3/F3-Blueteam12/Simulation_and_Risk/price_projections.csv"
oil_pred <- read_csv(file = file_path,
                     skip = 3,
                     col_names = column_names)


oil_pred$high_price <- as.numeric(oil_pred$high_price)
oil_pred$low_price <- as.numeric(oil_pred$low_price)
oil_pred$aeo_ref <- as.numeric(oil_pred$aeo_ref)


severance_tax <- (1 - 0.046)
rev <- c()
operating_cost <- c()

# Draw a net revenue interest (nri) from a normal distribution
nri <- rnorm(n = n_sims, mean = 0.75, sd = 0.02)

for (year in 1:n_years) {
  
    # Draw the oil price from a random triangle distribution
    oil_price <- rtriangle(n = n_sims, a = oil_pred$low_price[year],
                           b = oil_pred$high_price[year],
                           c = oil_pred$aeo_ref[year])
    
    #Grab that year's total volume produced
    production <- yearly_volume[, year]
    
    # Calculate revenue for the year
    rev <- cbind(rev,
                 oil_price*production*nri*severance_tax)
    
    # Now calculate total operating costs
    cost <- rnorm(n = 1, mean = 2.25, sd = 0.3)
    total_operating_cost <- cost*production
    operating_cost <- cbind(operating_cost, total_operating_cost)
}

# NPV ---------------------------------------------------------------------

sum <- 0
for (i in 1:n_years) {
  sum <- sum + (rev[, i] - operating_cost[, i] - overhead_costs) / (1.1^i)
}

NPV <- -(completion_costs + seismic_costs + cost_n + overhead_costs + lease_costs) + sum

dry_cost <- overhead_costs + lease_costs + seismic_costs + cost_n

NPV <- NPV / 1000
dry_cost <- dry_cost / 1000
well <- data.frame(cbind(NPV / 1000, dry_cost / 1000))
head(well)

ggplot(well) +
  geom_histogram(aes(x = NPV), bins = 50) +
  xlab("NPV - Thousand USD") +
  ylab("Frequency") +
  labs(title = "Possible Net Present Value of a Single Wet Well") +
  geom_vline(aes(xintercept = median(NPV), color = "Median")) +
  geom_vline(aes(xintercept = quantile(NPV, 0.001), color = "0.1% VaR")) +
  theme_classic() +
  theme(legend.title=element_blank())
  

ggplot(well) +
  geom_histogram(aes(x = dry_cost)) +
  xlab("Cost - Thousand USD") +
  ylab("Frequency") +
  labs(title = "Possible Cost of a Single Dry Well") +
  geom_vline(aes(xintercept = median(dry_cost), color = "Median")) +
  geom_vline(aes(xintercept = quantile(dry_cost, 0.001), color = "0.1% VaR")) +
  theme_classic() +
  theme(legend.title=element_blank())
  

# NPV ----------------------------------------
# median
median(NPV)

# worst case
min(NPV)
# negative NPV well rate
length(NPV[NPV < 0 ]) / length(NPV)

# .1% 
VaR_NPV <- quantile(NPV, 0.001)

# CVaR
mean(NPV[NPV <= VaR_NPV])

# dry cost ------------------------------------
# median
median(dry_cost)
# worst case
max(dry_cost)

# .1%
VaR_dry_cost <- quantile(dry_cost, 0.999)

# CVaR
mean(dry_cost[dry_cost >= VaR_dry_cost])
