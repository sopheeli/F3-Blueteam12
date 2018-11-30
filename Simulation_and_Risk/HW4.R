library(graphics)
library(quantmod)
library(TTR)
library(ks)
library(scales)
library(truncnorm)
library(stats)
library(ggplot2)
library(triangle)
library(readxl)
library(dplyr)
library(EnvStats)
library(readr)

setwd("C:/Users/Jerry/Documents/MSA18/Simulation_Risk_Analysis/HW/")
#setwd("C:/Users/Sophe/Desktop/FALL/Fall3/SimulationandRiskAnalysis/Project")
# number of single well simulations
n_sims <- 100000
# the number of years
n_years <- 15
set.seed(8888)

# oil price 
oil_pred <- read_excel("Analysis_Data.xlsx")
oil_pred <- oil_pred[c(3:nrow(oil_pred)),]
oil_pred <- data.frame(oil_pred)
colnames(oil_pred) <- c("year", "high_price", "low_price", "aeo_ref")
oil_pred$high_price <- as.numeric(oil_pred$high_price)
oil_pred$low_price <- as.numeric(oil_pred$low_price)
oil_pred$aeo_ref <- as.numeric(oil_pred$aeo_ref)

# tax
severance_tax <- (1 - 0.046)

# drilling cost data
# import data from the XLSX 
drill = read_excel("Analysis_Data.xlsx", sheet = 2, skip = 2)

# shortens data set to relivent years 
drill1 <- drill %>% filter(Date >= "1991-06-01") %>% filter(Date <= "2006-07-01")

# change character to a numeric 
drill1$Return.Crude.Oil = (as.numeric(drill1$`Arithmetic Return - Crude Oil`))
drill1$Return.Natural.Gas = (as.numeric(drill1$`Arithmetic Return - Natural Gas`))
drill1$Return.Dry.Well = (as.numeric(drill1$`Arithmetic Return - Dry Well`))

# creates average for the cost 
drill1$Average.Cost = ((drill1$`U.S. Nominal Cost per Crude Oil Well Drilled (Thousand Dollars per Well)`+
                          
                          drill1$`U.S. Nominal Cost per Natural Gas Well Drilled (Thousand Dollars per Well)` +
                          
                          drill1$`U.S. Nominal Cost per Dry Well Drilled (Thousand Dollars per Well)`)/3)
x = as.data.frame(c(drill1$Return.Crude.Oil, drill1$Return.Natural.Gas, drill1$Return.Dry.Well))

##################################################################################
# whether well is wet ----------------------------------------------------
drilling_all <- c()
lease_all <- c()
seismic_all <- c()
completion_all <- c()
overhead_all <- c()
NPV_all <- c()
dry_cost_all <- c()
final_NPV <- c()
for (i in 1:n_sims) {
  n_wells = round(runif(1, min = 10, max = 30))
  Hydrocarbons = rtruncnorm(n_wells, a=0, b=1, mean=0.99, sd = 0.05)
  Reservoir = rtruncnorm(n_wells, a=0, b=1, mean=0.8, sd = 0.1)
  probability = (Hydrocarbons*Reservoir)
  wet <- c()
  for (i in 1:n_wells) {
    wet <- c(wet, rbinom(n=1, 1, probability[i]))
  }
  n_wet <- sum(wet)
  n_dry <- n_wells - n_wet
  
  # Kernel Estimation for 2006 to 2012 based on historical data from 1991 to 2006
  Density.R <- density(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well),bw="SJ-ste")
  Density.R
  Est.R <- rkde(fhat=kde(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well), h=0.07935), n=n_wells)
  Est.Hist = as.data.frame(Est.R)
  
  # drilling cost simulation - Kernel Density -------------------------------
  drilling_cost <- rep(0,n_wells)
  for (i in 1:n_wells){
    r1 <- rkde(fhat=kde(c(drill1$Return.Crude.Oil,drill1$Return.Natural.Gas,drill1$Return.Dry.Well),
                        h=0.07935), n=6)
    r2 <- rtri(n=3, mode = -0.0917, min = -0.22, max = -0.07)
    r3 <- rtri(n=4, mode = 0.05, min = 0.02, max = 0.06)
    P <- drill1$Average.Cost[16]
    r <- append(r1,append(r2,r3)) # give the rate of return from years 2006 to 2019, 13 years
    
    for (j in 1:13){
      P <- P*(1+r[j]) # give the cost prediction for 2019 
    }
    drilling_cost[i] <- P # outputs cost prediciton for 2019 to vector "cost_k"
  }
  drilling_cost <- drilling_cost * 1000
  drilling_all <- c(drilling_all, drilling_cost)
  # Year "0" Costs ----------------------------------------------------------
  #  leased acres per well - Normally Distributed
  #  Each acre costs us $960 (this will be a recurring annual cost if we find a "WET" well)
  lease_costs <- rnorm(n = n_wells, mean = 600, sd = 50) * 960
  lease_all <- c(lease_all, lease_costs)
  # Number of seismic sections per well, Normally Distributed
  # Each section (per well) costs us $43,000
  seismic_costs <- rnorm(n = n_wells, mean = 3, sd = 0.35) * 43000
  seismic_all <- c(seismic_all, seismic_costs)
  # These costs will only be incurred IF the well is "WET"
  # So, "DRY" wells will not incur this cost
  # Normally distributed
  completion_costs <- rnorm(n = n_wet, mean = 390000, sd=50000)
  completion_all <- c(completion_all, completion_costs)
  
  # These costs always occur year 0, and recur only if the well is "WET"
  # So, "DRY" wells only have this cost for year 0
  # It remains constant throughout a well's lifetime
  # Triangular distribution
  overhead_costs_wet <- rtriangle(n=n_wet, a = 172000, b = 279500, c=215000)
  
  if (n_dry != 0){
    overhead_costs_dry <- rtriangle(n=n_dry, a = 172000, b = 279500, c=215000)
  } else {
    overhead_costs_dry <- 0
  }
  
  overhead_all <- c(overhead_all, overhead_costs_wet, overhead_costs_dry)
  # Production Risk ---------------------------------------------------------
  # We must simulate the oil production rate, described with
  # 1. Initial production  (lognormally distributed)
  # 2. Decline Rate (uniform distribution, b/w 15% and 32%)
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
  initial_production <- rlnorm(n = n_wet, meanlog = 6, sdlog = 0.28)
  rate_of_decline <- runif(n = n_wet, min = 15, max = 32)
  
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
  bopd_year_beg <- matrix(0L, nrow=n_wet, ncol = n_years)
  bopd_year_end <- matrix(0L, nrow=n_wet, ncol = n_years)
  
  # This matrix represents the total volume of oil produced by each well (row) in that year (columns)
  yearly_volume <- matrix(0L, nrow=n_wet, ncol = n_years)
  
  # We know the initial production rates
  bopd_year_beg[,1] <- final.ip
  
  # This could maybe be optimized using matrix math, but for loops will have to do for now
  for (year in seq(1:n_years)) {
    for (well_num in seq(1:n_wet)) {
      if (year != 1) {
        bopd_year_beg[well_num, year] = bopd_year_end[well_num, year-1]
      }
      bopd_year_end[well_num, year] = (1 - final.rod[well_num])*bopd_year_beg[well_num, year]
      yearly_volume[well_num, year] = 365 * (bopd_year_beg[well_num, year] + bopd_year_end[well_num, year])/2
    }
  }

  # Calculate Net Present Revenue (NPR) -----------------------------------------------------------------
  rev <- c()
  operating_cost <- c()
  
  # Draw a net revenue interest (nri) from a normal distribution
  nri <- rnorm(n = n_wet, mean = 0.75, sd = 0.02)
  
  for (year in 1:n_years) {
    
    # Draw the oil price from a random triangle distribution
    oil_price <- rtriangle(n = n_wet, a = oil_pred$low_price[year + 1],
                           b = oil_pred$high_price[year + 1],
                           c = oil_pred$aeo_ref[year + 1])
    
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
    sum <- sum + (rev[, i] - operating_cost[, i] - overhead_costs_wet) / (1.1^i)
  }
  
  NPV <- -(completion_costs + seismic_costs[1:n_wet] + drilling_cost[1:n_wet] +
             overhead_costs_wet + lease_costs[1:n_wet]) + sum
  
  NPV_all <- c(NPV_all,NPV)
  
  if (n_dry == 0) {
    dry_cost = 0
  } else {
    dry_cost <- overhead_costs_dry + lease_costs[(n_wet + 1):n_wells] + seismic_costs[(n_wet + 1):n_wells] + drilling_cost[(n_wet + 1):n_wells]
    dry_cost_all <- c(dry_cost_all,dry_cost)  
  }
  
  final_NPV <- c(final_NPV, (sum(NPV) - sum(dry_cost)))
}
final_NPV

##############################################################################

final_NPV <- final_NPV / 1000000
summary(final_NPV)

VaR_percentile = 0.01
VaR <- quantile(final_NPV, VaR_percentile)
VaR_percentile_1 = 0.0001
VaR_zero <- quantile(final_NPV, VaR_percentile_1)

ES = mean(final_NPV[final_NPV <= VaR])

# Confidence Intervals for Value at Risk & Expected Shortfall - Bootstrap Approach #
n.bootstraps <- 1000
sample.size <- 1000

VaR.boot <- rep(0,n.bootstraps)
ES.boot <- rep(0,n.bootstraps)
for(i in 1:n.bootstraps){
  bootstrap.sample <- sample(final_NPV, size=sample.size)
  VaR.boot[i] <- quantile(bootstrap.sample,VaR_percentile, na.rm=TRUE)
  ES.boot[i] <- mean(bootstrap.sample[bootstrap.sample < VaR.boot[i]], na.rm=TRUE)
}

VaR.boot.U <- quantile(VaR.boot, 0.975, na.rm=TRUE)
VaR.boot.L <- quantile(VaR.boot, 0.025, na.rm=TRUE)
dollar(VaR.boot.L)
dollar(VaR)
dollar(VaR.boot.U)

hist(VaR.boot, breaks = 50, main='Possible Value at Risk', xlab='VaR - Million USD')
abline(v = VaR.boot.L, col="blue", lwd=2, lty="dashed")
abline(v = VaR.boot.U, col="blue", lwd=2, lty="dashed")


ES.boot.U <- quantile(ES.boot, 0.975, na.rm=TRUE)
ES.boot.L <- quantile(ES.boot, 0.025, na.rm=TRUE)
dollar(ES.boot.L)
dollar(ES)
dollar(ES.boot.U)

df_ES.boot <- data.frame(ES.boot = ES.boot)

ggplot(df_ES.boot) +
  geom_histogram(aes(x = ES.boot), bins = 50) + 
  geom_vline(aes(xintercept = ES.boot.U, color = "Upper Bound"), linetype="dotted") +
  geom_vline(aes(xintercept = ES.boot.L, color = "Lower Bound"), linetype="dotted") +
  xlab("ES - Million USD") +
  ylab("Frequency") +
  labs(title = "Possible Expected Shortfall")

hist(ES.boot, breaks = 50, main='Possible Expected Shortfall', xlab='ES - Million USD')
abline(v = ES.boot.U, col="blue", lwd=2, lty="dashed")
abline(v = ES.boot.L, col="blue", lwd=2, lty="dashed")

#graphs ----------------------------------------------------------
well <- data.frame(final_NPV = final_NPV)
ggplot(well) +
  geom_histogram(aes(x = final_NPV), bins = 50) +
  xlab("NPV - Million USD") +
  ylab("Frequency") +
  labs(title = "Possible Final Net Present Value") +
  geom_vline(aes(xintercept = median(final_NPV), color = "Median")) +
  geom_vline(aes(xintercept = VaR, color = "1% VaR")) +
  geom_vline(aes(xintercept = VaR.boot.U, color = "Upper Bound VaR")) +
  geom_vline(aes(xintercept = VaR.boot.L, color = "Lower Bound VaR"))+
  theme_classic() +
  theme(legend.title=element_blank())

#drilling cost
drilling <- data.frame(drilling = drilling_all/1000000)
VaR_drilling <- quantile(drilling_all/1000000, 0.99)

ggplot(drilling) +
  geom_histogram(aes(x = drilling), bins = 100) +
  xlab("Drilling Cost - Million USD") +
  ylab("Frequency") +
  labs(title = "Possible Drilling Cost of a Single Well")+
  geom_vline(aes(xintercept = VaR_drilling, color = "99% VaR"))+
  theme_classic() +
  theme(legend.title=element_blank())
summary(drilling)

#NPV
npv_1 <- data.frame(npv = NPV_all/1000000)
var_npv <- quantile(NPV_all/1000000, 0.01)
ggplot(npv_1) +
  geom_histogram(aes(x = npv), bins = 100) +
  xlab("Net Present Value - Million USD") +
  ylab("Frequency") +
  labs(title = "Possible Net Present Value of a Single Wet Well")+
  geom_vline(aes(xintercept = var_npv, color = "1% VaR"))+
  geom_vline(aes(xintercept = median(npv), color = "Median"))+
  theme_classic() +
  theme(legend.title=element_blank())
summary(npv_1)

#dry cost
dry_cost_1 <- data.frame(dry_cost = dry_cost_all/1000000)
var_drycost <- quantile(dry_cost/1000000, 0.99)
ggplot(dry_cost_1) +
  geom_histogram(aes(x = dry_cost), bins = 100) +
  xlab("Cost - Thousand USD") +
  ylab("Frequency") +
  labs(title = "Possible Cost of a Single Dry Well") +
  geom_vline(aes(xintercept = median(dry_cost), color = "Median"), linetype="dotted") +
  geom_text(aes(x=median(dry_cost), label=as.character(round(median(dry_cost), 1)), y=20)) +
  geom_vline(aes(xintercept = var_drycost, color = "99% VaR"))+
  theme_classic() +
  theme(legend.title=element_blank())
summary(dry_cost_1)


length(final_NPV[final_NPV < 0]) / length(final_NPV)
