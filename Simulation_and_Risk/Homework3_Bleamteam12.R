# Needed Libraries for Analysis #

library(graphics)
library(quantmod)
library(TTR)
library(ks)
library(scales)
library(truncnorm)
library(stats)
library(ggplot2)

set.seed(245642)

Hydrocarbons = rtruncnorm(100000, a=0, b=1, mean=0.99, sd = 0.05)

dfHydrocarbons = as.data.frame(Hydrocarbons)

ggplot(dfHydrocarbons, aes(x=Hydrocarbons)) +
  geom_histogram(fill="gray", color="black") +
  labs(title="Hydrocarbon Distribution",x="Proportion Hydrocarbons", y = "Frequency")+
  theme_minimal() + theme(title = element_text(size = 18), axis.title.x = element_text(size =14), axis.title.y = element_text(size =14))

Reservoir = rtruncnorm(100000, a=0, b=1, mean=0.8, sd = 0.1)

dfReservoir = as.data.frame(Reservoir)

ggplot(dfReservoir, aes(x=Reservoir)) +
  geom_histogram(fill="gray", color="black") +
  labs(title="Reservoir Distribution",x="Proportion Reservoir", y = "Frequency")+
  theme_minimal() + theme(title = element_text(size = 18), axis.title.x = element_text(size =14), axis.title.y = element_text(size =14))


n = 100000
nn = 1000

proportion_productive = rep(0,n)
num_wet = rep(0,n)
num_dry = rep(0,n)


for(i in 1:n){

  Total_wells = round(runif(1, min = 10, max = 30))
  
  wet_wells <- rep(0,Total_wells)
  
  Hydrocarbons = rtruncnorm(1000, a=0, b=1, mean=0.99, sd = 0.05)
  
  Reservoir = rtruncnorm(1000, a=0, b=1, mean=0.8, sd = 0.1)
  
  probability = (Hydrocarbons*Reservoir)

  productive_well <- rep(0,Total_wells)
  
  for(j in 1:Total_wells){
    wet_wells[j] = rbinom(n=1, 1, probability)
  }
  
  proportion_productive[i] <- sum(wet_wells) / length(wet_wells)
  
  num_wet[i] = sum(wet_wells)
  
  num_dry[i] = length(wet_wells) - sum(wet_wells)
}



hist(proportion_productive)
hist(num_dry)
hist(num_wet)



VaR.percentile <- 0.05
VaR = quantile(proportion_productive, probs = VaR.percentile)
ES = mean(ifelse(proportion_productive <= VaR, proportion_productive, NA), na.rm = TRUE)
ES2 = mean(proportion_productive[proportion_productive <= VaR])



hist(proportion_productive, breaks=20)
abline(v = VaR, col="red", lwd=2)
mtext("VaR", at=VaR, col="red")
abline(v = ES, col="blue", lwd=2)
mtext("ES", at=ES, col="blue")



pp = as.data.frame(proportion_productive)
ggplot(pp, aes(x=proportion_productive)) +
  geom_histogram(fill="gray", color="black") +
  geom_vline(xintercept = VaR, colour = "red") +
  geom_text(aes(x=(VaR+0.05), label="VaR", y=7500, size = 18), colour="red") +
  geom_vline(xintercept = (ES-0.05), colour = "blue") +
  geom_text(aes(x=ES, label="ES", y=7500, size = 18), colour="blue") +
  labs(title="Estimated 2019 Proportion Productive Well Distribution",x="Proportion Productive Well", y = "Frequency")+
  theme_minimal() + theme(title = element_text(size = 18), axis.title.x = element_text(size =14), axis.title.y = element_text(size =14))