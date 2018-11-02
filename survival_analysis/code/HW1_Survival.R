library(survival)
library(survminer)
library(muhaz)
library(dplyr)
katrina <- read.csv("C:/Users/Jerry/Documents/MSA18/Survival_Analysis/Data/katrina.csv", header = T, stringsAsFactors = F)

katrina[, c(9:56)] <- NULL
# Total number of pumps in the dataset
num_pumps = nrow(katrina)

katrina %>%
  group_by(reason) %>%
  summarize(`Frequency` = n(),
            `Percent of Pumps` = n()/num_pumps * 100,
            `Median Survival Time` = median(hour))

# plot the survival function
# survive == 0 is the event
katrina_surv <- survfit(Surv(time = hour, event = survive == 0) ~ reason, data = katrina)
summary(katrina_surv)
ggsurvplot(katrina_surv)

# do the log-rank test
survdiff(Surv(time = hour, event = survive == 0) ~ reason, rho = 0, data = katrina[katrina$reason!=0, ])
survdiff(Surv(time = hour, event = survive == 0) ~ reason, rho = 1, data = katrina[katrina$reason!=0, ])

# pairwise test
survminer::pairwise_survdiff(Surv(time = hour, event = survive == 0) ~ reason, rho = 0, data = katrina[katrina$reason!=0, ])
survminer::pairwise_survdiff(Surv(time = hour, event = survive == 0) ~ reason, rho = 1, data = katrina[katrina$reason!=0, ])

# plot the hazard function
katrina$hour2 <- ifelse(katrina$hour == 48 & katrina$survive == 1, 49, katrina$hour)
# turn survive values opposite
katrina$survive2 <- ifelse(katrina$survive == 0, 1, 0)
katrina_haz <- with(katrina, kphaz.fit(hour2, survive2))
kphaz.plot(katrina_haz, main = "hazard function")

# cumulative hazard
katrina_fit <- survfit(Surv(hour, survive == 1) ~ 1, data = katrina)
ggsurvplot(katrina_fit, fun = "cumhaz", palette = "grey")


