##############survival Analysis hw3####################
##############  Sophee             ####################
##############  11/15/2018         ####################

library(survival)
library(survminer)
library(visreg)
library(dplyr)
library(flexsurv)
library(ggplot2)
library(tidyr)

katrina <- read.csv("C:/Users/Jerry/Documents/MSA18/Survival_Analysis/Data/katrina.csv", header = T, stringsAsFactors = F)

katrina$ID <- 1:nrow(katrina)

#-----------------------------AFT Model-------------------------------------#
#Weibull
fit_wbaft <-  flexsurvreg(Surv(time = hour, event = reason %in% c(2,3)) ~ backup + bridgecrane + servo + 
                 trashrack + elevation + slope + age, data = katrina, dist = "weibull")

plot(fit_wbaft, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "cumulative hazard", main = "weibull distribution")

# exponential distribution
  fit_expaft <- flexsurvreg(Surv(time = hour, event = reason %in% c(2,3)) ~ backup + bridgecrane + servo + 
                           trashrack + elevation + slope + age, data = katrina, dist = "exponential")
  
  plot(fit_expaft, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", 
       xlab = "Hour", ylab = "cumulative hazard",
       main = "exponential distribution")

# lognormal
fit_lnormaft <- flexsurvreg(Surv(time = hour, event = reason  %in% c(2,3)) ~ backup + bridgecrane + servo + 
                           trashrack + elevation + slope + age, data = katrina, dist = "lognormal")
# plot shows lack of fit in very early time periods
plot(fit_lnormaft, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "cumulative hazard", main = "lognormal distribution")

# log-logistic
fit_llogisaft <- flexsurvreg(Surv(time = hour, event = reason %in% c(2,3)) ~ backup + bridgecrane + servo + 
                            trashrack + elevation + slope + age, data = katrina, dist = "llogis")

plot(fit_llogisaft, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "cumulative hazard",
     main = "log-logistic distribution")

#create AFT model
fit_aft <- survreg(Surv(time = hour, event = reason %in% c(2,3)) ~ backup + bridgecrane + servo + 
                 trashrack + elevation + slope + age, data = katrina, dist = "weibull")
summary(fit_aft)
exp(coef(fit_aft)) # exponentiate estimates

#------------------------------------Cox Model------------------------------------------#

# fit proportional hazards model using coxph()
fit_cox <- coxph(Surv(time = hour, event = reason %in% c(2,3)) ~ backup + 
               bridgecrane + servo + trashrack + elevation + slope + age, data = katrina)
summary(fit_cox)

# plot survival curve
ggsurvplot(survfit(fit_cox), data = katrina, legend = "none", break.y.by = 0.1,
           xlab = "hour", ylab = "survival probability")

fit_cox$means


######check constant effect########################
#backup
fit_strat_backup <- coxph(Surv(hour, reason %in% c(2,3)) ~ strata(backup) + bridgecrane + 
                     servo + trashrack + elevation + slope + age, data = katrina)

ggsurvplot(survfit(fit_strat_backup), data = katrina, fun = "cloglog",
           palette = c("black", "purple"), legend.labs = c("no-upgrade", "upgrade"),
           legend.title = "backup", xlab = "log(hour)")

#bridgecrane
fit_strat_bridgecrane <- coxph(Surv(hour, reason %in% c(2,3)) ~ backup + strata(bridgecrane) + 
                     servo + trashrack + elevation + slope + age, data = katrina)

ggsurvplot(survfit(fit_strat_bridgecrane), data = katrina, fun = "cloglog",
           palette = c("black", "purple"), legend.labs = c("no-upgrade", "upgrade"),
           legend.title = "bridgecrane", xlab = "log(hour)")

#servo
fit_strat_servo <- coxph(Surv(hour, reason %in% c(2,3)) ~ backup + bridgecrane + 
                                 strata(servo) + trashrack + elevation + slope + age, data = katrina)

ggsurvplot(survfit(fit_strat_servo), data = katrina, fun = "cloglog",
           palette = c("black", "purple"), legend.labs = c("no-upgrade", "upgrade"),
           legend.title = "servo", xlab = "log(hour)")

#trashrack
fit_strat_trashrack <- coxph(Surv(hour, reason %in% c(2,3)) ~ backup + bridgecrane + 
                                 servo + strata(trashrack) + elevation + slope + age, data = katrina)

ggsurvplot(survfit(fit_strat_trashrack), data = katrina, fun = "cloglog",
           palette = c("black", "purple"), legend.labs = c("no-upgrade", "upgrade"),
           legend.title = "trashrack", xlab = "log(hour)")


#######checking linearity, Matt said we don't need to#############################
# elevation
visreg(fit_cox, "elevation", xlab = "elevation", ylab = "partial residuals", gg = TRUE,
       band = FALSE) +
  geom_smooth(col = "red", fill = "red") + theme_bw()
# slope
visreg(fit_cox, "slope", xlab = "slope", ylab = "partial residuals",
       gg = TRUE, band = FALSE) +
  geom_smooth(col = "red", fill = "red") + theme_bw()
#age
visreg(fit_cox, "age", xlab = "age", ylab = "partial residuals",
       gg = TRUE, band = FALSE) +
  geom_smooth(col = "red", fill = "red") + theme_bw()

### concordance
concordance(fit_cox)
corcordance(fit_aft)


resids <- data.frame(event = fit_cox$y[,dim(fit_cox$y)[2]],
                     time = fit_cox$y[,dim(fit_cox$y)[2] - 1],
                     res_m = residuals(fit_cox, type = "martingale"),
                     res_d = residuals(fit_cox, type = "deviance"),
                     ID = 1:length(residuals(fit_cox)))

# martingale vs. time
ggplot(resids, aes(x = time, y = res_m, color = factor(event))) +
  geom_point() +
  labs(x = "hour", y = "martingale residuals", color = "event") +
  scale_color_manual(values = c("purple", "orange"))

# deviance vs. time
ggplot(resids, aes(x = time, y = res_d, color = factor(event))) +
  geom_point() +
  labs(x = "hour", y = "deviance residuals", color = "event") +
  scale_color_manual(values = c("purple", "orange"))

# deviance vs. ID, to see which one is the largest
ggplot(resids, aes(x = ID, y = res_d, color = factor(event))) +
  geom_point() +
  labs(x = "ID", y = "deviance residuals", color = "event") +
  scale_color_manual(values = c("purple", "orange"))

# change the format of data to use time-dependent variable
katrina$ID <- factor(katrina$ID)
colnames(katrina)[9:56] <- 1:48
katrina_long <- gather(katrina, stop, value, 9:56, factor_key = T)
katrina_long$stop <- as.numeric(katrina_long$stop)
katrina_long$start <-  katrina_long$stop - 1

katrina_long$count <- 0

