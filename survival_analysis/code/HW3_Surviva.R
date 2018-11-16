##############survival Analysis hw3####################
##############  Sophee             ####################
##############  11/15/2018         ####################

library(survival)
library(survminer)
library(visreg)
library(dplyr)
library(flexsurv)
library(ggplot2)

katrina <- read.csv("C:/Users/Sophe/Desktop/FALL/Fall3/SurvivalAnalysis/Data/katrina.csv", header = T, stringsAsFactors = F)

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

# shrinkage factor demonstration:
# full model
df_model <- length(fit_cox$coefficients) # number of coefficients in model
LR_model <- 2*diff(fit_cox$loglik) # LRT statistic from model
(v_full <- 1 - (df_model/LR_model)) # estimate shrinkage factor
eta_shrunk <- v_full*predict(fit_cox, newdata = katrina, type = "lp") # shrunken predictions

# fit a smaller model
fit_small <- coxph(Surv(hour, reason %in% c(2,3)) ~ backup + servo + elevation , data = katrina)
df_small <- length(fit_small$coefficients)
LR_small <- 2*diff(fit_small$loglik)
(v_small <- 1 - (df_small/LR_small))
c(v_full, v_small) # smaller model estimates don't need to be shrunk as much

# see the effect of just adding a whole bunch of junk
# generate 17 random variables that i know are useless
junk <- matrix(rnorm(17*nrow(katrina)), nrow(katrina), 17)
fit_junk <- coxph(Surv(hour, reason %in% c(2,3)) ~ backup + 
    bridgecrane + servo + trashrack + elevation + slope + age, data = katrina)
df_junk <- length(fit_junk$coefficients)
LR_junk <- 2*diff(fit_junk$loglik)
(v_junk <- 1 - (df_junk/LR_junk))
c(v_full, v_small, v_junk) # need to shrink the junk model estimates much more

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
  labs(x = "week", y = "martingale residuals", color = "event") +
  scale_color_manual(values = c("purple", "orange"))

# deviance vs. time
ggplot(resids, aes(x = time, y = res_d, color = factor(event))) +
  geom_point() +
  labs(x = "week", y = "deviance residuals", color = "event") +
  scale_color_manual(values = c("purple", "orange"))

# deviance vs. ID, to see which one is the largest
ggplot(resids, aes(x = ID, y = res_d, color = factor(event))) +
  geom_point() +
  labs(x = "ID", y = "deviance residuals", color = "event") +
  scale_color_manual(values = c("purple", "orange"))

#######checking linearity#############################
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

#----------------------------------------------------------------------#

fit_strata <- coxph(Surv(time = hour, event = reason %in% c(2,3)) ~ strata(backup) + 
               bridgecrane + servo + trashrack + elevation + slope + age, data = katrina)
ggsurvplot(survfit(fit_strata), data = katrina, fun='cloglog',
           palette = c("black", "purple"), legend.labs = c("noupgrade", "upgrade"),
           legend.title = "backup", xlab = "log(hour)")
summary(fit_strata)
