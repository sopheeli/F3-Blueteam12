#########Survival hw2 ####################
#########11/10/2018   ####################

library(survival)
library(survminer)
library(muhaz)
library(dplyr)
library(flexsurv)


#katrina <- read.csv("C:/Users/Sophe/Desktop/FALL/Fall3/SurvivalAnalysis/Data/katrina.csv", header = T, stringsAsFactors = F)
katrina <- read.csv("~/Documents/RStudio/Survival_analysis/survivalcsv/katrina.csv", header = TRUE, stringsAsFactors = FALSE)

#-------------------------choose distributions -------------------------------------------#
# weibull distribution - best fit for the kattina data
fit_wb <- flexsurvreg(Surv(time = hour, event = reason == 1) ~ backup + bridgecrane + servo + 
                        trashrack + elevation + slope + age, data = katrina, dist = "weibull")

plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "cumulative hazard", main = "weibull distribution")

summary(fit_wb)
exp(coef(fit_wb)) # exponentiate estimates

#well_20 = katrina %>% filter(reason == 1) 
#well_20 = arrange(well_20, desc(scale)) 


# exponential distribution
fit_exp <- flexsurvreg(Surv(time = hour, event = reason == 1) ~ backup + bridgecrane + servo + 
                         trashrack + elevation + slope + age, data = katrina, dist = "exponential")

plot(fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", 
     xlab = "Hour", ylab = "cumulative hazard",
     main = "exponential distribution")



# lognormal
fit_lnorm <- flexsurvreg(Surv(time = hour, event = reason == 1) ~ backup + bridgecrane + servo + 
                           trashrack + elevation + slope + age, data = katrina, dist = "lognormal")
# plot shows lack of fit in very early time periods
plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "cumulative hazard", main = "lognormal distribution")



# log-logistic
fit_llogis <- flexsurvreg(Surv(time = hour, event = reason == 1) ~ backup + bridgecrane + servo + 
                            trashrack + elevation + slope + age, data = katrina, dist = "llogis")

plot(fit_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "Hour", ylab = "cumulative hazard",
     main = "log-logistic distribution")

#--------------------create ATF model---------------------------------------#
#only interested in flood, so reason ==1
fit <- survreg(Surv(time = hour, event = reason == 1) ~ backup + bridgecrane + servo + 
                 trashrack + elevation + slope + age, data = katrina, dist = "weibull")
summary(fit)
exp(coef(fit)) # exponentiate estimates

#---------------------find pumps to ungrade---------------------------------#

survprob_actual <- 1 - psurvreg(katrina$hour,
                                mean = predict(fit, type = "lp"),
                                scale = fit$scale,
                                distribution = fit$dist)
head(survprob_actual)

##############backup
katrina_nobackup <- katrina %>% 
  # first, we need to get the linear predictor from the old model
  mutate(old_lp = predict(fit, type = "lp"),
         # add ID variable so we know which pumps they are
         ID = row_number()) %>%
  # next, we're only interested in those failed by flooding AND no backup available 
  dplyr::filter(reason == 1, backup ==0) %>%
  # now i'm doing two things:
  # 1. find the survival prob at the time pump failed
  # 2. pretending the backup is upgraded (change fin from 0 to 1)
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = fit$scale,
                                  distribution = fit$dist),
         old_backup = backup,
         backup = old_backup + 1)

# now with that dataset, i need to find their new time
results_backup <- katrina_nobackup %>%
  # estimate their new linear predictor value if they had backup upgraded
  mutate(new_lp = predict(fit, newdata = katrina_nobackup, type = "lp"),
         # now, keeping the same survival probability as before, get the time
         # corresponding to the new_lp value
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = fit$scale,
                             distribution = fit$dist),
         # and compute the difference
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)%>%
  mutate(new_time = ifelse (new_time >=48, 48, new_time)) %>%
  mutate(pred_time_diff = new_time - old_time) %>%
  #filter(new_time <= 48) %>%
  arrange(desc(pred_time_diff))
head(results_backup)


##############servo
katrina_noservo <- katrina %>% 
  mutate(old_lp = predict(fit, type = "lp"),
         ID = row_number()) %>%
  dplyr::filter(reason == 1, servo ==0) %>%
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = fit$scale,
                                  distribution = fit$dist),
         old_servo = servo,
         servo = old_servo + 1)

results_servo <- katrina_noservo %>%
  mutate(new_lp = predict(fit, newdata = katrina_noservo, type = "lp"),
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = fit$scale,
                             distribution = fit$dist),
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)%>%
  mutate(new_time = ifelse (new_time >=48, 48, new_time)) %>%
  mutate(pred_time_diff = new_time - old_time) %>%
  #filter(new_time <= 48) %>%
  arrange(desc(pred_time_diff))
head(results_servo)


#compare the two variable
sum(results_backup$pred_time_diff[1:20])
sum(results_servo$pred_time_diff[1:20])

results_servo[1:20,]

view(results_servo)

#----------------------------codes from hw1---------------------------#

#katrina[, c(9:56)] <- NULL # Removes unnecessary columns
# Total number of pumps in the dataset
#num_pumps = nrow(katrina)

#katrina %>%
  #group_by(reason) %>%
  #summarize(`Frequency` = n(),
    #`Percent of Pumps` = n()/num_pumps * 100,
    # `Median Survival Time` = median(hour))
