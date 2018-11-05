library(survival)
library(survminer)
library(muhaz)
library(dplyr)

katrina <- read.csv("C:/Users/Christopher/Documents/IAA/Fall 3/F3-Blueteam12/survival_analysis/data/katrina.csv", header = T, stringsAsFactors = F)

katrina[, c(9:56)] <- NULL # Removes unnecessary columns

# Total number of pumps in the dataset
num_pumps = nrow(katrina)

katrina %>%
  group_by(reason) %>%
  summarize(`Frequency` = n(),
            `Percent of Pumps` = n()/num_pumps * 100,
            `Median Survival Time` = median(hour))

# plot the survival function
# survive == 0 is the event
katrina_surv <- survfit(Surv(time = hour, event = survive == 0) ~ reason, data = subset(katrina, reason!=0))
summary(katrina_surv)
ggsurvplot(katrina_surv, color='reason', legend.title='Failure Reason',
           legend="right", legend.labs=c("Flooded", "Motor", "Surged", "Jammed"),
           xlim=c(0,48)) +
  ggtitle("Pump Station Survival Rates", "By Failure Reason") +
  xlab("Hour")

  

# do the log-rank test
survdiff(Surv(time = hour, event = survive == 0) ~ reason, rho = 0, data = katrina[katrina$reason!=0, ])
survdiff(Surv(time = hour, event = survive == 0) ~ reason, rho = 1, data = katrina[katrina$reason!=0, ])

# pairwise test
survminer::pairwise_survdiff(Surv(time = hour, event = survive == 0) ~ reason, rho = 0, data = katrina[katrina$reason!=0, ])
survminer::pairwise_survdiff(Surv(time = hour, event = survive == 0) ~ reason, rho = 1, data = katrina[katrina$reason!=0, ])


# Hazard  Function --------------------------------------------------------

# plot the hazard function
katrina$hour2 <- ifelse(katrina$hour == 48 & katrina$survive == 1, 49, katrina$hour)

# turn survive values opposite
katrina$survive2 <- ifelse(katrina$survive == 0, 1, 0)
katrina_haz1 <- with(subset(katrina, reason==1), kphaz.fit(hour2, survive2))
katrina_haz2 <- with(subset(katrina, reason==2), kphaz.fit(hour2, survive2))
katrina_haz3 <- with(subset(katrina, reason==3), kphaz.fit(hour2, survive2))
katrina_haz4 <- with(subset(katrina, reason==4), kphaz.fit(hour2, survive2))


kat <- data.frame(hazard=katrina_haz1$haz, reason="1")
kat2 <-  data.frame(hazard=katrina_haz2$haz, reason="2")
kat3 <- data.frame(hazard=katrina_haz3$haz, reason="3")
kat4 <-  data.frame(hazard=katrina_haz4$haz, reason="4")

all_kat <- as.data.frame(rbind(kat, kat2,kat3, kat4))


ggplot(data=all_kat) + ggline(all_kat, x=seq(0, 48), y=hazard)



katrina_haz1$haz






kphaz.plot(katrina_haz, main = "hazard function")



?kphaz.fit

# cumulative hazard
katrina_fit <- survfit(Surv(hour, survive == 0) ~ reason, data = subset(katrina, reason!=0))
ggsurvplot(katrina_fit, fun = "cumhaz", color= 'reason') +
  ggtitle("Cumulative Hazard", "By Cause of Failure") 
  #+ scale_color_manual(labels=c("Flood", "Motor", "Surged", "Jammed"))
  


