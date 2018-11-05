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

katrina_haz <- with(katrina, kphaz.fit(hour2, survive2))

katrina_haz1 <- with(subset(katrina, reason==1), kphaz.fit(hour2, survive2))
katrina_haz2 <- with(subset(katrina, reason==2), kphaz.fit(hour2, survive2))
katrina_haz3 <- with(subset(katrina, reason==3), kphaz.fit(hour2, survive2))
katrina_haz4 <- with(subset(katrina, reason==4), kphaz.fit(hour2, survive2))

# make all the vectors have the same length
haz1 <- c(katrina_haz1$haz, rep(0, n - length(katrina_haz1$haz)))
haz2 <- c(katrina_haz2$haz, rep(0, n - length(katrina_haz2$haz)))
haz3 <- c(katrina_haz3$haz, rep(0, n - length(katrina_haz3$haz)))
haz4 <- c(katrina_haz4$haz, rep(0, n - length(katrina_haz4$haz)))

# create data frames for each reason's hazard
kat1 <- data.frame(hazard=haz1, reason="1")
kat2 <-  data.frame(hazard=haz2, reason="2")
kat3 <- data.frame(hazard=haz3, reason="3")
kat4 <-  data.frame(hazard=haz4, reason="4")

# combine them together
all_kat <- as.data.frame(rbind(kat1, kat2, kat3, kat4))

# plot the multiple hazard
ggplot(all_kat, aes(x = c(seq(1:47), seq(1:47), seq(1:47), seq(1:47)), y = hazard, group = reason, colour = reason)) +
  geom_line()


# cumulative hazard ----------------------------------------------------
katrina_fit <- survfit(Surv(hour, survive == 0) ~ reason, data = subset(katrina, reason!=0))
ggsurvplot(katrina_fit, fun = "cumhaz", color= 'reason') +
  ggtitle("Cumulative Hazard", "By Cause of Failure") 
  #+ scale_color_manual(labels=c("Flood", "Motor", "Surged", "Jammed"))
  


