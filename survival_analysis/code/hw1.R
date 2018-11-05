# hw1.R
# This contains all of the code used to produce the necessary information for
# the first homework report for Fall 3 Survival Analysis



# Global Variables --------------------------------------------------------
## Keep track of any global variables here
data_location = "IAA/Fall 3/F3-Blueteam12/survival_analysis/data/"


# Load Libraries ----------------------------------------------------------


library(survival)
library(survminer)
library(muhaz)

library(readr) # For reading data
library(tidyverse) # for tidy data manipulation
library(ggplot2) # For plotting nice plots

# Read Data ---------------------------------------------------------------

katrina <- read_csv(paste0(data_location,"katrina.csv"))

if (names(katrina)[9] == "h1")
  # remove the "h1" - "h48" columns
  katrina[9:56] <- NULL



# Summary Statistics Per Failure Type -------------------------------------

# Total number of pumps in the dataset
num_pumps = nrow(katrina)

katrina %>%
    group_by(reason) %>%
    summarize(`Frequency` = n(),
              `Percent of Pumps` = n()/num_pumps * 100,
              `Median Survival Time` = median(hour))



# Survival Curve Plots ----------------------------------------------------

# All
survival_pumps <- survfit(Surv(time = hour, event = survive==0) ~ 1, data = katrina)
summary(survival_pumps)
ggsurvplot(survival_pumps, conf.int = TRUE, palette = "grey")

# By Failure Reaso
survival_by_reason <- survfit(Surv(time = hour, event = survive==0) ~ reason, data = subset(katrina, reason!=0))
summary(survival_by_reason)
ggsurvplot(survival_by_reason, conf.int = TRUE, palette = "grey")





# Differences in Survival Curves ---------------------------------------------------


survminer::pairwise_survdiff(Surv(time = hour, event = survive==0) ~ reason, rho = 0, data = subset(katrina, reason != 0))





