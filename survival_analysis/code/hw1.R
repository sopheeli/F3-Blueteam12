# hw1.R
# This contains all of the code used to produce the necessary information for
# the first homework report for Fall 3 Survival Analysis



# Global Variables --------------------------------------------------------
## Keep track of any global variables here
data_location = "IAA/Fall 3/F3-Blueteam12/survival_analysis/data/"


# Load Libraries ----------------------------------------------------------


library(survival)
library(readr)
library(tidyverse)


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



