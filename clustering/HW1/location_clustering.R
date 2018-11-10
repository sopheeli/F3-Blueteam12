# location clustering


# libraries ---------------------------------------------------------------

library(readr) # For reading in data
library(dplyr) # for data manipulation


# Read in Data ------------------------------------------------------------

your_data_file_path <- "IAA/Fall 3/clustering/data/"


calendar <- read_csv(paste0(your_data_file_path,"calendar.csv"))
listings <- read_csv(paste0(your_data_file_path,"listings.csv"))
reviews <- read_csv(paste0(your_data_file_path,"reviews.csv"))



# Explore -----------------------------------------------------------------

names(listings)


# There are 39 "Smart" locations, some of them look exactly the same (i.e. Boston, MA vs. Boston , MA)
unique(as.factor(listings$smart_location))

listings %>%
  group_by(smart_location) %>%
  summarise(n=n()) %>%
  arrange(desc(n))

# There seem to mostly (>95%) listed as just boston. Therefore, this column doesnt seem to be useful


# Experiences offered are ALL NONE!
# Not a useful column
listings %>%
  group_by(experiences_offered) %>%
  summarise(n=n()) %>%
  arrange(desc(n))



# Seems to be clear separation of neighborhoods
listings %>%
  group_by(neighbourhood) %>%
  summarise(n=n()) %>%
  arrange(desc(n))


# Lots of different zipcodes too
listings %>%
  group_by(zipcode) %>%
  summarise(n=n()) %>%
  arrange(desc(n))


# Seems to be a description of whether there are nearby places
listings %>%
  group_by(transit) %>%
  summarise(n=n()) %>%
  arrange(desc(n))


# Also gives a ton of attraction
listings %>%
  group_by(neighborhood_overview) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
