######################clustering#########################
####################sentiment and location###############
####################11/9/2018############################

library(readr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidytext)
library(stringr)
library(text2vec)
library(readr)
library(tidytext)

#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup")

#--------------------------get google map work--------------------------------------#
#download boston_1.Rdata on github, then run this code. if ggmap is load, should work
#load("/Users/johnpamplin/Documents/GitHub/F3-Blueteam12/Clustering/boston_1.RData")

ggmap::register_google("AIzaSyBdQvxweoFWqcM6oHjuRTEPtCLguSXRiT0")

map <- get_map(location = "Boston", zoom=12)

ggmap(map)
#save(map,file = "boston_2.RData")   


#-------------------------------------------------------------------------------
reviews <- read_csv("/Users/johnpamplin/Documents/RStudio/Clustering/Data/reviews.csv")
#################################################################################################################
#     CJP - file load 
# listings <- read.csv("~/Documents/RStudio/Clustering/Data/listings.csv", comment.char="#")
# reviews <- read.csv("~/Documents/RStudio/Clustering/Data/reviews.csv", comment.char="#")
# calendar <- read.csv("~/Documents/RStudio/Clustering/Data/calendar.csv")
#################################################################################################################
View(reviews)

# the word bank with sentiment score
nrc_total <- get_sentiments("afinn")

rv <- reviews %>%
  group_by(listing_id) %>%
  count(listing_id, sort = TRUE) %>%
  filter(n >= 4) %>%
  select(-"n")

# get the sentiment score
new_reviews <- reviews %>%
  group_by(listing_id) %>%
  unnest_tokens(word, comments)  %>%
  right_join(rv,by="listing_id") %>% filter(!is.na(word)) %>%
  left_join(nrc_total,by="word") %>% filter(!is.na(score))

# summarize the score of each house, and count the number of words each house recieves, 

# then calculate the average score
score <- new_reviews %>%
  group_by(listing_id) %>%
  mutate(sscore = sum(score), n = n()) %>%
  distinct(listing_id,sscore, n) %>%
  mutate(avg = sscore / n)

hist(score$avg)

# standardize the score
score$avg <- scale(score$avg)

hist(score$avg)

# combine the house info
listings <- read_csv("/Users/johnpamplin/Documents/RStudio/Clustering/Data/listings.csv")

colnames(listings)[1] <- "listing_id"

combined <- score %>%
  left_join(listings) %>%
  mutate(price = as.numeric(str_sub(price, start = 2))) %>%
  mutate(price_per_bedroom = price / bedrooms, price_per_accommodate = price / accommodates)

plot(combined$price_per_bedroom,combined$avg)

plot(combined$price_per_accommodate,combined$avg)


combined$std.lat <- scale(combined$latitude)

combined$std.lon <- scale(combined$longitude)

############################# Adding listing data into the combine data ############################# 
summary(calendar)
calendar$price = as.numeric(calendar$price)
calendar$total = 1
calendar$is_full = ifelse((calendar$available == "f"), 0, 1)
calendar$price = ifelse((calendar$price == 1), 0, calendar$price)

DT <- data.table(calendar)
x = DT[, sum(total), by = listing_id]
y = DT[, sum(is_full), by = listing_id]
z = DT[, sum(price), by = listing_id]

calendar_percent = inner_join(x,y, by = "listing_id")
calendar_percent = inner_join(calendar_percent,z, by = "listing_id")

calendar_percent$percent_full = (calendar_percent$V1.y / calendar_percent$V1.x)
calendar_percent$sum_cost = calendar_percent$V1

calendar_unique = dplyr::select(calendar_percent, listing_id, percent_full, sum_cost)

combined_all = left_join(combined, calendar_unique, by = "listing_id")
#############################

toC<- cbind(combined$avg,combined$std.lat,combined$std.lon)

clusters.c <- hclust(dist(toC),method="complete")

clusters.s <- hclust(dist(toC), method="single")

clusters.a <- hclust(dist(toC), method="average")

plot(clusters.c)

plot(clusters.s)

plot(clusters.a)

#assumption made here, assume use complete method with 5 clusters
combined$clus <- cutree(clusters.c,5)

clu1 <- combined %>% filter(clus == 1)
mean(clu1$avg)
#-0.6988421

clu2 <- combined %>% filter(clus == 2)
mean(clu2$avg)
#0.762409

clu3 <- combined %>% filter(clus == 3)
mean(clu3$avg)
#0.1448943

clu4 <- combined %>% filter(clus == 4)
mean(clu4$avg)
#-0.1126243

clu5 <- combined %>% filter(clus == 5)
mean(clu5$avg)
#-3.275934

#clu6 <- combined %>% filter(clus == 6)

#clu7 <- combined %>% filter(clus == 7)

ggmap(map, fullpage = TRUE) + geom_point(data = clu1, aes(x = longitude, y = latitude), color = 'yellow', size = 2) + 
  geom_point(data = clu2, aes(x = longitude, y = latitude), color = 'red', size = 2) + 
  geom_point(data = clu3, aes(x = longitude, y = latitude), color = 'blue', size = 2)+
  geom_point(data = clu4, aes(x = longitude, y = latitude), color = 'purple', size = 2)+
  geom_point(data = clu4, aes(x = longitude, y = latitude), color = 'green', size = 2)
