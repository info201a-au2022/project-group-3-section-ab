library(dplyr)
library(tidyverse)
library(stringr)
library(readr)

# load all datasets 
data1 <- read.csv("../data/Abortion Knowledge and Attitudes Poll.csv")
data2 <- read.csv("../data/Abortion-Related Services Funded by Medi-Cal_2014_to_2020.csv")
data3 <- read.csv("../data/Driving Times to Abortion Clinics in the US.csv")

# get data1 for California
data1 <- data1 %>%
  group_by(state) %>%
  filter(state == "CALIFORNIA") %>%
  select("state", "q11a", "q11b", "q11c", "q11d", "q11e")

# remove empty responses
data1 <- data1[!data1$q11a==" ",]
num_people <- nrow(data1)

# calculate proportion of people who answer "Legal"
ca1 <- data1 %>%
  summarize("state" = "california", 
            "Do you think abortion should be legal or illegal in cases of rape or incest? (n=74)" = 
              format(round(length(which(data1$q11a == "Legal")) / num_people, 2), nsmall = 2), 
            "Do you think abortion should be legal or illegal if the patient's life is endangered? (n=74)" = 
              format(round(length(which(data1$q11b == "Legal")) / num_people, 2), nsmall = 2), 
            "Do you think abortion should be legal or illegal if the fetus is not suspected to survive? (n=74)" = 
              format(round(length(which(data1$q11c == "Legal")) / num_people, 2), nsmall = 2), 
            "Do you think abortion should be legal or illegal if the fetus is expected to have serious birth defects? (n=74)" = 
              format(round(length(which(data1$q11d == "Legal")) / num_people, 2), nsmall = 2), 
            "Do you think abortion should be legal or illegal for women who do not wish to be pregnant? (n=74)" = 
              format(round(length(which(data1$q11e == "Legal")) / num_people, 2), nsmall = 2))

# change data2 string amount to number
data2 <- data2[!data2$Total.Expenditures=="",] %>%
  mutate(Total.Expenditures = gsub("\\$", "", Total.Expenditures))
data2$Total.Expenditures <- parse_number(data2$Total.Expenditures)

# remove Total rows (large cumulative amounts that skew the data)
data2 <- data2[data2$County != "Total", ]
ca2 <- data2 %>% 
  summarize("state" = "california", 
            "Average total expenditures, 2014-2020 (n=402)" = format(round(mean(Total.Expenditures, na.rm=TRUE), 2), nsmall = 2))

# find the average amount of time it takes someone in California to get to the closest abortion clinic 
ca3 <- data3 %>% 
  group_by(state) %>%
  filter(state == "California") %>%
  summarize("state" = "california", 
            "Average driving time in minutes to closest abortion clinic, max 8 weeks pregnancy (n=177)" = 
              format(round(mean(gestation_8_duration, na.rm=TRUE), 2), nsmall = 2), 
            "Average driving time in minutes to second closest abortion clinic, max 8 weeks pregnancy (n=177)" = 
              format(round(mean(gestation_8_duration_closed, na.rm=TRUE), 2), nsmall = 2),
            "Average driving time in minutes to closest abortion clinic, max 20 weeks pregnancy (n=177)" = 
              format(round(mean(gestation_20_duration, na.rm=TRUE), 2), nsmall = 2),
            "Average driving time in minutes to second closest abortion clinic, max 20 weeks pregnancy (n=177)" = 
              format(round(mean(gestation_20_duration_closed, na.rm=TRUE), 2), nsmall = 2))

# merge all tables on state=="california"
merged_table <- ca1 %>%
  left_join(ca2, by='state') %>%
  left_join(ca3, by='state')

# merged_table <- t(merged_table)
# View(merged_table)