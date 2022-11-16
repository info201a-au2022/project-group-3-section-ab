library(dplyr)
library(tidyverse)
library(stringr)
library(readr)

data1 <- read.csv("Abortion Knowledge and Attitudes Poll.csv")
data2 <- read.csv("Abortion-Related Services Funded by Medi-Cal_2014_to_2020.csv")
data3 <- read.csv("Driving Times to Abortion Clinics in the US.csv")

data1 <- data1 %>%
  filter(state == "CALIFORNIA") %>%
  select("state", "q11a", "q11b", "q11c", "q11d", "q11e")

data1 <- data1[!data1$q11a==" ",]
num_people <- nrow(data1)

ca1 <- data1 %>%
  summarize("state" = "california", 
            "Do you think abortion should be legal or illegal in cases of rape or incest?" = 
              length(which(data1$q11a == "Legal")) / num_people, 
            "Do you think abortion should be legal or illegal if the patient's life is endangered?" = 
              length(which(data1$q11b == "Legal")) / num_people, 
            "Do you think abortion should be legal or illegal if the fetus is not suspected to survive?" = 
               length(which(data1$q11c == "Legal")) / num_people, 
            "Do you think abortion should be legal or illegal if the fetus is expected to have serious birth defects?" = 
               length(which(data1$q11d == "Legal")) / num_people, 
            "Do you think abortion should be legal or illegal for women who do not wish to be pregnant?" = 
               length(which(data1$q11e == "Legal")) / num_people)

data2 <- data2[!data2$Total.Expenditures=="",] %>%
  mutate(Total.Expenditures = gsub("\\$", "", Total.Expenditures))

data2$Total.Expenditures <- parse_number(data2$Total.Expenditures)

ca2 <- data2 %>% 
  summarize("state" = "california", 
            "avg(Total.Expenditures)" = mean(Total.Expenditures, na.rm=TRUE))


ca3 <- data3 %>% 
  filter(state == "California") %>%
  summarize("state" = "california", 
            "avg(gestation_8_duration)" = mean(gestation_8_duration, na.rm=TRUE), 
            "avg(gestation_8_duration_closed)" = mean(gestation_8_duration_closed, na.rm=TRUE),
            "avg(gestation_12_duration)" = mean(gestation_12_duration, na.rm=TRUE),
            "avg(gestation_12_duration_closed)" = mean(gestation_12_duration_closed, na.rm=TRUE),
            "avg(gestation_16_duration)" = mean(gestation_16_duration, na.rm=TRUE),
            "avg(gestation_16_duration_closed)" = mean(gestation_16_duration_closed, na.rm=TRUE),
            "avg(gestation_20_duration)" = mean(gestation_20_duration, na.rm=TRUE),
            "avg(gestation_20_duration_closed)" = mean(gestation_20_duration_closed, na.rm=TRUE))

merged_table <- ca1 %>%
  left_join(ca2, by='state') %>%
  left_join(ca3, by='state')
