library(dplyr)
library(tidyverse)
library(stringr)
library(readr)

# load all datasets 
data1 <- read.csv("../data/Abortion Knowledge and Attitudes Poll.csv")
data2 <- read.csv("../data/Abortion-Related Services Funded by Medi-Cal_2014_to_2020.csv")
data3 <- read.csv("../data/Driving Times to Abortion Clinics in the US.csv")

# ----------

# get data1 for California
data1 <- data1 %>%
  group_by(state) %>%
  filter(state == "CALIFORNIA") %>%
  select("state", "q11a", "q11b", "q11c", "q11d", "q11e")

# remove empty responses
data1 <- data1[!data1$q11a==" ",]

# get count of valid rows 
n_ca1 <- nrow(data1)
num_people <- paste0("(n=", n_ca1, ")")

# function to calculate the proportion of people who answer "Legal" 
calculate_legal_prop <- function(column,data=NULL){
  column<-eval(substitute(column),data, parent.frame())
  return(format(round(length(which(column == "Legal")) / n_ca1, 2), nsmall = 2))
}

# create df for data1: people who answer "Legal"
ca1 <- data1 %>%
  summarize("state" = "california", 
            !!paste("Do you think abortion should be legal or illegal in cases of rape or incest?", num_people) := 
              as.numeric(calculate_legal_prop(q11a, data1)),
            !!paste0("Do you think abortion should be legal or illegal if the patient's life is endangered?", num_people) := 
              as.numeric(calculate_legal_prop(q11b, data1)),
            !!paste0("Do you think abortion should be legal or illegal if the fetus is not suspected to survive?", num_people) :=  
              as.numeric(calculate_legal_prop(q11c, data1)),
            !!paste0("Do you think abortion should be legal or illegal if the fetus is expected to have serious birth defects?", num_people) :=  
              as.numeric(calculate_legal_prop(q11d, data1)),
            !!paste0("Do you think abortion should be legal or illegal for women who do not wish to be pregnant?", num_people) :=  
              as.numeric(calculate_legal_prop(q11e, data1)))

# ----------

# change data2 string amount to number
data2 <- data2[!data2$Total.Expenditures=="",] %>%
  mutate(Total.Expenditures = gsub("\\$", "", Total.Expenditures))
data2$Total.Expenditures <- parse_number(data2$Total.Expenditures)

# remove Total rows (large cumulative amounts that - not actual data points)
data2 <- data2[data2$County != "Total", ]

# create df for data2: mean of total expenditures on abortions 
ca2 <- data2 %>% 
  summarize("state" = "california", 
            !!paste0("Avg total expenditures, 2014-2020 (n=", nrow(data2), ")") := 
              as.numeric(format(round(mean(Total.Expenditures, na.rm=TRUE), 2), nsmall = 2)))

# ----------

# find the average amount of time it takes someone in California to get to the closest abortion clinic 

# filter data3 to california
filter_ca <- function(column,data=data3){
  column<-eval(substitute(column),data, parent.frame())
  filtered_states <- data[column != 0, ] %>% 
    filter(state == "California")
  return(filtered_states)
}

# use new california only data to calculate the mean
calculate_mean <- function(column, data=NULL) {
  column<-eval(substitute(column), data, parent.frame())
  return(format(round(mean(column, na.rm=TRUE), 2), nsmall = 2))
}

# generate titles for columns 
generate_title <- function(num, n) {
  return(paste0("Avg driving time in minutes to closest abortion clinic, ", 
                num, " weeks (n=", n, ")"))
}

# create df for data3: average time in min it takes to drive to an abortion clinic
ca3 <- data3 %>% 
  group_by(state) %>%
  filter(state == "California") %>%
  summarize("state" = "california", 
            !!generate_title(8, nrow(filter_ca(gestation_8_duration))) := 
              as.numeric(calculate_mean(gestation_8_duration, filter_ca(gestation_8_duration))),
            !!generate_title(12, nrow(filter_ca(gestation_12_duration))) :=
              as.numeric(calculate_mean(gestation_12_duration, filter_ca(gestation_12_duration))),
            !!generate_title(16, nrow(filter_ca(gestation_16_duration))) := 
              as.numeric(calculate_mean(gestation_16_duration, filter_ca(gestation_16_duration))),
            !!generate_title(20, nrow(filter_ca(gestation_20_duration))) :=  
              as.numeric(calculate_mean(gestation_20_duration, filter_ca(gestation_20_duration))))

merged_table <- ca1 %>%
  left_join(ca2, by='state') %>%
  left_join(ca3, by='state') %>%
  subset(select = -c(state))

summary_info <- transpose(as.list(merged_table))
summary_info <- summary_info[1]