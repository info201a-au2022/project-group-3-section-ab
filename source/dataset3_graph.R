library(dplyr)
library(tidyverse)
library(stringr)
library(readr)

# load dataset3
data3 <- read.csv("../data/Driving Times to Abortion Clinics in the US.csv")

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
              calculate_mean(gestation_8_duration, filter_ca(gestation_8_duration)),
            !!generate_title(8, nrow(filter_ca(gestation_8_duration_closed))) := 
              calculate_mean(gestation_8_duration_closed, filter_ca(gestation_8_duration_closed)),
            !!generate_title(12, nrow(filter_ca(gestation_12_duration))) :=
              calculate_mean(gestation_12_duration, filter_ca(gestation_12_duration)),
            !!generate_title(12, nrow(filter_ca(gestation_12_duration_closed))) := 
              calculate_mean(gestation_12_duration_closed, filter_ca(gestation_12_duration_closed)),
            !!generate_title(16, nrow(filter_ca(gestation_16_duration))) := 
              calculate_mean(gestation_16_duration, filter_ca(gestation_16_duration)),
            !!generate_title(16, nrow(filter_ca(gestation_16_duration_closed))) := 
              calculate_mean(gestation_16_duration_closed, filter_ca(gestation_16_duration_closed)),
            !!generate_title(20, nrow(filter_ca(gestation_20_duration))) := 
              calculate_mean(gestation_20_duration, filter_ca(gestation_20_duration)),
            !!generate_title(20, nrow(filter_ca(gestation_20_duration_closed))) :=  
              calculate_mean(gestation_20_duration_closed, filter_ca(gestation_20_duration_closed)))
