library(dplyr)
library(tidyverse)
library(stringr)
library(readr)
library(ggplot2)

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

weeks_in_pregnancy <- c(8, 12, 16, 20, 8, 12, 16, 20)

avg_driving_time <- c(calculate_mean(gestation_8_duration, filter_ca(gestation_8_duration)),
                     calculate_mean(gestation_12_duration, filter_ca(gestation_12_duration)),
                     calculate_mean(gestation_16_duration, filter_ca(gestation_16_duration)),
                     calculate_mean(gestation_20_duration, filter_ca(gestation_20_duration)))

avg_driving_time2 <- c(calculate_mean(gestation_8_duration_closed, filter_ca(gestation_8_duration_closed)),
                      calculate_mean(gestation_12_duration_closed, filter_ca(gestation_12_duration_closed)),
                      calculate_mean(gestation_16_duration_closed, filter_ca(gestation_16_duration_closed)),
                      calculate_mean(gestation_20_duration_closed, filter_ca(gestation_20_duration_closed)))

df = data.frame(weeks_in_pregnancy, avg_driving_time, avg_driving_time2)

df <- data.frame(supp=rep(c("Closest", "Second closest"), each=4),
                  weeks_in_pregnancy=rep(c(8, 12,16, 20),2),
                  avg_driving_times=c(calculate_mean(gestation_8_duration, filter_ca(gestation_8_duration)),
                                      calculate_mean(gestation_12_duration, filter_ca(gestation_12_duration)),
                                      calculate_mean(gestation_16_duration, filter_ca(gestation_16_duration)),
                                      calculate_mean(gestation_20_duration, filter_ca(gestation_20_duration)),
                                      calculate_mean(gestation_8_duration_closed, filter_ca(gestation_8_duration_closed)),
                                      calculate_mean(gestation_12_duration_closed, filter_ca(gestation_12_duration_closed)),
                                      calculate_mean(gestation_16_duration_closed, filter_ca(gestation_16_duration_closed)),
                                      calculate_mean(gestation_20_duration_closed, filter_ca(gestation_20_duration_closed))))

plot <- ggplot(data=df, aes(x=weeks_in_pregnancy, y=avg_driving_times, group=supp)) +
  geom_line(aes(color=supp))+
  geom_point(aes(color=supp))
