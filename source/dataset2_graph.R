library(ggplot2)
library(stringr)
library(dplyr)
library(tidyverse)
library(scales)

setwd("C:/Users/lunae/Desktop/INFO Apps") 

#load data
df <- read.csv("abortion-related-services-funded-by-medi-cal-calendar-years-2014-2020.csv", 
               header=TRUE, stringsAsFactors=FALSE); 

#set up scatter plot 
p1 <- ggplot(df, aes(Calendar.Year, Total.Expenditures, color = Delivery.System)) +
  geom_point()+
  geom_label(
    label="Top - Total in Year", 
    x=2017, 
    y=20000000, 
    label.padding = unit(0.55, "lines"), 
    label.size = 0.35,
    color = "black",
    fill="#bebebe"
    ) +
  geom_label(
    label="Middle - LA County Totals",
    x=2017, 
    y=11000000, 
    label.padding = unit(0.55, "lines"), 
    label.size = 0.35,
    color = "black",
    fill="#bebebe"
  )

#Adjust data set values and x,y axis  
p1 + scale_x_continuous(
      limits = c(2014, 2020),
      breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020),
      labels = c("2014", "2015", "2016", "2017", "2018", "2019", "2020"),
      xlab("Calendar Year")) +
     scale_y_continuous(
      limits = c(0, 20000000), 
      breaks = breaks_extended(10),
      labels = c("400,000", "600,000", "800,000", "10,000,000", 
                 "12,000,000", "14,000,000", "16,000,000", "18,000,000", "20,000,000"),
      ylab("Total Expenditures (In Dollars $)")) +
      labs(title = "Abortion Related Services Funded by Medi Cal 2014-2020", 
           subtitle = "Counties Expenditures In The State of California", 
           caption = "Data sourced from the California Health and Human Services Agency",
           colour = "Delivery System") 

ggsave("dataset2.jpg")
