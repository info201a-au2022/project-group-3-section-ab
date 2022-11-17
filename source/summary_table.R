library(dplyr)
library(tidyverse)
library(stringr)
library(readr)

source("../source/calculate_summary.R")

# merge all tables on state=="california"
merged_table <- ca1 %>%
  left_join(ca2, by='state') %>%
  left_join(ca3, by='state') %>%
  subset(select = -c(state))