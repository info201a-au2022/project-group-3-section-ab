library(dplyr)
library(tidyverse)
library(stringr)
library(readr)
library(ggplot2)

# load dataset3
data3 <- read.csv("../data/Driving Times to Abortion Clinics in the US.csv")

# find the average amount of time it takes someone in California to get
# to the closest abortion clinic

# filter data3 to california
filter_ca <- function(column, data = data3) {
  column <- eval(substitute(column), data, parent.frame())
  filtered_states <- data[column != 0, ] %>%
    filter(state == "California")
  return(filtered_states)
}

# use new california only data to calculate the mean
calculate_mean <- function(column, data = NULL) {
  column <- eval(substitute(column), data, parent.frame())
  return(format(round(mean(column, na.rm = TRUE), 2), nsmall = 2))
}

# generate titles for columns
generate_title <- function(num, n) {
  return(paste0(
    "Avg driving time in minutes to closest abortion clinic, ",
    num, " weeks (n=", n, ")"
  ))
}
mean_8 <- calculate_mean(gestation_8_duration,
                         filter_ca(gestation_8_duration))
mean_8_closed <- calculate_mean(gestation_8_duration_closed,
                                filter_ca(gestation_8_duration_closed))
mean_12 <- calculate_mean(gestation_12_duration,
                          filter_ca(gestation_12_duration))
mean_12_closed <- calculate_mean(gestation_12_duration_closed,
                                 filter_ca(gestation_12_duration_closed))
mean_16 <- calculate_mean(gestation_16_duration,
                          filter_ca(gestation_16_duration))
mean_16_closed <- calculate_mean(gestation_16_duration_closed,
                                 filter_ca(gestation_16_duration_closed))
mean_20 <- calculate_mean(gestation_20_duration,
                          filter_ca(gestation_20_duration))
mean_20_closed <- calculate_mean(gestation_20_duration_closed,
                                 filter_ca(gestation_20_duration_closed))

# create df for data3: average time in min it takes
# to drive to an abortion clinic
ca3 <- data3 %>%
  group_by(state) %>%
  filter(state == "California") %>%
  summarize(
    "state" = "california",
    !!generate_title(8, nrow(filter_ca(gestation_8_duration))) :=
      mean_8,
    !!generate_title(8, nrow(filter_ca(gestation_8_duration_closed))) :=
      mean_8_closed,
    !!generate_title(12, nrow(filter_ca(gestation_12_duration))) :=
      mean_12,
    !!generate_title(12, nrow(filter_ca(gestation_12_duration_closed))) :=
      mean_12_closed,
    !!generate_title(16, nrow(filter_ca(gestation_16_duration))) :=
      mean_16,
    !!generate_title(16, nrow(filter_ca(gestation_16_duration_closed))) :=
      mean_16_closed,
    !!generate_title(20, nrow(filter_ca(gestation_20_duration))) :=
      mean_20,
    !!generate_title(20, nrow(filter_ca(gestation_20_duration_closed))) :=
      mean_20_closed
  )

df <- data.frame(
  supp = rep(c("Closest", "Second closest"), each = 4),
  weeks_in_pregnancy = rep(c(8, 12, 16, 20), 2),
  avg_driving_times = c(
    as.numeric(mean_8), as.numeric(mean_12),
    as.numeric(mean_16), as.numeric(mean_20),
    as.numeric(mean_8_closed), as.numeric(mean_12_closed),
    as.numeric(mean_16_closed), as.numeric(mean_20_closed)
  )
)

# line_graph
# add first data: solid line
line_graph <- ggplot(df, aes(x = weeks_in_pregnancy,
                             y = avg_driving_times, group = supp)) +
  geom_line(aes(linetype = supp)) +
  geom_point()

# add second data: dashed line
line_graph <- line_graph +
  geom_line(aes(linetype = supp)) +
  geom_point() +
  labs(
    x = "Length of pregnancy before abortion (weeks)", y = "Time (min)",
    color = "Nearby abortion clinics", linetype = "Nearby abortion clinics"
  ) +
  ggtitle("Average driving time to abortion clinics in California,
          \nbased on duration of pregnancy in weeks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(8, 20, by = 4)) +
  scale_y_continuous(limits = c(0, 1.5))
line_graph

ggsave("dataset3.jpg")
