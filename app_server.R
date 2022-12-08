# Load libraries so they are available
library("shiny")
library("ggplot2")
library("tidyverse")
library("patchwork")
library("plotly")


# DATASET 1

opinions <- read.csv("./data/opinions.csv")

# create the bar graph
bar_1 <- function(question.var) {
  if (question.var == "q11a") {
    graph <- ggplot(opinions) +
      geom_col(mapping =aes(x = state, y = q11a, fill = "#ffd9db")) +
      scale_fill_manual(values = c("#edafb3")) +
      labs(x = "States", y = "Percentage who responded to Legalize")
    return(graph)
  } else if (question.var == "q11b") {
    graph <- ggplot(opinions) +
      geom_col(mapping = aes(x = state, y = q11b, fill = "#ffd9db")) +
      scale_fill_manual(values = c("#edafb3")) +
      labs(x = "States", y = "Percentage who responded to Legalize")
    return(graph)
  } else if (question.var == "q11c") {
    graph <- ggplot(opinions) +
      geom_col(mapping = aes(x = state, y = q11c, fill = "#ffd9db")) +
      scale_fill_manual(values = c("#edafb3")) +
      labs(x = "States", y = "Percentage who responded to Legalize")
    return(graph)
  } else if (question.var == "q11d") {
    graph <- ggplot(opinions) +
      geom_col(mapping = aes(x = state, y = q11d, fill = "#ffd9db")) +
      scale_fill_manual(values = c("#edafb3")) +
      labs(x = "States", y = "Percentage who responded to Legalize")
    return(graph)
  } else {
    graph <- ggplot(opinions) +
      geom_col(mapping = aes(x = state, y = q11e, fill = "#ffd9db")) +
      scale_fill_manual(values = c("#edafb3")) +
      labs(x = "States", y = "Percentage who responded to Legalize")
    return(graph)
  }
}

# DATASET 2
funding <- read.csv("./data/abortion-related-services-funded-by-medi-cal-calendar-years-2014-2020-10.csv")

line_1 <- function(ages.var, year1, year2) {
  if (ages.var == "Under 15") {
    funding_filtered <- filter(funding, Age.Group == "Under 15")
    graph <- ggplot(funding_filtered, aes(x = `Calendar.Year`,
                    y = `Total.Abortion.Related.Services`,
                    group = `Race.Ethnicity`)) +
      xlim(year1, year2) +
      geom_line(mapping = aes(color = `Race.Ethnicity`)) +
      geom_point(aes(color = `Race.Ethnicity`))
    return(graph)
  } else if (ages.var == "15 - 19") {
    funding_filtered <- filter(funding, Age.Group == "15 - 19")
    graph <- ggplot(funding_filtered, aes(x = `Calendar.Year`,
                    y = `Total.Abortion.Related.Services`,
                    group = `Race.Ethnicity`)) +
      xlim(year1, year2) +
      geom_line(mapping = aes(color = `Race.Ethnicity`)) +
      geom_point(aes(color = `Race.Ethnicity`))
    return(graph)
  } else if (ages.var == "20 - 24") {
    funding_filtered <- filter(funding, Age.Group == "20 - 24")
    graph <- ggplot(funding_filtered, aes(x = `Calendar.Year`,
                    y = `Total.Abortion.Related.Services`,
                    group = `Race.Ethnicity`)) +
      xlim(year1, year2) +
      geom_line(mapping = aes(color = `Race.Ethnicity`)) +
      geom_point(aes(color = `Race.Ethnicity`))
    return(graph)
  } else if (ages.var == "25 - 29") {
    funding_filtered <- filter(funding, Age.Group == "25 - 29")
    graph <- ggplot(funding_filtered, aes(x = `Calendar.Year`,
                    y = `Total.Abortion.Related.Services`,
                    group = `Race.Ethnicity`)) +
      xlim(year1, year2) +
      geom_line(mapping = aes(color = `Race.Ethnicity`)) +
      geom_point(aes(color = `Race.Ethnicity`))
    return(graph)
  } else if (ages.var == "30 - 34") {
    funding_filtered <- filter(funding, Age.Group == "30 - 34")
    graph <- ggplot(funding_filtered, aes(x = `Calendar.Year`,
                    y = `Total.Abortion.Related.Services`,
                    group = `Race.Ethnicity`)) +
      xlim(year1, year2) +
      geom_line(mapping = aes(color = `Race.Ethnicity`)) +
      geom_point(aes(color = `Race.Ethnicity`))
    return(graph)
  } else if (ages.var == "35 - 39") {
    funding_filtered <- filter(funding, Age.Group == "35 - 39")
    graph <- ggplot(funding_filtered, aes(x = `Calendar.Year`,
                    y = `Total.Abortion.Related.Services`,
                    group = `Race.Ethnicity`)) +
      xlim(year1, year2) +
      geom_line(mapping = aes(color = `Race.Ethnicity`)) +
      geom_point(aes(color = `Race.Ethnicity`))
    return(graph)
  } else if (ages.var == "40 - 44") {
    funding_filtered <- filter(funding, Age.Group == "40 - 44")
    graph <- ggplot(funding_filtered, aes(x = `Calendar.Year`,
                    y = `Total.Abortion.Related.Services`,
                    group = `Race.Ethnicity`)) +
      xlim(year1, year2) +
      geom_line(mapping = aes(color = `Race.Ethnicity`)) +
      geom_point(aes(color = `Race.Ethnicity`))
    return(graph)
  } else {
    funding_filtered <- filter(funding, Age.Group == "45 & Up")
    graph <- ggplot(funding_filtered, aes(x = `Calendar.Year`,
                    y = `Total.Abortion.Related.Services`,
                    group = `Race.Ethnicity`)) +
      xlim(year1, year2) +
      geom_line(mapping = aes(color = `Race.Ethnicity`)) +
      geom_point(aes(color = `Race.Ethnicity`))
    return(graph)
   } 
}


# DATASET 3
driving <- read.csv("./data/Driving Times to Abortion Clinics in the US.csv")

double_bar <- function(state.choice) {
  # filter driving to state of choice
  driving_filtered <- function(column, data = driving) {
    column <- eval(substitute(column), data, parent.frame())
    filtered_states <- data[column != 0, ] %>%
      filter(state == state.choice)
    return(filtered_states)
  }
  
  num_weeks <- c(8, 12, 16, 20)
  
  # use new state only data to calculate the mean
  calculate_mean <- function(column, data = NULL) {
    column <- eval(substitute(column), data, parent.frame())
    return(format(round(mean(column, na.rm = TRUE), 2), nsmall = 2))
  }
  # generate titles for columns
  generate_title <- function(num, n) {
    return(paste0(
      "Avg driving time in minutes to closest abortion clinic, ",
      num, " weeks (", state.choice, ")"
    ))
  }
  mean_8 <- calculate_mean(gestation_8_duration,
                           driving_filtered(gestation_8_duration))
  mean_8_closed <- calculate_mean(gestation_8_duration_closed,
                                  driving_filtered(gestation_8_duration_closed))
  mean_12 <- calculate_mean(gestation_12_duration,
                            driving_filtered(gestation_12_duration))
  mean_12_closed <- calculate_mean(gestation_12_duration_closed,
                                   driving_filtered(gestation_12_duration_closed))
  mean_16 <- calculate_mean(gestation_16_duration,
                            driving_filtered(gestation_16_duration))
  mean_16_closed <- calculate_mean(gestation_16_duration_closed,
                                   driving_filtered(gestation_16_duration_closed))
  mean_20 <- calculate_mean(gestation_20_duration,
                            driving_filtered(gestation_20_duration))
  mean_20_closed <- calculate_mean(gestation_20_duration_closed,
                                   driving_filtered(gestation_20_duration_closed))
  
  # create df for data3: average time in min it takes
  # to drive to an abortion clinic
  ca3 <- driving %>%
    group_by(state) %>%
    filter(state == state.choice) %>%
    summarize(
      "state" = state.choice,
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
  
  graph <- ggplot(df, aes(x = weeks_in_pregnancy,
                               y = avg_driving_times, group = supp)) +
    geom_bar(stat = "identity", position = position_dodge(),
             alpha = 0.75, aes(fill=supp), na.rm = TRUE) + 
    scale_color_manual(guide = guide_legend(title="Nearby abortion clinics")) +
    labs(x = "Length of pregnancy before abortion (weeks)", y = "Time (min)",
      fill = "Nearby abortion clinics") +
    ggtitle("Average driving time to abortion clinics in U.S. states,\nbased on duration of pregnancy in weeks") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(8, 20, by = 4)) +
    scale_y_continuous(limits = c(0, 1.5))
  graph
}
  

# Define a server function
server <- function(input, output) {
  output$plot <- renderPlotly({
    return(bar_1(input$question) + theme(legend.position="none"))
  })
  
  output$line <- renderPlotly({
    return(line_1(input$ages, input$year[1], input$year[2]))
  })
  
  output$doubleBar <- renderPlot({
    return(double_bar(input$state))
  })
}