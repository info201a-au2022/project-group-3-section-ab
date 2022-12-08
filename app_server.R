# Load libraries so they are available
library("shiny")
library("ggplot2")
library("tidyverse")


# DATASET 1

opinions_csv <- read.csv("./data/Abortion Knowledge and Attitudes Poll.csv")

# Filter data: we want to summarize the count for our 4 main questions
#   for each state and remove unnecessary columns
opinions <- opinions_csv %>%
  select("state", "q11a", "q11b", "q11c", "q11d", "q11e")

# remove empty rows
opinions <- opinions[!opinions$q11a==" ",]

# get proportions and percentages
opinions <- opinions %>%
  group_by(state) %>%
  summarise(
    "Total participants" = n(),
    across(q11a, (~ sum(. == "Legal") * 100 / `Total participants`)),
    across(q11b, (~ sum(. == "Legal") * 100 / `Total participants`)),
    across(q11c, (~ sum(. == "Legal") * 100 / `Total participants`)),
    across(q11d, (~ sum(. == "Legal") * 100 / `Total participants`)),
    across(q11e, (~ sum(. == "Legal") * 100 / `Total participants`)))

# change state (all caps) to state abbreviations
opinions$state <- str_to_title(opinions$state)
opinions$state <- state.abb[match(opinions$state, state.name)]

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
  

# Define a server function
server <- function(input, output) {
  # output$plot <- renderPlot({
  #   # return the plot
  #   ggplot(data = income_growth) +
  #     geom_point(mapping = aes(
  #       x = Income.Percentile, y = Average.Growth.Perc
  #     ), color = "gray") +
  #     geom_point(mapping = aes(
  #       x = Income.Percentile, y = Post.Tax.Growth.Perc
  #     ), color = "red") +
  #     labs(x = "Income Percentile", y = "Income Growth (%)") +
  #     scale_x_continuous(limits = input$percentile)
  # })
  output$plot <- renderPlot({
    return(bar_1(input$question) + theme(legend.position="none"))
  })
}