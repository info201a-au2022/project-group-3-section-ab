# Load libraries so they are available
library("shiny")
library("plotly")
library("ggplot2")
library("tidyverse")


# Introduction

intro_content <- mainPanel( 
  p(h4("Kayla Gibbs, Kristy Nhan, Elizabeth Luna-Santos")),
  p(h4("Autumn 2022 INFO 201 Project")),
  p(tags$br()),
  p(strong(h3("Introduction"))),
  p("This project is focused on analyzing abortion trends, specifically in 
    California. As abortion access is a very relevant and ongoing issue 
    that impacts those who can give birth, it is important to see the closer 
    details and specific obstacles or reasons why abortion is perceived the way 
    it is."),
  p("The problem domain is that of a financial, economic, as well as social 
    perspective for abortion. Many of which are intertwined and also often do 
    not make much sense. Especially when it comes to abortion, in which the 
    ability to get abortion is often limited based on the weeks of gestation 
    where it might be too early to test for the hCG hormone."), 
  p("Our group sought to research the following questions::"),
  tags$li("ow has legalizing and prohibiting abortions in the United States 
          affected public opinion on abortions?"),
  tags$li("How has funding for abortions impacted abortion-related services in 
          California?"),
  tags$li("What are some financial roadblocks that impact the accessibility 
          of abortion in California, and what are people doing to get around 
          them?"),
  p(),
  p("The data that was analyzed is that of public opinion on abortion of 
  Californian interviewees; abortion related serviced funded by Medi-Cal, 
  California's Medicaid program serving low-income individuals, including 
  families, seniors, persons with disabilities, pregnant women, and childless 
  adults with incomes below 138% of federal poverty level; as well as the 
  driving times to abortion clinics in California cities."),
  p("Some key findings were that public opinions on abortion in the state of 
    California was more liberal and more likely to want to legalize abortion. In 
    addition, there was a large amount of funding dedicated to abortion,
    especially in counties such as Los Angeles County, where it was more likely 
    to be funded by Medi-Cal. Lastly, there were less driving times present as 
    shown in our third dataset, due to the availability of abortion clinics 
    in close proximities within California, which has been an established 
    progressive state.")
)

intro_panel <- tabPanel(
  class = "inner-content",
  "Overview",
  intro_content
)

# Next, we'll define a few UI elements to render in our second panel

# Define a variable `sidebar_content` as a `sidebarPanel()` UI element
# containing the following information:

# sidebar_content <- sidebarPanel(
#   # A `sliderInput()` for the 'percentile' value, labeled "Income Percentile".
#   # This slider should let the user pick a range between 0 and 100
#   sliderInput(
#     inputId = "percentile",
#     label = "Income Percentile", min = 0, max = 100, value = c(0, 100)
#   )
# )

# Define a variable `main_content` as a `mainPanel()` UI element
# containing the following information:
# main_content <- mainPanel(
#   # A `plotOutput()` element showing the 'plot' output (defined in the server)
#   plotOutput("plot"),
#   
#   # A paragraph with a hyperlink to the data source
#   # http://gabriel-zucman.eu/usdina/
#   p(
#     "Source:",
#     a(
#       href = "http://gabriel-zucman.eu/usdina/",
#       "http://gabriel-zucman.eu/usdina/"
#     )
#   )
# )


# Define a variable `growth_panel` for your second page. It should be a
# `tabPanel()` with a title "Growth Chart" to represent the second tab.
# This layout will contain the following elements:
# growth_panel <- tabPanel(
#   "Growth Chart",
#   
#   # A `titlePanel()` with the text "Income growth 1980-2014"
#   titlePanel("Income growth 1980-2014"),
#   
#   # A `sidebarLayout()` to create two columns.
#   # The sidebar layout will contain elements:
#   sidebarLayout(
#     # Your `sidebar_content`
#     sidebar_content,
#     
#     # Your `main_content`
#     main_content
#   )
# )

chart1_main_content <- mainPanel(
  class = "display-question",
  selectInput(
    "question",
    label = "Select a public opinion question to display:",
    choices = list(
      "Do you think abortion should be legal or illegal in cases of rape or incest?" = "q11a",
      "Do you think abortion should be legal or illegal if the patient's life is endangered?" = "q11b",
      "Do you think abortion should be legal or illegal if the fetus is not suspected to survive?" = "q11c",
      "Do you think abortion should be legal or illegal if the fetus is expected to have serious birth defects?" = "q11d",
      "Do you think abortion should be legal or illegal for women who do not wish to be pregnant?" = "q11e"
    )
  ),
  plotOutput("plot"),
  p(em("Source: Henry J. Kaiser Family Foundation for Roper Center at Cornell 
       University\nThese graphs display the percentage of people surveyed in 
       each state who think that abortion should be legalized in the context 
       of the selected question. Survey participants were allowed to skip 
       questions as they liked, so some states may show 0, which means that 
       data was not recorded for that particular U.S. state."),
    align = "center", style = "font-size:12px;")
)

public_opinion_panel <- tabPanel(
  "Public Opinions",
  titlePanel("Public Opinions on Abortion"),
  chart1_main_content
)

funding_panel <- tabPanel(
  class = "inner-content",
  "Funding",
  
  # A `titlePanel()` with the text "Income growth 1980-2014"
  titlePanel("Funding for Abortion-Related Services in California"),
  
  # A `sidebarLayout()` to create two columns.
  # The sidebar layout will contain elements:
  # sidebarLayout(
  #   # Your `sidebar_content`
  #   sidebar_content,
  #   
  #   # Your `main_content`
  #   main_content
  # )
)

driving_access_panel <- tabPanel(
  class = "inner-content",
  "Access to Clinics",
  
  # A `titlePanel()` with the text "Income growth 1980-2014"
  titlePanel("Driving Times to Abortion Clinics in California"),
  
  # A `sidebarLayout()` to create two columns.
  # The sidebar layout will contain elements:
  # sidebarLayout(
  #   # Your `sidebar_content`
  #   sidebar_content,
  #   
  #   # Your `main_content`
  #   main_content
  # )
)

summary_content <- mainPanel(
  p("Our data works with points about public opinion of abortions, cost of 
    abortions, and driving times to abortion clinics in California state."),
  p(tags$br()),
  img(src="aggregated_data.png", class = "img-size"),
  p(em("Our table of aggregated data, grouping by the state of California."), class = "caption"),
  p(tags$br()),
  p("From the first five columns, the table reveals that over 62% of Californian 
    survey participants consistently agree that abortions should be legalized 
    under various conditions. This observation affirms our own opinions on 
    California’s liberal views that make them more likely to agree to 
    legalizing abortions."),
  p("The sixth column took the average of all total expenditures on abortions 
    and abortion-related procedures per county in California, and this result 
    came out to $293408.25. We acknowledge that California is a large state 
    with many large cities and also many rural cities, which was shown in our 
    original dataset with millions of dollars from major counties like Los 
    Angeles County, whereas abortions and abortion-related services were only a 
    few hundred dollars in the smaller, rural counties like Sierra County."),
  p("The last four columns in our table describe one’s average driving time (in 
    minutes) to the nearest abortion clinic in California, based on what week 
    of pregnancy they decided to abort at (8, 12, 16, 20). These numbers came 
    out to be very small (all around 1 minute: 1, 1, 1.13, and 1.26, 
    respectively), and we believe this is because there is an abundance of 
    clinics in California that are more easily accessible to serve more of the 
    state’s extremely large population. Though small, there was a positive 
    trend in the driving time as one’s pregnancy progressed until they decided 
    to get an abortion. We believe this is because it becomes increasingly 
    more difficult to perform abortions later in the pregnancy, and not all 
    clinics would have the capacity or skill to handle those abortions, so 
    people would have to drive farther out to find an abortion clinic that 
    serves them."),
  p(tags$br()),
  p(strong("Our three key takeaways:")),
  tags$li("People are more likely to be able to have access to an abortion 
          clinic as many are in close proximity to each other due to a large population 
          and cities (mostly liberal) which are able to provide these services."),
  tags$li("A majority of 62% of Californians are in support of legalizing 
          abortion under any circumstances."),
  tags$li("Driving times to an abortion clinic in California is an average of about 
          1 minute, which demonstrates the accessibility of abortion services.")
)

summary_panel <- tabPanel(
  class = "inner-content",
  "Summary",
  titlePanel("Summary"),
  summary_content
)

# Report
report_insert <- mainPanel(
  includeHTML("docs/index.html")
)

report_panel <- tabPanel(
  "Project Report",
  report_insert
)


# Include motivation

motivation_insert <- mainPanel(
  includeMarkdown("docs/p01-proposal.md")
)

motivation_panel <- tabPanel(
  "Motivation",
  motivation_insert
)

# Finally, define a `ui` variable, assigning it a `navbarPage()` layout.
# You will use `shinyUI()` to render this variable (in `app.R`)
# Give the layout a title of "Income Inequality".
# The layout should include the following elements:
# - Your `intro_panel`
# - Your `growth_panel`

ui <- fluidPage(
  navbarPage(
    "An Analysis of Abortion Trends",
    theme = shinythemes::shinytheme("readable"),
    intro_panel,
    public_opinion_panel, 
    funding_panel,
    driving_access_panel, 
    summary_panel,
    report_panel, 
    motivation_panel
  ),
  includeCSS("styles.css")
)
