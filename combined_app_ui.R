# combined_app_ui.R
# Fiona Jain, Vega Jethani, Aaron Jenkins, Mehr Mehta
# Final Deliverable

# Load Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(shinythemes)
library(readr)
library(plotly)
library(leaflet)
library(shinyWidgets)

# Load Dataset
drug_data <- read_csv("drugs.csv")


# Data Manipulation for Widget 1
cocaine_use <- drug_data %>%
  select(
    State, Year, `Population.12-17`, `Population.18-25`, `Population.26+`,
    `Totals.Illicit Drugs.Cocaine Used Past Year.12-17`,
    `Totals.Illicit Drugs.Cocaine Used Past Year.18-25`,
    `Totals.Illicit Drugs.Cocaine Used Past Year.26+`, 
    `Rates.Illicit Drugs.Cocaine Used Past Year.12-17`,
    `Rates.Illicit Drugs.Cocaine Used Past Year.18-25`, 
    `Rates.Illicit Drugs.Cocaine Used Past Year.26+`
  )

# First Widget- Checkbox
state_select <- selectInput(
  inputId = "state_select",
  label = h2("Select State"),
  choices = cocaine_use$State,
  selected = cocaine_use$State["California"],
  multiple = FALSE
)

# Data Manipulation for Widget 2
cocaine_years <- range(cocaine_use$Year)

# Second Widget- Slider - Fiona Jain
year_slider <- sliderInput(
  inputId = "year_select",
  label = h2("Select Year"),
  sep = "",
  min = cocaine_years[1],
  max = cocaine_years[2],
  value = cocaine_years[2],
  step = 1
)


data <- read.csv("drugs.csv")


alc_dis <- data %>%
  group_by(State, Year) %>%
  filter(State == "Alabama" |
    State == "Arizona" |
    State == "California" |
    State == "Indiana" |
    State == "New York" |
    State == "Ohio" |
    State == "Washington") %>%
  filter(Year > 2001)



state_choices <- unique(alc_dis$State)



# variable for dropdown menu that selects country 
state_select <- checkboxGroupInput(
  inputId = "State",
  label = "State Choices",
  choices = state_choices,
  selected = "California"
)


# variable for slider that selects year range
year_slider <- sliderInput(
  inputId = "Year",
  label = "Choose a year range",
  min = 2002,
  max = 2018,
  value = c(2002, 2018),
  sep = ""
)



intro <- tabPanel("Data Analysis - Introduction",
  style = "color: black",
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"
  ),
  h4("Group Members: Fiona Jain, Vega Jethani, Aaron Jenkins, Mehr Mehta",
    style = "color: red"
  ),
  p("We opted to investigate the topic of drug and alcohol use in the United 
  States for our research, focusing on the age group 12-17. We got the datset from
  https://corgis-edu.github.io/corgis/csv/drugs/. This dataset comes from CORGIS
  Dataset Project and highlights the issue of drug and alcohol abuse (cigarettes,
  marijuana, cocaine, alcohol). This data is collected from each state as part of
  the NSDUH study.There are 27 columns and 50 rows in this dataset, where each
  state is a row. The population was divided into three subcategories in this
  dataset: 12-17, 18-25, and 26+. The dataset then depicts the number of people
  who consumed these drugs in each age group, as well as the number of deaths and
  disorders caused. This dataset covers the 2002 to 2018, and shows that the
  population aged 26 and up consumes the most drugs. This data collection delves
  into a fascinating topic that has been a serious problem in the country. Every
  year, thousands of people die from drug overdose. Over the last few years,
  there has been an increase in the use of these deugs, which the CDC has been
  attempting to limit at both the federal and state levels. Teenagers have
  obviously been affected, and the risk of lung and oral cancer has also increased.
  As these have become increasingly acceptable in society, especially normalized
  in teenagers, the amount of people who use them has increased. Stress, peer
  pressure, legalization, and the recent opioid epidemic could all be factors.
  We'll be understanding more about these facts using the graphs in the following
    tabs.",
    style = "font-size:18px"
  ),
  img(
    src = "https://www.therecoveryvillage.com/wp-content/uploads/2017/05/shutterstock_139087055-600x400-300x200.jpg",
    style = "display: block; margin-left: auto; margin-right: auto;",
    height = 500, width = 500
  )
)

p1 <- tabPanel(
  "Plot and Summary: Cocaine",
  sidebarLayout(
    sidebarPanel(
      h4("Cocaine Usage in Children Ages 12-17",
        style = "color: #4fc3f7"
      ),
      state_select, year_slider
    ),
    mainPanel(
      plotlyOutput("plot"),
      p("I chose to make this graph to show the trend in cocaine use among
            children. This graph clearly displays the trend in cocaine usage
            among children ages 12-17 in different states. Users can select a
            state and even which specific years they want to look at. Drug use in
            children is a major problem; it can hinder development, lead to mental
            health and physical health issues, and it needs to be a topic that
            people are more aware of. Being able to see these trends helps get a
            better picture of the extent of this issue. For example, many states
            had drastic rises in cocaine use among children in 2015.")
    )
  )
)

p2 <- tabPanel(
  "Plot and Summary: Alcohol",
  fluidRow(
    h4("  Alcohol Disorder Percentage for Ages 12-17",
      style = "color: green"
    ),
    column(state_select, width = 6),
    column(year_slider, width = 6)
  ),
  fluidRow(
    column(plotlyOutput("scatter_plot"), width = 12)
  ),
  p(
    strong("Explanation of Graph and Trends")
  ),
  p(
    "For this graph I decided to focus on the rate of
        alcohol disorders in ages 12-17 per state.
        I choose to focus on the rate instead of whole
        numbers because each state
        has a different population, so the chart would therefore not be accurate.
        I wanted to focus on ages 12-17 because
        I thought that this age would be the most easily
        influenced to partake in such
        activities. Some trends in the graph I noticed are that
        all 7 states had decreasing rates. Indiana had the highest starting value 
        and 2002, while Alabama had the lowest. By 2018, Washington had the highest 
        percentage value, while Alabama had the lowest. There were many peaks for 
        each individual state; each state had about 5-8."
  )
)

p3 <- tabPanel(
  "Map and Summary: Marijuana, Cocaine, Alcohol",
  sidebarPanel(
    selectInput(
      inputId = "usage_type",
      label = "Drug Type",
      choices = list(
        "Alcohol Usage Rate" = "Alcohol Usage Rate",
        "Marijuana Usage Rate" = "Marijuana Usage Rate",
        "Illicit Drug Usage Rate" = "Illicit Drug Usage Rate"
      ),
      selected = "Alcohol Usage Rate"
    ),
    radioButtons(
      inputId = "year",
      label = "Select a Year",
      choices = list(
        "2002" = "2002",
        "2003" = "2003",
        "2004" = "2004",
        "2005" = "2005",
        "2006" = "2006",
        "2007" = "2007",
        "2008" = "2008",
        "2009" = "2009",
        "2010" = "2010",
        "2011" = "2011",
        "2012" = "2012",
        "2013" = "2013",
        "2014" = "2014",
        "2015" = "2015",
        "2016" = "2016",
        "2017" = "2017",
        "2018" = "2018"
      ),
      selected = "2002"
    )
  ),
  mainPanel(
    leafletOutput(outputId = "usageleaf")
  )
)

conclusion <- tabPanel(
  "Conclusion",
  mainPanel(
    h3("Every year, millions of Americans are affected by drug usage. We opted to
   focus our investigation on Cocaine, Marijuana, and Alcohol, as well as their
      use by people aged 12 to 17 in different states.",
      style = "font-size:18px"
    ),
    h3("One of the most intriguing findings from our examination was the rate of
      alcohol disorders in children and adolescents aged 12 to 17 by state was
      that Arizona had the highest percentage value of 0.07 in the year 2002.
      Alabama had the lowest in the year 2002; a percentage value of 0.05%. Some
      trends in the graph I noticed are that all 7 states had decreasing rates.
      Indiana had the highest starting value while Alabama had the lowest In 2002.
      By 2018, Washington had the highest percentage value, while Alabama had the
      lowest. There were many peaks for each individual state; each state had about
      5-8. Out of all the states, Arizona peaked in 2003 with a percentage of
      0.0749. The lowest peak in the graph was Indiana in 2017 with a percentage
      of 0.014. Our biggest takeaway was that alcohol rates continued to decline
      steadily.",
      style = "font-size:18px"
    ),
    h3("Another finding from examining cocaine use among minors aged 12 to 17 in
      various states was that Washington had the greatest cocaine use in 2003
      and the lowest in 2018. Our group also noticed a large spike in rates during
      the year 2015. We observed this for states such as Alaska, Hawaii,
      Massachusetts, and Kansas. However, Arkansas, Connecticut, Delaware,
      District of Columbia, Idaho, Indiana, Iowa, Kentucky, Michigan, and Maryland
      stayed flat prior and after 2015. Our biggest takeaway was that 2015 was a
      notable year for use of cocaine; there may have been a change of policy.
      However, all states had a general decreasing trend from 2002 to 2018.",
      style = "font-size:18px"
    ),
    h3("Another key conclusion from the illicit drug consumption rate research
   was that there was a surge in illicit drug use in 2004. Furthermore, marijuana
   use was highest in the states of Washington and Montana, possibly due to the
   drug's legalization and increased accessibility. The middle part of the
   country, which includes states like Kansas, Iowa, Missouri, and Montana, had
      the greatest alcohol consumption.",
      style = "font-size:18px"
    ),
    h3("In the United States, drug abuse has been a serious problem among the youth.
   This has an impact on teenagers' mental, physical, and emotional wellbeing.
   Drug misuse is frequently associated to mental health issues such as depression,
   developmental delays, indifference, withdrawals, and other psychological
   disorders in adolescents. It raises the risk of suicide and makes dropping
   out more likely. The problem of drug abuse has long been an issue. Drug abuse
   is on the rise, and it's becoming a major problem that needs to be addressed.
   It is critical to raise awareness about this issue and to take steps to assist
   individuals who are battling with drug addiction.",
      style = "font-size:18px"
    ),
    img(
      src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQ0ijrqdpl3_DM3LvZvBZ9ZlY_83dVE28e1Cw&usqp=CAU",
      style = "display: block; margin-left: auto; margin-right: auto;",
      height = 300, width = 400
    ),
  ),
)





ui <- navbarPage(
  title = "Final Deliverable: Drug and Alcohol Usage in Children",
  intro,
  p1,
  p2,
  p3,
  conclusion
)
