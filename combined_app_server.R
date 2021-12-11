# app_server.R
# Fiona Jain
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
library(tigris)
library(sf)
library(shinyWidgets)

# Load Dataset - Fiona Jain
drug_data <- read_csv("drugs.csv")

# Data Manipulation - Fiona Jain
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

cocaine_years <- range(cocaine_use$Year)

server <- function(input, output) {

  # Data for Plot - Fiona Jain
  plot_data <- reactive({
    data <- drug_data %>%
      group_by(State, Year) %>%
      filter(State == input$state_select) %>%
      filter(Year <= input$year_select)
    return(data)
  })

  # Actual Plot - Fiona Jain
  output$plot <- renderPlotly({
    plot_cocaine <- ggplot(plot_data()) +
      geom_line(
        mapping = aes(
          x = Year,
          y = `Totals.Illicit Drugs.Cocaine Used Past Year.12-17`,
          linetype = State
        ), stat = "identity", color = "#9f78eb"
      )

    return(ggplotly(plot_cocaine))
  })

  data <- read.csv("drugs.csv")

  # scatter plot: use renderPlotly for plot_ly plots
  output$scatter_plot <- renderPlotly({
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

    alc_dis_over_time <- alc_dis %>%
      filter(State %in% input$State) %>%
      filter(Year <= input$Year[2], Year >= input$Year[1])

    colnames(alc_dis_over_time)[9] <- "Percentage"

    # use ggplot to make a scatterplot
    chart_alc_dis_over_time <- ggplot(alc_dis_over_time) +
      geom_line(
        mapping = aes(x = Year, y = Percentage, color = State)
      ) +
      labs(
        title = "7 States' Alcohol Disorders in Ages 12-17",
        x = "Timeline in Years",
        y = "Percentage of Total Alcohol Disorders",
        color = "State"
      ) +
      scale_fill_distiller(palette = "Set1") +
      scale_x_continuous(limits = c(2002, 2018))

    # return the scatter plot
    return(ggplotly(chart_alc_dis_over_time))
  })
  ########
  data_df <- read.csv(file = "drugs.csv")

  output$usageleaf <- renderLeaflet({
    state_weed <- data_df %>%
      group_by(State) %>%
      filter(Year == input$year) %>%
      unique()

    states <- states(cb = T) %>%
      rename(State = NAME)


    state_shape <- states %>%
      left_join(state_weed, by = "State") %>%
      rename(
        "Alcohol Usage Rate" = Rates.Alcohol.Use.Disorder.Past.Year.12.17,
        "Marijuana Usage Rate" = Rates.Marijuana.Used.Past.Year.12.17,
        "Illicit Drug Usage Rate" = Rates.Illicit.Drugs.Cocaine.Used.Past.Year.12.17
      )

    pal <- colorNumeric("Reds", domain = NULL, na.color = NA)
    print(input$usage_type)

    usage_leaflet <- state_shape %>%
      leaflet(options = leafletOptions(
        maxZoom = 6,
        zoomControl = T,
        dragging = T
      )) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-98.483330, 38.712046, zoom = 4) %>%
      addPolygons(
        data = state_shape,
        fillColor = pal(state_shape[[input$usage_type]]),
        fillOpacity = 0.7,
        weight = 0.2,
        color = "black",
        smoothFactor = 0.2,
        popup = paste(
          "Year: ", state_shape$Year, "<br>",
          "Location: ", state_shape$State, "<br>",
          "Usage Rate: ",
          as.character(state_shape[[input$usage_type]])
        )
      ) %>%
      addLegend(
        pal = pal,
        values = state_shape[[input$usage_type]],
        position = "bottomright",
        title = input$usage_type
      ) %>%
      addControl(paste(input$usage_type, "Map in", input$year), position = "topleft")

    return(usage_leaflet)
  })
}
