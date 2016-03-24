library(shiny)
library(plyr)
library(ggplot2)
library(reshape2)

shinyUI(fluidPage(

  # Application title
  titlePanel("Field Goal Models"),
  
  # Sidebar with a slider input for the number of bins
  
  #drop-down: which model? 
  #   a, b, c, d, or all? 
  #cold, windy, etc. . . 
  
  # ask if they want to see the full funciton line as well 
  sidebarLayout(
    
    sidebarPanel(
      tags$style(type='text/css', ".selectize-input { padding: 0px; min-height: 0; margin-top: 0px;font-size: 75%} .selectize-dropdown { font-size: 95%;line-height: 10px; }"),
      
      selectInput("kicker", label = h3("Kicker"), 
                  choices = list("Adam Vinatieri" = 0, "Blair Walsh" = 1, "Jason Hanson" = 2, "Josh Scobee" = 3,
                                 "Justin Tucker" = 4, "Kris Brown" = 5, "Sebastian Janikowski" = 12, "Seth Marler" = 15, "Jose Cortez" = 14,"Lawrence Tynes" = 6, "Mike Vanderjagt" = 7,
                                 "Paul Edinger" = 8, "Rob Bironas" = 9, "Stephen Gostkowski" = 10, "Steven Hauschka" = 11, "Billy Cundiff" = 13), 
                  selected = 0),
      
      sliderInput("dist",
                  "Distance:",
                  min = 18,
                  max = 62,
                  value = 35),
      
      sliderInput("temp",
                  "Temperature:",
                  min = 10,
                  max = 90,
                  value = 50),
      sliderInput("wspd",
                  "Wind Speed:",
                  min = 0,
                  max = 30,
                  value = 5),
      
      selectInput("turfa", label = h3("Natural Grass or Turf?"), 
                  choices = list("Natural Grass" = 0, "Turf" = 1), 
                  selected = 0),
      
      checkboxInput("toggleTrendline", label = "View Entire Function", value = TRUE),
      checkboxInput("alta", label = "Mile High Effect", value = FALSE),
      checkboxInput("precipa", label = "Precipitation?", value = TRUE),
      checkboxInput("iceda", label = "Kicker was iced", value = FALSE)
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", height = 550, width = 700)
    )
  )
))
