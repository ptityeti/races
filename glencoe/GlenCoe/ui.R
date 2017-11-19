library(shiny)
library(ggplot2)
library(reshape2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  theme = 'style.css',
  
  # Application title
  titlePanel("Glen Coe Performances"),
  
  selectInput("runnername", "Runner to highlight", c("", sort(results2017$Name))),

  fluidRow(column(6, plotOutput("timePlot")), column(6, plotOutput("posPlot")))

))
