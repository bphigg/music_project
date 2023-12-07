library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(caret)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  theme = shinythemes::shinytheme("darkly"),
  "VU app",
  tabPanel("About",
           h5("Empty For Now")
  ),
  
  tabPanel("Plots/Graphs",

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          h3("Select Plot Type"),
            selectInput("plot", "Plot:", selected = 1, 
                        choices = list("Scatter"=1, "Box"=2, "Histogram"=3, "Kernal Smoother"=4)),
        conditionalPanel(condition = "input.plot == 1",
                         selectInput("scatter_x", "X-axis", selected = "valence", choices = levels(as.factor(quant_var))),
                         selectInput("scatter_y", "Y-axis", selected = "tempo", choices = levels(as.factor(quant_var))))
          ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("selectedPlot")
        )
    ),
)))
