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

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
          h3("Select Plot Type"),
            selectInput("plot", "Plot:", selected = 1, 
                        choices = list("Scatter"=1, "Box"=2, "Histogram"=3, "Kernal Smoother"=4)),
# ScatterPlot
        conditionalPanel(condition = "input.plot == 1",
                         selectInput("scatter_genre", "Genre", selected = "Alternative", 
                                     choices = levels(as.factor(music$music_genre))),
                         selectInput("scatter_x", "X-axis", selected = "valence", choices = quant_var),
                         selectInput("scatter_y", "Y-axis", selected = "tempo", choices = quant_var),
        checkboxInput("reg_line", h6("Add a Regression Line")),
        conditionalPanel(condition = "input.reg_line == 1",
                         checkboxInput("conf_int", h6("Add Confidence Interval")))),
# BoxPlot
       conditionalPanel(condition = "input.plot == 2",
                        p("Investigate the distribution of measurements by categories"),
                        radioButtons("box_radio", label = h5("Categories"), choices = list("Genre" = "music_genre", "Key" = "key"), selected = "music_genre"),
                        selectInput("box_y", "Measurement", selected = "valence", choices = quant_var),
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          conditionalPanel(condition = "input.plot == 1",
            plotOutput("scatterPlot")
        ),
          conditionalPanel(condition = "input.plot == 2",
            plotOutput("boxPlot")
            ),
          )
    ),
)))
