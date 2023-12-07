library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  theme = shinythemes::shinytheme("darkly"),
  "VU app",
  tabPanel("About",

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)))
