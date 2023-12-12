library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(caret)
library(DT) 

music <- read_csv("www/songs.csv")
music$key <- as.factor(music$key)
music$mode <- as.factor(music$mode)
music$music_genre <- as.factor(music$music_genre)

quant_var <- c("popularity", "acousticness", "danceability", "duration_ms", "energy", "liveness", "loudness", "tempo", "valence")

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  theme = shinythemes::shinytheme("cosmo"),
  "VU app",
  tabPanel("About",
           h2(strong("Welcome to the VU App")),
           strong("an exploration of song measurements"),
           img(src="vu_image1.png", align = "center", height = "50%", width = "100%"),
           h5("Empty For Now")
           ),
  
# Plots/Graphs
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
                        ),
# Histogram
        conditionalPanel(condition = "input.plot == 3",
                         p("Compare measurements by specific genres"),
                         selectInput("hist_genre1", "Genre 1", selected = "Hip-Hop", 
                                     choices = levels(as.factor(music$music_genre))),
                         selectInput("hist_genre2", "Genre 2", selected = "Country", 
                                     choices = levels(as.factor(music$music_genre))),
                         selectInput("hist_x", "Measurement", selected = "tempo", choices = quant_var),
                         sliderInput("hist_slide", label=h6("Select Bins"), min=20, max=150,
                                     value=50, step=10),
                         radioButtons("hist_radio", label = h6("Display"), choices = list("Side-by-Side" = "dodge", "Stacked" = "stack"), selected = "dodge")
                         ),
# Geom_Density
        conditionalPanel(condition = "input.plot == 4",
                         p("View density of measurements by key"),
                         selectInput("kernal_x", "Measurement", selected = "popularity", 
                                     choices = quant_var),
                         radioButtons("kernel", choices = list("Smooth" = "gaussian", 
                                                               "Rectangular" = "rectangular"),
                                      selected = "gaussian", label = NULL),
                         checkboxInput("facet", h6("Facet Wrap Genres")),
                         ),
         br(),
         h3("View Summary Statistics"),
         selectInput("sum_stats", "Measurement", selected = "danceability", choices = quant_var)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          conditionalPanel(condition = "input.plot == 1",
            plotOutput("scatterPlot")
        ),
          conditionalPanel(condition = "input.plot == 2",
            plotOutput("boxPlot")
            ),
        conditionalPanel(condition = "input.plot == 3",
            plotOutput("histPlot")
            ),
        conditionalPanel(condition = "input.plot == 4",
            plotOutput("densityPlot")
            ),
        dataTableOutput("sum_stats")
        )
    ),
),
# Modeling
  tabPanel("Modeling",
    tabsetPanel(type = "tabs",
                tabPanel("About"),
                tabPanel("Modeling",
                         sidebarLayout(
                           sidebarPanel(
                         h2("Build Models"),
                         sliderInput("test_size", label=h6("Test Size as %"), min = 0.1, max = 0.9,
                                     value = 0.8, step = 0.05),
                         selectInput("model_1", "1st Measurement", selected = "valence",
                                     choices = quant_var),
                         selectInput("model_2", "2nd Measurement", selected = "danceability",
                                     choices = quant_var),
                         radioButtons("model_cat", label = h6("Categorical Selection"),
                                      choices = list("Genre" = "music_genre", "Key" = "key"),
                                      selected = "music_genre"),
                         numericInput("num_cv", label = h6("CV folds"), value = 5, min = 1, max = 10),
                         sliderInput("mtry", label=h6("Tune Parameters"), min=1, max=10, value=c(1,10)),
                         fluidRow(
                           column(5, actionButton("train_lm", "Fit Linear")),
                           column(5, actionButton("train_rf", "Fit Rand Forest"))
                         ),
                         br(),
                         fluidRow(
                           column(6, 
                           conditionalPanel(condition = "input.train_lm != 0",
                                          actionButton("test_lm", "Test Linear"))),
                           column(6, 
                                  conditionalPanel(condition = "input.train_rf != 0",
                                                   actionButton("test_rf", "Test RF")))
                          )),
                         mainPanel(
                           fluidRow(h2("Modeling Song Popularity")),
                           fluidRow(
                             column(6, h4("Linear Model Summary")),
                             column(6, h4("Random Forest Summary")),
                             ),
                           fluidRow(
                             column(6, tableOutput("lm_rmse")),
                             column(6, tableOutput("rf_rmse")),
                           ),
                           fluidRow(
                             column(6, verbatimTextOutput("lm_sum")),
                             column(6, plotOutput("rf_mtry")),
                           ),
                           fluidRow(
                             column(6,
                             conditionalPanel(condition = "input.test_lm != 0",
                                              h4("LM Model Results on Test Data"))),
                             column(6,
                                    conditionalPanel(condition = "input.test_rf != 0",
                                                     h4("RF Model Results on Test Data")))
                           ),
                           fluidRow(
                             column(6, tableOutput("testlm")),
                             column(6, tableOutput("testrf"))
                           )
                         )
                         )),
                tabPanel("Predicting"),
                
                ))

))
