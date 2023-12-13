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
           HTML("This app is designed to explore song data information collected from <b>Spotify</b>. It comes from <b>Kaggle</b> and is available <a href='https://www.kaggle.com/datasets/vicsuperman/prediction-of-music-genre'> here </a>. The data was originally a collection of measurements from over 50,000 songs but has been paired down to 5,000 songs through random sampling. Variables consist of nine continuous measurements and two categorical measurements. The continuous measurement <b>Popularity</b> will be used as the outcome for predictions."),
           br(),
           br(),
           HTML("<b>Plots/Graphs</b> - In this tab, you will be able to explore visually the different relationships between song types and measurements as well look up summaries about each measurement. You will be able to view linear relationships between variables, distributions by category, and compare distribution frequencies between categories."),
           br(),
           br(),
           HTML("<b>Modeling</b> - In these tabs you will be able to read descriptions of the models used, build your own models by selecting from a variety of parameters, view and compare model training and testing statistics, and finally, run your own predictions to determine the popularity of a given song based on your selected input parameters.")
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
                tabPanel("About",
                         h3("Predict Popularity of a Song"),
                         HTML("In this section you will be able to fit two types of predictive models - <b>Multiple Linear Regression</b> and <b>Random Forest</b>. In the Modeling tab, you can select two measurements and one categorical variable to train your model. Once you run each model with the 'Fit' buttons you will be able to view the training results as well as test the models on a testing dataset. You may also select the size of both the test and training sets."),
                         br(),
                         HTML("On the Predict panel, you will be able to select specific measurements from your selected training variables and get a prediction of popularity."),
                         h4("Model Information"),
                         HTML("<b>Multiple Linear Regression</b> - Simple Linear Regression models create a linear equation between two variables - a dependent variable ('y') and independent variable ('x). The model is based on minimizing the sum of squared residuals and is used to predict the outcome of the dependent variable ('y'). Often, however, better predictions can be made if more variables are considered in the prediction model. <b>Multiple Linear Regression Models</b> allow for multiple variables to be used in calculating predictive outcomes. When more variables, or features, are added to the model the regression line can become more 'flexible' - as seen in the additional terms and features of this example regression equation."),
                         withMathJax(),
                         helpText('Where \\(x_{n}\\) is a separate predictor variable: \\(y_{i}\\) = \\(\\beta\\)\\(_{0}\\) + \\(\\beta\\)\\(_{1}\\)\\(x_{1}\\) + \\(\\beta\\)\\(_{2}\\)\\(x_{2}^{2}\\) + ... + \\(\\beta\\)\\(_{n}\\)\\(x_{n}\\)'),
                         HTML("However, when adding additional features to the MLR model, it is possible that certain variables will be correlated which will hinder the model's performance. Multiple Linear Regression models perform best when all features are independent of each other."),
                         br(),
                         br(),
                         HTML("<b>Random Forest</b> - Random Forest utilizes bootstrap aggregation to calculate the average response over many fitted trees. Whereas a Classification and Bagged Tree model will use all predictors in modeling, Random Forest will use a random subset of predictors for each bootstrap sample. By randomly selecting a subset of predictors, a good predictor or two will not dominate the tree fits and thus the variance from aggregation will not be reduced, resulting in a more robust model."),
                         br(),
                         HTML("However, Random Forests are very compuntationally expensive since it iterates over a range of variables to determine the best fit. Before deciding on a Random Forest Model, you will want to compare the training and testing results with other models to determine if the increase in accuracy (if any) is worth the computational expense."),
                         ),
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
                tabPanel("Predicting",
                conditionalPanel(condition = "input.train_lm == 0 | input.train_rf == 0",
                                 h3("You must fit both models in the Modeling tab to run a prediction")),
                fluidRow(
                column(4,
                conditionalPanel(condition = "input.model_1 == 'valence'",
                                 sliderInput("meas_1", label="Valence", min=min(music$valence),
                                             max=max(music$valence), value=mean(music$valence))),
                conditionalPanel(condition = "input.model_1 == 'acousticness'",
                                 sliderInput("meas_1", label="Acousticness", min=min(music$acousticness),
                                             max=max(music$acousticness), value=mean(music$acousticness))),
                conditionalPanel(condition = "input.model_1 == 'danceability'",
                                 sliderInput("meas_1", label="Danceability", min=min(music$danceability),
                                             max=max(music$danceability), value=mean(music$danceability))),
                conditionalPanel(condition = "input.model_1 == 'duration_ms'",
                                 sliderInput("meas_1", label="Duration_ms", min=min(music$duration_ms),
                                             max=max(music$duration_ms), value=mean(music$duration_ms))),
                conditionalPanel(condition = "input.model_1 == 'energy'",
                                 sliderInput("meas_1", label="Energy", min=min(music$energy),
                                             max=max(music$energy), value=mean(music$energy))),
                conditionalPanel(condition = "input.model_1 == 'liveness'",
                                 sliderInput("meas_1", label="Liveness", min=min(music$liveness),
                                             max=max(music$liveness), value=mean(music$liveness))),
                conditionalPanel(condition = "input.model_1 == 'loudness'",
                                 sliderInput("meas_1", label="Loudness", min=min(music$loudness),
                                             max=max(music$loudness), value=mean(music$loudness))),
                conditionalPanel(condition = "input.model_1 == 'tempo'",
                                 sliderInput("meas_1", label="Tempo", min=min(music$tempo),
                                             max=max(music$tempo), value=mean(music$tempo))),
                conditionalPanel(condition = "input.model_1 == 'popularity'",
                                 h4("Predicted outcome is for the measurement 'Popularity'. 
                                    Please select another measurement from the Modeling tab.")),
                ),
                column(4,
                       conditionalPanel(condition = "input.model_2 == 'valence'",
                            sliderInput("meas_2", label="Valence", min=min(music$valence),
                                max=max(music$valence), value=mean(music$valence))),
                       conditionalPanel(condition = "input.model_2 == 'acousticness'",
                            sliderInput("meas_2", label="Acousticness", min=min(music$acousticness),
                                max=max(music$acousticness), value=mean(music$acousticness))),
                       conditionalPanel(condition = "input.model_2 == 'danceability'",
                            sliderInput("meas_2", label="Danceability", min=min(music$danceability),
                                max=max(music$danceability), value=mean(music$danceability))),
                       conditionalPanel(condition = "input.model_2 == 'duration_ms'",
                            sliderInput("meas_2", label="Duration_ms", min=min(music$duration_ms),
                                max=max(music$duration_ms), value=mean(music$duration_ms))),
                       conditionalPanel(condition = "input.model_2 == 'energy'",
                            sliderInput("meas_2", label="Energy", min=min(music$energy),
                                max=max(music$energy), value=mean(music$energy))),
                       conditionalPanel(condition = "input.model_2 == 'liveness'",
                            sliderInput("meas_2", label="Liveness", min=min(music$liveness),
                                max=max(music$liveness), value=mean(music$liveness))),
                       conditionalPanel(condition = "input.model_2 == 'loudness'",
                            sliderInput("meas_2", label="Loudness", min=min(music$loudness),
                                max=max(music$loudness), value=mean(music$loudness))),
                       conditionalPanel(condition = "input.model_2 == 'tempo'",
                            sliderInput("meas_2", label="Tempo", min=min(music$tempo),
                                max=max(music$tempo), value=mean(music$tempo))),
                       conditionalPanel(condition = "input.model_2 == 'popularity'",
                                        h4("Predicted outcome is for the measurement 'Popularity'. 
                                    Please select another measurement from the Modeling tab.")),
                       ),
                column(4,
                       conditionalPanel(condition = "input.model_cat =='music_genre'",
                       selectInput("pred_cat", "Genre", selected = "Hip-Hop",
                                   choices=levels(as.factor(music$music_genre)))),
                       conditionalPanel(condition = "input.model_cat == 'key'",
                        selectInput("pred_cat", "Key", selected = "A",
                                    choices=levels(as.factor(music$key))))
                       )
                ),
                br(),
                fluidRow(
                  column(6,
                         conditionalPanel(condition = "input.train_lm != 0",
                                          actionButton("predict_lm", "Run LM Prediction"))),
                  column(6,
                         uiOutput("LM_Prediction")),
                ),
                br(),
                fluidRow(
                  column(6,
                         conditionalPanel(condition = "input.train_rf !=0",
                                          actionButton("predict_rf", "Run RF Prediction"))),
                  column(6,
                         uiOutput("RF_Prediction"))
                ),
                br(),
                fluidRow(
                  column(4,
                         uiOutput("col_1")),
                  column(4,
                         uiOutput("col_2")),
                  column(4)
                  )
                )
                
                )
  )
))
