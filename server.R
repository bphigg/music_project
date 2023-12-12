library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(caret)
library(DT)

quant_var <- c("popularity", "acousticness", "danceability", "duration_ms", "energy", "liveness", "loudness", "tempo", "valence")

music <- read_csv("www/songs.csv")
music$key <- as.factor(music$key)
music$mode <- as.factor(music$mode)
music$music_genre <- as.factor(music$music_genre)

# Define server logic required to draw a histogram
function(input, output, session) {
    scatter_data <- reactive({
      newdata <- music %>% filter(music_genre == input$scatter_genre)
    })
    output$scatterPlot <- renderPlot({
      
      newdata <- scatter_data()
      
      g <- ggplot(newdata, aes_string(x=input$scatter_x, y=input$scatter_y)) +
           geom_point(aes(colour = key), alpha = 0.5, position = "jitter")
        
      if(input$reg_line){
        g + geom_smooth(method="lm", fill="blue", se=input$conf_int)
      }  else {
        g
      }
    })
    
    output$boxPlot <- renderPlot({
      ggplot(music, aes_string(x=input$box_radio, y=input$box_y, fill = input$box_radio)) +
      geom_violin(alpha=0.5) + geom_boxplot(width=0.2, color="grey", alpha=0.9)
    })

    hist_data <- reactive({
      newdata1 <- music %>% filter(music_genre == input$hist_genre1 | music_genre == input$hist_genre2)
    })
    output$histPlot <- renderPlot({
      newdata1 <- hist_data()
      ggplot(newdata1, aes_string(x=input$hist_x, fill=newdata1$music_genre)) +
      geom_histogram(bins=input$hist_slide, position=input$hist_radio)
    })
    
    output$densityPlot <- renderPlot({
      h <- ggplot(music, aes_string(x=input$kernal_x)) +
      geom_density(aes_string(fill=music$key), adjust = 0.5, alpha = 0.5, kernel = input$kernel)
      
      if(input$facet){
        h + facet_wrap(~ music_genre)
      } else{
        h
      }
    })
    
    output$sum_stats <- DT::renderDataTable({
      music %>% group_by(music_genre) %>% summarize(Min = round(min(get(input$sum_stats)), 2), 
                Median = round(median(get(input$sum_stats)), 2), 
                Mean = round(mean(get(input$sum_stats)),2), 
                Max = round(max(get(input$sum_stats)),2),
                StDv = round(sd(get(input$sum_stats)), 2)
                )
    })
    
    musicTrain <- reactive({
      value <- music[createDataPartition(music$music_genre, p=input$test_size, list=FALSE), ]
    })
    output$train <- renderPrint({dim(musicTrain())})
    
    musicTest <- reactive({
      value <- music[createDataPartition(music$music_genre, p=1-input$test_size, list=FALSE), ]
    })
    output$test <- renderPrint({dim(musicTest())})
    
    music_lm <- eventReactive(input$train_lm, {
      formula <- as.formula(paste(sym("popularity"), "~", sym(input$model_1), "+", sym(input$model_2),
                                  "+", sym(input$model_cat)))
      value <- train(formula, data=musicTrain(),
                        method="lm",
                        preProcess=c("center", "scale"),
                        trControl=trainControl(method="cv", number=input$num_cv))
    })
      
    output$lm_rmse <- renderPrint({music_lm()$results[2:4]})
    output$lm_sum <- renderPrint({summary(music_lm())})
      
    music_rf <- eventReactive(input$train_rf, {
      formula <- as.formula(paste(sym("popularity"), "~", sym(input$model_1), "*", sym(input$model_2),
                                  "*", sym(input$model_cat)))
      value <- train(formula, data=musicTrain(),
                                 method="rf",
                                 preProcess=c("center", "scale"),
                                 trControl=trainControl(method="cv", number=input$num_cv),
                                 tuneGrid=data.frame(mtry=input$mtry[1]:input$mtry[2]))
    })
      
    output$rf_rmse <- renderTable({as_tibble(music_rf()$results[ ,1:4])})
    output$rf_mtry <- renderPlot({
      ggplot(music_rf()$results, aes(x=mtry, y=Rsquared)) + 
        geom_line()
    })
    
    lm_test <- eventReactive(input$test_lm, {
      value <- predict(music_lm(), newdata = musicTest())
    })
    
    output$testlm <- renderPrint({
      postResample(lm_test(), musicTest()$popularity)
    })
}
