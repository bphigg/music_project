library(shiny)

quant_var <- c("popularity", "acousticness", "danceability", "duration_ms", "energy", "liveliness", "loudness", "tempo", "valence")

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
}
