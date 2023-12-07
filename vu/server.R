library(shiny)

quant_var <- c("popularity", "acousticness", "danceability", "duration_ms", "energy", "liveliness", "loudness", "mode", "tempo", "valence")

# Define server logic required to draw a histogram
function(input, output, session) {

    output$selectedPlot <- renderPlot({

      g <- ggplot(music, aes(x=valence, y=danceability))
      g + geom_point(aes(colour = music_genre), alpha = 0.5, position = "jitter") +
        geom_smooth(method="lm", fill="blue", se=TRUE)

    })

}
