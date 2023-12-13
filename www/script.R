library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(caret)
library(DT)



music <- read_csv("music_genre.csv")
music$duration_ms <- replace(music$duration_ms, music$duration_ms == -1, 245503.5)
music <- music[-10001:-10005, ]
music$tempo <- as.double(music$tempo, length = 0)
music <- drop_na(music, tempo)
music$key <- as.factor(music$key)
music$mode <- as.factor(music$mode)
music$music_genre <- as.factor(music$music_genre)
music <- music %>% select(-obtained_date, -instance_id, -artist_name, -track_name)
set.seed(808)
tempindex <- sample(c(1:45000), 5000)
music <- music[tempindex, ]
write_csv(music, "songs.csv")

str(read_csv("www/songs.csv"))
music <- read_csv("www/songs.csv")
#music <- music %>% filter(is.na(tempo))
#temp <- drop_na(music, tempo)
#music$tempo <- 
#  if_else(music$music_genre == "Alternative", replace_na(music$tempo, 123),
#          if_else(music$music_genre == "Anime", replace_na(music$tempo, 127),
#          if_else(music$music_genre == "Blues", replace_na(music$tempo, 121),
#          if_else(music$music_genre == "Classical", replace_na(music$tempo, 104),
#          if_else(music$music_genre == "Country", replace_na(music$tempo, 124),
#          if_else(music$music_genre == "Electronic", replace_na(music$tempo, 126),
#          if_else(music$music_genre == "Hip-Hop", replace_na(music$tempo, 120),
#          if_else(music$music_genre == "Jazz", replace_na(music$tempo, 112),
#          if_else(music$music_genre == "Rap", replace_na(music$tempo, 121), replace_na(music$tempo, 123#))))))))))

###EDA

quant_var <- c("popularity", "acousticness", "danceability", "duration_ms", "energy", "liveness", "loudness", "tempo", "valence")

g <- ggplot(music, aes(x=music_genre, y=danceability, fill = music_genre))
g + geom_violin(alpha=0.5) + geom_boxplot(width=0.2, color="grey", alpha=0.9) +
  labs(x = "Genre", y = "Danceability", title = "Danceability by Music Genre")

music %>% group_by(music_genre) %>% summarize(Min = min(valence), Median = median(valence), Mean = mean(valence), Max = max(valence), StDv = sd(valence))

music_ <- music %>% filter(music_genre == "Hip-Hop")
g <- ggplot(music_, aes(x=tempo, y=tempo))
g + geom_point(aes(colour = key), alpha = 0.5, position = "jitter") +
  geom_smooth(method="lm", fill="blue", se=TRUE)

g <- ggplot(music, aes(x=tempo, fill=mode))
g+ geom_histogram(bins=100, position="dodge")

g <- ggplot(music, aes(x=tempo)) +
geom_density(aes(fill = key), adjust = 0.5, alpha = 0.5, kernel = "rectangular")
g
g + facet_wrap(~ music_genre)

### Prediction

set.seed(828)
dfIndex <- createDataPartition(music$music_genre, p=0.8, list=FALSE)
musicTrain <- music[dfIndex, ]
musicTest <- music[-dfIndex, ]

music[createDataPartition(music$music_genre, p=1-.99, list=FALSE), ]

music_lm <- train(popularity ~ loudness + valence + music_genre, data=musicTrain,
                  method="lm",
                  preProcess=c("center", "scale"),
                  trControl=trainControl(method="cv", number=5))
as_tibble(music_lm$results[2:3])
summary(music_lm)

music_lm$method

music_lm_p <- predict(music_lm, newdata = musicTest)
lm_score <- postResample(music_lm_p, musicTest$popularity)

predict(music_lm, newdata = data.frame(-7, .9, "Rock"))


music_rf <- train(popularity ~ loudness * valence * music_genre, data=musicTrain,
                  method="rf",
                  preProcess=c("center", "scale"),
                  trControl=trainControl(method="cv", number=5),
                  tuneGrid=data.frame(mtry=1:5))
music_rf$results[ ,1:4]
music_rf$bestTune

g <- ggplot(music_rf$results, aes(x=mtry, y=Rsquared))
g + geom_line()

music_rf_p <- predict(music_rf, newdata=musicTest)
postResample(music_rf_p, musicTest$popularity)

predict(music_rf, newdata = data.frame(loudness= -7, valence=.9, music_genre="Rock"))

### code trials
as.formula(music$popularity)
as.formula(paste(popularity, "~", quant_var[2]))
as.formula(quant_var[2])
formula(paste(quant_var[2], collapse=""))
as.formula(paste(sym("popularity"), "~", sym(quant_var[2]), "*", sym(quant_var[5]), "*", sym("key")))

min(music$valence)

paste(sym("valence"), "=", .9,",", sym("tempo"), "=", 3.4)
paste0(sym("valence"), "=", .9,",", sym("tempo"), "=", 3.4)

install.packages(c("shiny", "shinythemes", "tidyverse", "ggplot2", "caret", "DT"))

lapply(c("shiny", "shinythemes", "tidyverse", "ggplot2", "caret", "DT"), library, character.only=TRUE)


df <- data.frame(2, 4, 6)
colnames(df) <- c(t, u, s)
colnames(df) <- c("a", "b", "c")
df

substitute(music)

