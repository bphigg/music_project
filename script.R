library(tidyverse)
library(ggplot2)


music <- read_csv("music_genre.csv")
music$duration_ms <- replace(music$duration_ms, music$duration_ms == -1, 245503.5)
music <- music[-10001:-10005, ]
sum(is.na(music$tempo))
music$tempo <- as.double(music$tempo, length = 0)
#music <- music %>% filter(is.na(tempo))
#temp <- drop_na(music, tempo)
music$tempo <- 
  if_else(music$music_genre == "Alternative", replace_na(music$tempo, 123),
          if_else(music$music_genre == "Anime", replace_na(music$tempo, 127),
          if_else(music$music_genre == "Blues", replace_na(music$tempo, 121),
          if_else(music$music_genre == "Classical", replace_na(music$tempo, 104),
          if_else(music$music_genre == "Country", replace_na(music$tempo, 124),
          if_else(music$music_genre == "Electronic", replace_na(music$tempo, 126),
          if_else(music$music_genre == "Hip-Hop", replace_na(music$tempo, 120),
          if_else(music$music_genre == "Jazz", replace_na(music$tempo, 112),
          if_else(music$music_genre == "Rap", replace_na(music$tempo, 121), replace_na(music$tempo, 123))))))))))
music$key <- as.factor(music$key)
music$mode <- as.factor(music$mode)
music$music_genre <- as.factor(music$music_genre)

###EDA

g <- ggplot(music, aes(x=music_genre, y=danceability))
g + geom_boxplot(fill = "grey") +
  labs(x = "Genre", y = "Danceability", title = "Danceability by Music Genre")

music %>% select(artist_name, track_name, popularity, key, mode, music_genre) %>% filter(between(popularity, 1,2) & music_genre == "Rock")

music_ <- music %>% filter(music_genre == "Anime" | music_genre == "Country")
g <- ggplot(music_, aes(x=valence, y=acousticness))
g + geom_point(aes(colour = music_genre), alpha = 0.5, position = "jitter") +
  geom_smooth(method="lm", fill="blue", se=TRUE)

g <- ggplot(music, aes(x=valence, y=..density..,fill=key))
g+ geom_histogram(position="dodge", binwidth=10)
g+ geom_density(adjust = 0.5, alpha = 0.5)
