library(tidyverse)
library(ggplot2)


music <- read_csv("music_genre.csv")
music$duration_ms <- replace(music$duration_ms, music$duration_ms == -1, 245503.5)
music$tempo <- as.numeric(music$tempo)
temp <- music %>% filter(tempo != NA)
