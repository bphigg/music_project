# music_project

This app is designed to explore music data gathered from Spotify and predict the popularity of a song utilizing several variables.

Packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(caret)
library(DT)

nstall.packages(c("shiny", "shinythemes", "tidyverse", "ggplot2", "caret", "DT"))

lapply(c("shiny", "shinythemes", "tidyverse", "ggplot2", "caret", "DT"), library, character.only=TRUE)

shiny::runGitHub("music_project", "bphigg")
