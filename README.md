# Project 4 - shiny app
`brian higginbotham`

This app is designed to explore music data gathered from Spotify and predict the popularity of a song utilizing several variables.

The app can be run through an **R Studio** session with access to the internet. 

**The following R packages should be installed**:
* library(shiny)
* library(shinythemes)
* library(tidyverse)
* library(ggplot2)
* library(caret)
* library(DT)

**This code will `install` all the packages**:  
`install.packages(c("shiny", "shinythemes", "tidyverse", "ggplot2", "caret", "DT"))`

**This code will `load` all the packages to your current R session**:  
`lapply(c("shiny", "shinythemes", "tidyverse", "ggplot2", "caret", "DT"), library, character.only=TRUE)`

**Once the packages have been loaded, run the following code to execute the app in you R Studio Session**:  
`shiny::runGitHub("music_project", "bphigg")`
