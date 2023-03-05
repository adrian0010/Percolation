library(shiny)
library(shinyjs)
library(shinyBS)

setwd("/home/adrian/Facultate/Licenta/Percolation/R/")


### App
source("UI.R")
source("Server.R")
source("Percolation.R")

shinyApp(ui=ui, server=server)