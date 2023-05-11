library(shiny)
library(shinyjs)
library(shinyBS)
library(Rcpp)

setwd("/home/adrian/Facultate/Licenta/Percolation/R/")


### App
source("UI.R")
source("Server.R")
source("Percolation.R")

shinyApp(ui = ui, server = server, options = list(port = 9090))
