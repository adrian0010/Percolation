library(shiny)
library(shinyjs)
library(shinyBS)
library(Rcpp)
library(igraph)

setwd("/home/adrian/Facultate/Licenta/Percolation/R/")

### CPP Function
FUN = readLines("Percolation.Analysis.cpp")
FUN = paste0(FUN, collapse="\n")
cppFunction(FUN)

### App
source("UI.R")
source("Server.R")
source("Percolation.R")
source("Percolation.HelpUI.R")

run = function(){
    shinyApp(ui = ui, server = server, options = list(port = 9090))
}