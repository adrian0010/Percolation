# Student: Adrian Ivan
# Candidate BSc CS 2023
# West University, Timisoara

# Supervisors:
# Prof. Dr. Daniela Zaharie
# Dr. med. Leonard Mada (Syonic SRL)

# URL: https://github.com/adrian0010/Percolation

# Based on Previous Projects (2020-2022)

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