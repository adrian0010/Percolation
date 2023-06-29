# Percolation

### Description
The primary focus of this project is the simulation of certain biophysical or biological processes. The actual application focuses on 2D simulations of porous materials, including percolation phenomena, as it combines a diverse set of algorithms, as well as practical utility in diverse fields of the biosciences. The project was proposed by the company Syonic and continues some previous projects run during 2020-2022.

### Prerequisites
 - R software installed

### Packages to install
```R
install.packages("shiny")
install.packages("shinyjs")
install.packages("shinyBS")
install.packages("Rcpp")
install.packages("igraph")
```

### Run
You need to do the following to run the application
    
1. from file [Percolation.App.R](/R/Percolation.App.R) modify the following to set the working directory to the one where you put the application: 

    - setwd("...")

2.  Run this script: [Percolation.App.R](/R/Percolation.App.R) 

     - Then run the run() funtion in the interpreter

```R
run()
```

If the webserver does not start, it could be because port 9090 is already being used by another application. In this case, you can try changing the port number.

### How to use

The webserver opens automatically a web page, which displays a randomly generated 2D porous material.
The application visualizes the percolating and the non-percolating channels with inflow from the left of the material.

In addition, several statistics are computed and diplayed on the page: e.g. number of channels, area of the channels.

The user can change the probability of the background being open or closed using a slider. This permits the simulation of a coupled percolation process.

### Planed tasks
For further details, see file:
[Percolation.md](/R/Percolation.md)