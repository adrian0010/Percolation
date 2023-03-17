# Percolation

### Prerequisites
 - R language installed

### Packages to install
```R
install.package(shiny)
install.package(shinyjs)
install.package(shinyBS)
```

### Run
You need to do the following to run the application
    
1. from file [Percolation.App.R](/R/Percolation.App.R) modify the following to set the working directory to the one where you put the application: 

    - setwd()

2.  Run this script: [Percolation.App.R](/R/Percolation.App.R) 

     - If the webserver does not start paste this line in the R interpreter:

```R
shinyApp(ui=ui, server=server, options = list(port = 9090))
```