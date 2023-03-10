

ui = fluidPage("Simulation of diffusion processes", shinyjs::useShinyjs(),
		tabsetPanel(
			tabPanel("Percolation Process",
				fluidRow(
					column(4, textOutput("txtTitleSimple")) ),
				plotOutput("PercolationSimple"),
				tableOutput("Statistics"),
				fluidRow(
					column(4,
						sliderInput(inputId = "probSimple", label = "Probability Cutoff",
							value = 0.5, min = 0, max = 1, step = 0.01)
					),
					column(4,
						sliderInput(inputId = "heightSimple", label = "Height",
							value = 100, min = 100, max = 2000, step = 50)
					),
					column(4,
						sliderInput(inputId = "widthSimple", label = "Width", 
							value = 50, min = 20, max = 200, step = 20))
				)
			)
		)
)
