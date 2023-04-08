

ui = fluidPage("Simulation of diffusion processes", shinyjs::useShinyjs(),
		tabsetPanel(
			tabPanel("Percolation Process",
				fluidRow(
					column(4, textOutput("txtTitleSimple")) ),
				plotOutput("PercolationSimple"),
				fluidRow(
					column(4, tableOutput("Statistics")),
					column(4, tableOutput("Area"))
				),
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
							value = 50, min = 20, max = 200, step = 10))
				)
			),
			tabPanel("Linear Channels",
				fluidRow(
					column(4, textOutput("txtTitleLinear"))),
				plotOutput("LinearChannels"),
				fluidRow(
					column(4, tableOutput("StatisticsLinear")),
					column(4, tableOutput("AreaLinear")),
				),
				fluidRow(
					column(4,
						sliderInput(inputId = "heightLinear", label = "Height",
							value = 100, min = 40, max = 400, step = 20)
					),
					column(4,
						sliderInput(inputId = "widthLinear", label = "Width",
							value = 50, min = 20, max = 200, step = 10)
					)
				),
				fluidRow(
					column(4,
						sliderInput(inputId = "probLinear", label = "Probability Cutoff",
							value = 0.5, min = 0, max = 1, step = 0.01)
					),
					column(4,
						sliderInput(inputId = "ppore", label = "Pore prob",
							value = 4, min = 0, max = 10, step = 1)
					),
					column(4,
						sliderInput(inputId = "pblock", label = "Block prob",
							value = 5, min = 0, max = 10, step = 1)
					),
				)
				
			)
		)
)
