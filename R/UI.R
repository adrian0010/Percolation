

ui = fluidPage("Simulation of diffusion processes", shinyjs::useShinyjs(),
		tabsetPanel(
			tabPanel("Percolation Process",
				fluidRow(
					column(4, textOutput("txtTitleSimple")) ),
				fluidRow(
						column(8, plotOutput("PercolationSimple")),
						column(4,
							column(12, fluidRow(tableOutput("Statistics"))),
							column(12, fluidRow(tableOutput("Area"))),
						)
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
				),
				fluidRow(
					column(4,
						actionButton("newSimple", "New Material"))
				)
			),
			tabPanel("Linearly Correlated Process",
				fluidRow(
					column(4, textOutput("txtTitleLinearCorrelated"))),
				fluidRow(
						column(8, plotOutput("LinearCorrelated")),
						column(4,
							column(12, fluidRow(tableOutput("StatisticsLinearCorrelated"))),
							column(12, fluidRow(tableOutput("AreaLinearCorrelated"))),
						)
				),
				fluidRow(
					column(4,
						sliderInput(inputId = "heightLinearCorrelated", label = "Height",
							value = 100, min = 40, max = 400, step = 20)
					),
					column(4,
						sliderInput(inputId = "pChangeLinearCorrelated", label = "Change Probability",
							value = 0.5, min = 0, max = 1, step = 0.01)
					),
					column(4,
						sliderInput(inputId = "probLinearCorrelated", label = "Probability Cutoff",
							value = 0.5, min = 0, max = 1, step = 0.01)
					)
				),
				fluidRow(
					column(4,
						sliderInput(inputId = "widthLinearCorrelated", label = "Width",
							value = 50, min = 20, max = 200, step = 10)
					),
					column(4,
						selectInput(inputId = "typeLinearCorrelated", label = "Type",
						c("Constant" = "Constant",
						 "Bernoulli" = "Bernoulli"))
					)
				),
				fluidRow(
					column(4,
						actionButton("newLinearCorrelated", "New Material"))
				)
			),
			tabPanel("Binary Correlated Process",
				fluidRow(
					column(4, textOutput("txtTitleBinaryCorrelated"))),
				fluidRow(
						column(8, plotOutput("BinaryCorrelated")),
						column(4,
							column(12, fluidRow(tableOutput("StatisticsBinaryCorrelated"))),
							column(12, fluidRow(tableOutput("AreaBinaryCorrelated"))),
						)
				),
				fluidRow(
					column(4,
						sliderInput(inputId = "heightBinaryCorrelated", label = "Height",
							value = 100, min = 40, max = 400, step = 20)
					),
					column(4,
						sliderInput(inputId = "pChangeBinaryCorrelated", label = "Change Probability",
							value = 0.5, min = 0, max = 1, step = 0.01)
					),
					column(4,
						sliderInput(inputId = "probBinaryCorrelated", label = "Probability Cutoff",
							value = 0.5, min = 0, max = 1, step = 0.01)
					)
				),
				fluidRow(
					column(4,
						sliderInput(inputId = "widthBinaryCorrelated", label = "Width",
							value = 50, min = 20, max = 200, step = 10)
					),
					column(4,
						selectInput(inputId = "typeBinaryCorrelated", label = "Type",
						c("Constant" = "Constant",
						 "Bernoulli" = "Bernoulli"))
					)
				),
				fluidRow(
					column(4,
						actionButton("newBinaryCorrelated", "New Material"))
				)
			),

			tabPanel("Details",
				fluidRow(column(4, textOutput("txtTitleDetails"))),
				fluidRow(
					column(4, selectInput(inputId = "modelDetails", label = "Model type",
					c("Simple Model", "Linearly Correlated", "Binary Correlated"))),
					column(4, selectInput(inputId = "typeDetails", label = "Analysis Type",
					c("Channel Length" = "Channel Length", "Border" = "Border"))),
					column(4, selectInput(inputId = "idDetails", label = "Channel ID",
					c(1))),
				),
				fluidRow(
					column(8, plotOutput("plotDetails"))
				)
			),

			tabPanel("Linear Channels",
				fluidRow(
					column(4, textOutput("txtTitleLinear"))),
				fluidRow(
						column(8, plotOutput("channelsLinear")),
						column(4,
							column(12, fluidRow(tableOutput("StatisticsLinear"))),
							column(12, fluidRow(tableOutput("AreaLinear"))),
							column(12, fluidRow(tableOutput("LengthLinear")))
						)
				),
				fluidRow(
					column(4,
						sliderInput(inputId = "heightLinear", label = "Height",
							value = 100, min = 40, max = 400, step = 20)
					),
					column(4,
						sliderInput(inputId = "probLinear", label = "Probability Cutoff",
							value = 0.5, min = 0, max = 1, step = 0.01)
					),
					column(4,
						sliderInput(inputId = "probPoreLinear", label = "Pore prob",
							value = 4, min = 0, max = 10, step = 0.1)
					)
				),
				fluidRow(
					column(4,
						sliderInput(inputId = "widthLinear", label = "Width",
							value = 50, min = 20, max = 200, step = 10)
					),
					column(4,
						selectInput(inputId = "blockTypeLinear", label = "Block Type",
						c("Poisson" = "Poisson",
						 "Constant" = "Constant",
						 "Range 0:n" = "0:n", 
						 "Range 1:n" = "1:n"))),
					column(4,
						sliderInput(inputId = "probBlockLinear", label = "Block prob",
							value = 5, min = 0, max = 10, step = 0.1)
					)
				),
				fluidRow(
					column(4,
						actionButton("newLinear", "New Material"))
				)
				
			),
			tabPanel("Channel Levels",
				fluidRow(
					column(4, textOutput("txtTitleLinearLevels"))
				),
				fluidRow(
					column(8, plotOutput("LinearLevels"))
				),
				
			)
	)
)
