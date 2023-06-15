helper = function(id = "Help1"){



	txtHelp = list(
		"Simple Model" = list(
			txt = "Consists of a rectangular grid created using a random uniform distribution. The probability cutoff 
				specifies the value above which the grid points become pores and below which they become solid material.", 
			P = "probability Cutoff = "),

		"Linearly Correlated Model" = list(txt = "Consists of a rectangular grid in which each subsequent column 
									inherits the values of the previous column. Some of these values are 
									regenerated with a probability given by the parameter Probability of Column 
									Change. The values in the first column and the regenerated values follow a 
									random uniform distribution. The probability cutoff specifies the value above 
									which the grid points become pores and below which they become solid material.",
									P = ""),
		"Binary Correlated Model" = list(txt = "Consists of a rectangular grid in which the binary state of each 
									subsequent column is either the state of the previous column (open or closed) or 
									is inverted. During inversion: an open cell becomes closed and a closed cell 
									becomes open. The inversion is selected with a probability given by the parameter 
									Probability of Column Change. The values in the first column follow a random 
									uniform distribution. The probability cutoff specifies the value above which 
									the grid points become pores and below which they become solid material.",
									P = ""),
		"Details" = list(txt = ""))
		

	txtTitles = names(txtHelp)
  	idActive = as.numeric(substr(id, 5, 6))
  	txt = lapply(seq(length(txtTitles)), function(id){
		if(id == idActive){
			txtActive =  #paste(txtHelp[[id]]$txt, "<br>", 
			paste("&nbsp;", txtHelp[[id]]$P, collapse = "<br>")
		} else {
			txtActive = ""
		}
		fluidRow(
		column(1, actionButton(paste0("Help", id), label = "", icon = icon("expand"))),
		column(10, HTML("<p> <b>", txtTitles[id], "</b> <br>",
					txtHelp[[id]]$txt, "<br>", txtActive ) ) )
					# paste("&nbsp;", txtHelp[[id]]$C, collapse = "<br>"), "</p>") )
  	});
  	return(txt)	
}