# Student: Adrian Ivan
# Candidate BSc CS 2023
# West University, Timisoara

# Supervisors:
# Prof. Dr. Daniela Zaharie
# Dr. med. Leonard Mada (Syonic SRL)

# URL: https://github.com/adrian0010/Percolation

# Based on Previous Projects (2020-2022)

helper = function(id = "Help1"){



	txtHelp = list(
		"Simple Model" = list(
			txt = "Consists of a rectangular grid created using a random uniform distribution. The probability cutoff 
				specifies the value above which the grid points become pores and below which they become solid material.", 
			
			P = c("Probability Cutoff = the value above which the grid points become pores and below which they become solid material.")),

		"Linearly Correlated Model" = list(
			txt = "Consists of a rectangular grid in which each subsequent column 
				inherits the values of the previous column. Some of these values are 										
				regenerated with a probability given by the parameter Probability of Column 
				Change. The values in the first column and the regenerated values follow a 
				random uniform distribution. The probability cutoff specifies the value above 
				which the grid points become pores and below which they become solid material.",
		
			P = c("Probability Cutoff = the value above which the grid points become pores and below which they become solid material.",
				"Probability of Column Change = probability that the values in the subsequent columns are regenerated",
				"Type: Constant = a constant number of values are regenerated in each column;",
				"Type: Bernouly = the number of values which are regenerated varies in each column according
				to a Poisson process with λ = pChange.")),
		
		"Binary Correlated Model" = list(
			txt = "Consists of a rectangular grid in which the binary state of each 
				subsequent column is either the state of the previous column (open or closed) or 
				is inverted. During inversion: an open cell becomes closed and a closed cell 
				becomes open. The inversion is selected with a probability given by the parameter 
				Probability of Column Change. The values in the first column follow a random 
				uniform distribution. The probability cutoff specifies the value above which 
				the grid points become pores and below which they become solid material.",
		
			P = c("Probability Cutoff = the value above which the grid points become pores and below which they become solid material.",
				"Probability of Column Change = probability that the values in the subsequent colum is the state of the previous column or is inverted.",
				"Type: Constant = a constant number of values are regenerated in each column;",
				"Type: Bernouly = the number of values which are regenerated varies in each column according
				to a Poisson process with λ = pChange.")),
		
		"Details" = list(
			txt = "Implements 3 methods to perform an advanced analysis of a specific cluster. The first 
				method computes the length of the cluster. The 2nd one highlights the border/perimeter 
				of the cluster. The third method performs a min-cut operation to split a percolating 
				cluster into 2 non-percolating parts: the 2nd fragment is visualized in yellow; the 
				border around the cut-edges is marked in blue.", 
			
			P = c("Model type = which of the models to analyse; the instance of the model must be created first on the specific model page;",
				"Channel ID = ID of cluster to analyse; if there are percolating clusters, then only the 
				IDs of the percolating ones are listed;")),
		"Linear Channel" = list(
			txt = "The Linear channels process generates a specified number of linear channels separated 
				by straight walls. Each channel can be either patent or blocked by one or more blocks. 
				Neighbouring channels can communicate through pores through the channel walls.",
				
			P = c("Probability Cutoff = the value above which the grid points become pores and below which they become solid material.",
				"Pore probability = the average number of pores in each channel wall; the number 
				of pores varies randomly according to a Poisson process with parameter lambda = this value;",
				"Block probability = probability of blocks; the meaning differs slightly based on block type;",
				"Block type = specifies the type of blocks; there are 4 variants;",
				"Block type = Constant: a constant number of blocks in each channel;",
				"Block type = Poisson: the number of blocks varies in each channel according to a Poisson process;",
				"Block type = range [0, n]: the number of blocks varies in the range 0 to a maximum value given by n_Blocks;",
				"Block type = range [1, n]: the number of blocks varies in the range 1 to a maximum value given by n_{Blocks;",
				"Note: the parameter p_Block is reused to encode n_{Blocks in the last 2 variants."))
			)
		

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