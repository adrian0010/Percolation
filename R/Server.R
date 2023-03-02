

### Server
server = function(input, output) {
	output$txtTitle = renderText("Percolation: Uniform Random Lattice");
	
	values = reactiveValues();
	values$m = NULL;
	values$r = NULL;

	
	
	### Basic Model
	output$PercolationBasic = renderPlot({
		if(is.null(values$m)) {
			# Initialize Lattice:
			m = rgrid.unif(c(100, 50));
			values$m = m;
		}
	m = values$m;
    p = input$probSimple;
	m = as.grid(m, p);
	r = flood.all(m);
	values$r = r;
	plot.rs(r);
	})
}

# val.unique = unique(r[1])
