

### Server
server = function(input, output) {
	output$txtTitleSimple = renderText("Percolation: Uniform Random Lattice");
	
	values = reactiveValues();
	values$m = NULL;
	values$r = NULL;



	imageGenerator = reactive({
		print("Se executa")
	
		m = rgrid.unif(c(input$heightSimple, input$widthSimple));
		values$m = m;
	})
	
	
	### Basic Model
	output$PercolationSimple = renderPlot({
		
	imageGenerator();
	m = values$m;
    p = input$probSimple;
	m = as.grid(m, p);
	r = flood.all(m);
	values$r = r;
	plot.rs(r);
	})
}

# val.unique = unique(r[1])
