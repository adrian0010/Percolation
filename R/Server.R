

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
	
	analyseChannels = function(x) {
		# numar coloane
		nc = ncol(x);
		id1 = unique(x[,1]);
		id1 = id1[id1 > 0];
		id2 = unique(x[,nc]);
		id2 = id2[id2 > 0];
		dim = dim(x);
		x = rev(x);
		x = array(x, dim);
		x = flood.all(x, id.start = max(id1)+1);
		id3 = unique(x[,1]);
		id3 = id3[id3 > 0];
		if(length(id2) > 0){
			id1 = setdiff(id1, id2);
			id3 = setdiff(id3, id2);
		}
		print(id1);
		print(id2);
		print(id3);
		result = c(length(id1), length(id2), length(id3));
		return(result);
	}
	
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

	output$Statistics =renderTable({
		statChannels = analyseChannels(values$r);
	})
}

# val.unique = unique(r[1])
