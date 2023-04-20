	### Server
server = function(input, output) {
	output$txtTitleSimple = renderText("Percolation: Uniform Random Lattice")
	output$txtTitleLinear = renderText("Percolation: Linear Channels")
	output$txtTitleDetails = renderText("Detailed Analysis & Visualization")
	
	values = reactiveValues();
	values$m = NULL;
	values$r = NULL;
	values$ml = NULL;
	values$rl = NULL;



	imageGenerator = reactive({
		print("Se executa")
	
		m = rgrid.unif(c(input$heightSimple, input$widthSimple));
		values$m = m;
	})

	imageChannelGenerator = reactive({
		print("Se executa")
	
		m = rgrid.channel.poisson(input$heightLinear /3, input$widthLinear, 
			d = 1, type = input$blockTypeLinear, ppore = input$probPoreLinear, pBlock = input$probBlockLinear, val = 1.1);
		values$ml = m;
	})
	
	analyse.Channels = function(x) {
		ids = Channels(x);


		result = data.frame(
			Channels = c("Left", "Percolating", "Right"),  
			Number = c(length(ids$L), length(ids$P), length(ids$R)));
	}

	Channels = function(x) {
		nc = ncol(x);	# numar coloane
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
		result = list(L = id1, P = id2, R = id3);
		return (result);
	}
	

	analyse.Area = function(x) {
		count = table(x)
		idVal = as.integer(names(count))

		areas = data.frame(ID = idVal,
							Area = unclass(count))

		ids = Channels(x);
		

		countGroup = function(id) {
			isGroup = which(idVal %in% id);
			sum(count[isGroup]);
		}

		result = c(countGroup(-1), countGroup(0), 
				countGroup(ids$L), countGroup(ids$P), countGroup(ids$R));
		result = data.frame(
				Group = c("Blocks", "Free", "Left", "Percolating", "Right"),
				Area = result);


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

	output$Statistics = renderTable({
		if(is.null(values$r)){
			return();
		}
		statChannels = analyse.Channels(values$r);
	})

	output$Area = renderTable({
		if(is.null(values$r)){
			return();
		}
		areas = analyse.Area(values$r);
	})

### Details
	output$plotDetails = renderPlot({
		if(is.null(values$r)){
			return()
		}
		id = 1;
		if(input$typeDetails == "Channel Length"){
			plot.rs(length.path(values$r, id))
		}
		else {
			plot.surface(values$r, id)
		}
		 
		
	})


### Linear Channels
	output$channelsLinear = renderPlot({
		imageChannelGenerator()
		m = values$ml;
		p = input$probLinear;
		m = as.grid(m, p);
		r = flood.all(m);
		values$rl = r;
		# TODO global option
		plot.rs(expand.channel(r, 3));
	})
	
	output$StatisticsLinear = renderTable({
		if(is.null(values$rl)){
			return();
		}
		statChannels = analyse.Channels(values$rl);
	})

	output$AreaLinear = renderTable({
		if(is.null(values$rl)){
			return();
		}
		areas = analyse.Area(values$rl);
	})

}

# val.unique = unique(r[1])


### Linear Channels

