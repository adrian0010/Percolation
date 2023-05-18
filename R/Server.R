	### Server
server = function(input, output, session) {
	output$txtTitleSimple = renderText("Percolation: Uniform Random Lattice")
	output$txtTitleLinear = renderText("Percolation: Linear Channels")
	output$txtTitleDetails = renderText("Detailed Analysis & Visualization")
	output$txtTitleLinearCorrelated = renderText("Percolation: Linearly Correlated Procces")
	
	values = reactiveValues();
	values$mSimple= NULL;
	values$rSimple= NULL;
	values$mLinear = NULL;
	values$rLinear = NULL;
	values$mLinearCorrelated = NULL;
	values$rLinearCorrelated = NULL;



	imageGeneratorSimple = reactive({
		input$newSimple;
		print("Se executa")
	
		m = rgrid.unif(c(input$heightSimple, input$widthSimple));
		values$mSimple= m;
	})

	imageGeneratorLinear = reactive({
		input$newLinear;
		print("Se executa")
	
		m = rgrid.channel.poisson(input$heightLinear /3, input$widthLinear, 
			d = 1, type = input$blockTypeLinear, ppore = input$probPoreLinear, pBlock = input$probBlockLinear, val = 1.1);
		values$mLinear = m;
	})

	imageGeneratorLinearCorrelated = reactive({
		input$newLinearCorrelated;
		dim = c(input$heightLinearCorrelated, input$widthLinearCorrelated);
		print(input$pChangeLinearCorrelated)
		m = rgrid.unifCor(dim,
			pChange = input$pChangeLinearCorrelated, type = input$typeLinearCorrelated);
		values$mLinearCorrelated = m;
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
		imageGeneratorSimple();
		m = values$mSimple;
		p = input$probSimple;
		m = as.grid(m, p);
		r = flood.all(m);
		values$rSimple= r;
		plot.rs(r);
	})

		### Analyse
	
	# StatisticsSimple
	observe({
		m = values$rSimple;
		if(is.null(m)){
			return();
		}
		# Flood from Right
		m = flood.rev(m);
		#
		statChannels = analyse.Channels(m);
		statAreas = analyse.Area(m);
		output$Statistics = renderTable(statChannels);
		output$Area = renderTable(statAreas);
	})
	
	Channels = function(x) {
		nc = ncol(x);	# numar coloane
		id1 = unique(x[, 1]);
		id1 = id1[id1 > 0];
		id3 = unique(x[, nc]);
		id3 = id3[id3 > 0];
		id2 = intersect(id1, id3);
		if(length(id2) > 0) {
			id1 = setdiff(id1, id2);
			id3 = setdiff(id3, id2);
		}
		result = list(L = id1, P = id2, R = id3);
		return (result);
	}

	### Details

	# Which Percolates or all channels
	idChannels = function(x) {
		id = which.percol(x)
		if(length(id) == 0) {
			id = unique(x[,1]);
			id = id[id > 0];
		}
		return(id);
	}

	# Update list of Percolating Channels
	observe({
		if(is.null(values$rSimple)){
			return()
		}
		ids = idChannels(values$rSimple)
		updateSelectInput(session, "idDetails",
			choices = ids,
			selected = head(ids, 1)
    	)
	})

	output$plotDetails = renderPlot({
		if(is.null(values$rSimple)){
			return()
		}
		id = input$idDetails;
		if(input$typeDetails == "Channel Length"){
			plot.rs(length.path(values$rSimple, id))
		}
		else {
			plot.surface(values$rSimple, id)
		}
	})


	### Linear Channels
	output$channelsLinear = renderPlot({
		imageGeneratorLinear()
		m = values$mLinear;
		p = input$probLinear;
		m = as.grid(m, p);
		r = flood.all(m);
		values$rLinear = r;
		# TODO global option
		plot.rs(expand.channel(r, 3));
	})
	
	output$StatisticsLinear = renderTable({
		if(is.null(values$rLinear)){
			return();
		}
		statChannels = analyse.Channels(values$rLinear);
	})

	output$AreaLinear = renderTable({
		if(is.null(values$rLinear)){
			return();
		}
		areas = analyse.Area(values$rLinear);
	})
	
	output$LengthLinear = renderTable({
		if(is.null(values$rLinear)){
			return()
		}
		length = length.channel.linear(values$rLinear)
	})

	
	### Linearly Correlated Process
	output$LinearCorrelated = renderPlot({
		imageGeneratorLinearCorrelated()
		m = values$mLinearCorrelated;
		p = input$probLinearCorrelated;
		m = as.grid(m, p);
		r = flood.all(m);
		values$rLinearCorrelated = r;
		plot.rs(r);
	})

	output$StatisticsLinearCorrelated = renderTable({
		if(is.null(values$rLinearCorrelated)){
			return();
		}
		statChannels = analyse.Channels(values$rLinearCorrelated);
	})

	output$AreaLinearCorrelated = renderTable({
		if(is.null(values$rLinearCorrelated)){
			return();
		}
		areas = analyse.Area(values$rLinearCorrelated);
	})


	

}

# val.unique = unique(r[1])



