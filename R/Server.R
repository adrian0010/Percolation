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
	

	### Details


	# Update list of Percolating Channels
	observe({
		if(is.null(values$rSimple)){
			return()
		}
		ids = which.percolates.orAny(values$rSimple)
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

	observe({
		m = values$rLinearCorrelated;
		if(is.null(m)){
			return();
		}
		# Flood from Right
		m = flood.rev(m);
		#
		statChannels = analyse.Channels(m);
		statAreas = analyse.Area(m);
		output$StatisticsLinearCorrelated = renderTable(statChannels);
		output$AreaLinearCorrelated = renderTable(statAreas);
	})



	

}

# val.unique = unique(r[1])



