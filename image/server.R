library(shiny)
library(jpeg)
library(png)
library(ggplot2)
library(grid)
library(ggtern)
library(MASS)

shinyServer(function(input, output, session){
	
	#################### GLOBAL REACTIVE VALUES ####################
	values <- reactiveValues(
  	data = data.frame("value" = c(), "x" = c(), "y" = c()), 
  	index = NULL, 
  	num = NULL, 
		numRows = NULL)

	#################### OBSERVERS ####################

	# update values$data andupdate the record of number of rows when data changes
	observe({
		grid_data <- get_grid_file()
		isolate({
			values$data <- grid_data
			values$numRows <- sqrt(nrow(grid_data))
		})
	})
	
	# set values$num when numeric input is changed
	observe({
		values$num <- input$selectedValue
	})
	
	# set values$index of marker when clicked and update numeric input value
	observe({
		point <- get_nearPoints()
		isolate({
	    if(!is.null(point)){
	    	values$index <- as.numeric(rownames(point))
	    	updateNumericInput(session, "selectedX", value = point$x, max = tail(values$data$x, 1))
				updateNumericInput(session, "selectedY", value = point$y, max = tail(values$data$y, 1))
				updateNumericInput(session, "selectedValue", value = point$value)
	    }
		})
	})
	
	# calculate index from x and y coordinates
	find_index <- reactive({
		x <- input$selectedX
		y <- input$selectedY
		rows <- values$numRows
		tryCatch({
			z <- seq((x-1)*rows+1, x*rows)[[y]]
			values$index <- z
			updateNumericInput(session, 'selectedValue', value = values$data$value[[z]])
		}, error = function(err){
			updateNumericInput(session, 'selectedValue', value = "")
		})
	})
	
	output$xyCoordsError <- renderText({
		validate(need(!is.na(input$selectedValue), message="Please select valid x and y coordinates"))
	})
	
	observe({
		input$submitCoords
		isolate({
			if(!is.na(input$selectedX) && !is.na(input$selectedY)){
				find_index()
			}
		})
	})
	
	# click submit button to change value
	observe({
		input$submitValue
		isolate({
			if(!is.null(values$num) && !is.na(values$num) && values$num >= 0){
				values$data$value[values$index] <- values$num
			}
		})
	})
	
	# update input$contourSmoothness when values$numRows is changed
	observe({
		updateSliderInput(session, 'contourSmoothness', step = values$numRows+2, min = values$numRows+2)
	})
	
	# update layer show/hide options when display type changes
	observe({
		displayChange <- input$displayType 
		currentSelected <- isolate(input$layers)
		
		if(displayChange == 'square'){
			updateSelectInput(session, 'layers', 
				choices = c(
					"image" = 'showImage', 
	  			"grid lines" = 'showGrid', 
	  			"heatmap" = 'showHeatmap'), 
				selected = currentSelected)
		}
		if(displayChange == 'gaussian'){
			updateSelectInput(session, 'layers', 
				choices = c(
					"image" = 'showImage', 
	  			"grid lines" = 'showGrid', 
	  			"heatmap" = 'showHeatmap', 
					"contour lines" = 'showContour'), 
				selected = currentSelected)
		}
	})
	
	#################### GGPLOT HELPER FUNCTIONS ####################
	get_image_file <- reactive({
		if(input$imageSelect == 'imageExample'){
			readJPEG("example_input/jasper.jpg")
		}
		else if(!is.null(input$imageFile)){
			name <- input$imageFile$name
			extension <- tolower(substr(name, nchar(name)-3, nchar(name)))

			if(extension == ".jpg" || extension == "jpeg"){
				readJPEG(input$imageFile$datapath)
			}
			else if(extension == ".png"){
				readPNG(input$imageFile$datapath)
			}
			else{
				validate(txt = "Unfortunately the type of file you uploaded is not supported. Please upload a PNG or JPEG image file.")
			}
		}
		else{
			NULL
		}
	})
	
	get_grid_file <- reactive({
		
		# reset values$data if grid changes
		if(input$gridSelect == 'gridExample'){
			max <- input$numGridRows 
			newx <- unlist(lapply(1:max, function(x){rep(x, max)}))
			newy <- rep(seq(1, max), max)
			data.frame("value" = sample(x = c(0, 20), size = max*max, prob = c(0.99, 0.01), replace = TRUE), 
				"x" = newx, "y" = newy)
		}
		else if(!is.null(input$gridFile)){
			read.delim(input$gridFile$datapath)
			#read.delim("example_input/grid.txt")
		}
		else{
			max <- input$numGridRows
			newx <- unlist(lapply(1:max, function(x){rep(x, max)}))
			newy <- rep(seq(1, max), max)
			data.frame("value" = rep(0, max*max), "x" = newx, "y" = newy)
		}
	})
	
	# value of clicked point
	get_nearPoints <- reactive({
		point <- nearPoints(isolate(values$data), input$plot_click, maxpoints = 1, threshold = 1000)
		if(length(rownames(point))>0){
			point
		}
		else{
			NULL
		}
	})
	
	layer_selected <- function(name){
		if(length(grep(name, input$layers))>0){
			TRUE
		}
		else{
			FALSE
		}
	}
	
	get_background <- reactive({
		if(layer_selected("showImage") && !is.null(get_image_file())){
			if(input$stretchImage){
				g <- rasterGrob(get_image_file(), width=unit(1,"npc"), height=unit(1,"npc"), interpolate=TRUE)
			}
			else{
				g <- rasterGrob(get_image_file(), interpolate = TRUE)
			}
			annotation_custom(g, -Inf, Inf, -Inf, Inf)	
		}
	})

	get_theme <- reactive({
		theme_bw()
		#theme(panel.grid=element_blank())
		#theme(panel.grid.minor = element_line(color = "black"))
	})
	
	# number labels for axis
	get_breaks <- reactive({
		1:values$numRows
	})
	
	get_limits <- reactive({
		c(0.5, values$numRows+0.5)
	})
	
	get_bandwidth <- reactive({
		c(bandwidth.nrd(values$data$x)*input$gaussianRadius, bandwidth.nrd(values$data$y)*input$gaussianRadius)
	})

	# extend the data frame in each direction to connect the polygons at the edges of the grid
	add_padding <- function(x){
		
		max <-  values$numRows
		max1 <- max + 1
		row1 <- data.frame(x = 0:max1, y = 0, value = 0)
		row2 <- data.frame(x = 0:max1, y = max1, value = 0)
		col1 <- data.frame(x = 0, y = 1:max, value = 0)
		col2 <- data.frame(x = max1, y = 1:max, value = 0)
		
		rbind(x, rbind(row1,row2,col1,col2))
	}
	
	get_density <- reactive({
		
		# calculate weighted density, source: http://bit.ly/1JfZQYQ
		data <- values$data
		data <- add_padding(values$data)

		x <- data$x
		y <- data$y
		val <- data$value
		
		dens <- kde2d.weighted(x, y, val, n = input$contourSmoothness, h = get_bandwidth())
		
		# set NAs to 0
		dens$z[is.na(dens$z)] <- 0
		
		data.frame(expand.grid(x=dens$x, y=dens$y), z=as.vector(dens$z))
	})
	
	get_colours <- reactive({
		if(input$colourScheme == 'rainbow'){
			scale_fill_gradientn(colours = rev(rainbow(7)))
		}
		else if(input$colourScheme == 'custom'){
			scale_fill_gradient(low = input$lowColour, high = input$highColour)
		}
		else{
			scale_fill_gradientn(colours = rev(topo.colors(7)))
		}
	})
	
	get_selected_point <- reactive({
		if(!is.null(values$index)){
			x_val <- values$data$x[[values$index]]
			y_val <- values$data$y[[values$index]]
			geom_point(x = x_val, y = y_val, colour = "yellow", size = 4)
		}
	})
	
	get_plot <- reactive({

		plot1 <- 	ggplot(data = values$data, aes(x = x, y = y))  + 
			
			# prevent "no layers in plot" error
			geom_blank() +
			
			# add theme
			get_theme() +
			
			# add background 
			get_background() +
			
			# scale x and y axis values
			scale_x_continuous(limits = get_limits(), breaks=get_breaks(), expand = c(0, 0)) + 
			scale_y_continuous(limits = get_limits(), breaks=get_breaks(), expand = c(0, 0)) 
			
		
		if(input$displayType == 'square'){
			if(layer_selected("showHeatmap")){
				plot1 <- plot1 + geom_raster(aes(fill = value), alpha = input$fillOpacity, bins = input$numShades) + get_colours()
			}
		}
		
		else if(input$displayType == 'gaussian'){
			dfdens <- get_density()
			
			#dfdens <- add_extra(dfdens)
			
			# avoid contour/fill errors
			if(var(dfdens$z) != 0){
				#add fill
				if(layer_selected("showHeatmap")){
					plot1 <- plot1 + 
					stat_contour(aes(z = z,  fill=..level..), bins = input$numShades, 
						alpha = input$fillOpacity, data = dfdens, geom="polygon")  +
					get_colours() 
				}
					
				# add contour
				if(layer_selected("showContour")){
					plot1 <- plot1 + geom_contour(aes(z = z), data = dfdens, bins = input$numShades)
				}
			}
		}

		# add grid
		if(layer_selected("showGrid")){
			plot1 <- plot1 + 
				geom_vline(xintercept = 0.5:(values$numRows-0.5)) + 
				geom_hline(yintercept = 0.5:(values$numRows-0.5))
		}
		
		plot1
	})
	
	output$ggplotMap <- renderPlot({
		if(input$showSelectedPoint){
			get_plot() + get_selected_point()
		}
		else{
			get_plot()
		}
	}, width = reactive({input$plotWidth}), height = reactive({input$plotHeight}))
	
	#################### SIDEBAR HELPER FUNCTIONS ####################
	
	get_plot_download_name <- function(){
		paste0("plot.", input$downloadPlotFormat)
	}
	output$plotDownload <- downloadHandler(
		filename = reactive({get_plot_download_name()}),
		content = function(file){
			ggsave(file, get_plot(), width = input$plotWidth/72, height = input$plotHeight/72)
		}
	)
	
	output$tableDownload <- downloadHandler(
		filename = "table.txt", 
		content = function(file){
			write.table(values$data, file, quote = FALSE, sep = "\t")
		})
	#################### TABLE HELPER FUNCTIONS ####################
	output$table <- renderDataTable({
		values$data
	})

})