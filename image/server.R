library(shiny)
library(gdata)
library(jpeg)
library(png)
library(tiff)
library(ggplot2)
library(grid)
library(ggtern)
library(MASS)
library(reshape2)

source("../global_server.R")

# Constants
dimensions_msg <- "Input data can have up to 200 x and 200 y coordinates."

shinyServer(function(input, output, session){
	
	#################### GLOBAL REACTIVE VALUES ####################
	values <- reactiveValues(
  	data = data.frame("value" = c(), "x" = c(), "y" = c()), 
  	index = NULL, 
  	num = NULL, 
		numRows = NULL,
		imageFile = NULL,
		gridFile = NULL, 
		highlightPoint = NULL)

	#################### OBSERVERS ####################

	# update values$data and update the record of number of rows when data changes
	observe({
		grid_data <- get_grid_file()
		if(!is.null(grid_data) && !is.na(grid_data)){
			isolate({
				values$data <- grid_data
				values$numRows <- sqrt(nrow(grid_data))
			})
		}
	})
	
	# set values$num when numeric input is changed
	observe({
		values$num <- input$selectedValue
		log_activity('image', 'input$selectedValue')
	})
	
	observe({
		input$clearImage
		values$imageFile <- NULL
		log_activity('image', 'clearImage')
	})
	
	observe({
		input$clearFile
		values$gridFile <- NULL
		log_activity('image', 'clearFile')
	})
	
	observe({
		values$imageFile <- input$imageFile
		log_activity('image', 'input$imageFile')
	})
	
	observe({
		values$gridFile <- input$file
		log_activity('image', 'input$file')
	})

	
	# update numeric input value when plot is clicked
	observe({
		point <- get_nearPoints()
		if(!is.null(point)){
			updateNumericInput(session, "selectedX", value = point$x, max = tail(values$data$x, 1))
			updateNumericInput(session, "selectedY", value = point$y, max = tail(values$data$y, 1))	
		}
	})
	
	# check if coords are in the proper range
	validate_coords <- function(x, y, rows){
		valid <- TRUE
		
		if(is.na(x) || x < 1 || x > rows){
			updateNumericInput(session, 'selectedX', value = "")
			valid <- FALSE
		}
		if(is.na(y) || y < 1 || y > rows){
			updateNumericInput(session, 'selectedY', value = "")
			valid <- FALSE
		}
		
		# set selectedValue to NA and values$index to NULL if coords not valid
		if(!valid){
			updateNumericInput(session, 'selectedValue', value = "")
			values$index <- NULL
		}
		
		return(valid)
	}

	# calculate index from x and y coordinates
	find_index <- reactive({
		log_activity('image', 'find_index')
		
		x <- input$selectedX
		y <- input$selectedY
		rows <- values$numRows
		
		valid <- validate_coords(x, y, rows)
		
		# update values$index and numeric input for selectedValue if coords are valid
		if(valid){
			z <- seq((x-1)*rows+1, x*rows)[[y]]
			values$index <- z
			updateNumericInput(session, 'selectedValue', value = values$data$value[[z]])
		}
	})
		
	# update values$index when a new valid x and y coord is selected
	observe({
		find_index()
		
		# update highlighted point if values$index changes
		if(input$showSelectedPoint){
		  values$highlightPoint <- get_selected_point()
		}
		else{
			values$highlightPoint <- NULL
		}
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
		updateSliderInput(session, 'contourSmoothness', step = values$numRows, min = values$numRows)
	})

	# update layer show/hide options when display type changes
	observe({
		displayChange <- input$displayType 
		currentSelected <- isolate(input$layers)
		
		if(displayChange == 'square'){
			updateSelectInput(session, 'layers', 
				choices = c(
					"Image" = 'showImage', 
	  			"Grid Lines" = 'showGrid', 
	  			"Heatmap" = 'showHeatmap', 
					"Axis Labels" = 'showAxisLabels'), 
				selected = currentSelected)
		}
		if(displayChange == 'gaussian'){
			updateSelectInput(session, 'layers', 
				choices = c(
					"Image" = 'showImage', 
	  			"Grid Lines" = 'showGrid', 
	  			"Heatmap" = 'showHeatmap', 
					"Contour Lines" = 'showContour', 
					"Axis Labels" = 'showAxisLabels'), 
				selected = currentSelected)
		}
	})
	
	#################### FILE INPUT FUNCTIONS ####################
	
	# prints error message if file has not been uploaded
	selected_file_validate <- function(message = paste(ERR_file_upload, dimensions_msg)){
		validate(
			need(
				!is.null(values$imageFile) || input$imageSelect == 'imageExample' ||
				!is.null(values$gridFile) || input$gridSelect == 'fileExample', 
				message
			)
		)
	}
	
	# read and return background image file
	get_image_file <- reactive({
		log_activity('image', 'begin get_image_file')
		tryCatch({
			if(input$imageSelect == 'imageExample'){
				readJPEG("example_input/background.jpg")
			}
			else if(!is.null(values$imageFile)){
				name <- values$imageFile$name
				extension <- tolower(substr(name, nchar(name)-3, nchar(name)))

				if(extension == ".jpg" || extension == "jpeg"){
					readJPEG(values$imageFile$datapath)
				}
				else if(extension == ".png"){
					readPNG(values$imageFile$datapath)
				}
				else if(extension == ".tif" || extension == "tiff"){
					readTIFF(values$imageFile$datapath)
				}
				else{
					validate(txt = "Unfortunately the type of file you uploaded is not supported. Please upload a PNG or JPEG image file.")
				}
			}
			else{
				NULL
			}
		},
		finally = {
			log_activity('image', 'end get_image_file')
		})
	})
	
	# get column based on list of names
	get_column <- function(file, names_list){
		for(i in names_list){
			if(i %in% names(file)){
				return(file[[i]])
			}
		}
		NULL
	}
	
	# read and return grid file
	get_grid_file <- reactive({
		log_activity('image', 'begin get_grid_file')
		# reset values$data if grid changes
		if(input$gridSelect == 'fileExample'){
			if(input$exampleSelect == 'example'){
				ret <- read_grid_file("example.txt", "example_input/example.txt")
			}
			else{
				max <- input$numGridRows 
				newx <- unlist(lapply(1:max, function(x){rep(x, max)}))
				newy <- rep(seq(1, max), max)
				ret <- data.frame("value" = 0, "x" = newx, "y" = newy)
			}
		}
		else if(!is.null(values$gridFile)){
			ret <- read_grid_file(values$gridFile$name, values$gridFile$datapath)
		}
		else{
			max <- input$numGridRows
			newx <- unlist(lapply(1:max, function(x){rep(x, max)}))
			newy <- rep(seq(1, max), max)
			ret <- data.frame("value" = rep(0, max*max), "x" = newx, "y" = newy)
		}
		
		log_activity('image', 'end get_grid_file')
		return(ret)
	})
	
	
	read_grid_file <- function(filename, filepath){
		tryCatch({
			
			fileType <- tail(unlist(strsplit(x = filename, split = "[.]")), n=1)
			
			if(fileType == "xls" || fileType == "xlsx"){
				data_file <- read.xls(filepath, sheet=1, header = FALSE)
			}
			else if(fileType == "csv"){
				data_file <- read.csv(filepath, header = FALSE)
			}
			else{
				data_file <- read.delim(filepath, header = FALSE, sep="\t", row.names = NULL)
			}
			
			if (sum(grepl("^[-+]?([0-9]*[.])?[0-9]+([eE][+-]?[0-9]+)?$", lapply(data_file[1,], as.character), perl=TRUE)) < length(data_file[1,])) {
				# Not all cells in the first row are numbers, so we will assume that the first row is a header row.
				# We are expecting column headers for x, y, and value ('long format').
				
				# Re-read the file, but with header = TRUE
				if(fileType == "xls" || fileType == "xlsx"){
					data_file <- read.xls(filepath, sheet=1, header = TRUE)
				}
				else if(fileType == "csv"){
					data_file <- read.csv(filepath, header = TRUE)
				}
				else{
					data_file <- read.delim(filepath, header = TRUE, sep="\t", row.names = NULL)
				}
				
				# check alternative names
				x <- get_column(data_file, c("x", "X","col", "Col", "COL", "cols", "Cols", "COLS", "columns", "Columns", "COLUMNS"))
				y <- get_column(data_file, c("y", "Y", "row", "Row", "ROW", "rows", "Rows", "ROWS"))
				val <- get_column(data_file, c("value", "Value", "VALUE", "val", "Val", "VAL"))
				
				if(is.null(x) || is.null(y) || is.null(val)){
					# names couldn't be matched
					ret <- NA
				} else{
					# all names were matched
					ret <- data.frame(x = c(as.integer(x)), y = c(as.integer(y)), value = c(val))
				}
				
			} else {
				# First row contains all numeric values.
				# Treat data as matrix form ('wide format').
				
				colnames(data_file) <- NULL
				x <- as.matrix(data_file)
				
				# flip the data
				x <- x[nrow(x):1,]
				ret <- melt(x, varnames  = c("y","x"))
			}
		}, 
		error = function(err){
			ret <- validate(txt = ERR_file_read)
		})
		return(ret)
	}
	
	
	#################### KDE2D HELPER FUNCTIONS ####################
	
	# calculate bandwidth for kde2d given input$gaussianRadius setting
	get_bandwidth <- reactive({
		c(bandwidth.nrd(values$data$x), bandwidth.nrd(values$data$y))*input$gaussianRadius
	})

	# extend the data frame in each direction to connect the polygons at the edges of the grid
	add_padding <- function(x, n, max){
		
		oldRange <- seq(1, max, length = n)
		
		minSplit <- 0
		maxSplit <- max + 1
		z <- min(x$z)
		
		row1 <- data.frame(x = minSplit, y = oldRange, z = z)
		row2 <- data.frame(x = maxSplit, y = oldRange, z = z)
		col1 <- data.frame(x = oldRange, y = minSplit, z = z)
		col2 <- data.frame(x = oldRange, y = maxSplit, z = z)
		
		#newSplit <- seq(0, max + 1, length = n + 2)
		#minSplit <- newSplit[newSplit<1]
		#maxSplit <- newSplit[newSplit>max]
		#z <- 0
		#row1 <- data.frame(expand.grid(x = minSplit, y = oldRange), value = z)
		#row2 <- data.frame(expand.grid(x = maxSplit, y = oldRange), value = z)
		#col1 <- data.frame(expand.grid(x = oldRange, y = minSplit), value = z)
		#col2 <- data.frame(expand.grid(x = oldRange, y = maxSplit), value = z)
		
		rbind(x,row1,row2,col1,col2)
	}
	
	# kernel density estimation of values$data
	get_density <- reactive({
		
		# calculate weighted density, source: http://bit.ly/1JfZQYQ
		data <- values$data
		#data <- add_padding(data, input$contourSmoothness, values$numRows)
		
		x <- data$x
		y <- data$y
		val <- data$value
		
		dens <- kde2d.weighted(x, y, w = val, n = input$contourSmoothness, h = get_bandwidth())
		
		# set NAs to 0
		dens$z[is.na(dens$z)] <- 0
		
		# close polygons at corners
		add_padding(data.frame(expand.grid(x=dens$x, y=dens$y), z=as.vector(dens$z)), 
			input$contourSmoothness, values$numRows)
	})
	
	#################### GGPLOT HELPER FUNCTIONS ####################

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
	
	# see if a given layer name is shown or hidden by user
	layer_selected <- function(name){
		if(length(grep(name, input$layers))>0){
			TRUE
		}
		else{
			FALSE
		}
	}
	
	# get background image
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
	
	# return range for plot axis
	get_limits <- reactive({
		c(0.5, values$numRows+0.5)
	})
	
	# return colour scheme 
	get_colours <- reactive({
	  
	  if(input$colourScheme == 'red/green'){
	    scale_fill_gradientn(colours = colorRampPalette(c("#FF0000", "#000000", "#23B000"))(7),guide = "colourbar")
	  }else if(input$colourScheme == 'blue/yellow'){
	    scale_fill_gradientn(colours = colorRampPalette(c("#0016DB", "#FFFFFF", "#FFFF00"))(7),guide = "colourbar")
	  }else if(input$colourScheme == 'piyg'){
	    scale_fill_gradientn(colours = colorRampPalette(c("#C9438C", "#f7f7f7", "#7BC134"))(7),guide = "colourbar")
	  }else if(input$colourScheme == 'grayscale'){
	    scale_fill_gradientn(colours = colorRampPalette(c("#000000", "#bdbdbd", "#FFFFFF"))(7),guide = "colourbar")
	  }else if(input$colourScheme == 'rainbow'){
	    scale_fill_gradientn(colours = rev(rainbow(7)),guide = "colourbar")
	  }else if(input$colourScheme == 'topo'){
	    scale_fill_gradientn(colours = rev(topo.colors(7)), guide = "colourbar")
	  }else if(input$colourScheme == 'custom'){
	    scale_fill_gradient(low = input$lowColour, high = input$highColour,guide = "colourbar")
	  }
	  else if(input$colourScheme == 'rainbow'){
			scale_fill_gradientn(colours = rev(rainbow(7)),guide = "colourbar")
		}
		else if(input$colourScheme == 'custom'){
			scale_fill_gradient(low = input$lowColour, high = input$highColour,guide = "colourbar")
		}
		else{
			scale_fill_gradientn(colours = rev(topo.colors(7)),guide = "colourbar")
		}
})
	  
	
	# returns geom_point of selected point if valid selection is made
	get_selected_point <- function(){

		if(!is.null(values$index)){
			x_val <- values$data$x[[values$index]]
			y_val <- values$data$y[[values$index]]
			
			geom_point(x = x_val, y = y_val, fill = "yellow", size = 5, colour="black", pch=22)
		}
		else{
			NULL
		}	
	}
	
	get_plot <- reactive({
		log_activity('image', 'begin get_plot')
		tryCatch({
			plot1 <- 	ggplot(data = values$data, aes(x = x, y = y))  + 
			
				# prevent "no layers in plot" error
				geom_blank() +
			
				# add theme
				get_theme() +
			
				# add background 
				get_background() +
			
				# crop edges 
				coord_cartesian(xlim = get_limits(), ylim = get_limits())
			
			if(layer_selected("showAxisLabels")){
				# scale x and y axis values
				plot1 <- plot1 + 
					scale_x_continuous(breaks=get_breaks()) + 
					scale_y_continuous(breaks=get_breaks())
			}
			else{
				plot1 <- plot1 + 	
					theme(
						axis.line=element_blank(),
						axis.text.x=element_blank(),
						axis.text.y=element_blank(),
						axis.ticks=element_blank(),
						axis.title.x=element_blank(),
						axis.title.y=element_blank()
					) 
			}
		
			if(input$displayType == 'square'){
				if(layer_selected("showHeatmap")){
					plot1 <- plot1 + geom_raster(aes(fill = value), alpha = input$fillOpacity) + get_colours()
				}
			}
		
			else if(input$displayType == 'gaussian'){
				dfdens <- get_density()
				# avoid contour/fill errors
				if(var(dfdens$z) != 0){
					#add fill
					if(layer_selected("showHeatmap")){
						plot1 <- plot1 + 
						stat_contour(aes(z = z, fill=..level..), bins = input$binNumber, 
							alpha = input$fillOpacity, data = dfdens, geom="polygon") + 
						get_colours() 
					}
				
					# add contour
					if(layer_selected("showContour")){
						plot1 <- plot1 + geom_contour(aes(z = z), data = dfdens, bins = input$binNumber)
					}
				}
			}

			# add grid
			if(layer_selected("showGrid")){
				plot1 <- plot1 + 
					geom_vline(xintercept = 0.5:(values$numRows-0.5)) + 
					geom_hline(yintercept = 0.5:(values$numRows-0.5))
			}
			plot1 + theme(legend.title = element_text(colour="blue", size=10, 
																								face="bold"))
			plot1
		},
		finally = {
			log_activity('image', 'end get_plot')
		})
	})
	
	get_plot_download_name <- function(){
		paste0("plot.", input$downloadPlotFormat)
	}
		
	#################### OUTPUT FUNCTIONS ####################
	
	# error message for invalid coordinates
	output$xyCoordsError <- renderText({
		selected_file_validate("")
		validate(need(!is.na(input$selectedValue), message="Please select valid x and y coordinates"))
	})
	
	output$ggplotMap <- renderPlot({
		log_activity('image', 'begin output$ggplotMap')
		tryCatch({
			selected_file_validate()
			validate(need(!is.na(get_grid_file()),ERR_file_read))
			get_plot() +  values$highlightPoint
		},
		finally = {
			log_activity('image', 'end output$ggplotMap')
		})
	}, width = reactive({input$plotWidth}), height = reactive({input$plotHeight}))
	
	
	output$ggplotMapLegend <- renderPlot({
	  selected_file_validate()
	  validate(need(!is.na(get_grid_file()),ERR_file_read))
	  get_legend() + values$highlightPoint
	}, width = 20, height = 100)
	
	
	output$plotDownload <- downloadHandler(
		filename = reactive({get_plot_download_name()}),
		content = function(file){
			log_activity('image', 'plotDownload')

			if(input$downloadPlotFormat == "pdf"){
				ggsave(file, get_plot(), width = input$plotWidth/72, height = input$plotHeight/72)
			} else {
				ppi = as.numeric(input$downloadPlotResolution)
				widthInches = input$plotWidth/72
				heightInches = input$plotHeight/72
				
				ggsave(file, get_plot(), width = widthInches, height = heightInches, units = "in", dpi=ppi, type="cairo")
			}
		}
	)
	
	output$tableDownload <- downloadHandler(
		filename = reactive({paste0("table.", input$downloadTableFormat)}),
		content = function(file){
			log_activity('image', 'tableDownload')
			if(input$downloadTableFormat == "csv"){
				write.csv(values$data, quote = FALSE, file = file, row.names = FALSE)
			}
			else{
				write.table(values$data, sep = "\t", quote = FALSE, file = file, row.names = FALSE)
			}
		}
	)
	
	output$table <- renderDataTable({
		log_activity('image', 'renderDataTable')
		selected_file_validate()
		data.frame("X" = values$data$x, "Y" = values$data$y, "Value" = values$data$value)
	})

})