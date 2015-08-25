library(leaflet)
library(RColorBrewer)
library(raster)
library(htmlwidgets)

# reference: https://jcheng.shinyapps.io/choropleth3/
shinyServer(function(input, output, session) {
	
  values <- reactiveValues(
  	file = NULL,
  	highlight = NULL, 
  	density = NULL, 
  	colours = NULL, 
  	to = NULL, 
  	from = NULL, 
  	palette = NULL,
  	map = NULL)
	
	#################### OBSERVERS ####################
	observe({
		input$clearFile
		values$file <- NULL
	})
	
	observe({
		values$file <- input$file
	})
	
	# when a valid column is selected set values$density
	observe({
		if(input$colSelect != 0){
			values$density <- get_density()
			
			# update the slider with the new max and min from that column
			min <- floor(min(values$density, na.rm = TRUE))
			max <- ceiling(max(values$density, na.rm = TRUE))
			updateSliderInput(session, inputId = "range", min = min, max = max, value = c(min,max))
			isolate({
				update_colours(round(seq(min, max, length.out = input$binNumber+1), 0))
			})
		}
	})
	
	# update colours when range of interest is changed or new file is selected
	observe({
		input$rangeSubmit
		input$binNumber
			isolate({
				if(!is.null(values$density)){
				rangeMin <- input$range[[1]]
				rangeMax <- input$range[[2]]
				
				min <- floor(min(values$density, na.rm = TRUE))
				max <- ceiling(max(values$density, na.rm = TRUE))
				
				bins <- input$binNumber + 1
				
				# adjust selected range if difference between max and min is < # of bins
				if(rangeMax - rangeMin < bins){
					
					if(rangeMax - bins > min){
						rangeMin <- rangeMax - bins
					}
					else if(rangeMin + bins < max){
						rangeMax <- rangeMin + bins
					}
					# error handling for when range is < #bins, this should be improved upon
					else{
						rangeMin <- min
						rangeMax <- max
						bins <- max - min
					}
					updateSliderInput(session, inputId = "range", value = c(rangeMin,rangeMax))
				}
				
				densityBreaks <- get_breaks(rangeMin, rangeMax, min, max, bins)
				update_colours(densityBreaks)
			}
		})
	})
	
		# if input$area is updated change map
  observe({
  	values$map <- readRDS(input$area) 
	})
	
	# if values$density, values$colours, or values$map is changed update the polygons
	observe({
		get_file()
		
		if(is.null(values$density)){
			# remove old shapes when map is changed
  		leafletProxy("map") %>% clearShapes() %>% get_tiles() %>% get_view() 
		}
		else{
			mapData <- get_map_data()
  		leafletProxy("map", data =  mapData) %>% clearShapes() %>% get_shapes() %>% get_tiles() %>% get_view() 
		}

  })
	
	# update legend when needed
	observe({
		if(!is.null(values$density)){
			leafletProxy("map", data = isolate({get_map_data()})) %>% 	
				addLegend(layerId = "legendLayer", position = "bottomright", 
						opacity = 0.7, colors = values$palette, labels = paste(values$from, "-", values$to),
						title = input$legend)
		}
	})
	
	# get hover location over region
  observe({
    values$highlight <- input$map_shape_mouseover$id
  })
	
	
	#################### HELPER FUNCTIONS ####################

	# add spaces between distinct words if they don't exist
	fix_names <- function(x){
		state_pattern <- "(north|south|west|new|rhode)(\\S)"
		x <- sub(pattern = state_pattern, replacement = "\\1 \\2", x = x)
		return(x)
	}
	
	# return the values from the selected column
	get_nums_col <- function(data_file, col){
		nums_col <- data_file[[col]]
		if(is.null(nums_col)){
			nums_col <- data_file[[2]]
		}
		return(nums_col)
	}
	
	# assign density names and values based on the selected column
	get_density <- reactive({ 
		data_file <- get_file()
		name_col <- tolower(data_file[[1]])
		name_col <- fix_names(name_col)
		nums_col <- get_nums_col(data_file, input$colSelect)
		names(nums_col) <- name_col
		
		return(nums_col)
	})
	
	# read file if chooseInput is changed or file is uploaded
	get_file <- reactive({
		if(input$chooseInput == 'example'){
			data_file <- read.table(input$exampleFiles, header = TRUE, sep="\t")
		}
		else{
			
			# reset column selection to empty
			if(is.null(values$file$datapath)){
				updateSelectInput(session, inputId="colSelect", choices = c(" " = 0))
			}
			
			# return message if no file uploaded
			validate(need(values$file$datapath, "Please upload a file"))
			
			data_file <- read.delim(values$file$datapath, header = TRUE)
			
			# remove "%" if they exist
			data_file[-1] <- lapply(data_file[-1], function(data_file){
				as.numeric(sub(pattern = "%", replacement = "", data_file))
			})	 
		}
		
		# region names should be in lower case
		data_file[[1]] <- tolower(data_file[[1]])
		
		# update the column selection options when a new file is uploaded
		updateSelectInput(session, inputId="colSelect", choices = names(data_file)[-1])
		
		return(data_file)
	})
	
	# returns a list of break points given local min/max, global min/max, and # of bins
	get_breaks <- function(rangeMin, rangeMax, min, max, bins){
		minadd <- FALSE
		maxadd <- FALSE
	
		if(min < rangeMin){
			bins <- bins - 1
			minadd <- TRUE
		}
		if(max > rangeMax){
			bins <- bins - 1
			maxadd <- TRUE
		}
		
		densityBreaks <- round(seq(rangeMin, rangeMax, length.out = bins), 0)
		
		if(minadd){
			densityBreaks <- c(min, densityBreaks)
		}
		if(maxadd){
			densityBreaks <- c(densityBreaks, max)
		}
	
		densityBreaks
	}
	
	# update colours based on density breaks when value changes
	update_colours <- function(densityBreaks){

		# Construct break ranges for displaying in the legend
		values$from <- head(densityBreaks, length(densityBreaks)-1)
		values$to <- tail(densityBreaks, length(densityBreaks)-1)

		# Eight colors for eight buckets
		values$palette <- colorRampPalette(c(input$lowColour, input$highColour))(input$binNumber)

		# Assign colors to states
		values$colours <- structure(
			values$palette[as.integer(cut(values$density, densityBreaks, include.lowest = TRUE, ordered = TRUE))], 
			names = names(values$density)
		)
	}

	# The state names that come back from the maps package's state database has
	# state:qualifier format. This function strips off the qualifier.
	parseRegionName <- function(id) {
	  strsplit(id, ":")[[1]][1]
	}
	
	# add fillColour column to a map, depends on values$map and values$colours
	get_map_data <- reactive({
		
		mapData <- values$map
		
		i <- 1
  	fillArray <- rep("#000000", length(mapData$NAME))
	
  	for(region in mapData$NAME){
  		region <- tolower(parseRegionName(region))
  		
  		tryCatch({
  			fillArray[[i]] <- values$colours[[region]]
 				}, 
 				error = function(e){
 				})
  		i <- i + 1
  	}
		
  	mapData$fillColour <- fillArray
		
		return(mapData)
	})

  # default map
  output$map <- renderLeaflet({
  	get_file()
  	leaflet()
  })
	

	get_view <- function(m){
		
  	# calculate average lat and long for the selected area
  	lat <- c()
  	lon <- c()
  	
  	lapply(values$map@polygons, function(x){
  		lat <<- c(lat, x@labpt[[1]])
  		lon <<- c(lon, x@labpt[[2]])
  	})
		
		setView(m, mean(lat), mean(lon), zoom = 3) 
	}
	
	get_shapes <- function(m){
		addPolygons(m, layerId = ~NAME,
					weight = get_lines(), 
					color = "black", 
					opacity = 1, 
					fillColor = ~fillColour,
					fillOpacity = get_opacity()) 
	}
	
	get_lines <- reactive({
		if(layer_selected("showContours")){
			input$lineSize
		}
		else{
			0
		}
	})
	get_opacity <- reactive({
		if(layer_selected("showHeatmap")){
			input$fillOpacity
		}
		else{
			0
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
	
	get_tiles <- function(m){
		if(layer_selected("showTiles")){
			addTiles(m)
		}
		else{
			clearTiles(m)
		}
	}  

	################# OUTPUT FUNCTIONS ################# 
	output$table <- renderDataTable({
		get_file()
	})
  
  # Dynamically render the box in the upper-right
  output$stateInfo <- renderUI({
    if (is.null(values$highlight)) {
      return(tags$div("Hover over a region"))
    } 
  	else {
      # Get a properly formatted state name
      stateName <- values$highlight
      return(tags$div(
        tags$strong(stateName),
        tags$div(values$density[tolower(parseRegionName(stateName))], HTML(""))
      ))
    }
  })
	
	output$regionNames <- renderDataTable({
		data.frame("Regions" = levels(values$map$NAME))
	}, options = list(pageLength = 10))
	
	# save example file
	output$downloadExample <- downloadHandler(
		filename = "table.txt",
		content = function(file){
			write.table(isolate(get_file()), sep = "\t", quote = FALSE, file = file, row.names = FALSE)
		}
	)
	
	# save leaflet html page
	output$plotDownload <- downloadHandler(
		filename = function(){
			"choropleth.html"
		},
		content = function(file) {
			m <- get_shapes(leaflet(data = get_map_data())) %>% get_tiles()
			#m <- leaflet()
			saveWidget(m, file=file)
		}
	)

})