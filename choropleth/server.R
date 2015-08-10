library(leaflet)
library(RColorBrewer)
library(raster)
library(htmlwidgets)

# reference: https://jcheng.shinyapps.io/choropleth3/
shinyServer(function(input, output, session) {
	
  values <- reactiveValues(
  	file = NULL,
  	highlight = c(), 
  	density = c(), 
  	colours = c(), 
  	to = c(), 
  	from = c(), 
  	palette = c(),
  	map = c())
	
	#################### FILE CLEAR OBSERVERS ####################
	observe({
		input$clearFile
		values$file <- NULL
	})
	
	observe({
		values$file <- input$file
	})
	
	# get the values from the selected column
	get_nums_col <- function(data_file, col){
		print(col)
		nums_col <- data_file[[col]]
		if(is.null(nums_col)){
			nums_col <- data_file[[2]]
		}
		return(nums_col)
	}
	
	# add spaces between distinct words if they don't exist
	fix_names <- function(x){
		state_pattern <- "(north|south|west|new|rhode)(\\S)"
		x <- sub(pattern = state_pattern, replacement = "\\1 \\2", x = x)
		return(x)
	}
	# assign density names and values based on the selected column
	get_density <- reactive({ 
		print("GET DENSITY")
		data_file <- get_file()
		name_col <- tolower(data_file[[1]])
		name_col <- fix_names(name_col)
		nums_col <- get_nums_col(data_file, input$colSelect)
		names(nums_col) <- name_col
		
		return(nums_col)
	})
	
	# read file if chooseInput is changed or file is uploaded
	get_file <- reactive({
		print("GET FILE")
		if(input$chooseInput == 'example'){
			#path <- "data/counties.rds"
			#data_file <- readRDS(path)
			data_file <- read.table(input$exampleFiles, header = TRUE, sep="\t")
		}
		else{
			if(is.null(values$file$datapath)){
				print("first leaflet")
				leafletProxy("map") %>% clearShapes()
				updateSelectInput(session, inputId="colSelect", choices = c(" " = 0))
			}
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

	observe({
		if(input$colSelect != 0){
			values$density <- get_density()
		}
	})
	
	# update density information if file or column selection changes
	observe({
		if(!is.null(values$density)){
			rangeMin <- input$range[[1]]
			rangeMax <- input$range[[2]]
			
			min <- floor(min(values$density, na.rm = TRUE))
			max <- ceiling(max(values$density, na.rm = TRUE))
	
			# update range slider if different col is selected
			if(rangeMin < min || rangeMax > max){
				updateSliderInput(session, inputId = "range", min = min, max = max, value = c(min,max))
				rangeMin <- min
				rangeMax <- max
			}
			
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
		
		print("get_map_data")
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
	
	observe({
		values$map
		print("map value updated")
	})

  # default map
  output$map <- renderLeaflet({
  	print("second leaflet")
  	leaflet()
  })
	
	# if input$area is updated change map
  observe({
  	values$map <- readRDS(input$area) 
  	
  	# calculate average lat and long for the selected area
  	lat <- c()
  	lon <- c()
  	
  	lapply(values$map@polygons, function(x){
  		lat <<- c(lat, x@labpt[[1]])
  		lon <<- c(lon, x@labpt[[2]])
  	})
  	
		lat <- mean(lat)
  	lon <- mean(lon)
  	
  	# remove old shapes when map is changed
  	leafletProxy("map") %>% clearShapes() %>% setView(lat, lon, zoom = 3) 
	})
	
	# if values$density, values$colours, or values$map is changed update the polygons
	observe({

		if(!is.null(values$density)){

			mapData <- get_map_data()
				
  		leafletProxy("map", data =  mapData) %>% clearShapes() %>% get_shapes()
				
		}

  })
	
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
			addTiles(m, options = tileOptions(noWrap = TRUE))
		}
		else{
			clearTiles(m)
		}
	}  
	# if values$map is updated or showTiles checkbox input is changed
	observe({
		values$map
		print("fifth leaflet")
		get_tiles(leafletProxy("map"))
	})

	# input$map_shape_mouseover gets updated a lot, even if the id doesn't change.
  # We don't want to update the polygons and stateInfo except when the id
  # changes, so use values$highlight to insulate the downstream reactives (as 
  # writing to values$highlight doesn't trigger reactivity unless the new value 
  # is different than the previous value).
  observe({
    values$highlight <- input$map_shape_mouseover$id
  })
	
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
	
	# render the legend
	output$legend <- renderUI({
		if(is.null(get_file())){
			return(NULL)
		}
		tags$table(
			tags$strong(input$legend), 
			mapply(function(from, to, color) {
				tags$tr(
					tags$td(tags$div(
						style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
					)),
					tags$td(from, "-", to)
				)
			}, values$from, values$to, values$palette, SIMPLIFY=FALSE))
	})
	
	output$regionNames <- renderDataTable({
		data.frame("Regions" = levels(values$map$NAME))
	}, options = list(pageLength = 10))
	
	################# Save Example File ################# 
	output$downloadExample <- downloadHandler(
		filename = "table.txt",
		content = function(file){
			write.table(isolate(get_file()), sep = "\t", quote = FALSE, file = file, row.names = FALSE)
		}
	)
	
	################# Save Leaflet HTML Page ################# 
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