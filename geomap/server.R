library(leaflet)
library(RColorBrewer)
library(raster)
library(htmlwidgets)
library(gdata)
library(DT)
require(dendextend)
require(installr)
require(colorspace)
library(shiny)
library(d3heatmap)
library(gplots)
library(ggdendro)

source("../global_server.R")
source("../global_ui.R") # so we can see EXAMPLE_FILES

# Constants
dimensions_msg <- "Input data can have up to 50 data columns."

# reference: https://jcheng.shinyapps.io/choropleth3/
shinyServer(function(input, output, session) {
	
  values <- reactiveValues(
  	file = NULL,
  	inputFile = NULL, 
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
		values$inputFile <- NULL
		log_activity('geomap', 'clearFile')
	})
	
	observe({
		values$inputFile <- input$file
		log_activity('geomap', 'input$file')
	})
	
	# when a valid column is selected set values$density
	observe({
		if(input$colSelect != 0){
			log_activity('geomap', 'input$colSelect')
			values$density <- get_density()
			
			# update the slider with the new max and min from that column
			min <- floor(min(values$density, na.rm = TRUE))
			max <- ceiling(max(values$density, na.rm = TRUE))

			# set number of digit to which to round
			num_digits = 0
			if (max < 10) {
				num_digits = 2
			} else if (max < 100) {
				num_digits = 1
			}
			
			updateSliderInput(session, inputId = "range", min = min, max = max, value = c(min,max))
			isolate({
				update_colours(round(seq(min, max, length.out = input$binNumber+1), num_digits))
			})
			
			# When user chooses an example file, automatically set the
			# appropriate map.
			map_to_select = ''
			if (input$exampleFiles == EXAMPLE_FILES[1]) {
				map_to_select = 'data/CAN_1.rds'
			} else if (input$exampleFiles == EXAMPLE_FILES[2]) {
				map_to_select = 'data/CAN_1.rds'
			} else if (input$exampleFiles == EXAMPLE_FILES[3]) {
				map_to_select = 'data/USA_1.rds'
			}
			if (map_to_select != '') {
				updateSelectInput(session, inputId = "area", label = NULL, 
													choices = NULL, 
													selected = map_to_select)
			}
		}
	})
	
	# update colours when range of interest is changed or new file is selected
	observe({
		input$rangeSubmit
		input$binNumber
		input$lowColour
		input$highColour
		input$colourScheme
		log_activity('geomap', 'observe rangeSubmit binNumber lowColour highColour colourScheme')
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
  	log_activity('geomap', 'observe values$map')
  	values$map <- readRDS(input$area) 
	})
	
	# if values$density, values$colours, or values$map is changed update the polygons
	observe({
		log_activity('geomap', 'layer/tab changes')
		get_file()
		if(input$tabSelections == "Interactive"){
			if(is.null(values$density)){
				# remove old shapes when map is changed
	  		leafletProxy("map") %>% clearShapes() %>% get_tiles() %>% get_view() 
			}
			else{
				mapData <- get_map_data()
	  		leafletProxy("map", data =  mapData) %>% clearShapes() %>% get_shapes() %>% get_tiles() %>% get_view() 
			}
		}
  })
	
	# update legend when needed
	observe({
		get_file()
		if(!is.null(values$density) && input$tabSelections == "Interactive"){
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
	
	# use values$file to store file info instead of get_file() for editing table
	observe({
		values$file <- get_file()
	})
			
	# populate select input with region names for that map
	observe({
		updateSelectInput(session,'tableNames', choices = tolower(levels(values$map$NAME)), selected = " ")
	})
	
	# if selected row has name that matches a region name make that the default selection for select input
	observe({
		updateSelectInput(session, 'tableNames', selected = values$file[input$table_cell_clicked$row, 1])
	})
	
	# update name in values$file when user clicks submit
	observe({
		input$submitName
		isolate({
			if(!is.null(values$file)){
				values$file[input$table_cell_clicked$row, 1] <- input$tableNames
			}
		})
	})
	
	#################### HELPER FUNCTIONS ####################
	
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
		tryCatch({
			data_file <- values$file
			name_col <- tolower(data_file[[1]])
			nums_col <- get_nums_col(data_file, input$colSelect)
			names(nums_col) <- name_col
		
			return(nums_col)
		}, 
		error = function(err){
			validate(txt = paste(ERR_file_read, dimensions_msg))
		})
		
	})
	
	# read file if chooseInput is changed or file is uploaded
	get_file <- reactive({
		log_activity('geomap', 'begin get_file')
		
		tryCatch({
			if(input$chooseInput == 'example'){
				data_file <- read.table(input$exampleFiles, header = TRUE, sep="\t")
			}
			else{
			
				# reset column selection to empty
				if(is.null(values$inputFile$datapath)){
					updateSelectInput(session, inputId="colSelect", choices = c(" " = 0))
				}
			
				# return message if no file uploaded
				validate(need(values$inputFile$datapath, paste("Please upload a file.", dimensions_msg)))
			
				tryCatch({
					fileType <- tail(unlist(strsplit(x = values$inputFile$name, split = "[.]")), n=1)
				
					if(fileType == "xls" || fileType == "xlsx"){
						data_file <- read.xls(values$inputFile$datapath, sheet=1)
					}
					else if(fileType == "csv"){
						data_file <- read.csv(values$inputFile$datapath, header = TRUE)
					}
					else{
						data_file <- read.delim(values$inputFile$datapath, header = TRUE, sep="\t", row.names = NULL)
					}
					
					# remove "%" if they exist
					data_file[-1] <- lapply(data_file[-1], function(data_file){
						as.numeric(sub(pattern = "%", replacement = "", data_file))
					})
				}, 
				error = function(err){
					validate(txt = paste(ERR_file_read, dimensions_msg))
				})
			}
		
			# region names should be in lower case
			data_file[[1]] <- tolower(data_file[[1]])
		
			# update the column selection options when a new file is uploaded
			updateSelectInput(session, inputId="colSelect", choices = names(data_file)[-1])
			
			return(data_file)
		},
		finally = {
			log_activity('geomap', 'end get_file')
		})
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
		if(input$colourScheme == 'red/green'){
		  values$palette <- colorRampPalette(c("#FF0000", "#000000", "#33FF00"))(input$binNumber)
		}else if(input$colourScheme == 'blue/yellow'){
		  values$palette <- colorRampPalette(c("#0016DB", "#FFFFFF", "#FFFF00"))(input$binNumber)
		}else if(input$colourScheme == 'grayscale'){
		  values$palette <- colorRampPalette(c("#000000", "#bdbdbd", "#FFFFFF"))(input$binNumber)
		}else if(input$colourScheme == 'piyg'){
		  values$palette <- colorRampPalette(c("#C9438C", "#f7f7f7", "#7BC134"))(input$binNumber)
		}else if(input$colourScheme == 'rainbow'){
		  values$palette <- rainbow(input$binNumber)
		}else if(input$colourScheme == 'topo'){
		    values$palette <- topo.colors(input$binNumber)
		}else if(input$colourScheme == 'custom'){
		values$palette <- colorRampPalette(c(input$lowColour, input$highColour))(input$binNumber)
		}

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
			addTiles(m, options=tileOptions(detectRetina = input$detectRetina))
		}
		else{
			clearTiles(m)
		}
	}  

	################# OUTPUT FUNCTIONS ################# 
	output$table <- DT::renderDataTable({
		x <- datatable(rownames = FALSE,
			values$file, selection = 'single', 
			class = 'row-border strip hover'
    ) 
		# colour text red if name doesn't have a match
		if(!is.null(get_unmatched_names())){
			x <- formatStyle(x, 1, color = get_unmatched_names())
		}
		x
	})
	
	# return list of names in values$file that don't match any region name
	get_unmatched_names <- function(){
		x <- values$file[[1]][!values$file[[1]] %in% tolower(levels(values$map$NAME))]
		if(length(x)>0){
			styleEqual(x, rep("red", length(x)))
		}
		else{
			NULL
		}
	}
	
  # Dynamically render the box in the upper-right
  output$stateInfo <- renderUI({
  	log_activity('geomap', 'stateInfo')
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
	output$tableDownload <- downloadHandler(
		filename = "table.txt",
		content = function(file){
			log_activity('geomap', 'tableDownload')
			write.table(values$file, sep = "\t", quote = FALSE, file = file, row.names = FALSE)
		}
	)
	
	# save leaflet html page
	output$plotDownload <- downloadHandler(
		filename = function(){
			"geomap.html"
		},
		content = function(file) {
			log_activity('geomap', 'plotDownload')
			m <- get_shapes(leaflet(data = get_map_data())) %>% get_tiles()
			#m <- leaflet()
			saveWidget(m, file=file)
		}
	)

})