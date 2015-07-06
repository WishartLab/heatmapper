library(leaflet)
library(RColorBrewer)
library(maps)

# reference: https://jcheng.shinyapps.io/choropleth3/
shinyServer(function(input, output, session) {

  values <- reactiveValues(
  	highlight = c(), 
  	density = c(), 
  	colours = c(), 
  	to = c(), 
  	from = c(), 
  	palette = c(),
  	map = c())
	
	# update the column selection options when a new file is uploaded
	observe({
		data_file <- get_file()
		updateSelectInput(session, inputId="colSelect", choices = names(data_file)[-1])
	})
	
	# get the values from the selected column
	get_nums_col <- function(data_file){
		nums_col <- data_file[[input$colSelect]]
		if(is.null(nums_col)){
			nums_col <- data_file[[2]]
		}
		return(nums_col)
	}
	
	# add spaces between distinct words if they don't exist
	fix_names <- function(x){
		state_pattern <- "(north|south|west|district|new|rhode)(\\S)"
		x <- sub(pattern = state_pattern, replacement = "\\1 \\2", x = x)
		return(x)
	}
	# assign density names and values based on the selected column
	get_density <- reactive({ 
		data_file <- get_file()
		print("GET_DENSITY")
		name_col <- tolower(data_file[[1]])
		name_col <- fix_names(name_col)
		nums_col <- get_nums_col(data_file)
		names(nums_col) <- name_col
		return(nums_col)
	})
	
	# read file if chooseInput is changed or file is uploaded
	get_file <- reactive({
		if(input$chooseInput == 'example'){
			path <- "data/counties.rds"
			data_file <- readRDS(path)
			#data_file <- read.table("data/statetest2.txt", header = TRUE, sep="\t")
		}
		else{
			#path <- "data/counties.rds"
			#data_file <- readRDS(path)
			#data_file <- read.table("data/statetest.txt", header = TRUE, sep="\t")
			validate(need(input$file$datapath, "Please upload a file"))
			data_file <- read.delim(input$file$datapath, header = TRUE)
			
			# remove "%" if they exist
			data_file[-1] <- lapply(data_file[-1], function(data_file){
				as.numeric(sub(pattern = "%", replacement = "", data_file))
			})	 
		}
		
		# region names should be in lower case
		data_file[[1]] <- tolower(data_file[[1]])
		return(data_file)
	})

	# update density information if file or column selection changes
	observe({
		values$density <- get_density()
		
		# Breaks we'll use for coloring
		densityBreaks <- seq(
			floor(min(values$density, na.rm = TRUE)), 
			ceiling(max(values$density, na.rm = TRUE)), 
			length.out = input$binNumber + 1)
	
		# Construct break ranges for displaying in the legend
		values$from <- head(densityBreaks, length(densityBreaks)-1)
		values$to <- tail(densityBreaks, length(densityBreaks)-1)
	
		# Eight colors for eight buckets
		values$palette <- colorRampPalette(c(input$lowColour, input$highColour))(input$binNumber)
		
		# Assign colors to states
		values$colours <- structure(
			values$palette[cut(values$density, densityBreaks)], 
			names = tolower(names(values$density)))
	})

	# The state names that come back from the maps package's state database has
	# state:qualifier format. This function strips off the qualifier.
	parseRegionName <- function(id) {
	  strsplit(id, ":")[[1]][1]
	}
	
	# add fillColour column to a map
	get_map_data <- reactive({
		mapData <- values$map
		i <- 1
  	fillArray <- rep("#000000", length(mapData$names))
		
  	for(region in mapData$names){
  		region <- parseRegionName(region)
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
	
	# remove NA values from x and y columns
	prepare_fit_bounds <- function(map){
		map$x <- na.omit(map$x)
  	map$y <- na.omit(map$y)
		return(map)
	}
	
  # default map
  output$map <- renderLeaflet({
  	map <- map(input$area, plot=FALSE, fill=TRUE)
  	map <- prepare_fit_bounds(map)
    leaflet(map) %>% fitBounds(~min(x), ~min(y), ~max(x), ~max(y))
  })
	
	# if input$area is updated change map
  observe({
  	values$map <- map(input$area, plot=FALSE, fill=TRUE)
  	map <- prepare_fit_bounds(values$map)
  	leafletProxy("map") %>% 
  		clearShapes() %>%
  		fitBounds(min(map$x), min(map$y), max(map$x), max(map$y))
	})
	
	# if values$density is changed update the colors
	observe({
		values$density
		mapData <- get_map_data()
  	leafletProxy("map", data =  mapData) %>% 
			addPolygons(~x, ~y, ~names, 
				weight = input$lineSize, 
				color = ~fillColour, 
				opacity = 1, 
				fillColor = ~fillColour, 
				fillOpacity = input$fillOpacity)
  })
	
	# if values$map is updated or showTiles checkbox input is changed
	observe({
		values$map
		if(input$showTiles){
			leafletProxy("map") %>% addTiles()
		}
		else{
			leafletProxy("map") %>% clearTiles()
		}
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
      stateName <- names(values$density)[parseRegionName(values$highlight) == tolower(names(values$density))]
      return(tags$div(
        tags$strong(stateName),
        tags$div(values$density[stateName], HTML(""))
      ))
    }
  })
	
	# render the legend
	output$legend <- renderUI({
		tags$table(
			mapply(function(from, to, color) {
				tags$tr(
					tags$td(tags$div(
						style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
					)),
					tags$td(from, "-", to)
				)
			}, values$from, values$to, values$palette, SIMPLIFY=FALSE))
	})
})