library(leaflet)
library(RColorBrewer)
library(maps)

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
				leafletProxy("map") %>% clearShapes()
				updateSelectInput(session, inputId="colSelect", choices = c(" " = " "))
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

	# update density information if file or column selection changes
	observe({
		values$density <- get_density()

		min <- floor(min(values$density, na.rm = TRUE))
		max <- ceiling(max(values$density, na.rm = TRUE))
		update_range <- reactive({updateSliderInput(session, inputId = "range", min = min, max = max)})
		update_range()
		
		# Breaks we'll use for coloring		
		minadd <- FALSE
		maxadd <- FALSE
		bins <- input$binNumber + 1

		if(min < input$range[[1]]){
			bins <- bins - 1
			minadd <- TRUE
		}
		if(max > input$range[[2]]){
			bins <- bins - 1
			maxadd <- TRUE
		}
		densityBreaks <- round(seq(input$range[[1]], input$range[[2]], length.out = bins), 0)
		if(minadd){
			densityBreaks <- c(min, densityBreaks)
		}
		if(maxadd){
			densityBreaks <- c(densityBreaks, max)
		}
		
		# Construct break ranges for displaying in the legend
		values$from <- head(densityBreaks, length(densityBreaks)-1)
		values$to <- tail(densityBreaks, length(densityBreaks)-1)

		# Eight colors for eight buckets
		values$palette <- colorRampPalette(c(input$lowColour, input$highColour))(input$binNumber)

		# Assign colors to states
		values$colours <- structure(
			values$palette[as.integer(cut(values$density, densityBreaks, include.lowest = TRUE, ordered = TRUE))], 
			names = names(values$density))
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

		if(!is.null(get_file())){
			values$density
			mapData <- get_map_data()
  		leafletProxy("map", data =  mapData) %>% 
				addPolygons(~x, ~y, ~names, 
					weight = input$lineSize, 
					color = ~fillColour, 
					opacity = 1, 
					fillColor = ~fillColour, 
					fillOpacity = input$fillOpacity)
			
		}

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
	
	################# Save Example File ################# 
	output$downloadExample <- downloadHandler(
		filename = "example.txt",
		content = function(file){
			write.table(isolate(get_file()), sep = "\t", quote = FALSE, file = file)
		}
	)
})