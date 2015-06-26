library(leaflet)
library(RColorBrewer)
library(maps)

# source: https://jcheng.shinyapps.io/choropleth3/
shinyServer(function(input, output, session) {
	

	get_density <- function(){
		data_file <- get_file()
		name_col <- data_file[[1]]
		nums_col <- data_file[[2]]
		names(nums_col) <- name_col
		density <- c(nums_col)
		return(density)
	}
	
	get_file <- function(){
		
		data_file <- read.table("data/statetest2.txt", header = TRUE, sep="\t")
		data_file[,1] <- tolower(data_file[,1])

		return(data_file)
		
		if(input$chooseInput == 'example'){
			path <- "data/counties.rds"
			counties <- readRDS(path)
		}
		else{
		#	validate(need(input$file$datapath, "Please upload a file"))
		#	counties <- read.delim(input$file$datapath, header = TRUE)
			counties <- read.table("data/statetest2.txt", header = TRUE, sep="\t")
			counties[,1] <- tolower(counties[,1])
		}

		return(counties)
	}
	
	density <- get_density()
	

	# Breaks we'll use for coloring
	densityBreaks <- round(seq(min(density, na.rm = TRUE), max(density, na.rm = TRUE), length.out = 9), 0)

	# Construct break ranges for displaying in the legend
	densityRanges <- data.frame(
	  from = head(densityBreaks, length(densityBreaks)-1),
	  to = tail(densityBreaks, length(densityBreaks)-1)
	)

	# Eight colors for eight buckets
	palette <- colorRampPalette(c("#FFEDA0", "#800026"))(8)
	#c("#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C",
   #          "#FC4E2A", "#E31A1C", "#BD0026", "#800026")
	# Assign colors to states
	colors <- structure(
	  palette[cut(density, densityBreaks)],
	  names = tolower(names(density))
	)

	# The state names that come back from the maps package's state database has
	# state:qualifier format. This function strips off the qualifier.
	getStateName <- function(id) {
	  strsplit(id, ":")[[1]][1]
	}
	
  values <- reactiveValues(highlight = c())
  
  #map <- createLeafletMap(session, "map")
	
	get_map <- function(){
		return(map("county",  plot=FALSE, fill=TRUE))
	}
	

  # Draw the given states, with or without highlighting
  output$map <- renderLeaflet({
  	statesData <- map("state",  plot=FALSE, fill=TRUE)
  
  	
  	get_style <- function(){
  		i <- 1
  		sarray <- rep("#000000", length(statesData$names))
  		print(sarray)
  		for(s in statesData$names){
  			s <- strsplit(s, ":")[[1]][1]
  			tryCatch({
  				sarray[[i]] <- colors[[s]]
  				}, 
  				error = function(e){
  					print(s)
  				})
  			i <- i + 1
  		}
  		return(sarray)
  	}
  	statesData$fillColour <- get_style()
  	statesData$style = list(
  		weight = 0.5,
		  color = "#000000",
		  opacity = 1,
		  fillOpacity = 0.8
		)

  	
    leaflet() %>% addTiles() %>% addPolygons(statesData$x, statesData$y, statesData$names,weight = 0.5, color = "#000000", opacity = 1,
    	fillColor = statesData$fillColour, fillOpacity = 0.8)})
  	#	I(states$y), I(states$x), I(states$names),
     # I(lapply(states$names, function(x) {
      #  x <- strsplit(x, ":")[[1]][1]
      #	tryCatch({
      #		list(fillColor = colors[[x]])
      #	}, 
      	# if the region doesn't match anything
      #	error = function (err){
      #		list(fillColor = "#000000")
      #	})
        
      #})))
 # })

  # session$onFlushed is necessary to delay the drawing of the polygons until
  # after the map is created
  session$onFlushed(once=TRUE, function() {
    # Get shapes from the maps package
    #drawStates(names(density))
  })
  
  # input$map_shape_mouseover gets updated a lot, even if the id doesn't change.
  # We don't want to update the polygons and stateInfo except when the id
  # changes, so use values$highlight to insulate the downstream reactives (as 
  # writing to values$highlight doesn't trigger reactivity unless the new value 
  # is different than the previous value).
  observe({
    values$highlight <- input$map_shape_mouseover$id
  })
	
	testing <- reactive({
		file <- get_file(input$chooseInput)
		updateSelectInput(session, inputId="colSelect", choices = colnames(file)[-1], selected = input$colSelect)		
		name_col <- file[[1]]
		#print(name_col)
		if(input$colSelect != "a"){
			nums_col <- file[[input$colSelect]]
		}
		else{
			nums_col <- file[[2]]
		}
		print(head(name_col, 1))
		print(head(nums_col,1))
		if(!is.null(nums_col)){
			names(nums_col) <- name_col
			density <<- c(nums_col)
			densityBreaks <<- round(seq(min(density, na.rm = TRUE), max(density, na.rm = TRUE), length.out = 9), 0)
		}
	})
  
  # Dynamically render the box in the upper-right
  output$stateInfo <- renderUI({
    if (is.null(values$highlight)) {
      return(tags$div("Hover over a state"))
    } else {
      # Get a properly formatted state name
      stateName <- names(density)[getStateName(values$highlight) == tolower(names(density))]
      return(tags$div(
        tags$strong(stateName),
        tags$div(density[stateName], HTML(""))
      ))
    }
  })
})