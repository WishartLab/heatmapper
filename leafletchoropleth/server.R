library(leaflet)
library(RColorBrewer)
library(maps)

# source: https://jcheng.shinyapps.io/choropleth3/
shinyServer(function(input, output, session) {
  
	values <- reactiveValues(highlight = c())
	
	density <- reactive({
		data_file <- get_file()
		name_col <- data_file[[1]]
		nums_col <- data_file[[2]]
		names(nums_col) <- name_col
		density <- c(nums_col)
		return(density)
	})
	
	get_spec <- function(x){
		data_file <- get_file()
		name_col <- data_file[[1]]
		nums_col <- data_file[[2]]
		names(nums_col) <- name_col
		density2 <- c(nums_col)
		print(grep(pattern = x, x = name_col))
		return(density2[x])
	}
	
	get_file <- observe({
		if(input$chooseInput == 'example'){
			path <- "data/counties.rds"
			counties <- readRDS(path)
		}
		else{
			#validate(need(input$file$datapath, "Please upload a file"))
			#counties <- read.delim(input$file$datapath, header = TRUE)
			counties <- read.table("data/statetest2.txt", header = TRUE, sep="\t")
			counties[,1] <- tolower(counties[,1])
		}
		return(counties)
	})
	
	#density <- get_density()
all <- reactive({
	# Breaks we'll use for coloring
	densityBreaks <- round(seq(min(density, na.rm = TRUE), max(density, na.rm = TRUE), length.out = 9), 0) 

	# Construct break ranges for displaying in the legend
	densityRanges <- data.frame(
	  from = head(densityBreaks, length(densityBreaks)-1),
	  to = tail(densityBreaks, length(densityBreaks)-1)
	)

	# Eight colors for eight buckets
	palette <- colorRampPalette(c("#FFEDA0", "#800026"))(8)
		
	# Assign colors to states
	colors <- structure(
	  palette[cut(density, densityBreaks)],
	  names = tolower(names(density))
	)
	})

	# The state names that come back from the maps package's state database has
	# state:qualifier format. This function strips off the qualifier.
	getStateName <- function(id) {
	  strsplit(id, ":")[[1]][1]
	}

	get_map <- function(){
		return(map("county",  plot=FALSE, fill=TRUE))
	}
	

  # Draw the given states, with or without highlighting
  output$map <- renderLeaflet({
  	statesData <- map("state",  plot=FALSE, fill=TRUE)
  	#statesData <- get_map()
  	
  	get_style <- function(){
  		i <- 1
  		sarray <- rep("#000000", length(statesData$names))
  		
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

  	
    leaflet() %>% addTiles() %>% 
  	addPolygons(statesData$x, statesData$y, statesData$names,weight = 1, color = "#000000", opacity = 1,
    	fillColor = statesData$fillColour, fillOpacity = 0.8)})
  
  # input$map_shape_mouseover gets updated a lot, even if the id doesn't change.
  # We don't want to update the polygons and stateInfo except when the id
  # changes, so use values$highlight to insulate the downstream reactives (as 
  # writing to values$highlight doesn't trigger reactivity unless the new value 
  # is different than the previous value).
  observe({
    values$highlight <- input$map_shape_mouseover$id
  })
  
  # Dynamically render the box in the upper-right
  output$stateInfo <- renderUI({
    if (is.null(values$highlight)) {
      return(tags$div("Hover over a state"))
    } 
  	else {
      # Get a properly formatted state name
      stateName <- getStateName(values$highlight)
      return(tags$div(
        tags$strong(stateName),
        tags$div(get_spec(stateName), HTML(""))
      ))
    }
  })
})