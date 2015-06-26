library(leaflet)
library(RColorBrewer)
library(maps)

# source: https://jcheng.shinyapps.io/choropleth3/
shinyServer(function(input, output, session) {
	
  values <- reactiveValues(highlight = c())
  
  map <- createLeafletMap(session, "map")
	
  # Draw the given states, with or without highlighting
  drawStates <- function(stateNames, highlight = FALSE) {
  	
    states <- map("county",  plot=FALSE, fill=TRUE)
    map$addPolygon(I(states$y), I(states$x), I(states$names),
      I(lapply(states$names, function(x) {
        x <- strsplit(x, ":")[[1]][1]
      	tryCatch({
      		list(fillColor = colors[[x]])
      	}, 
      	# if the region doesn't match anything
      	error = function (err){
      		list(fillColor = "#000000")
      	})
        
      })),
      I(list(fill=TRUE, fillOpacity=0.7, 
        stroke=TRUE, opacity=1, color="white", weight=ifelse(highlight, 4, 1)
      ))
    )
  }

  # session$onFlushed is necessary to delay the drawing of the polygons until
  # after the map is created
  session$onFlushed(once=TRUE, function() {
    # Get shapes from the maps package
    drawStates(names(density))
  })
  
  # input$map_shape_mouseover gets updated a lot, even if the id doesn't change.
  # We don't want to update the polygons and stateInfo except when the id
  # changes, so use values$highlight to insulate the downstream reactives (as 
  # writing to values$highlight doesn't trigger reactivity unless the new value 
  # is different than the previous value).
  observe({
    values$highlight <- input$map_shape_mouseover$id
  })
	
	observe({
		
		file <- get_file(input$chooseInput)
		updateSelectInput(session, inputId="colSelect", choices = colnames(file), selected = input$colSelect)		
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
		#names(nums_col) <- name_col
#		density <<- c(nums_col)
#		drawStates(names(density))
#		drawStates(names(density))
#		print(x)
		
	#	if(x == "Interactive"){
			
	#	}
		
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