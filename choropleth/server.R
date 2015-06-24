library(shiny)
library(maps)
library(mapproj)
source("helpers.R")

library(leaflet)
library(RColorBrewer)

shinyServer(function(input, output, session){

	
	values <- reactiveValues(highlight = c())
  
  map <- createLeafletMap(session, "map")
  
  # Draw the given states, with or without highlighting
  drawStates <- function(stateNames, highlight = FALSE) {
    states <- map("state",  plot=FALSE, fill=TRUE)
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
	
	
	
	
	
	
	
	#### DISCRETE MAP ####
	get_file <- function(){
		
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

	output$plot <- renderPlot({
		
		counties <- get_file()
		
		if(input$legend == ''){
			legend <- paste("%", input$colSelect)
		}
		else{
			legend <-input$legend
		}
		
		updateSelectInput(session, inputId="colSelect", choices = colnames(counties[-1]), selected = input$colSelect)
		
		validate(need(input$colSelect, "Please select a column to use"))
		
		percent_map(
			area = input$area,
			var = counties[,input$colSelect], 
			lowColour =  input$lowColour, 
			highColour = input$highColour,
			legend.title = legend, 
			min = input$range[1], 
			max = input$range[2] 
			)
		
	})
	
	output$table <- renderDataTable({
		get_file()
	})	
	
})