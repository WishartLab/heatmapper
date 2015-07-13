library(shiny)
library(leaflet)
library(raster)
library(jpeg)

shinyServer(function(input, output, session){
	
	values <- reactiveValues(
  	data = data.frame("x" = c(1,1,1,2,2,2,3,3,3), "y" = c(1,2,3,1,2,3,1,2,3), "value" = seq(1,9)), 
  	index = NULL, 
  	num = NULL)
	
	output$map <- renderLeaflet({
		r <- raster("small.jpg")
		crs(r) <- CRS("+init=epsg:4326")
		leaflet() %>% addRasterImage(r) %>% setView(2,2,8) 
	})

	output$info <- renderUI({
		wellPanel(print_marker())
	})
	output$all <- renderTable({
		values$data
	})
	
	print_marker <- reactive({
		#updateNumericInput(session, "numInput", value = 10)
		if(!is.null(values$index)){
			x <- values$data$value[values$index]
			paste0("Current Value at Index ", values$index, ": ", x)
		}
		else{
			"Click a point"
		}
	})
	
	observe({
		values$num <- input$numInput 
		#print("set values$num")
		#print(values$num)
	})
	
	observe({
		values$index <- as.numeric(input$map_marker_click$id)
		updateNumericInput(session, "numInput", value = values$data$value[values$index])
		#print("set values$index")
		#print(values$index)
	})
	
	observe({
		input$submit
		values$data$value[isolate(values$index)] <- isolate(values$num)
	})
	
	observe({
		#print("leafletProxy")
		col <- colorRampPalette(c("red", "yellow"))(length(values$data$value))
		x <- leafletProxy("map", session, data = values$data) %>% clearMarkers() %>% 
			addCircleMarkers(lng = ~x, lat = ~y, color = col, layerId = rownames(values$data), 
				popup = paste("Current Value:", values$data$value))
		})
})