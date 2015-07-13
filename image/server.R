library(shiny)
library(leaflet)
library(raster)
library(jpeg)
library(googleVis)
library(ggplot2)
shinyServer(function(input, output, session){
	
	values <- reactiveValues(
  	data = data.frame("value" = seq(1,9), "x" = c(1,1,1,2,2,2,3,3,3), "y" = c(1,2,3,1,2,3,1,2,3)), 
  	index = NULL, 
  	num = NULL)
	
	
	output$ggplotMap <- renderPlot({
		require("grid")
		img <- readJPEG("small.jpg")
		g <- rasterGrob(img, interpolate=TRUE)

		mdat <- values$data
		plot1 <- ggplot(mdat, aes(x = x, y = y)) + annotation_custom(g) 
 		plot1 <- plot1 + geom_point(size = 5, color = "red")	 + theme_bw()
		plot1
	})
	output$map <- renderLeaflet({
		r <- raster("small.jpg")
		print(r)
		crs(r) <- CRS("+init=epsg:4326")
		leaflet() %>% addRasterImage(r) %>% setView(2,2,8) %>% removeControl("leaflet-control-zoom")
	})
	
	output$gvisMap <- renderGvis({
		#gvisComboChart(values$data)
		image <- as.raster(readJPEG("small.jpg"))
		gvisBubbleChart(values$data, options = list(backgroundColor = image))
		#gvisScatterChart(values$data)
	})

	output$info <- renderUI({
		wellPanel(print_marker())
	})
	output$all <- renderTable({
		values$data
	})
	
	print_marker <- reactive({
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