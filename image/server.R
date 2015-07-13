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
		print("set values$num")
		print(values$num)
	})
	
	observe({
		point <- nearPoints(values$data, input$plot_click, addDist = TRUE)
    if(length(rownames(point))>0){
    	values$index <- as.numeric(rownames(point))
			updateNumericInput(session, "numInput", value = values$data$value[values$index])
    	print("set values$index")
			print(values$index)
    }
	})
	
	observe({
		input$submit
		isolate(values$data$value[values$index] <- values$num)
	})

})