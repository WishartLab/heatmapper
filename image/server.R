library(shiny)
library(leaflet)
library(jpeg)
library(ggplot2)
library(grid)

shinyServer(function(input, output, session){
	
	values <- reactiveValues(
  	data = data.frame("value" = seq(1,9), "x" = c(1,1,1,2,2,2,3,3,3), "y" = c(1,2,3,1,2,3,1,2,3)), 
  	index = NULL, 
  	num = NULL)
		
	get_background <- reactive({
		img <- readJPEG("jasper.jpg")
		g <- rasterGrob(img, interpolate=TRUE)
		annotation_custom(g,1,3,1,3)	
	})
	
	output$ggplotMap <- renderPlot({
		plot1 <- ggplot(values$data, aes(x = x, y = y, color = value)) 
 		plot1 <- plot1 + get_background() + geom_point(size = 5) + theme_bw() + scale_color_gradientn(colours = rainbow(7))
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