library(shiny)
library(leaflet)
library(jpeg)
library(ggplot2)
library(grid)

shinyServer(function(input, output, session){
	
	values <- reactiveValues(
  	data = data.frame("value" = c(), "x" = c(), "y" = c()), 
  	index = NULL, 
  	num = NULL)
		
	observe({
		max <- input$numGridRows
		newx <- unlist(lapply(1:max, function(x){rep(x, max)}))
		newy <- rep(seq(1, max), max)
		values$data <- data.frame("value" = seq(1,max), "x" = newx, "y" = newy)
	})
	get_background <- reactive({
		img <- readJPEG("jasper.jpg")
		g <- rasterGrob(img, interpolate=TRUE)
		annotation_custom(g,1,input$numGridRows,1,input$numGridRows)	
	})
	
	get_theme <- reactive({
		theme(panel.grid.minor = element_line(color = "black"))
	})
	
	get_breaks <- reactive({
		1:input$numGridRows
	})
	
	get_points <- reactive({
		if(input$showPoints){
			geom_point(size = 5)
		}
	})
	
	output$ggplotMap <- renderPlot({
		plot1 <- ggplot(values$data, aes(x = x, y = y, color = value)) 
		plot1 <- plot1 + get_background() + scale_x_continuous(breaks=get_breaks()) + scale_y_continuous(breaks=get_breaks())
 		plot1 <- plot1 + get_points() + get_theme() + scale_color_gradientn(colours = rainbow(7))
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