library(shiny)
library(leaflet)
library(jpeg)
library(ggplot2)
library(grid)
library(ggtern)

shinyServer(function(input, output, session){
	
	values <- reactiveValues(
  	data = data.frame("value" = c(), "x" = c(), "y" = c()), 
  	index = NULL, 
  	num = NULL, 
		dfdens = NULL)
		
	observe({
		max <- input$numGridRows
		newx <- unlist(lapply(1:max, function(x){rep(x, max)}))
		newy <- rep(seq(1, max), max)
		values$data <- data.frame("value" = rep(0,max), "x" = newx, "y" = newy)
	})
	get_background <- reactive({
		if(input$showImage){
			img <- readJPEG("jasper.jpg")
			g <- rasterGrob(img, interpolate=TRUE)
			# remove
			annotation_custom(g,1,input$numGridRows,1,input$numGridRows)	
		}
	})
	
	get_theme <- reactive({
		theme(panel.grid.minor = element_line(color = "black"))
	})
	
	get_breaks <- reactive({
		1:input$numGridRows
	})
	
	get_points <- reactive({
		# hollow square = 0, filled square = 15
		# hollow circle = 1, filled circle = 16
		if(input$showPoints){
			geom_point(size = 5, shape = as.numeric(input$pointType))
		}
	})

	get_density <- reactive({
		# calculate weighted density, source: http://bit.ly/1JfZQYQ
		data <- values$data
		dens <- kde2d.weighted(data$x, data$y, data$value)
		
		# set NaNs to 0
		dens$z[is.na(dens$z)] <- 0
		data.frame(expand.grid(x=dens$x, y=dens$y), z=as.vector(dens$z))
	})
	
	output$ggplotMap <- renderPlot({

		dfdens <- get_density()
		
		# add background and theme
		plot1 <- 	ggplot(data = values$data, aes(x = x, y = y))  + get_background() + get_theme()
		
		# scale x and y axis values
		plot1 <- plot1 + scale_x_continuous(breaks=get_breaks()) + scale_y_continuous(breaks=get_breaks())
	
		# avoid contour/fill errors
		if(var(dfdens$z) != 0){
			
			# add contour
			if(input$showContour){
				plot1 <- plot1 + geom_contour(aes(z = z), data = dfdens)
			}
			
			#add fill
			if(input$showFill){
				plot1 <- plot1 + 
					stat_contour(aes(z = z,  fill=..level.., alpha = ..level..), data = dfdens, geom="polygon")  +
					scale_fill_gradient(low = "yellow", high = "red")
			}
		}
		
		# add points
		plot1 <- plot1 + get_points() 
		
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
			paste("Click a point")
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