library(shiny)
library(jpeg)
library(png)
library(ggplot2)
library(grid)
library(ggtern)

shinyServer(function(input, output, session){
	
	#################### GLOBAL REACTIVE VALUES ####################
	values <- reactiveValues(
  	data = data.frame("value" = c(), "x" = c(), "y" = c()), 
  	index = NULL, 
  	num = NULL)

	#################### OBSERVERS ####################
	# reset values$data if grid changes
	observe({
		max <- input$numGridRows
		newx <- unlist(lapply(1:max, function(x){rep(x, max)}))
		newy <- rep(seq(1, max), max)
		values$data <- data.frame("value" = rep(0,max), "x" = newx, "y" = newy)
	})
	
	# set values$num when numeric input is changed
	observe({
		values$num <- input$numInput
	})
	
	# set values$index of marker when clicked and update numeric input value
	observe({
		point <- get_nearPoints()
    if(!is.null(point)){
    	values$index <- as.numeric(rownames(point))
			updateNumericInput(session, "numInput", value = values$data$value[values$index])
    }
	})
	
	# click submit button to change value
	observe({
		input$submit
		isolate({
			if(!is.null(values$num) && !is.na(values$num)){
				values$data$value[values$index] <- values$num
			}
		})
	})
	
	#################### GGPLOT HELPER FUNCTIONS ####################
	get_file <- reactive({
		if(input$chooseInput == "example"){
			readJPEG("example_input/jasper.jpg")
		}
		else if(!is.null(input$imageFile)){

			name <- input$imageFile$name
			extension <- tolower(substr(name, nchar(name)-3, nchar(name)))

			if(extension == ".jpg" || extension == "jpeg"){
				readJPEG(input$imageFile$datapath)
			}
			else if(extension == ".png"){
				readPNG(input$imageFile$datapath)
			}
			else{
				validate(txt = "Unfortunately the type of file you uploaded is not supported. Please upload a PNG or JPEG image file.")
			}
		}
		else{
			NULL
		}
	})
	
	# value of clicked point
	get_nearPoints <- reactive({
		point <- nearPoints(values$data, input$plot_click, maxpoints = 1)
		if(length(rownames(point))>0){
			point
		}
		else{
			NULL
		}
	})
	
	get_background <- reactive({
		if(input$showImage && !is.null(get_file())){
			if(input$stretchImage){
				g <- rasterGrob(get_file(), width=unit(1,"npc"), height=unit(1,"npc"), interpolate=TRUE)
			}
			else{
				g <- rasterGrob(get_file(), interpolate = TRUE)
			}
			annotation_custom(g, -Inf, Inf, -Inf, Inf)	
		}
	})
	
	get_theme <- reactive({
		theme_bw()
		#theme(panel.grid.minor = element_line(color = "black"))
	})
	
	get_breaks <- reactive({
		1:input$numGridRows
	})
	
	get_points <- reactive({
		# hollow square = 0, filled square = 15, hollow circle = 1, filled circle = 16
		if(input$showPoints){
			geom_point( shape = as.numeric(input$pointType))
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
		plot1 <- plot1 + 
			scale_x_continuous(breaks=get_breaks(), expand = c(0, 0)) + 
			scale_y_continuous(breaks=get_breaks(), expand = c(0, 0))
	
		# avoid contour/fill errors
		if(var(dfdens$z) != 0){
			
			#add fill
			if(input$showFill){
				plot1 <- plot1 + 
					stat_contour(aes(z = z,  fill=..level.., alpha = ..level..), data = dfdens, geom="polygon")  +
					scale_fill_gradientn(colours = rev(rainbow(7)))
			}
			
			# add contour
			if(input$showContour){
				plot1 <- plot1 + geom_contour(aes(z = z), data = dfdens)
			}
		}
		
		# prevent "no layers in plot" error
		plot1 <- plot1 + geom_blank()
		
		# add points
		plot1 + get_points() 
		
	})
	
	#################### SIDEBAR HELPER FUNCTIONS ####################
	output$clickTable <- renderTable({
		point <- values$data[values$index,]
		if(length(rownames(point))>0){
			x <- data.frame(
				"Index" = rownames(point), 
				"X" = point$x, 
				"Y" = point$y, 
				"Value" = point$value)
		}
		else{
			x <- data.frame(
				"Index" = " ", 
				"X" = " ", 
				"Y" = " ", 
				"Value" = " ")
		}
	}, include.rownames = FALSE)
	
	#################### TABLE HELPER FUNCTIONS ####################
	output$table <- renderDataTable({
		values$data
	})

})