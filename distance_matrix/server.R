library(shiny)
library(ggplot2)
library(reshape2)

shinyServer(function(input, output, session){

	read_file <- function(fileName) {
		tryCatch({
				scan(fileName,  nlines = 1)
				header <<- FALSE
			}, 
			error = function(e){
				header <<- TRUE
			})
		file <- read.table(fileName, header = header, sep = "\t")
		return(file)
	}
	
	get_file <- function() {
		if(input$chooseInput == 'example'){
			file <- read_file("data/dist.txt")
		}
		else {
			if(is.null(input$file$datapath)){
				return(NULL)
			}
			file <- read_file(input$file$datapath)
		}
		
		# if no row names are specified use the column names
		if(is.numeric(file[[1]])){
			file <- cbind(colnames(file), file)
		}
		
		colnames(file)[1] <- "cols"
		
		return(file)
	}
	output$map <- renderPlot({
		file <- get_file()
		if(is.null(file)){
			return(NULL)
		}
		data <- melt(file, id.vars = "cols", variable.name = "rows")
		data$cols <- factor(data$cols, levels = data$cols)
		
		q <- qplot(asp = 1,
			data = data, 
			x=cols, 
			y=rows, 
			fill=as.numeric(value), 
			geom="tile", 
			xlab = input$xlab, 
			ylab = input$ylab, 
			main = input$title) 
		
		if(input$colour == 'rainbow'){
			q <- q + scale_fill_gradientn(colours = rainbow(7), name = "Values")
		}
		else if(input$colour == 'custom'){
			my_palette <- colorRampPalette(c(input$lowColour, input$midColour, input$highColour))
			q <- q + scale_fill_gradientn(colours = my_palette(7), name = "Values")
		}
		else{
			q <- q + scale_fill_gradientn(colours = topo.colors(7), name = "Values")
		}
		
		return(q)
	})
	
	output$info <- renderPrint({
		file <- get_file()
		if(!is.null(file)){
			brushedPoints(melt(file, id.vars = "cols", variable.name = "rows"), input$brush)
		}
  })
	
	output$table <- renderDataTable({
		file <- get_file()
	})
})