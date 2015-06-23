library(shiny)
library(ggplot2)
library(reshape2)
library(d3heatmap)

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

	get_plot <- function(){
		file <- get_file()
		if(is.null(file)){
			return(NULL)
		}
		data <- melt(file, id.vars = "cols", variable.name = "rows")
		data$cols <- factor(data$cols, levels = data$cols)

		q <- qplot(
			asp = 1,
			data = data,
			x=cols,
			y=rows,
			fill=as.numeric(value),
			geom="tile",
			xlab = input$xlab,
			ylab = input$ylab,
			margins = FALSE,
			main = input$title)

		q <- q + scale_fill_gradientn(colours = get_colour_palette(), name = "Values")

		return(q)
	}
	
	get_colour_palette <- function() {
		if(input$colour == 'rainbow'){
			return(rainbow(7))
		}
		else if(input$colour == 'custom'){
			if(input$customVars == 'custom2'){
				return(colorRampPalette(c(input$lowColour, input$highColour))(7))
			}
			else{
				return(colorRampPalette(c(input$lowColour, input$midColour, input$highColour))(7))
			}
		}
		else{
			return(topo.colors(7))
		}
	}
	
	output$d3map <- renderD3heatmap({
	file <- get_file()
		if(is.null(file)){
			return(NULL)
		}
		row.names(file)<- file[,1]
		file <- file[,-1]
		d3heatmap(file,colors = get_colour_palette(), Colv = NULL, Rowv = NULL)
	})
	
	output$map <- renderPlot({
		get_plot()
	})

	output$table <- renderDataTable({
		file <- get_file()
	})

	output$download <- downloadHandler(
		filename = "distanceMatrix.pdf",
		content = function(file){
			pdf(file)
			plot(get_plot())
			dev.off()
		}
	)
})
