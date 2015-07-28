library(shiny)
library(ggplot2)
library(reshape2)
library(d3heatmap)

shinyServer(function(input, output, session){

	#################### GLOBAL REACTIVE VALUES ####################
	values <- reactiveValues(
		file = NULL
	)
	
	#################### OBSERVERS ####################
	observe({
		input$clearFile
		values$file <- NULL
	})
	
	observe({
		values$file <- input$file
	})
	
	
	#################### FILE INPUT FUNCTIONS ####################
	# read a file given a file name
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
	
	# retrieve original data
	get_file <- reactive({
		if(input$chooseInput == 'example'){
			file <- read_file("data/dist.txt")
		}
		else {
			if(is.null(values$file$datapath)){
				return(NULL)
			}
			file <- read_file(values$file$datapath)
		}

		# if no row names are specified use the column names
		if(is.numeric(file[[1]])){
			file <- cbind(colnames(file), file)
		}

		colnames(file)[1] <- "cols"

		return(file)
	})

	# melt data
	melt_file <- reactive({
		file <- get_file()
		if(!is.null(file)){
			data <- melt(file, id.vars = "cols", variable.name = "rows")
			data$cols <- factor(data$cols, levels = data$cols)
			return(data)
		}
		else{
			return(NULL)
		}
	})
	
	#################### GGPLOT HELPER FUNCTIONS ####################
	# get colour palette based on input$colourScheme selection
	get_colour_palette <- function() {
		if(input$colourScheme == 'rainbow'){
			return(rainbow(7))
		}
		else if(input$colourScheme == 'custom'){
			return(colorRampPalette(c(input$lowColour, input$midColour, input$highColour))(7))
		}
		else{
			return(topo.colors(7))
		}
	}
	
	# set aspect ratio
	get_asp <- reactive({
		if(input$asp){
			coord_fixed(ratio = 1)
		}
		else{
			NULL
		}
	})
	
	# plot using ggplot
	get_plot <- reactive({
		
		data <- melt_file()
		
		if(is.null(data)){
			return(NULL)
		}

		q <- qplot(
			data = data,
			x=cols,
			y=rows,
			fill=as.numeric(value),
			geom="tile",
			xlab = input$xlab,
			ylab = input$ylab,
			margins = FALSE,
			main = input$title)

		q <- q + scale_x_discrete(expand = c(0,0)) + scale_y_discrete(expand = c(0,0))
		q <- q + scale_fill_gradientn(colours = get_colour_palette(), name = "Values")
		q <- q + get_asp()
		return(q)
	})
	
	#################### OUTPUT FUNCTIONS ####################
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
	}, width = reactive({input$plotWidth}), height = reactive({input$plotHeight}))

	output$table <- renderDataTable({
		file <- get_file()
	})

	get_plot_download_name <- function(){
		paste0("distanceMatrix.", input$downloadPlotFormat)
	}
	
	output$download <- downloadHandler(
		filename = reactive({get_plot_download_name()}),
		content = function(file){
			ggsave(file, get_plot(), width = input$plotWidth/72, height = input$plotHeight/72)
		}
	)
})
