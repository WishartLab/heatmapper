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
	read_file <- function(filePath, fileName = "txt") {
		sep <- "\t"
		if(tolower(substr(fileName, nchar(fileName)-2, nchar(fileName))) == "csv"){
			sep <- ","
		}
		tryCatch({
				scan(filePath,  nlines = 1, sep=sep)
				header <<- FALSE
			},
			error = function(e){
				header <<- TRUE
			})

		read.table(filePath, header = header, sep = sep)
	}
	
	# retrieve original data
	get_file <- reactive({
		if(input$chooseInput == 'example'){
			file <- read_file(input$exampleFiles)
		}
		else {
			if(is.null(values$file$datapath)){
				return(NULL)
			}
			file <- read_file(values$file$datapath, values$file$name)
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
			data$cols <- factor(data$cols, levels = unique(data$cols))
			
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
			return(colorRampPalette(c(input$lowColour, input$midColour, input$highColour))(3))
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
	
	get_scale_fill_gradientn <- function(min, max, length){
		shades <- input$binNumber
		
		if(length > shades){
			length <- shades
		}
		
		legend_breaks <- rev(seq.int(1, shades, length.out = length))
		legend_labels <- round(rev(seq.int(min, max, length.out = length)), 4)

		scale_fill_gradientn(colours = get_colour_palette(), breaks = legend_breaks, labels = legend_labels, name = "Values")

	}
	
	# finds if a string is a current selected item in input$layers
	layer_selected <- function(layer){
		if(length(grep(layer, input$layers))>0){
			TRUE
		}
		else{
			FALSE
		}
	}
	
	# plot using ggplot
	get_plot <- reactive({
		
		data <- melt_file()
		
		if(is.null(data)){
			return(NULL)
		}
		
		q <- ggplot(aes(x=cols, y=rows), 
			data = transform(data, binned = cut(value, breaks = input$binNumber, include.lowest = TRUE))) + 
			geom_tile(aes(fill = as.numeric(binned))) +
			get_scale_fill_gradientn(min(data$value), max(data$value), 5)
		
		q <- q + 
			xlab(input$xlab) + 
			ylab(input$ylab) + 
			ggtitle(input$title) + 
			scale_x_discrete(expand = c(0,0)) + 
			scale_y_discrete(expand = c(0,0)) + 
			get_asp()
		
		if(!layer_selected('showAxisLabels')){
			q <- q + theme(
		      axis.text.x=element_blank(),
		      axis.text.y=element_blank(),
		      axis.ticks=element_blank()) 
		}
		if(!layer_selected('showLegend')){
			q <- q + guides(fill=FALSE)
		}
		
		
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
