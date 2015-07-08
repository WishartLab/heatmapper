library(shiny)
library(d3heatmap)
shinyServer(function(input, output, session){
	
	################# get_file ################# 
	get_file <- reactive({
		if(input$chooseInput == 'fileUpload'){
			validate(need(!is.null(input$file$datapath), "Please upload a file"))
			data <- read.delim(input$file$datapath)
		}
		else{
			data <- read.delim(file = input$exampleFiles, header = TRUE, sep = "\t")
		}
		return(data)
	})
	
	################# get_data_matrix ################# 
	get_data_matrix <- reactive({
		data <- remove_strings(get_file())
		return(data.matrix(data))
	})
	
	get_colours <- reactive({
		palette <- colorRampPalette(c(input$lowColour, "black", input$highColour))
		colours <- palette(input$binNumber)
		return(colours)
	})
	
	################# remove_strings ################# 
	# removes strings from file content and assigns the 'NAME' column as the row labels
	remove_strings <- function(x){
		nums <- sapply(x, is.numeric)
		y <- x[,nums]
		
		# try to find a column with title name
		name = 'NAME'
		tryCatch({
			nameRow <- x[,name]
			rownames(y) <- make.names(nameRow, unique=TRUE)
			},
			finally = {return(y)}
		)
	}
	
	################# Display Heatmap ################# 
	output$map <- renderPlot({
		x <- get_data_matrix()
		heatmap(x, 
			Rowv = NA, 
			Colv = NA, 
			col = get_colours()
			)
	})
	
	################# Display D3Heatmap ################# 
	output$d3map <- renderD3heatmap({
		x <- get_data_matrix()
		validate(need(length(x)<10000, 
			"File is too large for this feature. Please select a smaller file with no more than 10,000 cells."))
		d3heatmap(x, Rowv = NULL, Colv = NULL, colors = get_colours(), scale = "row")
	})
	
	################# Display Table ################# 
	output$table <- renderDataTable({
		get_file()
	})
	
	################# Save Example File ################# 
	output$downloadExample <- downloadHandler(
		filename = "example.txt",
		content = function(file){
			write.table(read.delim(input$exampleFiles, header=TRUE, sep="\t"), sep = "\t",  file)
		}
	)
})