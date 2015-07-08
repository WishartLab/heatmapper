library(shiny)

shinyServer(function(input, output, session){
	
	################# get_file ################# 
	get_file <- reactive({
		if(input$chooseInput == 'fileUpload')
			data <- read.delim(input$file$datapath)
		else
			data <- read.delim(file = input$exampleFiles, header = TRUE, sep = "\t")
		data <- remove_strings(data)
		return(data)
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
	
	
	output$map <- renderPlot({
		x <- data.matrix(get_file())
		#x <- matrix(nrow = 10, ncol = 10, data = seq(from = 1, to = 100))
		heatmap(x)
	})
})