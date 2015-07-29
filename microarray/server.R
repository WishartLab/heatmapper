library(shiny)
library(d3heatmap)
library(gplots)
library(ggdendro)
# memory testing
library(pryr)

shinyServer(function(input, output, session){
	
	# source: http://stackoverflow.com/questions/18237987/show-that-shiny-is-busy-or-loading-when-changing-tab-panels
	output$activeTab <- reactive({return(input$tabSelections)})
  outputOptions(output, 'activeTab', suspendWhenHidden=FALSE)
	
	values <- reactiveValues(
		file = NULL,
		rowMatrix = c(), 
		colMatrix = c(), 
		rowDist = c(), 
		colDist = c(), 
		rowHclust = c(), 
		colHclust = c())
	
	#################### FILE CLEAR OBSERVERS ####################
	observe({
		input$clearFile
		values$file <- NULL
	})
	
	observe({
		values$file <- input$file
	})
	
	################# get_file ################# 
	get_file <- reactive({
		if(input$chooseInput == 'fileUpload'){
			validate(need(!is.null(values$file$datapath), "Please upload a file"))
			data <- read.delim(values$file$datapath)
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
	
	get_colour_palette <- reactive({
		palette <- colorRampPalette(c(input$lowColour, "black", input$highColour))
		return(palette)
	})
	
	################# get_dist #################
	# calculates a distance matrix 
	get_dist <- function(x){
		print("dist")
		# source http://stackoverflow.com/questions/15773189/remove-na-nan-inf-in-a-matrix
		# replace all non-finite values with 0
		x[!rowSums(!is.finite(x)),]
		x[!is.finite(x)] <- 0
		
		if(input$distanceMethod == 'euclidean' || input$distanceMethod == 'manhattan'){
			x <- dist(x, method = input$distanceMethod)
		}
		else{
			x <- as.dist(1-cor(t(data.matrix(x)), method=input$distanceMethod))
		}
		return(x)
	}
	
	
	################# get_hclust #################
	# uses hclust to cluster data using get_dist distance matrix
	get_hclust <- function(x){
		print("hclust")
		if(input$clusterMethod != 'none'){
			x <- hclust(x, method = input$clusterMethod)
		}
		else{
			x <- NULL
		}
		return(x)
	}

	
	get_row_dist <- reactive({
		values$rowDist <- get_dist(values$rowMatrix)
	})
	
	get_col_dist <- reactive({
		values$colDist <- get_dist(values$colMatrix)
	})
	
	get_row_hclust <- reactive({
		values$rowHclust <- get_hclust(values$rowDist)
	})
	
	get_col_hclust <- reactive({
		values$colHclust <- get_hclust(values$colDist)
	})
	
	observe({
		if(clust_select("row")){
			values$rowMatrix <- get_data_matrix()
			get_row_dist()
			get_row_hclust()
		}
		if(clust_select("col")){
			values$colMatrix <- t(get_data_matrix())
			get_col_dist()
			get_col_hclust()
		}
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
	
	clust_select <- function(rc){
		if(length(grep(rc, input$clusterSelectRC))>0){
			TRUE
		}
		else{
			FALSE
		}
	}
	# check which of "row" and "col" is selected
	dend_select <- function(){
	
		if(clust_select("row")){
			rowCheck <- length(grep("row", input$dendSelectRC))>0
		}
		else{
			rowCheck <- FALSE
		}
		
		if(clust_select("col")){
			colCheck <- length(grep("col", input$dendSelectRC))>0
		}
		else{
			colCheck <- FALSE
		}
		
		if(rowCheck && colCheck){
			"both"
		}
		else if(rowCheck){
			"row"
		}
		else if(colCheck){
			"col"
		}
		else{
			"none"
		}
	}
	
	get_plot <- reactive({
		x <- get_data_matrix()
		ifelse(clust_select("row") && input$clusterMethod != 'none', hr<-as.dendrogram(values$rowHclust), hr<-NA)
		ifelse(clust_select("col") && input$clusterMethod != 'none', hc<-as.dendrogram(values$colHclust), hc<-NA)
		
		#print(mem_used())
		#print(object_size(hr))
		heatmap.2(x,
			na.color = input$missingColour, 
			key=FALSE, 
			symkey=FALSE, 
			density.info="none", 
			trace="none",
			
			keysize=0.6, 
			offsetCol = 0, 
			offsetRow = 0,
			
			dendrogram = dend_select(),
			Rowv = hr, 
			Colv = hc, 
			col = get_colour_palette()(input$binNumber), 
			scale = input$scale,
			main = input$title, 
			xlab = input$xlab, 
			ylab = input$ylab
		)
		graphics.off()
	})
	
	################# Display Heatmap ################# 
	output$map <- renderPlot(
		get_plot(),
		width =  reactive({input$plotWidth}),
		height = reactive({
			if(input$fullSize){
				input$plotWidth / ncol(values$rowMatrix) * nrow(values$rowMatrix)
			}
			else{
				input$plotHeight
			}
		}) )
	
	################# Display D3Heatmap ################# 
	output$d3map <- renderD3heatmap({
		x <- get_data_matrix()
		
		validate(need(length(x)<20000, 
			"File is too large for this feature. Please select a smaller file with no more than 20,000 cells."))
		
		ifelse(clust_select("row") && input$clusterMethod != 'none', hr<-as.dendrogram(values$rowHclust), hr<-FALSE)
		ifelse(clust_select("col") && input$clusterMethod != 'none', hc<-as.dendrogram(values$colHclust), hc<-FALSE)
		
		d3heatmap(x, 
			Rowv = hr, 
			Colv = hc, 
			colors = get_colour_palette()(3),
			scale = input$scale, 
			show_grid = FALSE, 
			anim_duration = 0)
	})

	get_dendrogram_plot <- function(x, message){
		validate(need(!is.null(x), paste0("Select a clusting method and apply clustering to ", message, " to view this dendrogram")))
		x$labels <- strtrim(x$labels, 60)
		ggdendrogram(x, rotate = TRUE)
	}
	
	output$rowDendrogram <- renderPlot({
		validate(need(clust_select("row"), "Apply clustering to rows to view this dendrogram"))
		get_dendrogram_plot(values$rowHclust, "row")
	}, height = reactive({ifelse(is.null(values$rowHclust), 100, length(values$rowHclust$labels)*12)}) )
	
	output$colDendrogram <- renderPlot({
		validate(need(clust_select("col"), "Apply clustering to columns to view this dendrogram"))
		get_dendrogram_plot(values$colHclust, "column")
	}, height = reactive({ifelse(is.null(values$colHclust), 100, length(values$colHclust$labels)*12)}) )
	
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
		
	################# Save Plot ################# 

	output$plotDownload <- downloadHandler(
		
		filename = reactive({paste("heatmap.", input$downloadFormat, sep="")}),
		
		content = function(file) {
			if(input$downloadFormat == "pdf"){
				pdf(file)
				get_plot()
			}
			else{
				tryCatch({
					png(file)	
					get_plot()
				}, 
				error = function(err){
					validate(need(FALSE,"PNG image too large. Please decrease the dimensions or resolution of the image."))
					return(NULL)
				})
			}
		}
	)
	################# Save Current File ################# 
	output$tableDownload <- downloadHandler(
		filename = "table.txt",
		content = function(file){
			write.table(get_file(), file)
		}
	)

})