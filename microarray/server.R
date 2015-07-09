library(shiny)
library(d3heatmap)
library(gplots)
library(ggdendro)
# memory testing
library(pryr)

shinyServer(function(input, output, session){
	values <- reactiveValues(
		rowMatrix = c(), 
		colMatrix = c(), 
		rowDist = c(), 
		colDist = c(), 
		rowHclust = c(), 
		colHclust = c())
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
		if(input$rowv){
			values$rowMatrix <- get_data_matrix()
			get_row_dist()
			get_row_hclust()
		}
		if(input$colv){
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
	
	get_dendrogram <- function(){
		if(input$dendRow && input$dendCol){
			if(input$rowv && input$colv){
				return("both")
			}
			else if(input$rowv){
				return("row")
			}
			else if(input$colv){
				return("column")
			}
			else{
				return("none")
			}	
		}
		else if(input$dendRow){
			return("row")
		}
		else if(input$dendCol){
			return("column")
		}
		else{
			return("none")
		}
	}
	
	################# Display Heatmap ################# 
	output$map <- renderPlot({
		x <- get_data_matrix()
		ifelse(input$rowv && input$clusterMethod != 'none', hr<-as.dendrogram(values$rowHclust), hr<-NA)
		ifelse(input$colv && input$clusterMethod != 'none', hc<-as.dendrogram(values$colHclust), hc<-NA)
		
		print(mem_used())
		print(object_size(hr))
		heatmap.2(x,
			na.color = input$missingColour, 
			key=FALSE, 
			symkey=FALSE, 
			density.info="none", 
			trace="none",
			
			keysize=0.6, 
			offsetCol = 0, 
			offsetRow = 0,
			
			dendrogram = get_dendrogram(),
			Rowv = hr, 
			Colv = hc, 
			col = get_colour_palette()(input$binNumber), 
			scale = input$scale,
			main = input$title, 
			xlab = input$xlab, 
			ylab = input$ylab
			)
	}, height = reactive({input$mapHeight}) )
	
	################# Display D3Heatmap ################# 
	output$d3map <- renderD3heatmap({
		x <- get_data_matrix()
		
		validate(need(length(x)<20000, 
			"File is too large for this feature. Please select a smaller file with no more than 20,000 cells."))
		
		ifelse(input$rowv && input$clusterMethod != 'none', hr<-as.dendrogram(values$rowHclust), hr<-FALSE)
		ifelse(input$colv && input$clusterMethod != 'none', hc<-as.dendrogram(values$colHclust), hc<-FALSE)
		
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
		validate(need(input$rowv, "Apply clustering to rows to view this dendrogram"))
		get_dendrogram_plot(values$rowHclust, "row")
	}, height = reactive({ifelse(is.null(values$rowHclust), 100, length(values$rowHclust$labels)*12)}) )
	
	output$colDendrogram <- renderPlot({
		validate(need(input$colv, "Apply clustering to columns to view this dendrogram"))
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
})