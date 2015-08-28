library(shiny)
library(d3heatmap)
library(gplots)
library(ggdendro)
# memory testing
# library(pryr)

shinyServer(function(input, output, session){
	
	# http://stackoverflow.com/questions/18237987/show-that-shiny-is-busy-or-loading-when-changing-tab-panels
	output$activeTab <- reactive({return(input$tabSelections)})
  outputOptions(output, 'activeTab', suspendWhenHidden=FALSE)
	
	values <- reactiveValues(
		file = NULL,
		rowMatrix = NULL, 
		colMatrix = NULL, 
		rowHclust = NULL, 
		colHclust = NULL)
	
	################################## OBSERVERS ##################################
	observe({
		input$clearFile
		values$file <- NULL
	})
	
	# update values$file when a file is uploaded, set to NA if file cannot be read
	observe({
		if(!is.null(input$file$datapath)){
			tryCatch({
				values$file <- read.delim(input$file$datapath)
			}, 
			error = function(err){
				values$file <- NA
			})
		}
	})
	
	# try to update reactive values when row and/or col clustering is selected
	observe({
		try({
			# if clustering rows update rowMatrix and rowHclust in values
			if(clust_selected("row")){
				update_row_clust()
			}
			
			# if clustering cols update colMatrix and colHclust in values
			if(clust_selected("col")){
				update_col_clust()
			}
		})
	})
	
	observe({
		if(input$chooseInput == 'fileUpload'){
			values$rowHclust <- NA
			values$colHclust <- NA
		}
	})
	
	################################## HELPER FUNCTIONS ##################################

	# returns raw data from file input or selected example file
	get_file <- reactive({
		if(input$chooseInput == 'fileUpload'){
			values$file
		}
		else{
			read.delim(file = input$exampleFiles, header = TRUE, sep = "\t")
		}
	})
	
	# converts file from data.frame to data.matrix
	# returns data matrix or NULL if non valid input
	get_data_matrix <- reactive({
		validate(need(!is.null(get_file()), ERR_file_upload))
		tryCatch({
			data <- remove_strings(get_file())
			return(data.matrix(data))
		}, 
		error = function(err){
			validate(txt = ERR_file_read)
		})
	})
	
	# returns colorRampPalette of input$lowColour and input$highColour
	get_colour_palette <- reactive({
		colorRampPalette(c(input$lowColour, input$midColour, input$highColour))
	})
	
	# calculates and returns a distance matrix
	# param: data matrix
	get_dist <- function(x){
		tryCatch({
			# http://stackoverflow.com/questions/15773189/remove-na-nan-inf-in-a-matrix
			# replace all non-finite values with 0
			x[!rowSums(!is.finite(x)),]
			x[!is.finite(x)] <- 0
			
			if(input$distanceMethod == 'euclidean' || input$distanceMethod == 'manhattan'){
				dist(x, method = input$distanceMethod)
			}
			else{
				as.dist(1-cor(t(data.matrix(x)), method=input$distanceMethod))
			}
		}, 
		error = function(err){
			NULL
		})
	}
	
	# calculates and returns an hclust object using get_dist() distance matrix
	# param: distance matrix
	get_hclust <- function(x){

		if(input$clusterMethod != 'none'){
			tryCatch({
				hclust(x, method = input$clusterMethod)
			}, 
			error = function(err){
				NA
			})
		}
		else{
			NULL
		}
	}

	# updated when change in file, dist method, or hclust method
	# sets values$rowMatrix and values$rowHclust
	update_row_clust <- reactive({
		if(!is.null(get_file())){
			values$rowMatrix <- get_data_matrix()
			values$rowHclust <- get_hclust( get_dist(values$rowMatrix) )
		}
	})
	
	# updated when change in file, dist method, or hclust method
	# sets values$colMatrix and values$colHclust
	update_col_clust <- reactive({
		if(!is.null(get_file())){
			values$colMatrix <- t(get_data_matrix())
			values$colHclust <- get_hclust( get_dist(values$colMatrix) )
		}
	})

	# removes strings from file content and assigns the 'NAME' column as the row labels
	remove_strings <- function(x){
		
		# subset of numeric values
		nums <- x[,sapply(x, is.numeric)]

		# try to find a column with title name
		name = 'NAME'
		
		try({
			nameRow <- x[,name]
			rownames(nums) <- make.names(nameRow, unique=TRUE)
		})
		
		return(nums)
	}
	
	# finds if a string is a current selected item in input$clusterSelectRC
	clust_selected <- function(rc){
		if(length(grep(rc, input$clusterSelectRC))>0){
			TRUE
		}
		else{
			FALSE
		}
	}
	
	# check which of "row" and "col" is selected
	dend_select <- function(){
	
		if(clust_selected("row")){
			rowCheck <- length(grep("row", input$dendSelectRC))>0
		}
		else{
			rowCheck <- FALSE
		}
		
		if(clust_selected("col")){
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
	
	# return a list of dendrogram objects, first row then column
	get_dendrograms <- reactive({
		hr <- NA
		hc <- NA
		if(input$clusterMethod != 'none'){
			if(clust_selected("row")){
				hr <- as.dendrogram(values$rowHclust)
			}
			if(clust_selected("col")){
				hc <- as.dendrogram(values$colHclust)
			}
		}
		list(hr,hc)
	})
	
	# returns a heatmap.2 image based on get_data_matrix()
	get_plot <- function(){
		x <- get_data_matrix()
		tryCatch({
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
				Rowv = get_dendrograms()[[1]], 
				Colv = get_dendrograms()[[2]], 
				col = get_colour_palette()(input$binNumber), 
				scale = input$scale,
				main = input$title, 
				xlab = input$xlab, 
				ylab = input$ylab
			)
			graphics.off()
		},
		error = function(err){
			validate(txt=ERR_plot_display)
		})
	}
	
	# returns the result of ggdendrogram() on param x
	get_dendrogram_plot <- function(x, message){
		validate(need(!is.null(get_file()), ERR_file_upload))
		validate(need(!is.null(x), paste0("Select a clusting method and apply clustering to ", message, " to view this dendrogram")))
		validate(need(!is.na(x), ERR_file_read))

		x$labels <- strtrim(x$labels, 60)
		ggdendrogram(x, rotate = TRUE)

	}
	
	################################## OUTPUT FUNCTIONS ##################################
	
	# heatmap.2 plot
	output$heatmap <- renderPlot(
		get_plot(),
		width =  reactive({input$plotWidth}),
		height = reactive({
			if(input$fullSize){
				if(!is.null(values$rowMatrix) && !is.na(values$rowMatrix)){
					input$plotWidth/ncol(values$rowMatrix) * nrow(values$rowMatrix)
				}
				else{
					input$plotHeight
				}
			}
			else{
				input$plotHeight
			}
		})
	)
	
	
	# d3heatmap plot
	output$d3map <- renderD3heatmap({
		x <- get_data_matrix()
		
		validate(need(length(x) <= 10000, 
			"File is too large for this feature. Please select a smaller file with no more than 10,000 cells."))
		
		tryCatch({
			d3heatmap(x, 
				Rowv = get_dendrograms()[[1]], 
				Colv = get_dendrograms()[[2]],  
				colors = get_colour_palette()(3),
				scale = input$scale, 
				show_grid = FALSE, 
				anim_duration = 0)
		}, 
		error = function(err){
			validate(txt=ERR_plot_display)
		})
	})
	
	# row dendrogram plot
	output$rowDendrogram <- renderPlot({
		validate(need(clust_selected("row"), "Apply clustering to rows to view this dendrogram"))
		get_dendrogram_plot(values$rowHclust, "row")
	}, height = reactive({ifelse(is.null(values$rowHclust) || is.na(values$rowHclust), 100, length(values$rowHclust$labels)*10)}) )
	
	# col dendrogram plot
	output$colDendrogram <- renderPlot({
		validate(need(clust_selected("col"), "Apply clustering to columns to view this dendrogram"))
		get_dendrogram_plot(values$colHclust, "column")
	}, height = reactive({ifelse(is.null(values$colHclust) || is.na(values$colHclust), 100, length(values$colHclust[[1]])*10)}) )
	
	# display table
	output$table <- renderDataTable({
		get_file()
	})
		
	# save plot
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
	
	# save current file 
	output$tableDownload <- downloadHandler(
		filename = "table.txt",
		content = function(file){
			write.table(get_file(), file)
		}
	)

})