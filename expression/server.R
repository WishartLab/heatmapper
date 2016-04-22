library(shiny)
library(gdata)
library(d3heatmap)
library(gplots)
library(ggdendro)
# memory testing
# library(pryr)

# Constants
q = 5; # Use q*2 + 1 colors when brightening the expression heat map.

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
				
				fileType <- tail(unlist(strsplit(x = input$file$name, split = "[.]")), n=1)
				
				print(fileType)
				
				if(fileType == "xls" || fileType == "xlsx"){
					values$file <- read.xls(input$file$datapath, sheet=1)
				}
				else if(fileType == "csv"){
					values$file <- read.csv(input$file$datapath)
				}
				else{
					values$file <- read.delim(input$file$datapath)
				}
				
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
		else{ # Example
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
		
		# Modify the user-specified low, middle, and high colors based on
		# the value of the brightness slider. The brightness slider ranges
		# from -50 to +50. If it is < 0, darken the colors. If it is > 0,
		# brighten the colors by shifting them closer to the low and high
		# colors.
		brightness_adj = as.integer(input$plotBrightness)
		
		if(input$colourScheme == 'red/green'){
		  lowCol = "#FF0000"
		  midCol = "#000000"
		  highCol = "#23B000"
		}else if (input$colourScheme == 'blue/yellow'){
		  lowColour = "#0016DB"
		  midColour = "#FFFFFF"
		  highColour = "#FFFF00"
		}else if(input$colourScheme == 'custom'){
		  lowCol = input$lowColour
		  midCol = input$midColour
		  highCol = input$highColour
		}

		if(input$colourScheme == 'rainbow' || input$colourScheme == 'topo'){
		  if(input$colourScheme == 'rainbow'){
		  cl = rainbow(input$binNumber)
		  }else{
		  cl = topo.colors(input$binNumber)
		  }
		  
		  adjusted_colours = c()
		  for (color in cl){
		    if (brightness_adj == 0) {
		      new_color = color
		    }else if (brightness_adj < 0) {
		      new_color = darken(color, brightness_adj)
		    }else{
		      new_color = lighten(color, brightness_adj)
		    }
		    adjusted_colours <- c(adjusted_colours, new_color)
		  }
		  
		  adjusted_colours
		 }else if(input$colourScheme == 'custom' || input$colourScheme == 'red/green' || input$colourScheme == 'blue/yellow'){
		  if (brightness_adj == 0) {
			  lowCol = lowCol
			  midCol = midCol
			  highCol = highCol
			  colorRampPalette(c(lowCol, midCol, highCol))(input$binNumber)
		  } else if (brightness_adj < 0) {
			  lowCol = darken(lowCol, brightness_adj)
			  midCol = darken(midCol, brightness_adj)
			  highCol = darken(highCol, brightness_adj)
			  colorRampPalette(c(lowCol, midCol, highCol))(input$binNumber)
		  } else {
			  lowCol = lowCol
			  midCol = midCol
			  highCol = highCol
			  colorRampPalette(get_brightness_adjusted_color_set(lowCol, midCol, highCol, brightness_adj))(input$binNumber)
		  }
		}
		
	})
	
	# Return a set of colors ranging from lowCol to highCol, with midCol in the
	# middle, adjusted brighter based on the value of adj.
	get_brightness_adjusted_color_set <- function(lowCol, midCol, highCol, adj) {
		
		adj = (adj)/20.0 + 1.0
		
		r1 = strtoi(paste('0x', substr(lowCol, 2,3), sep=''))
		g1 = strtoi(paste('0x', substr(lowCol, 4,5), sep=''))
		b1 = strtoi(paste('0x', substr(lowCol, 6,7), sep=''))
		
		r2 = strtoi(paste('0x', substr(midCol, 2,3), sep=''))
		g2 = strtoi(paste('0x', substr(midCol, 4,5), sep=''))
		b2 = strtoi(paste('0x', substr(midCol, 6,7), sep=''))
		
		r3 = strtoi(paste('0x', substr(highCol, 2,3), sep=''))
		g3 = strtoi(paste('0x', substr(highCol, 4,5), sep=''))
		b3 = strtoi(paste('0x', substr(highCol, 6,7), sep=''))
		
		# Create a matrix of colors to be used for the colorRampPalette function.
		# Each row will be 3 integers representing R, G, and B values. The first
		# row is the low color, and the last is the high color, with a total of
		# q*2 + 1 colors.
		arr = matrix(c(
					subarr(r1, r2, q)[1:q], subarr(r2, r3, q),
					subarr(g1, g2, q)[1:q], subarr(g2, g3, q),
					subarr(b1, b2, q)[1:q], subarr(b2, b3, q)
				),
				nrow = q*2 + 1
		)
		
		arr = change_brightness(arr, adj)
		# print (arr)
		return (apply(arr, 1, tuple2hex))
	}
	
	# Convert array of 3 integers, representing R, G, and B values, to
	# a hexidecimal-based string representing the color.
	tuple2hex <- function(c) {
		return (toupper(sprintf("#%02x%02x%02x",
			as.integer(c[1]), as.integer(c[2]), as.integer(c[3])
			)));
	}
	
	# Return an array with values ranging from c1 to c2, with q+1 elements
	# equally spaced. If c1 == c2, the returned array will have q+1 elements
	# all with the same value.
	subarr <-function(c1, c2, q) {
		if (c1 == c2) {
			return (rep(c1, q+1));
		} else {
			return (seq(c1, c2, (c2-c1)/q));
		}
	}
	
	# Change brightness of given colors.
	# c: n x 3 array in which each row consists of 3 decimal values representing
	#		R, G, and B values.
	# adj: brightness adjustment value.
	change_brightness <- function(c, adj) {
		mid = (dim(c)[1]-1)/2 + 1 # middle index
		end = dim(c)[1]
		
		midcol = c[mid,]
		minstop = apply(c[1:mid,],2,min)
		minsbot = apply(c[mid:end,],2,min)

		# adjust top half of color array
		
		d = abs(c[1,] - midcol)
		
		tmp = apply(c[1:mid,], 1, addtorow, x=-minstop) # substract minimum
		tmp = apply(tmp, 1, identity) # un-transpose
		tmp = apply(tmp, 1, divbyd, d=d)
		tmp = apply(tmp, 1, identity) # un-transpose
		
		tmp = apply(tmp^(1/adj), 1, multbyd, d=d) # apply brightness adjustment and begin undoing the other transformations
		tmp = apply(tmp, 1, identity) # un-transpose
		tmp = apply(tmp, 1, addtorow, x=minstop)
		c[1:mid,] = apply(tmp, 1, identity) # un-transpose
		
		# adjust bottom half of color array
		
		d = abs(c[end,] - midcol)
		
		tmp = apply(c[(mid+1):end,], 1, addtorow, x=-minsbot) # substract minimum
		tmp = apply(tmp, 1, identity) # un-transpose
		tmp = apply(tmp, 1, divbyd, d=d)
		tmp = apply(tmp, 1, identity) # un-transpose
		
		tmp = apply(tmp^(1/adj), 1, multbyd, d=d) # apply brightness adjustment and begin undoing the other transformations
		tmp = apply(tmp, 1, identity) # un-transpose
		tmp = apply(tmp, 1, addtorow, x=minsbot)
		c[(mid+1):end,] = apply(tmp, 1, identity) # un-transpose
		
		return(c)
	}
	
	divbyd <- function(r, d) {
		return(ifelse(d == 0, r, r/d))
	}
	
	multbyd <- function(r, d) {
		return(ifelse(d == 0, r, r * d))
	}
	
	addtorow <- function(r, x) {
		return(r + x)
	}
	
	
	# Darken the given color based on adj (a negative integer).
	darken <- function(col, adj){
		r = strtoi(paste('0x', substr(col, 2,3), sep=''))
		g = strtoi(paste('0x', substr(col, 4,5), sep=''))
		b = strtoi(paste('0x', substr(col, 6,7), sep=''))
		
		adj = (100.0 + adj)/100.0
		
		r = as.integer(r*adj)
		g = as.integer(g*adj)
		b = as.integer(b*adj)
		
		col = toupper(sprintf("#%02x%02x%02x",r,g,b))
		return (col)
	}
	
	
	# Return a set of colors ranging from lowCol to highCol, with midCol in the
	# middle, adjusted brighter based on the value of adj.
	lighten <- function(col, adj) {
	  
	  adj = (100.0 - adj)/100.0
    adj = 1-adj
	  r1 = strtoi(paste('0x', substr(col, 2,3), sep=''))
	  g1 = strtoi(paste('0x', substr(col, 4,5), sep=''))
	  b1 = strtoi(paste('0x', substr(col, 6,7), sep=''))
	  
	  r = as.integer((255-r1)*adj+r1)
	  g = as.integer((255-g1)*adj+g1)
	  b = as.integer((255-b1)*adj+b1)
	  
	  col = toupper(sprintf("#%02x%02x%02x",r,g,b))
	  return (col)
	}
	
	
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
		
		if (clust_selected("col") && length(grep("col", input$dendSelectRC))>0) {
			col_dendrogram_height = 120
		} else {
			col_dendrogram_height = 60
		}
		heatmap_height = get_plot_height() - col_dendrogram_height
		
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
				col = get_colour_palette(),#(input$binNumber), 
				scale = input$scale,
				main = input$title, 
				xlab = input$xlab, 
				ylab = input$ylab,

				lhei = c(col_dendrogram_height, heatmap_height) # set column dendrogram height relative to heatmap height
			)
			graphics.off()
		},
		error = function(err){
			print(paste("ERROR: ", err))
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
			get_plot_height()
		})
	)
	
	get_plot_height <- (
		
		reactive({
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
		
		print(length(x))
		
		validate(need(length(x) <= 400000, 
			"File is too large for this feature. Please select a smaller file with no more than 400,000 cells."))
		
		tryCatch({
			
			# Get number of colors to use in color palette, which depends on whether
			# or not we are brightening the plot.
			brightness_adj = as.integer(input$plotBrightness)
			if (brightness_adj > 0) {
				num_colors = q*2 + 1
			} else {
				num_colors = 3;
			}
			
			d3heatmap(x, 
				Rowv = get_dendrograms()[[1]], 
				Colv = get_dendrograms()[[2]],  
				colors = get_colour_palette()(num_colors),
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
	}, height = reactive({ifelse(is.null(values$colHclust) || is.na(values$colHclust), 100, length(values$colHclust[[1]])*10)})
	)
	
	# display table
	output$table <- renderDataTable({
		get_file()
	})
		
	# save plot
	output$plotDownload <- downloadHandler(
		
		filename = reactive({paste("heatmap.", input$downloadPlotFormat, sep="")}),
		
		content = function(file) {
			if(input$downloadPlotFormat == "pdf"){
				pdf(file, width=input$plotWidth/72, height=input$plotHeight/72)
				get_plot()
			}
			else if(input$downloadPlotFormat == "jpg"){
				jpeg(file, width=input$plotWidth, height=input$plotHeight)
				get_plot()
			}
			else if(input$downloadPlotFormat == "tiff"){
				tiff(file, width=input$plotWidth, height=input$plotHeight)
				get_plot()
			}
			else{
				tryCatch({
					png(file, width=input$plotWidth, height=input$plotHeight)
					get_plot()
				}, 
				error = function(err){
					print(paste("ERROR:  ", err))
					validate(need(FALSE,"PNG image too large. Please decrease the dimensions or resolution of the image."))
					return(NULL)
				})
			}
		}
	)
	
	# save current file 
	output$tableDownload <- downloadHandler(
		filename = reactive({paste0("table.", input$downloadTableFormat)}),
		content = function(file){
			if(input$downloadTableFormat == "csv"){
				write.csv(get_file(), quote = FALSE, file = file, row.names = FALSE)
			}
			else{
				write.table(get_file(), sep = "\t", quote = FALSE, file = file, row.names = FALSE)
			}
		}
	)

})