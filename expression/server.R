library(shiny)
library(gdata)
library(d3heatmap)
library(gplots)
library(ggdendro)
library(RColorBrewer)
library(ape)
# memory testing
# library(pryr)

source("../global_server.R")

# Increase max upload file size to 10 MB
options(shiny.maxRequestSize=10*1024^2)

# Constants
dimensions_msg <- "Input data can have up to 2,500 rows and 300 columns."
q = 5; # Use q*2 + 1 colors when brightening the expression heat map.

# because there is no way to fix the first look of the plot output from ui.R. the plot width and plot
# height are from the "Show advanced options" of ui.R. That is not good for showing file with many rows,
# especially more than 100 rows. So here we adjust the plot height with this parameter to make it bigger if row 
# count number is big. So we can read the y axis lable visually with the reasonable height.
scope <<- 0.020107349; 
intercept <<- 0.37506195;
# the upper free params used in this formula. adjust_plot_height_weight = scope * rows(input_file) + intercept.
# please check the get_image_weight function


# for adjusting the font size, please read function 'get_label_weight'
EACH_ROW_SIZE_LIMIT <- 13; # the minimum pixel size of each row in heatmap.2 image, if larger than it, start making font size 
                          # of heatmap.2 image propotionaly. if the font size is bigger than 1.5, the font size not increaseed 
                          # anymore. If lower than it, keep the minum front size.
MIN_FONT_SIZE <- 1.0;
MAX_FONT_SIZE <- 1.45;

MANY_FILE_ROWS <- 100; # More than this many rows is considered a lot.

shinyServer(function(input, output, session){
	
	# http://stackoverflow.com/questions/18237987/show-that-shiny-is-busy-or-loading-when-changing-tab-panels
	output$activeTab <- reactive({return(input$tabSelections)})
  outputOptions(output, 'activeTab', suspendWhenHidden=FALSE)
	
	values <- reactiveValues(
		file = NULL,
		rowClusterFile = NULL,
		colClusterFile = NULL,
		rowMatrix = NULL, 
		colMatrix = NULL, 
		rowHclust = NULL, 
		colHclust = NULL,
		fileMulti = NULL,
		plotnum = 1 # For multiple file upload. First plot is 1.
		)
	
	################################## OBSERVERS ##################################
	observe({
		input$clearFile
		values$file <- NULL
		log_activity('expression', 'clearFile')
	})

	observe({
		input$clearFileMulti
		values$fileMulti <- NULL
		log_activity('expression', 'clearFileMulti')
	})

	observe({
		if (is.null(input$fileMulti)) {
			values$fileMulti <- NULL
		} else {
			values$fileMulti <- input$fileMulti[with(input$fileMulti, order(name)), ] # sort by file name as we copy the data table
		}
		log_activity('expression', 'fileMulti')
	})

	observeEvent(input$cyclePlotsStart, {
		if (is.null(nrow(values$fileMulti))) {
			values$plotnum
		} else if (values$plotnum > 1) {
			values$plotnum <- 1
		}
		log_activity('expression', 'cyclePlotsStart')
	})

	observeEvent(input$cyclePlotsLeft, {
		if (is.null(nrow(values$fileMulti))) {
			values$plotnum
		} else if (values$plotnum > 1) {
			values$plotnum <- values$plotnum - 1
		}
		log_activity('expression', 'cyclePlotsLeft')
	})

	observeEvent(input$cyclePlotsRight, {
		if (is.null(nrow(values$fileMulti))) {
			values$plotnum
		} else if (values$plotnum < nrow(values$fileMulti)) {
			values$plotnum <- values$plotnum + 1
		}
		log_activity('expression', 'cyclePlotsRight')
	})

	observeEvent(input$cyclePlotsEnd, {
		if (is.null(nrow(values$fileMulti))) {
			values$plotnum
		} else if (values$plotnum < nrow(values$fileMulti)) {
			values$plotnum <- nrow(values$fileMulti)
		}
		log_activity('expression', 'cyclePlotsEnd')
	})
	
	observe({
		input$clearColClusterFile
		values$colClusterFile <- NULL
		log_activity('expression', 'clearColClusterFile')
	})

	observe({
		input$clearRowClusterFile
		values$rowClusterFile <- NULL
		log_activity('expression', 'clearRowClusterFile')
	})

	# update values$file when a file is uploaded, set to NA if file cannot be read
	observe({

		if(input$chooseInput == 'fileUpload'){
			datapath = input$file$datapath
			filename = input$file$name
		} else if (input$chooseInput == 'fileMultiUpload') {
			datapath = values$fileMulti[[get_plot_num(), 'datapath']]
			filename = values$fileMulti[[get_plot_num(), 'name']]
		} else {
			datapath = NULL
		}

		if(!is.null(datapath)){
			log_activity('expression', 'begin load file')
			tryCatch({

				fileType <- tail(unlist(strsplit(x = filename, split = "[.]")), n=1)

				print(fileType)

				if(fileType == "xls" || fileType == "xlsx"){
					values$file <- read.xls(datapath, sheet=1)
				}
				else if(fileType == "csv"){
					values$file <- read.csv(datapath)
				}
				else{
					values$file <- read.delim(datapath)
				}

			}, 
			error = function(err){
				values$file <- NA
			}, finally = {
				log_activity('expression', 'end load file')
			})
		}

	})
	
	# update values$colClusterFile when a file is uploaded, set to NA if file cannot be read
	observe({
		if(!is.null(input$colClusterFile$datapath)){
			log_activity('expression', 'begin update colClusterFile')
			tryCatch({
			  values$colClusterFile <- read.tree(input$colClusterFile$datapath)
			  # Validate that labels match data file
			  if(input$chooseInput == 'fileUpload'){
			  	validate(need(!is.null(get_file()), paste(ERR_file_upload, dimensions_msg)))
			  } else if (input$chooseInput == 'fileMultiUpload') {
			  	validate(need(!is.null(get_file()), paste(ERR_file_multi_upload, dimensions_msg)))
			  }
			  data <- remove_strings(get_file())
			  data_cols = colnames(data)
			  # cluster_labels = values$colClusterFile$tip.label
			  cluster_labels = read.tree(input$colClusterFile$datapath)$tip.label
			  if(all(cluster_labels %in% data_cols)) {
			    update_col_clust()
			  } else {
			    values$colClusterFile <- NA
			    print("Error: with col cluster labels")
			  }
			}, 
			error = function(err){
				values$colClusterFile <- NA
			}, finally = {
				log_activity('expression', 'end update colClusterFile')
			})
		}
	})
	
	# update values$rowClusterFile when a file is uploaded, set to NA if file cannot be read
	observe({
		if(!is.null(input$rowClusterFile$datapath)){
			log_activity('expression', 'begin update rowClusterFile')
			tryCatch({
			  values$rowClusterFile <- read.tree(input$rowClusterFile$datapath)
			  # Validate that labels match data file
			  if(input$chooseInput == 'fileUpload'){
			  	validate(need(!is.null(get_file()), paste(ERR_file_upload, dimensions_msg)))
			  } else if (input$chooseInput == 'fileMultiUpload') {
			  	validate(need(!is.null(get_file()), paste(ERR_file_multi_upload, dimensions_msg)))
			  }
			  data <- remove_strings(get_file())
			  data_rows = rownames(data)
			  # cluster_labels = values$rowClusterFile$tip.label
			  cluster_labels = read.tree(input$rowClusterFile$datapath)$tip.label
			  if(all(cluster_labels %in% data_rows)) {
			    update_row_clust()
			  } else {
			    values$rowClusterFile <- NA
			    print("Error: with row cluster labels")
			  }
			}, 
			error = function(err){
				values$rowClusterFile <- NA
			}, finally = {
				log_activity('expression', 'end update rowClusterFile')
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

			log_activity('expression', 'update clustering')
		})
	})
	
	observe({
		if(input$chooseInput == 'fileUpload'){
			values$rowHclust <- NA
			values$colHclust <- NA
		}
	})
	
	################################## HELPER FUNCTIONS ##################################

	get_plot_num <- reactive({
		values$plotnum
	})

	# returns raw data from file input or selected example file
	get_file <- reactive({
		log_activity('expression', 'begin get_file')
		tryCatch({
			if(input$chooseInput == 'fileUpload'){
				file <- values$file
			} else if (input$chooseInput == 'fileMultiUpload') {
				# Multiple file upload
	# 			if(is.null(values$fileMulti[[get_plot_num(), 'datapath']])){
	# 				return(NULL)
	# 			}
	# 			file <- values$fileMulti[[get_plot_num(), 'datapath']]
				file <- values$file
			} else{ # Example
				file <- read.delim(file = input$exampleFiles, header = TRUE, sep = "\t")
			}
		},
		finally = {
			#log_activity('expression', 'end get_file')
		})
	})
	
	get_rowClusterFile <- reactive({
	  values$rowClusterFile
	})
	
	get_colClusterFile <- reactive({
	  values$colClusterFile
	})
	
	get_clusterFile <- function(margin) {
	  if(margin == 'row') {
	    get_rowClusterFile()
	  } else if (margin == 'col') {
	    get_colClusterFile()
	  }
	}
	
	# returns raw data from file input or selected example file
	get_image_weight <- reactive({
	  file <- get_file()
	  if (is.null(file)){
	    return(0)
	  }else{
	   adjust_plot_height_weight = scope * nrow(file) + intercept
	   return(adjust_plot_height_weight)
	  }
	})
	
	# converts file from data.frame to data.matrix
	# returns data matrix or NULL if non valid input
	get_data_matrix <- reactive({
		if(input$chooseInput == 'fileUpload'){
			validate(need(!is.null(get_file()), paste(ERR_file_upload, dimensions_msg)))
		} else if (input$chooseInput == 'fileMultiUpload') {
			validate(need(!is.null(get_file()), paste(ERR_file_multi_upload, dimensions_msg)))
		}
		tryCatch({
			data <- remove_strings(get_file())
			if(input$clusterMethod == 'import') {
			  data = reorder_data(data)
			}
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

		# Set number of colors to use in the color ramp palette, depending
		# on whether we are rendering a heatmap.2 or d3heatmap plot.
		if (input$tabSelections == 'Interactive') {
			# Rendering d3heatmap, so ignore user's chosen no. of shades.
			if (brightness_adj > 0) {
				num_colors = q*2 + 1
			} else {
				#num_colors = 3
				num_colors = q*2 + 1 # was 3 before, but use this since otherwise the color is off
			}
		} else {
			# Rendering heatmap.2
			num_colors = input$binNumber
		}

		if(input$colourScheme == 'red/green'){
		  lowCol = "#FF0000"
		  midCol = "#000000"
		  highCol = "#33FF00"
		}else if (input$colourScheme == 'blue/yellow'){
		  lowCol = "#0016DB"
		  midCol = "#FFFFFF"
		  highCol = "#FFFF00"
		}else if(input$colourScheme == 'grayscale'){
		  lowCol = "#000000"
		  midCol = "#bdbdbd"
		  highCol = "#FFFFFF"
		}else if(input$colourScheme == 'piyg'){
		  lowCol = "#C9438C"
		  midCol = "#f7f7f7"
		  highCol = "#7BC134"
		}else if(input$colourScheme == 'custom'){
		  lowCol = input$lowColour
		  midCol = input$midColour
		  highCol = input$highColour
		}

		if(input$colourScheme == 'rainbow' || input$colourScheme == 'topo'){
		  if(input$colourScheme == 'rainbow'){
		  cl = rainbow(num_colors)
		  }else{
		  cl = topo.colors(num_colors)
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
		 }else if(input$colourScheme == 'custom' || input$colourScheme == 'red/green' || input$colourScheme == 'blue/yellow' || input$colourScheme == 'grayscale' || input$colourScheme == 'piyg'){
		  if (brightness_adj == 0) {
			  lowCol = lowCol
			  midCol = midCol
			  highCol = highCol
			  colorRampPalette(c(lowCol, midCol, highCol))(num_colors)
		  } else if (brightness_adj < 0) {
			  lowCol = darken(lowCol, brightness_adj)
			  midCol = darken(midCol, brightness_adj)
			  highCol = darken(highCol, brightness_adj)
			  colorRampPalette(c(lowCol, midCol, highCol))(num_colors)
		  } else {
			  lowCol = lowCol
			  midCol = midCol
			  highCol = highCol
			  colorRampPalette(get_brightness_adjusted_color_set(lowCol, midCol, highCol, brightness_adj))(num_colors)
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
	
	get_hclust_from_file <- function(margin) {
	  clusterFile = get_clusterFile(margin)
		if(!is.empty(clusterFile)){
			tryCatch({
			  as.hclust.phylo(clusterFile) 
			}, 
			error = function(err){
				NA
			})
		}
		else{
			NULL
		}
	}
	
	# Reorder the data to match imported clusters
	reorder_data <- function(data) {
	  # TODO check and catch errors on label names: try doing this in file observer
	  # Reorder columns
	  if(!is.empty(get_colClusterFile()) && class(values$colHclust) == 'hclust') {
	    data = data[,values$colHclust$labels]
	  }
	  # Reorder Rows
	  if(!is.empty(get_rowClusterFile()) && class(values$rowHclust) == 'hclust') {
	    data = data[values$rowHclust$labels,]
	  }
	  data
	}

	# updated when change in file, dist method, or hclust method
	# sets values$rowMatrix and values$rowHclust
	update_row_clust <- reactive({
		if(!is.null(get_file())){
			values$rowMatrix <- get_data_matrix()
			if(input$clusterMethod == 'import') {
			  values$rowHclust <- get_hclust_from_file('row')
			} else {
			  values$rowHclust <- get_hclust( get_dist(values$rowMatrix) )
			}
		}
	})
	
	# updated when change in file, dist method, or hclust method
	# sets values$colMatrix and values$colHclust
	update_col_clust <- reactive({
		if(!is.null(get_file())){
			values$colMatrix <- t(get_data_matrix())
			if(input$clusterMethod == 'import') {
			  values$colHclust <- get_hclust_from_file('col')
			} else {
			  values$colHclust <- get_hclust( get_dist(values$colMatrix) )
			}
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
	
	is.empty <- function(x) {
	  is.null(x) || is.na(x)
	}
	
	# finds if a string is a current selected item in input$clusterSelectRC
	clust_selected <- function(rc){
	  if(input$clusterMethod == 'import') { 
	    !is.empty(get_clusterFile(rc))
	  } else {
	    if(length(grep(rc, input$clusterSelectRC))>0){
	      TRUE
	    }
	    else{
	      FALSE
	    }
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
    if(input$clusterMethod == 'import') {
			if(clust_selected("row") && !is.empty(get_rowClusterFile()) && !is.empty(values$rowHclust)){
				hr <- as.dendrogram(values$rowHclust)
			}
			if(clust_selected("col") && !is.empty(get_colClusterFile()) && !is.empty(values$colHclust)){
				hc <- as.dendrogram(values$colHclust)
			}
    }
    else if(input$clusterMethod != 'none'){
			if(clust_selected("row")){
				hr <- as.dendrogram(values$rowHclust)
			}
			if(clust_selected("col")){
				hc <- as.dendrogram(values$colHclust)
			}
		}
		list(hr,hc)
	})
	
	# to get the proper weight for font size of the lables
	get_label_weight <- reactive({
	  file <- get_file()
	  if (is.null(file)){
	    return(1)
	  }else{
	    each_row_size = input$plotHeight / nrow(file)
	    if (each_row_size > EACH_ROW_SIZE_LIMIT){
	       font_size = each_row_size/EACH_ROW_SIZE_LIMIT * MIN_FONT_SIZE
	       if (font_size > MAX_FONT_SIZE){
	         font_size = MAX_FONT_SIZE
	       }
	    }else{
	      font_size = MIN_FONT_SIZE
	    }
	    return(font_size)
	  }
	})
	
	# returns a heatmap.2 image based on get_data_matrix()
	get_plot <- function(){
		x <- get_data_matrix()
		log_activity('expression', 'begin get heatmap.2 plot') # call this after get_data_matrix()
		
		col_dendrogram_height = 100
		heatmap_height = input$plotHeight - col_dendrogram_height
		# creates a own color palette from red to green
		#print (paste("get_label_weight ", get_label_weight()))

		tryCatch({
			heatmap.2(x,
				na.color = input$missingColour, 
				key=TRUE, 
				symkey=FALSE, 
				density.info="none", 
				trace="none",
				key.title = input$distanceMethod,
				keysize=0.9, 
				offsetCol = 0, 
				offsetRow = 0,
				
				dendrogram = dend_select(),
				Rowv = get_dendrograms()[[1]], 
				Colv = get_dendrograms()[[2]], 
				col = get_colour_palette(),
				scale = input$scale,
				main = input$title,
				xlab = input$xlab, 
				ylab = input$ylab,
				cexRow = get_label_weight(),
				cexCol = get_label_weight(),
				lhei = c(col_dendrogram_height, heatmap_height) # set column dendrogram height relative to heatmap height
			)
		},
		error = function(err){
			print(paste("ERROR in get_plot(): ", err))
			validate(txt=ERR_plot_display)
		},
		finally = {
			log_activity('expression', 'end get heatmap.2 plot')
		})
		
		
	}
	
	# returns the result of ggdendrogram() on param x
	get_dendrogram_plot <- function(x, message){
		log_activity('expression', 'begin get_dendrogram_plot')
		tryCatch({
			if(input$chooseInput == 'fileUpload'){
				validate(need(!is.null(get_file()), paste(ERR_file_upload, dimensions_msg)))
			} else if (input$chooseInput == 'fileMultiUpload') {
				validate(need(!is.null(get_file()), paste(ERR_file_multi_upload, dimensions_msg)))
			}
			validate(need(!is.null(x), paste0("Select a clusting method and apply clustering to ", message, " to view this dendrogram")))
			validate(need(!is.na(x), ERR_file_read))

			x$labels <- strtrim(x$labels, 60)
			dend <- ggdendrogram(x, rotate = TRUE)
		
			return(dend)
		},
		finally = {
			log_activity('expression', 'end get_dendrogram_plot')
		})
	}
	
	################################## OUTPUT FUNCTIONS ##################################
	
	# Plot message advising user that they can adjust the height of the plot.
	output$plotMesage <- renderText(
		get_plot_message()
	)
	
	get_plot_message <- (
	  reactive({
	    file <- get_file()
	    if (input$chooseInput != 'fileMultiUpload' && !is.null(file) && nrow(file) > MANY_FILE_ROWS) {
		    "Tip: See below in Advanced Options to adjust plot size."
		  } else {
		  	""
		  }
	  })
	)
	
	# heatmap.2 plot
	output$heatmap <- renderPlot(
	  
		get_plot(),
		width =  reactive({input$plotWidth}),
		height = reactive({input$plotHeight})
	)
	
	# d3heatmap plot
	output$d3map <- renderD3heatmap({
		x <- get_data_matrix()
		
# 		print(length(x))
		
		validate(need(length(x) <= 400000, 
			"File is too large for this feature. Please select a smaller file with no more than 400,000 cells."))
		
		tryCatch({
			log_activity('expression', 'begin renderD3heatmap') # call this after get_data_matrix()
			d3heatmap(x, 
				Rowv = get_dendrograms()[[1]],
				Colv = get_dendrograms()[[2]],
				colors = get_colour_palette(),
				scale = input$scale,
				show_grid = FALSE,
				anim_duration = 0)
		}, 
		error = function(err){
			print(paste("ERROR: ", err))
			validate(txt=ERR_plot_display)
		},
		finally = {
			log_activity('expression', 'end renderD3heatmap') # though plot may not appear to user until a few seconds later
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

	output$currentFileNameLabel <- renderText({ 
		paste(values$fileMulti[[get_plot_num(), 'name']])
	})

	output$currentFilePositionLabel <- renderText({
		if (!is.null(nrow(values$fileMulti)) && nrow(values$fileMulti) > 0) {
			paste(get_plot_num(), "of", nrow(values$fileMulti))
		} else {
			""
		}
	})

	# display table
	output$table <- renderDataTable({
		log_activity('expression', 'renderDataTable')
		get_file()
	})
		
	# save plot
	output$plotDownload <- downloadHandler(
		
		filename = reactive({paste("heatmap.", input$downloadPlotFormat, sep="")}),
		
		content = function(file) {
			log_activity('expression', 'plotDownload')
			
			# Plot width and height are set in pixels, but for Retina/HiDPI displays, "pixels"
			# here refers to virtual pixels. A single virtual pixel may be rendered by more than
			# one physical pixel on the display. On a Retina/HiDPI display with a typical pixel
			# ratio of 2 (i.e. a 2x2 section of pixels for every standard/virtual pixel), the
			# plot is displayed at a nominal 144 pixels per inch (ppi) on screen (though the
			# exact number of inches as measured by a tape measure held up to the screen may vary
			# depending on the display model).
			#
			# For purpose of rendering a downloaded plot as JPEG, TIFF or PNG, we scale the plot
			# dimensions based on the user's chosen resolution. PNG, TIFF and JPEG files are
			# only saved with the appropriate resolution metadata with type = "cairo" or
			# type = "Xlib".
			ppi = as.numeric(input$downloadPlotResolution)
			plotWidthPixels = input$plotWidth
			plotHeightPixels = input$plotHeight
			widthInches = plotWidthPixels/72
			heightInches = plotHeightPixels/72
			
			if(input$downloadPlotFormat == "pdf"){
				if (plotHeightPixels > 14400) {
					stop("ERROR: Maximum height for downloaded PDF is 14,400 pixels (200 inches).")
				} else if (plotWidthPixels > 14400) {
					stop("ERROR: Maximum width for downloaded PDF is 14,400 pixels (200 inches).")
				}
				
				pdf(file, width=widthInches, height=heightInches)
				get_plot()
			} else {
				
				# Note: the true maximum values are slightly higher, but these are nice round numbers.
				plotFormatString = toupper(input$downloadPlotFormat)
				if (ppi == 72) {
					if (plotHeightPixels > 32000) {
						stop(paste("ERROR: Maximum height for downloaded", plotFormatString, "is 32,000 pixels at", ppi, "ppi."))
					}
				} else if (ppi == 144) {
					if (plotHeightPixels > 16000) {
						stop(paste("ERROR: Maximum height for downloaded", plotFormatString, "is 16,000 pixels at", ppi, "ppi. Decrease plot height and/or resolution."))
					}
				} else { # 300
					if (plotHeightPixels > 8000) {
						stop(paste("ERROR: Maximum height for downloaded", plotFormatString, "is 8,000 pixels at", ppi, "ppi. Decrease plot height and/or resolution."))
					}
				}
				
				if(input$downloadPlotFormat == "jpg"){
					jpeg(file, width=widthInches, height=heightInches, units = "in", res=ppi, type="cairo")
					get_plot()
				}
				else if(input$downloadPlotFormat == "tiff"){
					tiff(file, width=widthInches, height=heightInches, units = "in", res=ppi, compression="lzw", type="cairo")
					get_plot()
				}
				else{
					tryCatch({
						png(file, width=widthInches, height=heightInches, units = "in", res=ppi, type="cairo")
						get_plot()
					}, 
					error = function(err){
						print(paste("ERROR:  ", err))
						validate(need(FALSE,"Error generating PNG image."))
						return(NULL)
					})
				}
			}
		}
	)
	
	# save current file 
	output$tableDownload <- downloadHandler(
		filename = reactive({paste0("table.", input$downloadTableFormat)}),
		content = function(file){
			log_activity('expression', 'download data file')
			if(input$downloadTableFormat == "csv"){
				write.csv(get_file(), quote = FALSE, file = file, row.names = FALSE)
			}
			else{
				write.table(get_file(), sep = "\t", quote = FALSE, file = file, row.names = FALSE)
			}
		}
	)

})
