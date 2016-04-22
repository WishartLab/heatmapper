library(shiny)
library(gdata)
library(ggplot2)
library(reshape2)
library(d3heatmap)
#library(shinyjs)

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
		input$uploadFormat
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
	
	read.coords <- function(filePath,	fileName = "txt") {
		fileType <- tail(unlist(strsplit(x = fileName, split = "[.]")), n=1)
		
		# Initial read of file.
		if(fileType == "xls" || fileType == "xlsx"){
			data_file <- read.xls(filePath, sheet=1, header = FALSE, row.names = NULL)
		}
		else if(fileType == "csv"){
			data_file <- read.csv(filePath, header = FALSE, row.names = NULL)
		}
		else{
			data_file <- read.delim(filePath, header = FALSE, row.names = NULL, sep="\t")
		}
		
		use_col_labels <<- FALSE
		use_row_labels <<- FALSE
		
		row1 = as.character(unlist(data_file[1,]))
		isNonNumeric = suppressWarnings(is.na(as.numeric(row1)))
		
		if (length(row1) == 1) {
			if (isNonNumeric == TRUE) {
				use_col_labels <<- TRUE # Assume first row is header row
			}
		} else {
			# Check elements from the 2nd one to the last one.
			# If any are not numbers, we assume this is a header row.
			if (sum(isNonNumeric[2:length(isNonNumeric)]) > 0) {
				use_col_labels <<- TRUE
			}
		}
		
		col1 = as.character(data_file[,1])
		isNonNumeric = suppressWarnings(is.na(as.numeric(col1)))
		
		if (length(col1) == 1) {
			if (isNonNumeric == TRUE) {
				use_row_labels <<- TRUE # Assume first column is a labels column.
			}
		} else {
			# Check elements from the 2nd one to the last one.
			# If any are not numbers, we assume this is a labels column.
			if (sum(isNonNumeric[2:length(isNonNumeric)]) > 0) {
				use_row_labels <<- TRUE
			}
		}
		
		if ("useColLabels" %in% input$labels) {
			use_col_labels <<- TRUE
		}
		
		if ("useRowLabels" %in% input$labels) {
			use_row_labels <<- TRUE
		}
		
		row.names.val = -99
		if (use_row_labels == FALSE) {
			row.names.val <- NULL
		} else {
			row.names.val <- 1
		}
		
		# Read the file again, this time with header/row labels specified.
		if(fileType == "xls" || fileType == "xlsx"){
			read.xls(filePath, sheet=1, header = use_col_labels, row.names = row.names.val)
		}
		else if(fileType == "csv"){
			read.csv(filePath, header = use_col_labels, row.names = row.names.val)
		}
		else{
			read.delim(filePath, header = use_col_labels, row.names = row.names.val, sep="\t")
		}
		
	}
	
	# Returns TRUE if the given file looks like a PDB file; otherwise FALSE.
	is.pdb <- function(filePath) {
		# Traverse file and look for characteristic lines.
		con <- file(filePath, "rt")
		while(TRUE) {
			line = readLines(con, 1) # Read one line
			if (length(line) >= 54) {
				if (substr(line, 1,6) == 'ATOM  ' || substr(line, 1,6) == 'HETATM') {
					if (substr(line, 13, 16) == ' CA ') { # require at least 1 C-alpha atom
						if (grepl("^ *[0-9]+ *$", substr(line, 7, 11), perl=TRUE) &&
								grepl("\\S", substr(line, 22, 22), perl=TRUE) &&
								grepl("^ *[0-9]+ *$", substr(line, 23, 26), perl=TRUE) &&
								grepl("^ *[0-9]+\\.[0-9]+ *$", substr(line, 31, 38), perl=TRUE) &&
								grepl("^ *[0-9]+\\.[0-9]+ *$", substr(line, 39, 46), perl=TRUE) &&
								grepl("^ *[0-9]+\\.[0-9]+ *$", substr(line, 47, 54), perl=TRUE)
								) {
							close(con)
							return(TRUE)
						}
					}
				}
			}
		}
		close(con)
		return(FALSE)
	}
	
	# returns table of 3-D coordinates of the first chain in the given PDB file
	read.pdb <- function(filePath) {
		
		# Traverse file and count how many atoms we want to use.
		con <- file(filePath, "rt")
		count = 0
		while(TRUE) {
			line = readLines(con, 1) # Read one line
			if (length(line) == 0) {
				break
			}
			if (substr(line, 1,6) == 'ATOM  ' || substr(line, 1,6) == 'HETATM') {
				if (use_atom(line)) {
					count = count + 1
				}
			} else if (substr(line, 1,3) == 'TER') {
				break
			}
		}
		
		# Create data frame to hold atom coordinates
		coords = as.data.frame(matrix(NA, nrow=count, ncol=3))
		rownames(coords) = paste("old", rownames(coords)) # set names of rows to ones we will not use
		
		# Traverse the file again and parse the atoms.
		num_atoms_parsed = 0
		seek(con, where = 0) # jump to start of file again
		while(TRUE) {
			line = readLines(con, 1) # Read one line
			if (length(line) == 0) {
				break
			} else if (substr(line, 1,6) == 'ATOM  ' || substr(line, 1,6) == 'HETATM') {
				
				if (!use_atom(line)) {
					next
				}
				
				if (input$atomSelect == 'ca') {
					id = trim(substr(line, 23, 26)) # use residue id
				} else {
					id = trim(substr(line, 7, 11)) # use atom id
				}

				num_atoms_parsed = num_atoms_parsed + 1
				coords[num_atoms_parsed,1] = as.numeric(substr(line, 31, 38))
				coords[num_atoms_parsed,2] = as.numeric(substr(line, 39, 46))
				coords[num_atoms_parsed,3] = as.numeric(substr(line, 47, 54))
				rownames(coords)[num_atoms_parsed] <- id
				
			} else if (substr(line, 1,3) == 'TER') {
				break
			}
		}
		close(con)
		return(coords)
	}
	
	# Given a line from a PDB file, return whether we want to use it.
	use_atom <- function(line) {
		if (input$atomSelect == 'ca') {
			if (!(substr(line, 13, 16) == ' CA ')) {
				return (FALSE)
			}
		} else if (input$atomSelect == 'bb') {
			atom = substr(line, 13, 16)
			if (!(atom == ' N  ' || atom == ' CA ' || atom == ' C  ' || atom == ' O  ')) {
				return (FALSE)
			}
		}
		return (TRUE)
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
			
			if (input$uploadFormat == "pdb") {
			#if (is.pdb(values$file$datapath)) {
				file <- read.pdb(values$file$datapath)
			} else {
				file <- read.coords(values$file$datapath, values$file$name)
			}
		}
		
		if (input$chooseInput != 'example') {
			if (input$matType == "distMat") {
				# calculate distance matrix
				file <- dist(file, diag = TRUE, upper = TRUE)
				file <- as.data.frame(as.matrix(file))
				file <- cbind(rownames(file), file)
			} else if (input$matType == "corrMat") {
				# calculate correlation matrix
				file <- cor(file, file)
				file <- as.data.frame(as.matrix(file))
				file <- cbind(rownames(file), file)
			} else {
				# Will display as-is.
				file <- t(file) # Transpose the data so it will be shown in the same orientation as in the input file.
				file <- as.data.frame(as.matrix(file))
				file <- cbind(rownames(file), file)
			}
		}
		
		colnames(file)[1] <- "cols"
		
		return(file)
	})

	# melt data
	melt_file <- reactive({
		file <- get_file()
		if(input$reverseOrder){
			file <- rev(file)
		}

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
		else if(input$colourScheme == 'red/green'){
		  lowColour = "#FF0000"
		  midColour = "#000000"
		  highColour = "#23B000"
		  
		  return(colorRampPalette(c(lowColour, midColour, highColour))(3))
		}else if(input$colourScheme == 'blue/yellow'){
		  lowColour = "#0016DB"
		  midColour = "#FFFFFF"
		  highColour = "#FFFF00"
		  
		  return(colorRampPalette(c(lowColour, midColour, highColour))(3))
		}else{
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
		validate(need(!is.null(data), ERR_file_upload))
		
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
		print(length(file))
		validate(need(length(file)^2 <= 50000, 
			"File is too large for this feature. Please select a smaller file with no more than 50,000 cells."))
		
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
		get_file()	
	})

	get_plot_download_name <- function(){
		paste0("pairwise.", input$downloadPlotFormat)
	}

	output$plotDownload <- downloadHandler(
		filename = reactive({get_plot_download_name()}),
		content = function(file){
			ggsave(file, get_plot(), width = input$plotWidth/72, height = input$plotHeight/72)
		}
	)
	
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
	
	trim <- function (x) gsub("^\\s+|\\s+$", "", x)
	
})
