library(shiny)
library(gdata)
library(ggplot2)
library(reshape2)
library(d3heatmap)

library(gplots)
library(ggdendro)
library(stringr)
#library(shinyjs)

source("../global_server.R")

# Constants
dimensions_msg <- "Input data can have up to 300 rows and 500 columns for distance matrix, or 500 rows and 300 columns for correlation matrix."
q = 5;

shinyServer(function(input, output, session){

	#################### GLOBAL REACTIVE VALUES ####################
	values <- reactiveValues(
		file = NULL,
		fileMulti = NULL,
		plotnum = 1 # For multiple file upload. First plot is 1.
	)
	
	#################### OBSERVERS ####################
	observe({
		input$clearFile
		values$file <- NULL
		log_activity('pairwise', 'clearFile')
	})

	observe({
		input$clearFileMulti
		values$fileMulti <- NULL
		log_activity('pairwise', 'clearFileMulti')
	})

	observe({
		input$uploadFormat
		values$file <- NULL
		log_activity('pairwise', 'uploadFormat')
	})
	
	observe({
		values$file <- input$file
		log_activity('pairwise', 'input$file')
	})
	
	observe({
		if (is.null(input$fileMulti)) {
			values$fileMulti <- NULL
		} else {
			values$fileMulti <- input$fileMulti[with(input$fileMulti, order(name)), ] # sort by file name as we copy the data table
		}
		log_activity('pairwise', 'fileMulti')
	})

	observeEvent(input$cyclePlotsStart, {
		if (is.null(nrow(values$fileMulti))) {
			values$plotnum
		} else if (values$plotnum > 1) {
			values$plotnum <- 1
		}
		log_activity('pairwise', 'cyclePlotsStart')
	})

	observeEvent(input$cyclePlotsLeft, {
		if (is.null(nrow(values$fileMulti))) {
			values$plotnum
		} else if (values$plotnum > 1) {
			values$plotnum <- values$plotnum - 1
		}
		log_activity('pairwise', 'cyclePlotsLeft')
	})

	observeEvent(input$cyclePlotsRight, {
		if (is.null(nrow(values$fileMulti))) {
			values$plotnum
		} else if (values$plotnum < nrow(values$fileMulti)) {
			values$plotnum <- values$plotnum + 1
		}
		log_activity('pairwise', 'cyclePlotsRight')
	})

	observeEvent(input$cyclePlotsEnd, {
		if (is.null(nrow(values$fileMulti))) {
			values$plotnum
		} else if (values$plotnum < nrow(values$fileMulti)) {
			values$plotnum <- nrow(values$fileMulti)
		}
		log_activity('pairwise', 'cyclePlotsEnd')
	})

	#################### FILE INPUT FUNCTIONS ####################
	# read a file given a file name
	read_file <- function(filePath, fileName = "txt") {
		log_activity('pairwise', 'begin read_file')
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
		
		file <- read.table(filePath, header = header, sep = sep)
		rname <- file[,1]
		if (!is.numeric(rname[1]) & !is.na(rname[1])){ # check the second line's first item, numeric or not, if not, it is a rowname
		  file <- read.table(filePath, header = header, row.names=1, sep = sep)
		}
		log_activity('pairwise', 'end read_file')
		file
	}
	
	read.coords <- function(filePath,	fileName = "txt") {
		log_activity('pairwise', 'begin read.coords')
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
			ret <- read.xls(filePath, sheet=1, header = use_col_labels, row.names = row.names.val)
		}
		else if(fileType == "csv"){
			ret <- read.csv(filePath, header = use_col_labels, row.names = row.names.val)
		}
		else{
			ret <- read.delim(filePath, header = use_col_labels, row.names = row.names.val, sep="\t")
		}
		
		log_activity('pairwise', 'end read.coords')
		return(ret)
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
		log_activity('pairwise', 'begin read.pdb')
		
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
		log_activity('pairwise', 'end read.pdb')
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
	
	# Read multi-FASTA file and compute k-mer count statistics. The k-mer counts are
	# intended to be used in a distance matrix for alignment-free phylogenetic analysis.
	read.fasta <- function(filePath) {
		log_activity('pairwise', 'begin read.fasta')
	
		k = input$kmerSelect # k-mer length

		# Traverse file and count the number of sequences
		con <- file(filePath, "rt")
		count = 0
		while(TRUE) {
			line = readLines(con, 1) # Read one line
			if (length(line) == 0) {
				break
			}
			if (substr(line, 1,1) == '>') {
				count = count + 1
			}
		}

		# Compute number of columns needed based on k-mer length chosen
		col.count = 4**k # assume DNA with only ATGC

		# Create data frame to hold the k-mer count stats
		kmerstats = as.data.frame(matrix(0, nrow=count, ncol=col.count))
		rownames(kmerstats) = paste("old", rownames(kmerstats)) # set names of rows to ones we will not use

		# Traverse the file again and parse the sequences.
		num_seqs_parsed = 0
		seq = ''
		id = ''
		seek(con, where = 0) # jump to start of file again
		while(TRUE) {
			line = readLines(con, 1) # Read one line
			if (length(line) == 0) {

				# Process the previous sequence
				if (num_seqs_parsed > 0) {
					seq = toupper(seq)
					for (i in 1:(nchar(seq) - k + 1) ) {
						kmer = substr(seq, i,i+k-1)
						if (!is.na(str_match(kmer, "^[ATGC]+$"))) {
							idx = get_kmer_index(kmer)
							kmerstats[num_seqs_parsed, idx] = kmerstats[num_seqs_parsed, idx] + 1
						}
					}

					# Normalize by sequence length (or rather total number of k-mers in the sequence)
					kmerstats[num_seqs_parsed,] = kmerstats[num_seqs_parsed,]/(nchar(seq) - k + 1)

					rownames(kmerstats)[num_seqs_parsed] <- id
				}

				# done parsing file
				break
			} else if (substr(line, 1,1) == '>') {

				# Process the previous sequence
				if (num_seqs_parsed > 0) {
					seq = toupper(seq)
					for (i in 1:(nchar(seq) - k + 1) ) {
						kmer = substr(seq, i,i+k-1)
						if (!is.na(str_match(kmer, "^[ATGC]+$"))) {
							idx = get_kmer_index(kmer)
							kmerstats[num_seqs_parsed, idx] = kmerstats[num_seqs_parsed, idx] + 1
						}
					}

					# Normalize by sequence length (or rather total number of k-mers in the sequence)
					kmerstats[num_seqs_parsed,] = kmerstats[num_seqs_parsed,]/(nchar(seq) - k + 1)

					rownames(kmerstats)[num_seqs_parsed] <- id
				}

				# parse new FASTA entry
				num_seqs_parsed = num_seqs_parsed + 1
				seq = ''
				id = str_match(line, "^>(\\S+)")[2]
				if (is.na(id)) {
					id = toString(num_seqs_parsed)
				}
			} else {
				# sequence line - append to the parsed sequence
				seq_part = str_match(line, "([A-Za-z]+)")[2]
				seq = paste(seq, seq_part, sep="")
			}
		}
		close(con)
		log_activity('pairwise', 'end read.fasta')
# 		print(kmerstats)
		return(kmerstats)
	}

	# Given a k-mer (uppercase string of DNA nucleotides), calculate its index in our
	# data frame. The data frame is arranged thus:
	#
	# AAA
	# AAT
	# AAG
	# AAC
	# ATA
	# ATT
	# ATG
	# ...
	#
	get_kmer_index <- function(kmer) {
		k_val = nchar(kmer)
		idx = 1
		for (i in 1:k_val) {
			char = substr(kmer, k_val-i+1, k_val-i+1) # going in reverse
			if (char == 'A') {
				idx = idx + 0
			} else if (char == 'T') {
				idx = idx + 1*(4**(i-1))
			} else if (char == 'G') {
				idx = idx + 2*(4**(i-1))
			} else if (char == 'C') {
				idx = idx + 3*(4**(i-1))
			}
		}
		return(idx)
	}

	get_plot_num <- reactive({
		values$plotnum
	})

	# retrieve original data
	get_file <- reactive({
		log_activity('pairwise', 'begin get_file')
		if(input$chooseInput == 'example'){
			file <- read_file(input$exampleFiles)
		}
		else if(input$chooseInput == 'fileUpload'){
			if(is.null(values$file$datapath)){
				log_activity('pairwise', 'end get_file')
				return(NULL)
			}
			
			if (input$uploadFormat == "pdb") {
			#if (is.pdb(values$file$datapath)) {
				file <- read.pdb(values$file$datapath)
			} else if (input$uploadFormat == "fasta") {
				file <- read.fasta(values$file$datapath)
			} else {
				file <- read.coords(values$file$datapath, values$file$name)
			}
		} else {
			# Multiple file upload
			if(is.null(values$fileMulti[[get_plot_num(), 'datapath']])){
				log_activity('pairwise', 'end get_file')
				return(NULL)
			}
			file <- read.coords(values$fileMulti[[get_plot_num(), 'datapath']], values$fileMulti[[get_plot_num(), 'name']])
		}
		
		#if (input$chooseInput != 'example') {
			if (input$matType == "distMat") {
				# calculate distance matrix
			  if(input$distanceMethod == 'euclidean' || input$distanceMethod == 'manhattan'){
			    file <- dist(file,  method = input$distanceMethod, diag = TRUE, upper = TRUE)
			  }
			  else{
			    file <- as.dist(1-cor(t(data.matrix(file)), method=input$distanceMethod), diag = TRUE, upper = TRUE)
			  }
				#file <- dist(file,  method = input$distanceMethod, diag = TRUE, upper = TRUE)  # old code for "euclidean" only
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
		#}
		
		colnames(file)[1] <- "cols"
		
		log_activity('pairwise', 'end get_file')
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
	
	get_colour_palette <- function() {
	  log_activity('pairwise', 'get_colour_palette')
	  brightness_adj = as.integer(input$plotBrightness)
	  
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
	  }else if(input$colourScheme == 'custom' || input$colourScheme == 'red/green' || input$colourScheme == 'blue/yellow' || input$colourScheme == 'grayscale' || input$colourScheme == 'piyg'){
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
	  
	}
	
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
    #print(arr)
	  return (apply(arr, 1, tuple2hex))
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
	
	# Convert array of 3 integers, representing R, G, and B values, to
	# a hexidecimal-based string representing the color.
	tuple2hex <- function(c) {
	  return (toupper(sprintf("#%02x%02x%02x",
	                          as.integer(c[1]), as.integer(c[2]), as.integer(c[3])
	  )));
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
	  log_activity('pairwise', 'begin get_plot')
		data <- melt_file()
		
		if(input$chooseInput == 'fileUpload'){
			validate(need(!is.null(data), paste(ERR_file_upload, dimensions_msg)))
		} else if (input$chooseInput == 'fileMultiUpload') {
			validate(need(!is.null(data), paste(ERR_file_multi_upload, dimensions_msg)))
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
		}else{
		  q <- q + theme(
		    axis.text.x=element_text(angle = 90, hjust = 1))
		}
		if(!layer_selected('showLegend')){
			q <- q + guides(fill=FALSE)
		}
		
		log_activity('pairwise', 'end get_plot')
		return(q)
	})
	
	#################### OUTPUT FUNCTIONS ####################
	output$d3map <- renderD3heatmap({
		log_activity('pairwise', 'begin renderD3heatmap')
		file <- get_file()
		print(length(file))
		validate(need(length(file)^2 <= 50000, 
			"File is too large for this feature. Please select a smaller file with no more than 50,000 cells."))
		
		if(is.null(file)){
			return(NULL)
		}
		row.names(file)<- file[,1]
		file <- file[,-1]
		
		tryCatch({
			d3heatmap(file,colors = get_colour_palette(), Colv = NULL, Rowv = NULL)
		},
		finally = {
			log_activity('pairwise', 'end renderD3heatmap')
		})
	})
	
	output$map <- renderPlot({
		get_plot()
	}, width = reactive({input$plotWidth}), height = reactive({input$plotHeight}))

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
	
	output$table <- renderDataTable({
		log_activity('pairwise', 'renderDataTable')
		get_file()	
	})

	get_plot_download_name <- function(){
		paste0("pairwise.", input$downloadPlotFormat)
	}

	output$plotDownload <- downloadHandler(
		filename = reactive({get_plot_download_name()}),
		content = function(file){
			log_activity('pairwise', 'plotDownload')
			
			if(input$downloadPlotFormat == "pdf"){
				ggsave(file, get_plot(), width = input$plotWidth/72, height = input$plotHeight/72)
			} else {
				ppi = as.numeric(input$downloadPlotResolution)
				widthInches = input$plotWidth/72
				heightInches = input$plotHeight/72
				
				ggsave(file, get_plot(), width = widthInches, height = heightInches, units = "in", dpi=ppi, type="cairo")
			}
		}
	)
	
	output$tableDownload <- downloadHandler(
		filename = reactive({paste0("table.", input$downloadTableFormat)}),
		content = function(file){
			log_activity('pairwise', 'tableDownload')
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
