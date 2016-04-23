library(shiny)
library(gdata)
library(leaflet)
library(htmlwidgets)
library(ggtern)
library(MASS)

shinyServer(function(input, output, session){
	
	values <- reactiveValues(file = NULL)
		
	observe({
		input$clearFile
		values$file <- NULL
	})
	
	observe({
		values$file <- input$file
	})
	
	get_file <- reactive({
		if(input$chooseInput == 'example'){
			file <- read.delim(input$exampleFiles, header = TRUE, sep="\t", row.names = NULL)
		}
		else{
			validate(need(values$file$datapath, ERR_file_upload))
			
			fileType <- tail(unlist(strsplit(x = values$file$name, split = "[.]")), n=1)
			
			tryCatch({
				if(fileType == "xls" || fileType == "xlsx"){
					file <- read.xls(values$file$datapath, sheet=1)
				}
				else if(fileType == "csv"){
					file <- read.csv(values$file$datapath, header = TRUE)
				}
				else{
					file <- read.delim(values$file$datapath, header = TRUE, sep="\t", row.names = NULL)
				}
				
			},
			error = function(err){
				validate(txt = ERR_file_read)
			})
		}
		
		# get rid of NAs
		file <- na.omit(file)
		
		lat <- get_column(file, c("Latitude", "latitude", "Lat", "lat"))
		lon <- get_column(file, c("Longitude", "longitude", "Long", "long", "Lon", "lon"))
		
		validate(need(length(lat)>1, "File could not be read. Please ensure the file contains more than 1 row."))
		
		if(!is.null(lat) && !is.null(lon)){
			if(!is.null(file$Value)){
				points <- data.frame(
				Longitude = c(lon), 
				Latitude = c(lat), 
				Value = c(file$Value))
			}
			else{
				points <- data.frame(
				Longitude = c(lon), 
				Latitude = c(lat))
			}
		}
		else{
			validate(txt = "File could not be read. Please ensure the columns are correctly labeled.")
		}
		return(points)
	})
	
	get_column <- function(file, names_list){
		for(i in names_list){
			if(i %in% names(file)){
				return(file[[i]])
			}
		}
		NULL
	}

	# source: http://www.r-bloggers.com/interactive-maps-for-john-snows-cholera-data/
	get_density <- reactive({

		df <- get_file()
		x <- df[[1]]
		y <- df[[2]]

		nlevels <- input$binNumber 
		bandwidth <- c(bandwidth.nrd(x), bandwidth.nrd(y))*input$gaussianRadius 
		if(bandwidth[[1]] == 0){
			bandwidth[[1]] <- 0.00001
		}
		if(bandwidth[[2]] == 0){
			bandwidth[[2]] <- 0.00001
		}
		xmin <- min(x) - bandwidth[[1]]*nlevels
		xmax <- max(x) + bandwidth[[1]]*nlevels
		
		ymin <- min(y) - bandwidth[[2]]*nlevels
		ymax <- max(y) + bandwidth[[2]]*nlevels
		
		if(is.null(df$Value) || var(df$Value) == 0){
			dens <- kde2d(x, y, h = bandwidth, n = input$contourSmoothness, lims = c(xmin,xmax, ymin,ymax))
		}
		else{
			dens <- kde2d.weighted(x, y, df$Value, h = bandwidth, n = input$contourSmoothness, lims = c(xmin,xmax, ymin,ymax))
		}

		return(contourLines(x = dens$x, y = dens$y, z = dens$z, nlevels = nlevels))

	})

	
	# see if a given layer name is shown or hidden by user
	layer_selected <- function(name){
		if(length(grep(name, input$layers))>0){
			TRUE
		}
		else{
			FALSE
		}
	}
	
	get_fill_opacity <- reactive({
		if(layer_selected("showHeatmap")){
			input$fillOpacity
		}
		else{
			0
		}
	})
	
	get_contour_size <- reactive({
		if(layer_selected("showContours")){
			input$contourSize
		}
		else{
			0
		}
	})
	
	
	get_point_shapes <- function(m){
		
		if(layer_selected("showPoints")){
			df <- get_file()
			
			popup_message <- paste0("Latitude: ", df$Latitude, "<br/>Longitude: ", df$Longitude)
			if(!is.null(df$Value)){
				popup_message <- paste0(popup_message, "<br/>Value: ", df$Value)
			}
			
			m <- addCircles(m, 
				stroke = FALSE,
				fill = TRUE,
				fillOpacity = input$pointOpacity,
				radius = input$pointSize,
				popup = as.character(popup_message)
			)
			
		}
		
		return(m)
	}
	
	get_colours <- function(level_list){
		n <- length(level_list)
		
		if(input$colourScheme == 'custom'){
			palette <- colorRampPalette(c(input$lowColour, input$highColour))(n)
		}else if(input$colourScheme == 'red/green'){
		  palette <- colorRampPalette(c("#FF0000", "#000000", "#23B000"))(n)
		}else if(input$colourScheme == 'blue/yellow'){
		  palette <- colorRampPalette(c("#0016DB", "#FFFFFF", "#FFFF00"))(n)
		}else if(input$colourScheme == 'grayscale'){
		  palette <- colorRampPalette(c("#000000", "#bdbdbd", "#FFFFFF"))(n)
		}else if(input$colourScheme == 'piyg'){
		  palette <- colorRampPalette(c("#C9438C", "#f7f7f7", "#7BC134"))(n)
		}
		else if(input$colourScheme == 'rainbow'){
			palette <- substr(rev(rainbow(n, end = 5/6)), 0, 7)
		}
		else{
			palette <- substr(topo.colors(n), 0, 7)
		}
		
		names(palette) <- level_list
		
		return(palette)
	}
	
	get_contour_shapes <- function(m){
		cl <- get_density()
		max_cl <- length(cl)
		
		cl_levels <- as.character(unique(unlist(lapply(cl, function(x){x$level}))))

		colours <- get_colours(cl_levels)

		fill_op <- get_fill_opacity()
		contours <- get_contour_size()
		
		for(i in 1:max_cl){	
			m <- addPolygons(m, cl[[i]]$x,cl[[i]]$y, 
				fillColor = colours[[as.character(cl[[i]]$level)]], 
				fillOpacity = fill_op, weight = contours)
		}
		
		return(m)
	}

	# http://leaflet-extras.github.io/leaflet-providers/preview/index.html
	get_tiles <- function(m){
		get_file()
		
		m <- clearTiles(m)
		
		if(layer_selected("showMap")){
			m <- addProviderTiles(m, input$mapType)
			
			# prevent zooming out further than provider tile allows
			if(!is.null(input$map_zoom) && input$map_zoom <1){
				m %>% setView(0,0,1)
			}
		}
		
		return(m)
	}
	
	get_shapes <- function(m){
		
		m <- get_contour_shapes(m)
		
		m <- get_point_shapes(m)
		
		m
	}
	
	observe({
		if(input$tabSelections == 'Interactive'){
			m <- leafletProxy("map", session, get_file()) 
			m %>% clearShapes()
			get_shapes(m)
		}
	})
	
	observe({
		if(input$tabSelections == 'Interactive'){
			m <- leafletProxy("map", session) 
			get_tiles(m)
		}
	})
	
	output$map <- renderLeaflet({
		leaflet(get_file()) %>% fitBounds(~min(Longitude, na.rm = TRUE), ~min(Latitude, na.rm = TRUE), ~max(Longitude, na.rm = TRUE), ~max(Latitude, na.rm = TRUE))
	})
	
	output$table <- renderDataTable({
		get_file()
	})
	
	output$plotDownload <- downloadHandler(
		filename = function(){
			"geoHeatmap.html"
		},
		content = function(file) {
			m <- get_shapes(leaflet(get_file())) %>% get_tiles()
			
			saveWidget(m, file=file)
		}
	)
	
	output$tableDownload <- downloadHandler(
		filename = function(){
			"table.txt"
		},
		content = function(file) {
			write.table(get_file(), file, sep = "\t", row.names = FALSE, quote = FALSE)
		}
	)
})