library(shiny)
library(xlsx)
library(jscolourR)
library(leaflet)
library(KernSmooth)
library(htmlwidgets)

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
			points <- data.frame(
				Longitude = c(-1+rnorm(50,0,.5),-2+rnorm(50,0,0.5),-4.5+rnorm(50,0,.5)),
				Latitude = c(52+rnorm(50,0,.5),54+rnorm(50,0,0.5),56+rnorm(50,0,.5))
			)
		}
		else{
			validate(need(values$file$datapath, "Please upload a file"))
			
			fileType <- tail(unlist(strsplit(x = values$file$name, split = "[.]")), n=1)
			
			if(fileType == "xls" || fileType == "xlsx"){
				file <- read.xlsx(values$file$datapath, 1)
			}
			else if(fileType == "csv"){
				file <- read.csv(values$file$datapath, header = TRUE)
			}
			else{
				file <- read.delim(values$file$datapath, header = TRUE, sep="\t", row.names = NULL)
			}
			points <- data.frame(
				Longitude = c(file$Longitude), 
				Latitude = c(file$Latitude))
		}
		return(points)
	})

	# source: http://www.r-bloggers.com/interactive-maps-for-john-snows-cholera-data/
	get_density <- reactive({
		df <- get_file()
		bandwidth <- c(bw.ucv(df[,1]),bw.ucv(df[,2]))*input$gaussianRadius
		nlevels <- input$binNumber 
		
		xmin <- min(df$Longitude) - bandwidth[[1]]*nlevels
		xmax <- max(df$Longitude) + bandwidth[[1]]*nlevels
		
		ymin <- min(df$Latitude) - bandwidth[[2]]*nlevels
		ymax <- max(df$Latitude) + bandwidth[[2]]*nlevels

		dens <- bkde2D(df, range.x = list(c(xmin,xmax), c(ymin,ymax)),
			bandwidth = bandwidth, 
			gridsize =c(51,51)*input$contourSmoothness)
		return(contourLines(x = dens$x1, y = dens$x2, z = dens$fhat, nlevels = nlevels))
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
			print("GETPOINTS")
			df <- get_file()
			m <- addCircles(m, opacity = input$pointOpacity, 
				radius =  input$pointSize,  
				weight = input$pointSize, 
				popup = as.character(paste0("Latitude: ", df$Latitude, "<br/>Longitude: ", df$Longitude)))
		}
		
		return(m)
	}
	
	get_colours <- function(level_list){
		n <- length(level_list)
		
		if(input$colourScheme == 'custom'){
			palette <- colorRampPalette(c(input$lowColour, input$highColour))(n)
		}
		else if(input$colourScheme == 'rainbow'){
			palette <- rev(rainbow(n, end = 5/6))
		}
		else{
			palette <- rev(topo.colors(n))
		}
		
		# trim colours to #______ format
		palette <- substr(palette, 0, 7)
		names(palette) <- level_list
		
		return(palette)
	}
	
	get_contour_shapes <- function(m){
		print("GETCONTOURS")
		
		CL <- get_density()
		max_CL <- length(CL)
		
		CL_levels <- as.character(unique(unlist(lapply(CL, function(x){x$level}))))

		colours <- get_colours(CL_levels)
		fill_op <- get_fill_opacity()
		contours <- get_contour_size()
		
		for(i in 1:max_CL){	
			m <- addPolygons(m, CL[[i]]$x,CL[[i]]$y, 
				fillColor = colours[[as.character(CL[[i]]$level)]], 
				fillOpacity = fill_op, weight = contours)
		}
		
		return(m)
	}

	# http://leaflet-extras.github.io/leaflet-providers/preview/index.html
	get_tiles <- function(m){
		
		m <- clearTiles(m)
		
		if(layer_selected("showMap")){
			m <- addProviderTiles(m, input$mapType, options = providerTileOptions(noWrap = TRUE))
			
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
		m <- leafletProxy("map", session, get_file()) 
		m %>% clearShapes()
		get_shapes(m)
	})
	
	observe({
		m <- leafletProxy("map", session) 
		get_tiles(m)
	})
	
	output$map <- renderLeaflet({
		leaflet(get_file()) %>% fitBounds(~min(Longitude, na.rm = TRUE), ~min(Latitude, na.rm = TRUE), ~max(Longitude, na.rm = TRUE), ~max(Latitude, na.rm = TRUE))
	})
	
	output$table <- renderDataTable({
		get_file()
	})
	
	output$download <- downloadHandler(
		filename = function(){
			"geoHeatmap.html"
		},
		content = function(file) {
			m <- get_shapes(leaflet(get_file())) %>% get_tiles()
			
			saveWidget(m, file=file)
		}
	)
})