library(shiny)
#library(ctc)
#library(ggmap)
#library(ggplot2)
library(xlsx)
library(jscolourR)
library(leaflet)
library(KernSmooth)

shinyServer(function(input, output, session){

	get_file <- reactive({
		if(input$chooseInput == 'example'){
			points <- data.frame(
				Longitude = c(-1+rnorm(50,0,.5),-2+rnorm(50,0,0.5),-4.5+rnorm(50,0,.5)),
				Latitude = c(52+rnorm(50,0,.5),54+rnorm(50,0,0.5),56+rnorm(50,0,.5))
				)
		}
		else{
			validate(need(input$file$datapath, "Please upload a file"))
			
			fileType <- tail(unlist(strsplit(x = input$file$name, split = "[.]")), n=1)
			
			if(fileType == "xls" || fileType == "xlsx"){
				file <- read.xlsx(input$file$datapath, 1)
			}
			else if(fileType == "csv"){
				file <- read.csv(input$file$datapath, header = TRUE)
			}
			else{
				file <- read.delim(input$file$datapath, header = TRUE, sep="\t", row.names = NULL)
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
	
	get_plot <- reactive({
		CL <- get_density()
		max_CL <- length(CL)

		m <- leaflet()
		
		for(i in 1:max_CL){	
			m	<- addPolygons(m, CL[[i]]$x,CL[[i]]$y)
		}
		m %>% clearShapes()
		return(m)
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
	
	get_contour_lines <- reactive({
		if(layer_selected("showContours")){
			input$contourSize
		}
		else{
			0
		}
	})
	
	get_shapes <- reactive({
		
		df <- get_file() 
		CL <- get_density()
		max_CL <- length(CL)
		colours <- colorRampPalette(c(input$lowColour, input$highColour))(max_CL)
		fill_op <- get_fill_opacity()
		contours <- get_contour_lines()
		m <- leafletProxy("map", session, df)
		m %>% clearShapes()

		for(i in 1:max_CL){	
			m	<- addPolygons(m, CL[[i]]$x,CL[[i]]$y, fillColor  = substr(x = colours[i], start=0, stop=7), fillOpacity = fill_op, weight = contours, 
				popup = paste("level:", CL[[i]]$level)) # for testing, remove later
		}
		
		if(layer_selected("showPoints")){
			m <-	addCircles(m, opacity = input$pointOpacity,radius =  input$pointSize,  weight = input$pointSize, popup = as.character(paste0("Latitude: ",df$Latitude, "<br/>Longitude: ", df$Longitude)))
		}
		
		return(m)
	})

	# http://leaflet-extras.github.io/leaflet-providers/preview/index.html
	get_tiles <- function(m){
		if(input$mapType == 'toner'){
			m %>% addProviderTiles("Stamen.Toner")
		}
		else if(input$mapType == 'positron'){
			m %>% addProviderTiles("CartoDB.Positron")
		}
		else if(input$mapType == 'watercolour'){
			m %>% addProviderTiles("Stamen.Watercolor")
		}
		else if(input$mapType == 'temperature'){
			m %>% addTiles() %>% addProviderTiles("OpenWeatherMap.Temperature")
		}
		else{
			m %>% addTiles()
		}	
	}
	
	observe({
		m <- get_shapes()
		
		m %>% clearTiles()
		if(layer_selected("showMap")){
			get_tiles(m)
		}
	})
	
	output$map <- renderLeaflet({
		get_plot()
	})
	
	output$table <- renderDataTable({
		get_file()
	})
	
	library(ggplot2)
	output$ggplot <- renderPlot({
		df <- get_file() 
		CL <- get_density()
		max_CL <- length(CL)
		colours <- colorRampPalette(c(input$lowColour, input$highColour))(max_CL)
		
		d <- data.frame(x = CL[[1]]$x, y = CL[[1]]$y, z = CL[[1]]$level)
		d2 <- data.frame(x = CL[[4]]$x, y = CL[[4]]$y, z = CL[[4]]$level)
		d <- rbind(d, d2)
		print(d)
		print(str(d))
		plot1 <- ggplot(data = df, aes(x = Latitude, y = Longitude)) + geom_blank() + geom_contour(aes(x = x, y = y, z = z), data = d)
		#for(i in 1:max_CL){	
		#	d <- data.frame(x = CL[[i]]$x, y = CL[[i]]$y, z = CL[[i]]$level) 
			
		#}
		plot1
	})
	
	output$download <- downloadHandler(
		filename = function(){
			"geoHeatmap.png"
		},
		content = function(file) {
			png(file)
			get_plot()
			dev.off()
		}
	)
})