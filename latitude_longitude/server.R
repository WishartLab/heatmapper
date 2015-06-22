library(shiny)
#library(ctc)
#library(ggmap)
#library(ggplot2)
library(xlsx)
library(jscolourR)
library(leaflet)
library(KernSmooth)

shinyServer(function(input, output, session){

	get_file <- function(){
		if(input$chooseInput == 'example'){
			points <- data.frame(
				Longitude = c(-1+rnorm(50,0,.5),-2+rnorm(50,0,0.5),-4.5+rnorm(50,0,.5)),
				Latitude = c(52+rnorm(50,0,.5),54+rnorm(50,0,0.5),56+rnorm(50,0,.5))
				)
		}
		else{
			#file <- read.table("data/latlong.txt", header = TRUE, sep="\t")
			
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
	}
	
	get_plot <- function() {
		df <- get_file() 
		
		# source: http://www.r-bloggers.com/interactive-maps-for-john-snows-cholera-data/
		dens <- bkde2D(df, bandwidth=c(bw.ucv(df[,1]),bw.ucv(df[,2])))
		CL <- contourLines(x = dens$x1, y = dens$x2, z = dens$fhat)
	
		max_CL <- length(CL)
		colours <- colorRampPalette(c(input$lowColour, input$highColour))(max_CL)
		fill_op <- input$fillOpacity
		
		m <- leaflet(df)
		
		if(input$showMap){
			m <- addTiles(m)
		}
		
		for(i in 1:max_CL){	
			m	<- addPolygons(m, CL[[i]]$x,CL[[i]]$y, fillColor  = substr(x = colours[i], start=0, stop=7), fillOpacity = fill_op, weight = input$contourSize) 
		}
		
		m <- addCircles(m, opacity = input$pointOpacity, weight = input$pointSize, popup = as.character(paste0("Latitude: ",df$Latitude, "<br/>Longitude:", df$Longitude)))
		return(m)
	}
	output$map <- renderLeaflet({
		get_plot()
	})
	
	
	output$table <- renderDataTable({
		get_file()
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