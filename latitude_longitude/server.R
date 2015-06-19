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
		dens <- bkde2D(df, bandwidth=c(bw.ucv(df[,1]),bw.ucv(df[,2])))
		CL <- contourLines(x = dens$x1, y = dens$x2, z = dens$fhat)
	
		max_CL <- length(CL)
		colours <- colorRampPalette(c("yellow", "red"))(max_CL)
		
		m <- leaflet(df) %>% addTiles() %>% addCircles() 
		
		for(i in 1:max_CL){	
			m	<- addPolygons(m, CL[[i]]$x,CL[[i]]$y, fillColor  = substr(x = colours[i], start=0, stop=7), stroke = FALSE, fillOpacity = i/(max_CL*2)) 
		}
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
			paste0("geoHeatmap.", input$downloadType)
		},
		content = function(file) {
			if(input$downloadType == 'pdf'){
				pdf(file)
			}
			else{
				png(file)
			}
			plot(get_plot())
			dev.off()
		}
	)
})