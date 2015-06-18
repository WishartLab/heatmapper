library(shiny)
library(ctc)
library(ggmap)
library(xlsx)
library(jscolourR)

shinyServer(function(input, output, session){

	get_cmFile <- function(){
		if(input$cmChooseInput == 'cmExample'){
			points <- data.frame(
				Longitude=c(-1+rnorm(50,0,.5),-2+rnorm(50,0,0.5),-4.5+rnorm(50,0,.5)),
				Latitude =c(52+rnorm(50,0,.5),54+rnorm(50,0,0.5),56+rnorm(50,0,.5))
				)
		}
		else{
			#file <- read.table("data/latlong.txt", header = TRUE, sep="\t")
			
			validate(need(input$cmFile$datapath, "Please upload a file"))
			
			fileType <- tail(unlist(strsplit(x = input$cmFile$name, split = "[.]")), n=1)
			
			if(fileType == "xls" || fileType == "xlsx"){
				file <- read.xlsx(input$cmFile$datapath, 1)
			}
			else if(fileType == "csv"){
				file <- read.csv(input$cmFile$datapath, header = TRUE)
			}
			else{
				file <- read.delim(input$cmFile$datapath, header = TRUE, sep="\t", row.names = NULL)
			}
			points <- data.frame(
				Longitude = c(file$Longitude), 
				Latitude = c(file$Latitude))
		}
		return(points)
	}
	
	cm_ranges <- reactiveValues(x = NULL, y = NULL)
	
	get_cm_plot <- function(){
		points <- get_cmFile()
		
		map <- get_map(
			location = c(
				lon = median(na.rm = TRUE, points$Longitude), 
				lat = median(na.rm = TRUE, points$Latitude)), 
			zoom = input$cmZoom, 
			maptype = input$cmType
			)
		
		ggmap(map) + 
		geom_point(data = points, aes(x = Longitude, y = Latitude), size = input$cmPointSize) +
		geom_density2d(data = points, aes(x = Longitude, y = Latitude), size = input$cmContourSize) +
		stat_density2d(data = points, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), 
			size = 0.01, bins = 16, geom = "polygon") + 
		scale_fill_gradient(breaks = NULL, low = input$cmLowColour, high = input$cmHighColour) + 
    scale_alpha(range = c(0, 0.3), guide = FALSE)
	}
	
	get_zoom_cm_plot <- function(){
		get_cm_plot() + coord_cartesian(xlim = cm_ranges$x, ylim = cm_ranges$y)
	}
	
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$cm_dblclick, {
    brush <- input$cm_brush
    if (!is.null(brush)) {
      cm_ranges$x <- c(brush$xmin, brush$xmax)
      cm_ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      cm_ranges$x <- NULL
      cm_ranges$y <- NULL
    }
  })
	
	
	output$continuousMap <- renderPlot({
		get_cm_plot()
		
	})
	
	output$continuousMapZoom <- renderPlot({
		get_zoom_cm_plot()
	})
	
	output$continuousTable <- renderDataTable({
		get_cmFile()
	})
	
	output$cmDownload <- downloadHandler(
		filename = function(){
			paste0("geoHeatmap.", input$cmDownloadType)
		},
		content = function(file) {
			if(input$cmDownloadType == 'pdf'){
				pdf(file)
			}
			else{
				png(file)
			}
			plot(get_cm_plot())
			dev.off()
		}
	)
})