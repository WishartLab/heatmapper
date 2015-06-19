library(shiny)
library(ctc)
library(ggmap)
library(ggplot2)
library(xlsx)
library(jscolourR)

shinyServer(function(input, output, session){

	get_file <- function(){
		if(input$chooseInput == 'example'){
			points <- data.frame(
				Longitude=c(-1+rnorm(50,0,.5),-2+rnorm(50,0,0.5),-4.5+rnorm(50,0,.5)),
				Latitude =c(52+rnorm(50,0,.5),54+rnorm(50,0,0.5),56+rnorm(50,0,.5))
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
	
	ranges <- reactiveValues(x = NULL, y = NULL)
	
	get_plot <- function(){
		points <- get_file()
		
		map <- get_map(
			location = c(
				lon = median(na.rm = TRUE, points$Longitude), 
				lat = median(na.rm = TRUE, points$Latitude)), 
			zoom = input$zoom, 
			maptype = input$type
			)
		
		GET_MAP <<- ggmap(map) + 
		geom_point(data = points, aes(x = Longitude, y = Latitude), size = input$pointSize) +
		geom_density2d(data = points, aes(x = Longitude, y = Latitude), size = input$contourSize) +
		stat_density2d(data = points, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), 
			size = 0.01, bins = 16, geom = "polygon") + 
		scale_fill_gradient(breaks = NULL, low = input$lowColour, high = input$highColour) + 
    scale_alpha(range = c(0, 0.3), guide = FALSE) + coord_cartesian(xlim = NULL, ylim = NULL)
		return(GET_MAP)
	}
	
	get_zoom_plot <- function(){
		GET_MAP + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
	}
	
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$dblclick, {
    brush <- input$brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
	
	
	output$map <- renderPlot({
		get_plot()
		
	})
	
	output$mapZoom <- renderPlot({
		get_zoom_plot()
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