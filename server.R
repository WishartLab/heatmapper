library(shiny)
library(maps)
library(mapproj)
library(ctc)
library(ggmap)
library(xlsx)

source("helpers.R")

options(shiny.deprecation.messages=FALSE)

shinyServer(function(input, output, session){
	
	#### HEATMAP ####
	get_heatmap_file <- function(){
		path <- get_path(input$cdtFile)
		
		if(!is.null(path)){
			data <- read.delim(path, header=TRUE, sep="\t", row.names = NULL)
			if(is.null(data)){
				return(NULL)
			}
	    data <- remove_strings(data)
	    return(data)
		}
		else{
			return(NULL)
		}
	}
	
	get_cdt <- function(){
		path <- get_path(input$cdtFile)
		if(!is.null(path)){
			#cdt <-read.table(path, sep='\t', header=TRUE, row.names=NULL)
			x <- data.matrix(read.eisen(path))
			return(x)
		}
		else{
			return(NULL)
		}
	}
	
	get_gtr <- function(){
		path <- get_path(input$gtrFile)
		if(!is.null(path)){
			return(as.dendrogram(xcluster2r(path)))
		}
		else{
			return(NA)
		}
	}
	
	get_atr <- function(){
		path <- get_path(input$atrFile)
		if(!is.null(path)){
			return(as.dendrogram(xcluster2r(path)))
		}
		else{
			return(NA)
		}
	}
	
	output$heatmap <- renderPlot({
		get_heatmap(get_heatmap_file(), rowv = get_gtr(), colv = get_atr())
	})
	
	output$rowDendrogram <- renderPlot({
		x <- get_gtr()
		validate(need(!is.na(x), "No row dendrogram file found"))
		plot(x)
		
	})
	
	output$colDendrogram <- renderPlot({
		x <- get_atr()
		validate(need(!is.na(x), "No column dendrogram file found"))
		plot(x)
		
	})
	
	
	output$heatmapTable <- renderDataTable({
		get_heatmap_file()
	})
	
	#### CONTINUOUS MAP ####
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
				file <- read.delim(input$cmFile$datapath, header = TRUE, sep="\t")
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
		
	#### DISCRETE MAP ####
	get_dmFile <- function(){
		
		if(input$dmChooseInput == 'dmExample'){
			path <- "data/counties.rds"
			counties <- readRDS(path)
		}
		else{
		#	validate(need(input$dmFile$datapath, "Please upload a file"))
		#	counties <- read.delim(input$dmFile$datapath, header = TRUE)
			counties <- read.table("data/statetest2.txt", header = TRUE, sep="\t")
			counties[,1] <- tolower(counties[,1])
		}
		return(counties)
	}

	output$discreteMap <- renderPlot({
		
		counties <- get_dmFile()
		
		if(input$dmLegend == ''){
			legend <- paste("%", input$dmColSelect)
		}
		else{
			legend <-input$dmLegend
		}
		
		updateSelectInput(session, inputId="dmColSelect", choices = colnames(counties[-1]), selected = input$dmColSelect)
		
		validate(need(input$dmColSelect, "Please select a column to use"))
		
		percent_map(
			area = input$dmArea,
			var = counties[,input$dmColSelect], 
			lowColour =  input$dmLowColour, 
			highColour = input$dmHighColour,
			legend.title = legend, 
			min = input$dmRange[1], 
			max = input$dmRange[2] 
			)
		
	})
	
	output$discreteTable <- renderDataTable({
		get_dmFile()
	})
	
library(pheatmap)
	
	get_dist_file <- function() {
		file <- read.delim("data/dist.txt", header=TRUE, sep="\t")
		return(file)
	}
	output$distMap <- renderPlot({
		file <- get_dist_file()
		print(rownames(file))
		print(colnames(file))
		if(!is.numeric(file[,1])){
			rownames(file) <- file[,1]
			file <- file[,-1]
		}
		else{
			rownames(file) <- colnames(file)
		}
		pheatmap(file, 
			color=rainbow(25, end = 5/6), 
			cluster_rows = FALSE, 
			cluster_cols = FALSE, 
			display_numbers = input$cellNums, 
			labels_row = rownames(file), 
			labels_col = colnames(file)) 
	})
	
	output$distTable <- renderDataTable({
		file <- get_dist_file
	})
	
	#### Body Map #### SOURCE: http://stackoverflow.com/questions/28664798/how-to-make-a-heat-map-in-r-based-on-a-gif-of-the-human-body
	
	library(grImport)
	library(XML)
	library(gridExtra)
	output$bodyMap <- renderPlot({
		bodypartInput <- c(
			input$head,input$handright,input$handleft,input$footleft,
			input$footright,input$lowerlegleft,input$lowerlegright,
			input$upperlegleft,input$upperlegright,input$torso,
			input$forearmright,input$forearmleft,input$upperarmright,
			input$upperarmleft)
		
		get_body_map(bodypartInput)
	})
	
})