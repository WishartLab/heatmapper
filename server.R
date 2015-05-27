library(shiny)
library(maps)
library(mapproj)
source("helpers.R")

shinyServer(function(input, output){
	
	#### HEATMAP ####
	get_heatmap_file <- function(){
		inFile <- input$heatmapFile
		if(is.null(inFile)){
			return(NULL)
		}
		else{
	    path <- inFile$datapath
			data <- read.delim(path, header=TRUE, sep="\t")
	    data <- remove_strings(data)
	    return(data)
		}
	}
	output$heatmap <- renderPlot({
		get_heatmap(get_heatmap_file(), rowv = TRUE)
	})
	
	output$rowDendrogram <- renderPlot({
		x <- get_row_dendrogram(get_heatmap_file())
		validate(need(!is.null(x), "No row dendrogram file found"))
		plot(x)
		
	})
	
	output$colDendrogram <- renderPlot({
		x <- get_col_dendrogram(get_heatmap_file())
		validate(need(!is.null(x), "No column dendrogram file found"))
		plot(x)
		
	})
	
	
	output$heatmapTable <- renderDataTable({
		get_heatmap_file()
	})
	
	
	#### CONTINUOUS MAP ####
	output$continuousMap <- renderPlot({
	})
	
	output$continuousTable <- renderDataTable({
	})
	
	
	#### DISCRETE MAP ####
	output$discreteMap <- renderPlot({
		counties <- readRDS("data/counties.rds")
		percent_map(counties$white, "darkgreen", "% white")
	})
	
	output$discreteTable <- renderDataTable({
		readRDS(file = "data/counties.rds")
	})
})