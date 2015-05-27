library(shiny)
library(maps)
library(mapproj)
source("helpers.R")

shinyServer(function(input, output){
	
	#### HEATMAP ####
	get_heatmap_file <- function(){
		path <- get_path(input$cdtFile)
		
		if(!is.null(path)){
			data <- read.delim(path, header=TRUE, sep="\t")
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
	
	get_gtr <- function(){
		path <- get_path(input$gtrFile)
		if(!is.null(path)){
			return(read.table(path, sep='\t', header=FALSE, as.is=TRUE))
		}
		else{
			return(NA)
		}
	}
	
	get_atr <- function(){
		path <- get_path(input$atrFile)
		if(!is.null(path)){
			return(read.table(path, sep='\t', header=FALSE, as.is=TRUE))
		}
		else{
			return(NA)
		}
	}
	
	output$heatmap <- renderPlot({
		get_heatmap(get_heatmap_file(), rowv = get_gtr(), colv = get_atr())
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