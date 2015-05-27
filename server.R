library(shiny)
library(maps)
library(mapproj)
library(ctc)
source("helpers.R")

shinyServer(function(input, output){
	
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
			print(class(xcluster2r(path)))
			return(xcluster2r(path))
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
		get_heatmap(get_cdt(), rowv = get_gtr(), colv = get_atr())
	})
	
	output$rowDendrogram <- renderPlot({
		x <- get_gtr()
		validate(need(!is.na(x), "No row dendrogram file found"))
		plot(as.hclust(x))
		
	})
	
	output$colDendrogram <- renderPlot({
		x <- get_atr()
		validate(need(!is.na(x), "No column dendrogram file found"))
		plot(as.hclust(x))
		
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