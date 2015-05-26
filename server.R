library(shiny)
library(maps)
library(mapproj)
source("helpers.R")

shinyServer(function(input, output){
	
	output$continuousMap <- renderPlot({
	})
	
	output$continuousTable <- renderDataTable({
	})
	
	output$discreteMap <- renderPlot({
		counties <- readRDS("data/counties.rds")
		percent_map(counties$white, "darkgreen", "% white")
	})
	
	output$discreteTable <- renderDataTable({
		readRDS(file = "data/counties.rds")
	})
})