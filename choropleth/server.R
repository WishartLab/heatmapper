library(shiny)
library(maps)
library(mapproj)
source("helpers.R")

shinyServer(function(input, output, session){

	#### DISCRETE MAP ####
	get_file <- function(){
		
		if(input$chooseInput == 'example'){
			path <- "data/counties.rds"
			counties <- readRDS(path)
		}
		else{
		#	validate(need(input$file$datapath, "Please upload a file"))
		#	counties <- read.delim(input$file$datapath, header = TRUE)
			counties <- read.table("data/statetest2.txt", header = TRUE, sep="\t")
			counties[,1] <- tolower(counties[,1])
		}
		return(counties)
	}

	output$map <- renderPlot({
		
		counties <- get_file()
		
		if(input$legend == ''){
			legend <- paste("%", input$colSelect)
		}
		else{
			legend <-input$legend
		}
		
		updateSelectInput(session, inputId="colSelect", choices = colnames(counties[-1]), selected = input$colSelect)
		
		validate(need(input$colSelect, "Please select a column to use"))
		
		percent_map(
			area = input$area,
			var = counties[,input$colSelect], 
			lowColour =  input$lowColour, 
			highColour = input$highColour,
			legend.title = legend, 
			min = input$range[1], 
			max = input$range[2] 
			)
		
	})
	
	output$table <- renderDataTable({
		get_file()
	})	
})