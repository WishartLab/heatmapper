library(shiny)
library(maps)
library(mapproj)
library(ctc)
source("helpers.R")

library(maptools)
library(rgdal)         # for readOGR(...)
library(ggplot2)
library(RColorBrewer)  # for brewer.pal(...)

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
		get_heatmap(get_cdt(), rowv = get_gtr(), colv = get_atr())
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
	output$continuousMap <- renderPlot({
		sample <- data.frame(Longitude=c(-1+rnorm(50,0,.5),-2+rnorm(50,0,0.5),-4.5+rnorm(50,0,.5)),
		                     Latitude =c(52+rnorm(50,0,.5),54+rnorm(50,0,0.5),56+rnorm(50,0,.5)))
		UKmap  <- readOGR(dsn="data/GBR_am/",layer="GBR_adm2")
		map.df <- fortify(UKmap)
		
		ggplot(sample, aes(x=Longitude, y=Latitude)) + 
		  stat_density2d(aes(fill = ..level..), alpha=0.5, geom="polygon")+
		  #geom_point(colour="red")+
		  geom_path(data=map.df,aes(x=long, y=lat,group=group), colour="grey50")+
		  scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral")))+
		  xlim(-10,+2.5) +
		  coord_fixed()
	})
	
	output$continuousTable <- renderDataTable({
		data.frame(Longitude=c(-1+rnorm(50,0,.5),-2+rnorm(50,0,0.5),-4.5+rnorm(50,0,.5)),
		                     Latitude =c(52+rnorm(50,0,.5),54+rnorm(50,0,0.5),56+rnorm(50,0,.5)))
	})
	
	get_dmFile <- function(){
		
		if(input$dmChooseInput == 'dmExample'){
			path <- "data/counties.rds"
			counties <- readRDS(path)
		}
		else{
		#	validate(need(input$dmFile$datapath, "Please upload a file"))
		#	counties <- read.delim(input$dmFile$datapath, header = TRUE)
			counties <- read.table("data/statetest.txt", header = TRUE, sep="\t")	
			counties[,-1] <- as.numeric(sub("%","",counties[,-1]))
			counties[,1] <- tolower(counties[,1])
		}
		return(counties)
	}
	
	#### DISCRETE MAP ####
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
})