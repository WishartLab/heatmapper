library(jscolourR)
library(spin)
library(shinyBS)
source("../global_ui.R")
library(d3heatmap)

shinyUI(fluidPage(
	HEAD_TASKS("#distanceMatrixTab", "50%", "40%"),
	sidebarLayout(
		sidebarPanel(id = "sidebarPanel",
			
			FILE_UPLOAD_PANEL('example'), 
			
			COLOUR_SCHEME_SELECT(), 
			
			conditionalPanel(condition = "input.colourScheme == 'custom'",	
				fluidRow(
	  			column(4,jscolourInput("lowColour", label = "Low Colour", value = "#FF0000")), 
	  			column(4, jscolourInput("midColour", label = "Middle Colour")),
	  			column(4, jscolourInput("highColour", label = "High Colour", value = "#23B000")))
	 		),
			
			BIN_SLIDER(3, 299, 160), 
			
			textInput('title', label = "Title", value = ""),
				
			textInput('xlab', label = "X Axis Label", value = ""),
				
			textInput('ylab', label = "Y Axis Label", value = ""),
			
			DOWNLOAD_BUTTONS(),
			
			ADVANCED_OPTIONS_PANEL(
				list(
					checkboxInput('asp', label = "Set Aspect Ratio = 1", value = TRUE),
					
		  		sliderInput('plotWidth', label = WIDTH, min = 400, max = 2000, value = 600),
					
					sliderInput('plotHeight', label = HEIGHT, min = 400, max = 2000, value = 500),
						
					selectInput('downloadPlotFormat', label = "Plot download file type", 
						choices = c(
							"JPEG" = 'jpg',
							"PDF" = 'pdf',
							"PNG" = 'png'
						), 
						selected = 'png')
				)
			)
  	), 
		mainPanel(id = "mainPanel",
			tabsetPanel(id = "tabSelections", type = "tabs", 
				tabPanel(title = "Plot", tags$br(), 
					plotOutput("map", height = 500)),
				tabPanel(title = "Interactive", tags$br(), tags$br(), d3heatmapOutput("d3map", height = 500)),
				tabPanel(title = "Table", tags$br(), dataTableOutput("table"))
				))),	
	singleton(includeScript("www/js/active.js"))
	))