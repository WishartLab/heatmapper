source("../global_ui.R")
library(d3heatmap)

shinyUI(list(HEAD_TASKS("#distanceMatrixTab", "50%", "40%"), fluidPage(title = "Distance Matrix", 
	
	sidebarLayout(position = "right",
		sidebarPanel(id = "sidebarPanel", width = 1,
			
			FILE_UPLOAD_PANEL('example'), 
			EXAMPLE_FILE_SELECT(),
			
			LAYERS_SELECT(c("Legend" = 'showLegend', "Axis Labels" = 'showAxisLabels'), c('showLegend', 'showAxisLabels')),
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
					checkboxInput('reverseOrder', label = tags$strong("Reverse Data Order"), TRUE), 
					
					checkboxInput('asp', label = tags$strong("Set Aspect Ratio = 1"), value = TRUE),
					
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
					plotOutput("map")),
				tabPanel(title = "Interactive", tags$br(), d3heatmapOutput("d3map", height = 500)),
				tabPanel(title = "Table", tags$br(), dataTableOutput("table"))
				))),	
	INCLUDE_JS()
)))