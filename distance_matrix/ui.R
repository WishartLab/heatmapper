source("../global_ui.R")
library(d3heatmap)

shinyUI(list(HEAD_TASKS("#distanceMatrixTab", "50%", "40%"), fluidPage(title = "Distance Matrix", 
	
	sidebarLayout(position = "right",
		sidebarPanel(id = "sidebarPanel", width = 1,
			
			# File upload panel with option to select file upload format.
			wellPanel(
				radioButtons('chooseInput', label = FILE_UPLOAD, 
										inline=TRUE, 
										choices = c(
											"Upload File" = 'fileUpload',
											"Example File" = 'example'), 
										selected = 'fileUpload'),
				conditionalPanel(condition = "input.chooseInput == 'fileUpload'",
										HTML("<button id='clearFile' class='action-button clearButton'>Clear</button>"),
										fileInput('file', label = NULL, width="82%"),
										
										fluidRow(
											column(4, tags$label("Upload Format")),
											column(8,
												selectInput('uploadFormat', label = NULL, 
																choices = c(
																	"Distance matrix" = 'dm',
																	"Coordinates list" = 'coords',
																	"PDB format" = 'pdb'),
																selected = 'dm')
											)
										),
										conditionalPanel(condition = "input.uploadFormat == 'coords'",
																		 fluidRow(
																		 	column(1),
																		 	column(11,
																		 		checkboxInput('useRowLabels', label = tags$strong("Labels in first column"), FALSE)
																		 	)
																		 )
										),
										conditionalPanel(condition = "input.uploadFormat == 'pdb'",
																		 fluidRow(
																		 	column(4, tags$label("Atoms")),
																		 	column(8,
																		 				 selectInput('atomSelect', label = NULL, 
																		 				 						choices = c(
																		 				 							"C-alpha atoms" = 'ca',
																		 				 							"Backbone atoms" = 'bb',
																		 				 							"All atoms" = 'all'),
																		 				 						selected = 'ca')
																		 	)
																		 	)
										)
				),
				EXAMPLE_FILE_SELECT()
			),
			
			LAYERS_SELECT(c("Legend" = 'showLegend', "Axis Labels" = 'showAxisLabels'), c('showLegend', 'showAxisLabels')),
			
			BIN_SLIDER(3, 299, 160), 
			
			COLOUR_SCHEME_SELECT(), 
			
			conditionalPanel(condition = "input.colourScheme == 'custom'",	
				fluidRow(
	  			column(4,jscolourInput("lowColour", label = "Low Colour", value = "#FF0000")), 
	  			column(4, jscolourInput("midColour", label = "Middle Colour")),
	  			column(4, jscolourInput("highColour", label = "High Colour", value = "#23B000")))
	 		),
			
			checkboxInput('reverseOrder', label = tags$strong("Reverse Data Order"), TRUE), 
			
			checkboxInput('asp', label = tags$strong("Set Aspect Ratio = 1"), value = TRUE),

			DOWNLOAD_BUTTONS_WITH_SELECTION(),
			
			ADVANCED_OPTIONS_PANEL(
				list(
					
		  		sliderInput('plotWidth', label = WIDTH, min = 400, max = 2000, value = 600),
					
					sliderInput('plotHeight', label = HEIGHT, min = 400, max = 2000, value = 500),
							
					textInput('title', label = "Title", value = ""),
						
					textInput('xlab', label = "X Axis Label", value = ""),
						
					textInput('ylab', label = "Y Axis Label", value = "")
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