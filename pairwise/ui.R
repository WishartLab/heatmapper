source("../global_ui.R")
library(d3heatmap)

shinyUI(list(HEAD_TASKS("#pairwiseTab", "50%", "40%"), fluidPage(title = "Pairwise Comparison", 
	
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
						column(3, tags$label("Upload Format")),
						column(9,
							selectInput('uploadFormat', label = NULL,
								choices = c(
									"Generic data table" = 'generic',
									"PDB format" = 'pdb'),
								selected = 'dm')
						)
					),
					conditionalPanel(condition = "input.uploadFormat == 'generic'",
						checkboxGroupInput("labels", NULL,
							 c("Labels in first row" = "useColLabels",
								"Labels in first column" = "useRowLabels"))
					),
					conditionalPanel(condition = "input.uploadFormat == 'pdb'",
					fluidRow(
						column(3, tags$label("Atoms")),
						column(9,
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
			
			fluidRow(
				column(10,
					radioButtons('matType', label = "Choose matrix type to calculate",
						 inline=FALSE,
						 choices = c(
							"Calculate distance matrix" = 'distMat',
							"Calculate correlation matrix" = 'corrMat',
							"Display data as-is" = 'simpleMat'
							),
						 selected = 'distMat')
				),
				tipify(actionButton('matrixInfoButton', label = NULL, class = "btn-info", icon = icon("fa fa-info-circle")),
					title = "Matrix Type Info", placement = "right")
			),

		
			conditionalPanel(condition = "input.matType == 'distMat'",
				 tipify(selectInput('distanceMethod', 
						label = "Distance Measurement Method",
						choices = c(
							"Euclidean" = 'euclidean',
							"Pearson" = 'pearson',
							"Kendall's Tau" = 'kendall',
							"Spearman Rank Correlation" = 'spearman',
							"Manhattan" = 'manhattan'),
						selected = 'euclidean'),
						"Select method for computing distance between rows and columns",
						placement = "right")
			),
			
			conditionalPanel(condition = "input.matrixInfoButton>0",
				wellPanel(id = "matrixInfo",
					tags$label("Matrix Type"),
					HTML("<button id='closeMatrixInfoButton' class='action-button' style='float:right;'><i class='fa fa-times'></i></button>"),
					HTML("<br />Distance matrix: Each row is treated as a point, and Euclidean distances between points are calculated.<br />Correlation matrix: Calculate correlations between the variables in each data column.")
				)),
			
			
			LAYERS_SELECT(c("Legend" = 'showLegend', "Axis Labels" = 'showAxisLabels'), c('showLegend', 'showAxisLabels')),
			
			#BRIGHTNESS_SLIDER(), 
			
			BIN_SLIDER(3, 100, 50), 
			
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