source("../global_ui.R")

library(d3heatmap)

# maximum number of nested expressions to be evaluated
options(expressions = 500000)

shinyUI(list(HEAD_TASKS("#expressionTab"), fluidPage(title = "Expression Heat Map",
	
	sidebarLayout(position = "right",
		sidebarPanel(id = "sidebarPanel", width = 1,

			fluidRow(
			column(7,
			radioButtons('chooseInput', label = FILE_UPLOAD,
				inline=FALSE,
				choices = c(
					"Example File" = 'example',
					"Upload File" = 'fileUpload',
					"Upload Multiple Files" = 'fileMultiUpload'
					),
				selected = 'fileUpload')
			),
			column(5,
				tipify(actionButton('moreButton', label = NULL, class = "btn-info", icon = icon("fa fa-info-circle")), 
						 title = "More Info", placement = "right")
				, align="right")
			),
			tags$style(type='text/css', "#moreButton {margin-top: 30px;}"),
			
			conditionalPanel(condition = "input.moreButton>0",
											 wellPanel(id = "moreInfo",
											 					tags$label("Expression Heatmap Info"),
											 					HTML("<button id='closeMoreInfoButton' class='action-button' style='float:right;'><i class='fa fa-times'></i></button>"),
											 					tags$br(),
											 					includeHTML("www/expressionInfo.html")
											 )),
			
			conditionalPanel(condition = "input.chooseInput == 'fileUpload'",
				HTML("<button id='clearFile' class='action-button clearButton'>Clear</button>"),
				fileInput('file', label = NULL, width="82%")
			),

			EXAMPLE_FILE_SELECT(),

			conditionalPanel(condition = "input.chooseInput == 'fileMultiUpload'",
				HTML("<button id='clearFileMulti' class='action-button clearButton'>Clear</button>"),
				fileInput('fileMulti', label = NULL, width="82%", multiple=TRUE)
			),

			tipify(fluidRow(
				column(3, tags$label("Scale Type")),
				column(9,	
						selectInput('scale', label = NULL,
							choices = c(
								"Row" = 'row',
								"Column" = 'column',
								"None" = 'none'),
						selected = 'row')) 
    	),"Select direction to scale values in", placement ="right"),
    	
			BRIGHTNESS_SLIDER(),
    	
    	BIN_SLIDER(3, 100, 50),
			
			COLOUR_SCHEME_SELECT(), 
			
			conditionalPanel(condition = "input.colourScheme == 'custom'",	
			 fluidRow(
			   column(4,jscolourInput("lowColour", label = "Low Colour", value = "#0016DB")), 
			   column(4, jscolourInput("midColour", label = "Middle Colour", value = "#FFFFFF")),
			   column(4, jscolourInput("highColour", label = "High Colour", value = "#FFFF00")))
			),
    
			
    	#JSCOLOUR_3_ROW(),
			
			jscolourInput("missingColour", label = "Missing Data Colour"),
			
    	tipify(selectInput('clusterMethod', 
    		label = "Clustering Method",	
    		choices = c(
					"None" = 'none',
					"Average Linkage" = 'average',
					"Centroid Linkage" = 'centroid',
					"Complete Linkage" = 'complete',
					"Single Linkage" = 'single',
					"Import Existing Clusters" = 'import'
				),	
    		selected = 'average'), "Select method for computing hierarchical clustering", placement = "right"),
    	
    	conditionalPanel(condition = "input.clusterMethod != 'none' && input.clusterMethod != 'import'",
	    	tipify(selectInput('distanceMethod', 
					label = "Distance Measurement Method",
					choices = c(
						"Euclidean" = 'euclidean',
						"Pearson" = 'pearson',
						"Kendall's Tau" = 'kendall',
						"Spearman Rank Correlation" = 'spearman',
						"Manhattan" = 'manhattan'),
					selected = 'euclidean'), "Select method for computing distance between rows and columns", placement = "right"),
    		
    		tipify(fluidRow(
    			column(5, tags$label("Apply Clustering To")),
    			column(7,
		    		selectInput('clusterSelectRC', label = NULL, 
							multiple = TRUE, 
							choices = c(
								"Rows" = 'row', 
								"Columns" = 'col'), 
							selected = 'row'))
    		  ),"Apply clustering to rows and/or columns", "right")
    	),
			conditionalPanel(condition = "input.clusterMethod == 'import'",
			  tags$label("Row Cluster File"),
        HTML("<button id='clearRowClusterFile' class='action-button clearButton clearClusterFile'>Clear File</button>"),
        bsTooltip("clearRowClusterFile", "Clear uploaded file", "right"),
			  tipify(fileInput('rowClusterFile', label = NULL), "A file containing existing clusters in Newick tree format", placement = "right"),
			  tags$label("Column Cluster File"),
        HTML("<button id='clearColClusterFile' class='action-button clearButton clearClusterFile'>Clear File</button>"),
        bsTooltip("clearColClusterFile", "Clear uploaded file", "right"),
			  tipify(fileInput('colClusterFile', label = NULL), "A file containing existing clusters in Newick tree format", placement = "right")
			),
    		
    	conditionalPanel(condition = "input.tabSelections == 'Plot' && input.clusterMethod != 'none'",
	    	tipify(fluidRow(
	    		column(5, tags$label("Show Dendrogram")),
	    		column(7,
	    			selectInput('dendSelectRC', label = NULL, 
							multiple = TRUE,
							choices = c(
								"Rows" = 'row', 
								"Columns" = 'col'), 
						selected = 'row'))
	      	), "Show or hide dendrograms", "right")
    	),
    	
    	DOWNLOAD_BUTTONS_WITH_SELECTION(),
    	
    	ADVANCED_OPTIONS_PANEL(
				list(
					
					selectInput('downloadPlotResolution', label = RESOLUTION,
						choices = c("72" = '72',"144" = '144',"300" = '300'),
						selected = '144'),
							# We set default to 144 ppi since this is the resolution on screen for
							# typical Retina/hiDPI displays with pixel ratios of 2. We want users
							# of such displays to download plots by default that will look the
							# same as what they see on screen in Heatmapper.
					
					sliderInput('plotWidth', label = WIDTH, 
		    		min = 500,
		    		max = 3000,
		    		value = 600),
					
		    	sliderInput('plotHeight', label = HEIGHT, 
		    		min = 500,
		    		max = 32000,
		    		value = 600),
					
					textInput('title', label = "Title", value = ""),
					textInput('xlab', label = "X Axis Label", value = ""),
					textInput('ylab', label = "Y Axis Label",	value = "")
						
					#jscolourInput("missingColour", label = "Missing Data Colour")
				)
			)
    ),
		
		mainPanel(id = "mainPanel",
			tabsetPanel(id = "tabSelections", type = "tabs",
				tabPanel("Plot", tags$br(),

					# Multiple file navigation controls
					conditionalPanel(condition = "input.chooseInput == 'fileMultiUpload'",
					fluidRow(
						column(2, actionButton("cyclePlotsStart", label = "Start"), align="left"),
						column(2, actionButton("cyclePlotsLeft", label = "Previous"), align="right"),
						column(4,
								wellPanel(
								textOutput("currentFileNameLabel"),
								textOutput("currentFilePositionLabel"),
								tags$head(tags$style("#currentFilePositionLabel{color: grey;
										font-size: 12px;
										}"
									)
								)
							), align="center"),
						column(2, actionButton("cyclePlotsRight", label = "Next"), align="left"),
						column(2, actionButton("cyclePlotsEnd", label = "End"), align="right")
					)
					),

					# Message about automatic plot dimension adjustment
					textOutput("plotMesage"),

					# Main heat map plot
					plotOutput("heatmap")),

				tabPanel("Interactive", tags$br(), d3heatmapOutput("d3map", height = 600)),
				
				tabPanel("Row Dendrogram", 
					uiOutput("rowInfo"),
					h3("Row Dendrogram"), 
					plotOutput("rowDendrogram")), 
				
				tabPanel("Column Dendrogram",
					h3("Column Dendrogram"), 
					plotOutput("colDendrogram")), 
				
				tabPanel("Table", tags$br(), dataTableOutput("table"))
			)
		)
	), 
	
	INCLUDE_JS()
)))
