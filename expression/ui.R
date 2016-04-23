source("../global_ui.R")

library(d3heatmap)

# maximum number of nested expressions to be evaluated
options(expressions = 500000)

shinyUI(list(HEAD_TASKS("#expressionTab"), fluidPage(title = "Expression Heat Map",
	
	sidebarLayout(position = "right",
    sidebarPanel(id = "sidebarPanel", width = 1,
			FILE_UPLOAD_PANEL(),
    	EXAMPLE_FILE_SELECT(),
    	
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
			   column(4,jscolourInput("lowColour", label = "Low Colour", value = "#FF0000")), 
			   column(4, jscolourInput("midColour", label = "Middle Colour")),
			   column(4, jscolourInput("highColour", label = "High Colour", value = "#23B000")))
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
					
					sliderInput('plotWidth', label = WIDTH, 
		    		min = 500,
		    		max = 2000,
		    		value = 600),
					
					tipify(
						checkboxInput('fullSize', label = "Preview Full Height", value = FALSE), 
						"Not Recomended for Large Files", placement = "top"), 
					
					conditionalPanel(condition = "input.fullSize == false", 
			    	sliderInput('plotHeight', label = HEIGHT, 
			    		min = 500,
			    		max = 8000,
			    		value = 600)),
					
					textInput('title', label = "Title", value = ""),
					textInput('xlab', label = "X Axis Label", value = ""),
					textInput('ylab', label = "Y Axis Label",	value = "")
						
					#jscolourInput("missingColour", label = "Missing Data Colour")
				)
			)
    ),
		
		mainPanel(id = "mainPanel",
			tabsetPanel(id = "tabSelections", type = "tabs",
				tabPanel("Plot", tags$br(), plotOutput("heatmap")), 
				
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
