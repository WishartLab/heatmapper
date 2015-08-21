source("../global_ui.R")

library(d3heatmap)

# maximum number of nested expressions to be evaluated
options(expressions = 500000)

shinyUI(list(HEAD_TASKS("#microarrayTab"), fluidPage(title = "Microarray",
	
	sidebarLayout(position = "right",
    sidebarPanel(id = "sidebarPanel", width = 1,
			FILE_UPLOAD_PANEL('example'),
    	EXAMPLE_FILE_SELECT(),

    	JSCOLOUR_ROW("#66CD00", "#FF0000"), 
    	
    	fluidRow(
    		column(3, tags$label("Scale Type")),
				column(9,	
					selectInput('scale', label = NULL,
					choices = c(
						"Row" = 'row',
						"Column" = 'column',
						"None" = 'none'),
					selected = 'row'))
    	), 
    	
    	BIN_SLIDER(3, 299, 160),
    	
    	selectInput('clusterMethod', 
    		label = "Clustering Method",	
    		choices = c(
					"none" = 'none',
					"average linkage" = 'average',
					"centroid linkage" = 'centroid',
					"complete linkage" = 'complete',
					"single linkage" = 'single'
				),	
    		selected = 'average'),
    	
    	conditionalPanel(condition = "input.clusterMethod != 'none'",
	    	selectInput('distanceMethod', 
					label = "Distance Measurement Method",
					choices = c(
						"euclidean" = 'euclidean',
						"pearson" = 'pearson',
						"kendall's tau" = 'kendall',
						"spearman rank correlation" = 'spearman',
						"manhattan" = 'manhattan'),
					selected = 'euclidean'),
    		
    		fluidRow(
    			column(5, tags$label("Apply Clustering To")),
    			column(7,
		    		selectInput('clusterSelectRC', label = NULL, 
							multiple = TRUE, 
							choices = c(
								"Rows" = 'row', 
								"Columns" = 'col'), 
							selected = 'row'))
    		),
    		
    		conditionalPanel(condition = "input.tabSelections == 'Plot'",
	    		fluidRow(
	    			column(5, tags$label("Show Dendrogram")),
	    			column(7,
	    				selectInput('dendSelectRC', label = NULL, 
								multiple = TRUE,
								choices = c(
									"Rows" = 'row', 
									"Columns" = 'col'), 
							selected = 'row'))
	    		)
    		)
    	),
    	
    	DOWNLOAD_BUTTONS(),
    	
    	ADVANCED_OPTIONS_PANEL( 
				list(
					jscolourInput("missingColour", label = "Missing Data Colour"),
	    		textInput('title', label = "Title", value = ""),
					textInput('xlab', label = "X Axis Label", value = ""),
					textInput('ylab', label = "Y Axis Label",	value = ""),
					
					selectInput('downloadFormat', label = "Plot Download Format", 
						choices = c(
							"PNG" = 'png', 
							"PDF" = 'pdf'),
						selected = 'png'),
					
					tipify(
						checkboxInput('fullSize', label = "Preview Full Height", value = FALSE), 
						"Not Recomended for Large Files", placement = "top"
					), 
		    	sliderInput('plotHeight', label = HEIGHT, 
		    		min = 500,
		    		max = 2000, 
		    		value = 600),
		    	
		    	sliderInput('plotWidth', label = WIDTH, 
		    		min = 500,
		    		max = 2000, 
		    		value = 600)
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