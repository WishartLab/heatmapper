library(jscolourR)
library(d3heatmap)
library(spin)
library(shinyBS)

# maximum number of nested expressions to be evaluated
options(expressions = 500000)

shinyUI(fluidPage(
	includeHTML("www/navbar.html"),
	tags$head(
		HTML("<link rel=\"stylesheet\" href=\"//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css\">"),
		tags$style(".toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}")),

	div(class = "busy", absolutePanel(width = "50px", height = "100px",
		fixed = TRUE, left = "65%", top = "45%", 
		h5("Loading"), tags$br(), spin())),
	
	sidebarLayout(
    sidebarPanel(
			radioButtons('chooseInput', label = "Select Microarray Data File", 
	  		inline=TRUE, 
	  		choices = c(
	  			"Upload File" = 'fileUpload',
    			"Example File" = 'examples'), 
	  		selected = 'examples'
	  	),
	  	conditionalPanel(condition = "input.chooseInput == 'examples'",
				tags$label("Choose Example File"), 
	  		fluidRow(
	  			column(7,	
	  				selectInput('exampleFiles',
							label = NULL,
							choices = c(
								"Example 1" = 'example_input/example1.txt',
								"Example 2" = 'example_input/example2.txt',
								"Example 3" = 'example_input/example3.txt'),
							selected = 1)),
	  			column(2,	
	  				actionButton('exampleButton', label = NULL, class = "btn-info",icon = icon("fa fa-info-circle")), 
	  				bsTooltip(id = "exampleButton", title = "View Example File Details", placement = "right")),
	  			column(2,	
	  				downloadButton(class = "btn-info", outputId = 'downloadExample', label = NULL),
						bsTooltip(id = "downloadExample", title = "Download Example Text File", placement = "right") 
	  		)),
	  		
	  		conditionalPanel(condition = "input.exampleButton>0",
	  			wellPanel(id = "exampleInfo",
	  				HTML("<button id='closeExampleButton' class='action-button' style='float:right;'><i class='fa fa-times'></i></button>"),
						conditionalPanel(condition = "input.exampleFiles == \'example_input/example1.txt\'", includeHTML("www/example1info.html")),
						conditionalPanel(condition = "input.exampleFiles == \'example_input/example2.txt\'", includeHTML("www/example2info.html")),
						conditionalPanel(condition = "input.exampleFiles == \'example_input/example3.txt\'", includeHTML("www/example3info.html"))
				))),
    	
    	conditionalPanel(condition = "input.chooseInput == 'fileUpload'",
				fluidRow(
					column(8, fileInput('file', label = NULL)), 
					column(4, HTML("<button id='clearFile' class='action-button' style='display:inline;float:right;'>Clear File</button>"))
				)
	  	),
    	
    	wellPanel(
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
	    	selectInput('distanceMethod', 
									label = "Distance Measurement Method",
									choices = c(
										"euclidean" = 'euclidean',
										"pearson" = 'pearson',
										"kendall's tau" = 'kendall',
										"spearman rank correlation" = 'spearman',
										"manhattan" = 'manhattan'),
									selected = 'euclidean'),
	    	
				
				selectInput('clusterSelectRC', label = "Apply Clustering To", 
					multiple = TRUE, 
					choices = c(
						"Rows" = 'row', 
						"Columns" = 'col'
					), 
					selected = 'row'),
			
			conditionalPanel(condition = "input.tabSelections == 'Plot'",
					selectInput('dendSelectRC', label = "Show Dendrogram", 
					multiple = TRUE,
					choices = c(
						"Rows" = 'row', 
						"Columns" = 'col'
					), 
					selected = 'row')
			)),
    		
  		
    		jscolourInput('lowColour', label = "Colour for low numbers", value = "#66CD00"),
    		
    		jscolourInput('highColour', label = "Colour for high numbers", value = "#FF0000"), 
				
				jscolourInput('missingColour', label = "Colour for missing values", value = "#000000"),
				
				sliderInput('binNumber', label = "Number of bins", 
					min = 3, 
					max = 299, 
					value = 160), 
				
				selectInput('scale', label = "Scale Type",
					choices = c(
						"row" = 'row',
						"column" = 'column',
						"none" = 'none'),
					selected = 'row')
			, 
    	
    	
    	actionButton('advancedOptionsButton', label = "Show Advanced Options", class = "toggleButton fa fa-angle-down"),
			conditionalPanel(condition = "input.advancedOptionsButton%2", 
			wellPanel(
				
    	
    		textInput('title', label = "Title", value = ""),
				textInput('xlab', label = "X Axis Label", value = ""),
				textInput('ylab', label = "Y Axis Label",	value = "")
			, 
    	checkboxInput('fullSize', label = "Preview Full Height (not recomended for large files)", value = FALSE),
    	
    	sliderInput('mapHeight', label = "Plot Height", 
    		min = 500,
    		max = 2000, 
    		value = 600),
    	
    	sliderInput('mapWidth', label = "Plot Width", 
    		min = 500,
    		max = 2000, 
    		value = 600)
    ))),
		
		mainPanel(
			tabsetPanel(id = "tabSelections", type = "tabs",
				tabPanel("Plot", tags$br(), plotOutput("map")), 
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
	
	singleton(includeScript("www/js/active.js"))
	))