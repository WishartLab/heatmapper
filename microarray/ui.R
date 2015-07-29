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
		tags$style(".toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}
			#lowColour, #highColour, #missingColour {width:100%}
			#file_progress {height:0;}
			#sidebarPanel {width:23.45em;}
			#mainPanel {left:24.45em; position:absolute;}
			#tableDownload {float:right;}")),

	div(class = "busy", absolutePanel(width = "50px", height = "100px",
		fixed = TRUE, left = "65%", top = "45%", 
		h5("Loading"), tags$br(), spin())),
	
	sidebarLayout(
    sidebarPanel(id = "sidebarPanel",
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
	  				tags$label("Example File Information"),
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
    	
   
    	
    	fluidRow(
    		column(6,jscolourInput("lowColour", label = "Low Colour", value = "#66CD00")), 
    		column(6, jscolourInput("highColour", label = "High Colour", value = "#FF0000"))
    	),
    		jscolourInput("missingColour", label = "Missing Data Colour"),

				fluidRow(
					column(3, tags$label("Scale Type") ),
		column(9,		selectInput('scale', label = NULL,
					choices = c(
						"row" = 'row',
						"column" = 'column',
						"none" = 'none'),
					selected = 'row') ))
			, 
    	
    					
    	fluidRow(
    		column(3, tags$label("Number of Shades")),
			column(9,
    		sliderInput('binNumber', label = NULL, 
					min = 3, 
					max = 299, 
					value = 160))), 
    	
    	 
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
					selected = 'row'))),
    	
    	downloadButton('plotDownload', label = "Download Plot", class = "btn-info"),
	  	downloadButton('tableDownload', label = "Download Table", class = "btn-info"),
    	
	  	tags$br(), 
	  	tags$br(),
    	
    	
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
		
		mainPanel(id = "mainPanel",
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