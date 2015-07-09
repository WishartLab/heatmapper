library(jscolourR)
library(d3heatmap)

# maximum number of nested expressions to be evaluated
options(expressions = 500000)

shinyUI(fluidPage(
	includeHTML("www/navbar.html"),
	tags$head(
		HTML("<link rel=\"stylesheet\" href=\"//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css\">"),
		tags$style(".toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}")),

	sidebarLayout(
    sidebarPanel(
    	actionButton('fileInputOptionsButton', label = "Hide File Options", class = "toggleButton fa fa-angle-up"),
			wellPanel(id = "fileInputPanel",
    		radioButtons('chooseInput',
    			label = "Choose Input Type",
    			choices = c(
    				"Upload File" = 'fileUpload',
    				"Example File" = 'examples'),
    			selected = 'examples'),
				
				conditionalPanel(condition = "input.chooseInput == \'examples\'",
					selectInput('exampleFiles',
						label = "Choose Example File",
						choices = c(
							"Example 1" = 'example_input/example1.txt',
							"Example 2" = 'example_input/example2.txt',
							"Example 3" = 'example_input/example3.txt'),
						selected = 1),
					tags$div(class="exampleInfo",
						wellPanel(
							conditionalPanel(condition = "input.exampleFiles == \'example_input/example1.txt\'", includeHTML("www/example1info.html")),
							conditionalPanel(condition = "input.exampleFiles == \'example_input/example2.txt\'", includeHTML("www/example2info.html")),
							conditionalPanel(condition = "input.exampleFiles == \'example_input/example3.txt\'", includeHTML("www/example3info.html"))
						)
					),
					
					downloadButton(class = "btn-info", outputId = 'downloadExample', label = "Download Example Text File")),
				
    		conditionalPanel(condition = "input.chooseInput == \'fileUpload\'",
    			fileInput('file', label = "Upload File (4MB maximum file size)"))
    	),
    	
    	actionButton('clusterOptionsButton', label = "Hide Cluster Options", class = "toggleButton fa fa-angle-up"),
			wellPanel(id = "clusterPanel",
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
	    	
				strong("Apply Clustering To"),
				checkboxInput('rowv', label = "Rows", value = TRUE),
				checkboxInput('colv', label = "Columns", value = FALSE),
				
				strong("Show Dendrogram"),
				checkboxInput('dendRow',	label = "Rows", value = TRUE),
				checkboxInput('dendCol', label = "Columns", value = FALSE)
			),
    		
  		actionButton('colourOptionsButton', label = "Hide Colour Options", class = "toggleButton fa fa-angle-up"),
			wellPanel(id = "colourPanel", 
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
			), 
    	actionButton('labelOptionsButton', label = "Hide Label Options", class = "toggleButton fa fa-angle-up"),
			wellPanel(id = "labelPanel", 
    		textInput('title', label = "Title", value = ""),
				textInput('xlab', label = "X Axis Label", value = ""),
				textInput('ylab', label = "Y Axis Label",	value = "")
			)
    ),
		
		mainPanel(
			tabsetPanel(id = "tabSelections", type = "tabs",
				tabPanel("Plot", tags$br(), plotOutput("map", height = 500)), 
				tabPanel("Interactive", tags$br(), d3heatmapOutput("d3map", height = 500)),
				tabPanel("RowDendrogram", 
					h3("Row Dendrogram"), 
					plotOutput("rowDendrogram")), 
				tabPanel("ColDendrogram",
					h3("Column Dendrogram"), 
					plotOutput("colDendrogram")), 
				tabPanel("Table", tags$br(), dataTableOutput("table"))
			)
		)
	), 
	
	singleton(includeScript("www/js/active.js"))
	))