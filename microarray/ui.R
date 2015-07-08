library(jscolourR)
library(d3heatmap)
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
    		
  		actionButton('colourOptionsButton', label = "Hide Colour Options", class = "toggleButton fa fa-angle-up"),
			wellPanel(id = "colourPanel", 
    		jscolourInput('lowColour', label = "Colour for low numbers", value = "#FFFA00"),
    		
    		jscolourInput('highColour', label = "Colour for high numbers", value = "#FF0000"), 
				
				sliderInput('binNumber', label = "Number of bins", 
					min = 3, 
					max = 299, 
					value = 160)
							
			)),
		
		mainPanel(
			tabsetPanel(id = "tabSelections", type = "tabs",
				tabPanel("Plot", tags$br(), plotOutput("map", height = 500)), 
				tabPanel("Interactive", tags$br(), d3heatmapOutput("d3map", height = 500)),
				tabPanel("Table", tags$br(), dataTableOutput("table"))
			)
		)
	), 
	
	singleton(includeScript("www/js/active.js"))
	))