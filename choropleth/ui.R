library(leaflet)
library(jscolourR)
library(spin)
library(shinyBS)
source("../strings.R")

shinyUI(fluidPage(
  includeHTML("www/navbar.html"),
  
	# Add a little CSS to make the map background pure white
  tags$head(
  	HTML("<link rel=\"stylesheet\" href=\"//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css\">"),
		tags$style("
			.toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}
			#showcase-code-position-toggle, #showcase-sxs-code { display: none; }
    	.floater { background-color: white; padding: 8px; opacity: 0.7; border-radius: 6px; 
			box-shadow: 0 0 15px rgba(0,0,0,0.2); }
			#lowColour, #highColour, #missingColour {width:100%}
			#file_progress {height:0;}
			#sidebarPanel {width:23.45em;}
			#mainPanel {left:24.45em; position:absolute;}")),
	
	sidebarLayout(
		sidebarPanel(id = "sidebarPanel",
			radioButtons('chooseInput', label = FILE_UPLOAD, 
	  		inline=TRUE, 
	  		choices = c(
	  			"Upload File" = 'fileUpload',
    			"Example File" = 'example'), 
	  		selected = 'fileUpload'
	  	),

	  	conditionalPanel(condition = "input.chooseInput == 'example'",
				tags$label("Choose Example File"), 
	  		fluidRow(
	  			column(9,	
	  				selectInput('exampleFiles',
							label = NULL,
							choices = c(
								"Example 1" = 'example_input/example1.txt',
								"Example 2" = 'example_input/example2.txt'),
							selected = 1)),
	  			column(2,	
	  				actionButton('exampleButton', label = NULL, class = "btn-info",icon = icon("fa fa-info-circle")), 
	  				bsTooltip(id = "exampleButton", title = "View Example File Details", placement = "right")
	  			)),
	  		
	  		conditionalPanel(condition = "input.exampleButton>0",
	  			wellPanel(id = "exampleInfo",
	  				tags$label("Example File Information"),
	  				HTML("<button id='closeExampleButton' class='action-button' style='float:right;'><i class='fa fa-times'></i></button>"),
						conditionalPanel(condition = "input.exampleFiles == \'example_input/example1.txt\'", includeHTML("www/example1info.html")),
						conditionalPanel(condition = "input.exampleFiles == \'example_input/example2.txt\'", includeHTML("www/example2info.html"))
				))),
    	
    	conditionalPanel(condition = "input.chooseInput == 'fileUpload'",
				fluidRow(
					column(8, fileInput('file', label = NULL)), 
					column(4, HTML("<button id='clearFile' class='action-button' style='display:inline;float:right;'>Clear File</button>"))
				)
	  	),

			fluidRow(
				column(3, tags$label("Area to use")),
				column(9, 
		    	selectInput("area", label = NULL, 
		    		choices = c(
		    			"Canada: By Province" = 'data/CAN_1.rds', 
		    			"USA: By State" = 'data/USA_1.rds',
		    			"United Kingdom" = 'data/GBR_1.rds'), 
		   			selected = 'data/USA_1.rds'))
				),
		
   		sliderInput("range", 
   			label = "Range of interest", 
   			min = 0, 
   			max = 100, 
   			value = c(0, 100)),
			selectInput("colSelect", label = "Column to use", choices = c(" " = 0)),
			
			
			selectInput('layers', label = "Show/Hide Layers", 
				multiple = TRUE,
				choices = c(
					"Map" = 'showTiles', 
					"Contour Lines" = 'showContours', 
					"Heatmap" = 'showHeatmap'
				), 
				selected = c('showTiles', 'showContours', 'showHeatmap')),
			
			fluidRow(
    		column(6,jscolourInput("lowColour", label = "Low Colour", value = "#FFEDA0")), 
    		column(6, jscolourInput("highColour", label = "High Colour", value = "#800026"))
    	),
			
			fluidRow(
	  		column(3, tags$label("Heatmap Opacity")), 
	  		column(9, sliderInput('fillOpacity', label = NULL, min = 0, max = 1, value = 0.8, step = 0.05))
	  	), 
			    	
    	fluidRow(
    		column(3, tags$label("Number of Shades")),
				column(9,
					sliderInput("binNumber", 
						label = NULL, 
						min = 2, 
						max = 12, 
						value = 8))
    	), 
    	
			downloadButton('plotDownload', "Download Plot", class = "btn-info"),
			downloadButton('downloadExample', "Download Table", class = "btn-info"),
			tags$br(), tags$br(),
		 		
		actionButton('advancedOptionsButton', label = "Show Advanced Options", class = "toggleButton fa fa-angle-down"),
		conditionalPanel(condition = "input.advancedOptionsButton%2", 
			wellPanel(
				textInput('legend',
	    		label = "Custom legend title", 
	   			value = "Legend"), 
		
				sliderInput("lineSize", 
					label = "Contour Line Width", 
					min = 0,
					max = 5,
					value = 1)
		))
	),
	mainPanel(id = "mainPanel",
		tabsetPanel(id = "tabSelections", type = "tabs",
			#tabPanel(title= "sadf"),
			tabPanel(title = "Interactive", 
			  leafletOutput(
			    "map", "100%", 600
			  ),
			  absolutePanel(
			    right = 30, top = 60, width = 200, class = "floater",
			    
			    h4("Region Information"),
			    uiOutput("stateInfo")
			  ),
			 
				# Legend
			  absolutePanel(
			    right = 30, top = 320, style = "", class = "floater",
			    uiOutput("legend")
			  )
			),
			tabPanel(title = "Table", 
				tags$br(),
				dataTableOutput("table")), 
			tabPanel(title = "Region Names", 
				tags$br(),
				dataTableOutput("regionNames"))
			))),
	singleton(includeScript("www/js/active.js"))
))