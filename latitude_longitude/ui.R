library(jscolourR)
library(leaflet)

shinyUI(fluidPage(
	includeHTML("www/navbar.html"),
	tags$head(
		tags$style(".toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}
			#lowColour, #highColour, #missingColour {width:100%}
			#file_progress {height:0;}
			#sidebarPanel {width:23.45em;}
			#mainPanel {left:24.45em; position:absolute;}")),
	
	sidebarLayout(
		sidebarPanel(id = "sidebarPanel",
			radioButtons('chooseInput', label = "Select Choropleth Data File", 
	  		inline=TRUE, 
	  		choices = c(
	  			"Upload File" = 'fileUpload',
    			"Example File" = 'example'), 
	  		selected = 'fileUpload'
	  	),

    	conditionalPanel(condition = "input.chooseInput == 'fileUpload'",
				fluidRow(
					column(8, fileInput('file', label = NULL)), 
					column(4, HTML("<button id='clearFile' class='action-button' style='display:inline;float:right;'>Clear File</button>"))
				)
	  	),
						
			selectInput('layers', label = "Show/Hide Layers", 
				multiple = TRUE,
				choices = c(
					"Map" = 'showMap', 
					"Contour Lines" = 'showContours', 
					"Heatmap" = 'showHeatmap', 
					"Points" = 'showPoints'
				), 
				selected = c('showMap', 'showContours', 'showHeatmap', 'showPoints')),
			
			fluidRow(
  			column(6, jscolourInput("lowColour", label = "Low Colour", value = "#FFFA00")),
    		column(6, jscolourInput("highColour", label = "High Colour", value = "#FF0000"))
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
						min = 3, 
						max = 50, 
						value = 20))
    	), 
		 	
    		
			
    	actionButton('advancedOptionsButton', label = "Show Advanced Options", class = "toggleButton fa fa-angle-down"),
			conditionalPanel(condition = "input.advancedOptionsButton%2",
				wellPanel(
	    		sliderInput('contourSize', 
	    			label = "Contour Line Width (in pixels)", 
	    			min = 0, 
	    			max = 4,
	    			value = 1), 
					
					sliderInput('pointSize', 
		    			label = "Point Width (in pixels)", 
		    			min = 0, 
		    			max = 10, 
		    			value = 2), 
		    		
		    	sliderInput('pointOpacity', 
		    			label = "Point Opacity", 
		    			min = 0, 
		    			max = 1, 
		    			value = 0.8)
				),
				
    		radioButtons('downloadType', 
    			label = "Downlaod file format", 
    			choices = c(
    				"PDF" = 'pdf', 
    				"PNG" = 'png'), 
    			selected = 'pdf'),
    		
    		downloadButton('download', "Download image")
    		)
    		),
			mainPanel(id = "mainPanel",
				tabsetPanel(type = "tabs", 
					tabPanel(title = "Plot", leafletOutput("map", height = 600)),
					tabPanel(title = "Table", dataTableOutput("table"))))
			), 
	singleton(includeScript("www/js/active.js"))
	))