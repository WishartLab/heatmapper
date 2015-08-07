library(jscolourR)
library(leaflet)
source("../strings.R")

shinyUI(fluidPage(
	includeHTML("www/navbar.html"),
	tags$head(
		tags$style(".toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}
			#lowColour, #highColour, #missingColour {width:100%}
			#file_progress {height:0;}
			#sidebarPanel {width:23.45em;}
			#mainPanel {left:24.45em; position:absolute;}
			#tableDownload {float:right;}")),
	
	sidebarLayout(
		sidebarPanel(id = "sidebarPanel",
			FILE_UPLOAD_PANEL(),
		
			selectInput('layers', label = LAYERS, 
				multiple = TRUE,
				choices = c(
					"Map" = 'showMap', 
					"Contour Lines" = 'showContours', 
					"Heatmap" = 'showHeatmap', 
					"Points" = 'showPoints'
				), 
				selected = c('showMap', 'showContours', 'showHeatmap')),

			fluidRow(
    		column(3, tags$label("Map Type")),
				column(9,	
					selectInput('mapType', 
						label = NULL, 
						choices = c(
							"Default" = 'OpenStreetMap.Mapnik',
							"Positron" = 'CartoDB.Positron', 
							"Toner" = 'Stamen.Toner',
							"Watercolour" = 'Stamen.Watercolor'
							), 
						selected = 'OpenStreetMap.Mapnik'))
			),
			
			COLOUR_SCHEME_SELECT(),
	  	
	  	conditionalPanel(condition = "input.colourScheme == 'custom'", 
				fluidRow(
	  			column(6, jscolourInput("lowColour", label = "Low Colour", value = "#FFFA00")),
	    		column(6, jscolourInput("highColour", label = "High Colour", value = "#FF0000"))
				)
	  	), 
						
			BIN_SLIDER(3, 50, 10), 
			
			FILL_OPACITY_SLIDER(), 
			
			BANDWIDTH_SLIDER(0.2, 4, 1, 0.05), 
			
			GRID_POINTS_SLIDER(1, 15, 5, 1),
			
			DOWNLOAD_BUTTONS(),
		 	
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
					)
    		)
    	),
		
			mainPanel(id = "mainPanel",
				tabsetPanel(type = "tabs", 
					tabPanel(title = "Interactive", leafletOutput("map", height = 600)),
					tabPanel(title = "Table", dataTableOutput("table"))
				))
			), 
	singleton(includeScript("www/js/active.js"))
	))