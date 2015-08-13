source("../global_ui.R")
library(leaflet)

shinyUI(list(HEAD_TASKS("#latlongTab"), fluidPage(title = "Latitude/Longitude",
	
	sidebarLayout(position = "right",
		sidebarPanel(id = "sidebarPanel",
			FILE_UPLOAD_PANEL('example'),
			
			LAYERS_SELECT(
				c("Map" = 'showMap', "Contour Lines" = 'showContours', "Heatmap" = 'showHeatmap', "Points" = 'showPoints'), 
				c('showMap', 'showContours', 'showHeatmap')
			),

			fluidRow(
    		column(3, tags$label("Map Type")),
				column(9,	
					selectInput('mapType', 
						label = NULL, 
						choices = c(
							"Default" = 'OpenStreetMap.Mapnik',
							"Positron" = 'CartoDB.Positron', 
							"Toner" = 'Stamen.Toner',
							"Watercolour" = 'Stamen.Watercolor'), 
						selected = 'OpenStreetMap.Mapnik'))
			),
			
			COLOUR_SCHEME_SELECT(),
			
	  	conditionalPanel(condition = "input.colourScheme == 'custom'", 
				JSCOLOUR_ROW("#FFFA00", "#FF0000")
	  	), 
						
			BIN_SLIDER(3, 50, 10), 
			
			FILL_OPACITY_SLIDER(), 
			
			BANDWIDTH_SLIDER(0.05, 2, 0.4, 0.05), 
			
			GRID_POINTS_SLIDER(10, 1000, 400, 10),
			
			DOWNLOAD_BUTTONS(),
		 	
			ADVANCED_OPTIONS_PANEL(
				list(
					sliderInput('contourSize', 
	    			label = CONTOUR_WIDTH, 
	    			min = 0, 
	    			max = 4,
	    			value = 1), 
					
					sliderInput('pointSize', 
		    			label = "Point Width (in px)", 
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
			)
		)
	), 
	INCLUDE_JS()
)))