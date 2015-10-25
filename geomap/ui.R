source("../global_ui.R")
library(leaflet)

shinyUI(list(HEAD_TASKS("#geomapTab"), fluidPage(title = "Geomap",
	
	sidebarLayout(position = "right",
		sidebarPanel(id = "sidebarPanel", width = 1,
			
			FILE_UPLOAD_PANEL(),
			EXAMPLE_FILE_SELECT(),
		
			tipify(fluidRow(
				column(3, tags$label("Area to use")),
				column(9, 
		    	selectInput("area", label = NULL, 
		    		choices = c(
		    			"Canada: By Province" = 'data/CAN_1.rds', 
		    			"USA: By State" = 'data/USA_1.rds',
		    			"United Kingdom" = 'data/GBR_1.rds', 
		    			"Africa: By Country" = 'data/Africa.rds', 
		    			"Asia: By Country" = 'data/Asia.rds', 
		    			"Australia" = 'data/AUS_1.rds', 
		    			"Europe: By Country" = 'data/Europe.rds', 
		    			"North America: By Country" = 'data/North_America.rds', 
		    			"South America: By Country" = 'data/South_America.rds', 
		    			"World: By Country" = 'data/World_Countries.rds'), 
		    		selected = 'data/CAN_1.rds'))
			), "Select map to display", placement = "right"),
			
			HTML("<button id='rangeSubmit' class='action-button clearButton'>Submit Range</button>"), 
			bsTooltip(id = "rangeSubmit", "Update heatmap using range of interest", placement = "right"),
			tags$label("Range of Interest"),
			tipify(
				sliderInput("range", 	   			
					label = NULL, 
			   	min = 0, 
			   	max = 100, 
			 		value = c(0, 100)), 
			"Adjust the colour assignment cutoff values, then click Submit Range button to update the heatmap", 
			placement = "right"),
			
			tipify(
				selectInput("colSelect", label = "Column to Use", choices = c(" " = 0)), 
			"Select column from input file", placement = "right", 
			),
			
			LAYERS_SELECT(
				c("Map" = 'showTiles', "Contour Lines" = 'showContours', "Heatmap" = 'showHeatmap'), 
				c('showTiles', 'showContours', 'showHeatmap')
			),
			
			FILL_OPACITY_SLIDER(0.8),
			
			BIN_SLIDER(2, 12, 8),
			
			JSCOLOUR_ROW("#FFEDA0", "#800026"),

    	DOWNLOAD_BUTTONS(),
		 		
			ADVANCED_OPTIONS_PANEL(
				list(
					textInput('legend',
		    		label = "Custom legend title", 
		   			value = "Legend"), 
			
					sliderInput("lineSize", 
						label = "Contour Line Width", 
						min = 0,
						max = 5,
						value = 1)
				)
			)
	),
	mainPanel(id = "mainPanel",
		tabsetPanel(id = "tabSelections", type = "tabs",
			tabPanel(title = "Interactive", 
				tags$br(),
			  leafletOutput("map", "100%", 600),
				conditionalPanel("output.map",
					absolutePanel(id = "controls", class = "info legend leaflet-control", right = 30, top = 70, width=200,
			  		h4(strong("Region Information")),
			  		uiOutput("stateInfo")
					)
				)
			),
			tabPanel(title = "Table", 
				tags$br(),
				wellPanel(
					tags$label("Update Region Name"), 
					HTML("<button id='submitName' class='action-button clearButton'>Submit Name</button>"), 
					selectInput('tableNames', label = NULL, choices = c(" " = 0))		
				),
				DT::dataTableOutput("table")), 
			tabPanel(title = "Region Names", 
				tags$br(),
				DT::dataTableOutput("regionNames"))
			))),
	INCLUDE_JS()
)))
