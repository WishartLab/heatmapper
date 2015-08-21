source("../global_ui.R")
library(leaflet)

shinyUI(list(HEAD_TASKS("#choroplethTab"), fluidPage(title = "Choropleth",
  
	# Add a little CSS to make the map background pure white
  tags$head(
		tags$style("
			#showcase-code-position-toggle, #showcase-sxs-code { display: none; }
    	.floater { background-color: white; padding: 8px; opacity: 0.7; border-radius: 6px; 
			box-shadow: 0 0 15px rgba(0,0,0,0.2); }")
  ),
	
	sidebarLayout(position = "right",
		sidebarPanel(id = "sidebarPanel", width = 1,
			
			FILE_UPLOAD_PANEL(),
			EXAMPLE_FILE_SELECT(),
		
			fluidRow(
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
		    		selected = 'data/USA_1.rds'))
			),
		
   		sliderInput("range", 
   			label = "Range of interest", 
   			min = 0, 
   			max = 100, 
   			value = c(0, 100)),
			selectInput("colSelect", label = "Column to use", choices = c(" " = 0)),
			
			
			LAYERS_SELECT(
				c("Map" = 'showTiles', "Contour Lines" = 'showContours', "Heatmap" = 'showHeatmap'), 
				c('showTiles', 'showContours', 'showHeatmap')
			),
			
			JSCOLOUR_ROW("#FFEDA0", "#800026"),
			
			FILL_OPACITY_SLIDER(0.8),
			
			BIN_SLIDER(2, 12, 8),

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
				
			  absolutePanel(
			    right = 30, top = 70, width = 200, class = "floater",
			    h4("Region Information"),
			    uiOutput("stateInfo")
			  )
			),
			tabPanel(title = "Table", 
				tags$br(),
				dataTableOutput("table")), 
			tabPanel(title = "Region Names", 
				tags$br(),
				dataTableOutput("regionNames"))
			))),
	INCLUDE_JS()
)))