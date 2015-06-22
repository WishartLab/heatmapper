library(jscolourR)
library(leaflet)

shinyUI(fluidPage(
	includeHTML("navbar.html"),
		sidebarLayout(
    	sidebarPanel(
    		
    		radioButtons('chooseInput',
    			label = "Choose Input Type",
    			choices = c(
    				"Upload File" = 'fileUpload',
						"Example File" = 'example'),
    			selected = 'example'),
    		
    		conditionalPanel(condition = "input.chooseInput == 'fileUpload'", 
    			fileInput("file", label = h3("File input"))), 
    		
    		strong("Background"),
    		checkboxInput('showMap', label = "Show Map", value = TRUE),
    		
    		jscolourInput("lowColour", label = "Colour for low numbers", value = "#FFFA00"),
    		
    		jscolourInput("highColour", label = "Colour for high numbers", value = "#FF0000"),
    		
    		
    		sliderInput('contourSize', 
    			label = "Contour Line Width (in pixels)", 
    			min = 0, 
    			max = 4,
    			value = 1), 
    		
    		checkboxInput('showPoints', label = "Show Points", value = TRUE),
    		
    		conditionalPanel(condition = "input.showPoints == true", 
	    		sliderInput('pointSize', 
	    			label = "Point Width (in pixels)", 
	    			min = 0, 
	    			max = 10, 
	    			value = 2), 
	    		
	    		sliderInput('pointOpacity', 
	    			label = "Point Opacity", 
	    			min = 0, 
	    			max = 1, 
	    			value = 0.8)),
    		
    		sliderInput('fillOpacity', 
    			label = "Fill Opacity", 
    			min = 0, 
    			max = 1, 
    			value = 0.5), 
    		
    		
    		selectInput('type', 
    			label = "Map type", 
    			choices = c(
    				"terrain" = 'terrain', 
    				"terrain background" = 'terrain-background', 
    				"satellite" = 'satellite',
    				"roadmap" = 'roadmap', 
    				"hybrid" = 'hybrid', 
    				"toner" = 'toner',
    				"watercolor" = 'watercolor'), 
    			selected = 'terrain'),
    		
    		radioButtons('downloadType', 
    			label = "Downlaod file format", 
    			choices = c(
    				"PDF" = 'pdf', 
    				"PNG" = 'png'), 
    			selected = 'pdf'),
    		
    		downloadButton('download', "Download image")
    		),
			mainPanel(
				tabsetPanel(type = "tabs", 
					tabPanel(title = "Plot",includeScript("https://api.tiles.mapbox.com/mapbox.js/plugins/leaflet-image/v0.0.4/leaflet-image.js"), 
						tags$script("L_PREFER_CANVAS = true;"), leafletOutput("map", height = 600)),
					tabPanel(title = "Table", dataTableOutput("table")))))))