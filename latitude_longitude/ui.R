library(jscolourR)
library(leaflet)

shinyUI(fluidPage(
	includeHTML("www/navbar.html"),
	tags$style(".toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}"),
		sidebarLayout(
    	sidebarPanel(
    		actionButton('fileInputOptionsButton', label = "Hide File Options", class = "toggleButton fa fa-angle-up"),
				wellPanel(id = "fileInputPanel",
    		radioButtons('chooseInput',
    			label = "Choose Input Type",
    			choices = c(
    				"Upload File" = 'fileUpload',
						"Example File" = 'example'),
    			selected = 'example'),
    		
    		conditionalPanel(condition = "input.chooseInput == 'fileUpload'", 
    			fileInput("file", label = strong("File input")))
					), 
    		
  actionButton('colourOptionsButton', label = "Hide Colour Options", class = "toggleButton fa fa-angle-up"),
	wellPanel(id = "colourPanel", 
    		jscolourInput("lowColour", label = "Colour for low numbers", value = "#FFFA00"),
    		
    		jscolourInput("highColour", label = "Colour for high numbers", value = "#FF0000")
	),
		actionButton('mapOptionsButton', label = "Hide Map Options", class = "toggleButton fa fa-angle-up"),
		wellPanel(id = "mapPanel", 	 		
    		strong("Background"),
    		checkboxInput('showMap', label = "Show Map", value = TRUE),
    		
    		sliderInput('contourSize', 
    			label = "Contour Line Width (in pixels)", 
    			min = 0, 
    			max = 4,
    			value = 1), 
    		
			strong("Points"),
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
    			selected = 'terrain')
			),
	actionButton('downloadOptionsButton', label = "Hide Download Options", class = "toggleButton fa fa-angle-up"),
	wellPanel(id = "downloadPanel", 		
    		radioButtons('downloadType', 
    			label = "Downlaod file format", 
    			choices = c(
    				"PDF" = 'pdf', 
    				"PNG" = 'png'), 
    			selected = 'pdf'),
    		
    		downloadButton('download', "Download image")
    		)
    		),
			mainPanel(
				tabsetPanel(type = "tabs", 
					tabPanel(title = "Plot", leafletOutput("map", height = 600)),
					tabPanel(title = "Table", dataTableOutput("table"))))
			), 
	singleton(includeScript("www/js/active.js"))
	))