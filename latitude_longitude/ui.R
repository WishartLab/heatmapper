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
    		
    		jscolourInput("lowColour", label = "Colour for low numbers", value = "#FFFA00"),
    		
    		jscolourInput("highColour", label = "Colour for high numbers", value = "#FF0000"),
    		
    		
    		sliderInput('contourSize', 
    			label = "Contour line size", 
    			min = 0, 
    			max = 1,
    			value = 0.5), 
    		
    		
    		sliderInput('pointSize', 
    			label = "Point size", 
    			min = 0, 
    			max = 5, 
    			value = 2), 
    		
    		sliderInput('pointOpacity', 
    			label = "Point Opacity", 
    			min = 0, 
    			max = 1, 
    			value = 0.8),
    		
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
					tabPanel(title = "Plot", leafletOutput("map")),
					tabPanel(title = "Table", dataTableOutput("table")))))))