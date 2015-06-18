library(jscolourR)

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
    		
    		sliderInput('zoom', 
    			label = "Zoom", 
    			min = 3, 
    			max = 20,
    			value = 10), 
    		
    		jscolourInput("lowColour", label = "Colour for low numbers", value = "#23B000"),
    		
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
					tabPanel(title = "Plot", 
						plotOutput("map", 
							click = "click",
							dblclick = "dblclick",
							hover = "hover",
							brush = "brush"),
						plotOutput("mapZoom")),
					tabPanel(title = "Table", dataTableOutput("table")))))))