library(jscolourR)

shinyUI(fluidPage(
	includeHTML("navbar.html"),
		sidebarLayout(
    	sidebarPanel(
    		
    		radioButtons('cmChooseInput',
    			label = "Choose Input Type",
    			choices = c(
    				"Upload File" = 'cmFileUpload',
						"Example File" = 'cmExample'),
    			selected = 'cmExample'),
    		
    		conditionalPanel(condition = "input.cmChooseInput == 'cmFileUpload'", 
    			fileInput("cmFile", label = h3("File input"))), 
    		
    		sliderInput('cmZoom', 
    			label = "Zoom", 
    			min = 3, 
    			max = 20,
    			value = 10), 
    		
    		jscolourInput("cmLowColour", label = "Colour for low numbers", value = "#23B000"),
    		
    		jscolourInput("cmHighColour", label = "Colour for high numbers", value = "#FF0000"),
    		
    		
    		sliderInput('cmContourSize', 
    			label = "Contour line size", 
    			min = 0, 
    			max = 1,
    			value = 0.5), 
    		
    		
    		sliderInput('cmPointSize', 
    			label = "Point size", 
    			min = 0, 
    			max = 5, 
    			value = 2), 
    		
    		selectInput('cmType', 
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
    		
    		radioButtons('cmDownloadType', 
    			label = "Downlaod file format", 
    			choices = c(
    				"PDF" = 'pdf', 
    				"PNG" = 'png'), 
    			selected = 'pdf'),
    		
    		downloadButton('cmDownload', "Download image")
    		),
			mainPanel(
				tabsetPanel(type = "tabs", 
					tabPanel(title = "Plot", 
						plotOutput("continuousMap", 
							click = "cm_click",
							dblclick = "cm_dblclick",
							hover = "cm_hover",
							brush = "cm_brush"),
						plotOutput("continuousMapZoom")),
					tabPanel(title = "Table", dataTableOutput("continuousTable")))))))