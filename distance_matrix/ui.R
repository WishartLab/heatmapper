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
    			fileInput("file", label = h3("Distance Matrix File Input"))), 
				
				textInput('title', label = "Title", value = ""),
				textInput('xlab', label = "X Axis Label", value = ""),
				textInput('ylab', label = "Y Axis Label", value = ""),
				selectInput('colour', label = "Colour Scheme", 
					choices = c(
						"Rainbow" = 'rainbow', 
						"Topo" = 'topo', 
						"Custom" = 'custom'), 
					selected = 'custom'), 
				
				conditionalPanel(condition = "input.colour == 'custom'",
					
					jscolourInput("lowColour", label = "Colour for low numbers", value = "#FF0000"),
					
					jscolourInput("midColour", label = "Colour for middle numbers"),
					
					jscolourInput("highColour", label = "Colour for high numbers", value = "#23B000"))
    	), 
			mainPanel(
				tabsetPanel(type = "tabs", 
					tabPanel(title = "Plot", plotOutput("map", 
						brush = "brush"), 
						verbatimTextOutput("info")),
					tabPanel(title = "Table", dataTableOutput("table"))
					)))))