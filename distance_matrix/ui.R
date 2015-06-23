library(jscolourR)
library(d3heatmap)

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
    			fileInput("file", label = "Upload Distance Matrix File")), 
				
				selectInput('colour', label = "Colour Scheme", selectize = FALSE,
					choices = c(
						"Rainbow" = 'rainbow', 
						"Topo" = 'topo', 
						"Custom" = 'custom'), 
					selected = 'custom'), 
				
				conditionalPanel(condition = "input.colour == 'custom'", id = "colourSection", 
					
					radioButtons('customVars', 
						label = "Number of Colour Variables", 
						choices = c(
							"3 (low, middle, high)" = 'custom3',
							"2 (low, high)" = 'custom2'), 
						selected = 'custom3'), 
					
					jscolourInput("lowColour", label = "Colour for low numbers", value = "#FF0000"),
					
					conditionalPanel(condition = "input.customVars == 'custom3'", 
						jscolourInput("midColour", label = "Colour for middle numbers")),
					
					jscolourInput("highColour", label = "Colour for high numbers", value = "#23B000")), 
				
				textInput('title', label = "Title", value = ""),
				
				textInput('xlab', label = "X Axis Label", value = ""),
				
				textInput('ylab', label = "Y Axis Label", value = ""),		
				
				downloadButton('download', label = "Download Plot")
    	), 
			mainPanel(
				tabsetPanel(id = "tabSelections", type = "tabs", 
					tabPanel(title = "Plot", plotOutput("map")),
					tabPanel(title = "Interactive", tags$br(), tags$br(), d3heatmapOutput("d3map")),
					tabPanel(title = "Table", dataTableOutput("table"))
					))),	
	singleton(includeScript("active.js"))
	))