library(jscolourR)
library(d3heatmap)

shinyUI(fluidPage(
	includeHTML("www/navbar.html"),
	tags$style(".toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}"),
		sidebarLayout(
			sidebarPanel(
				
				actionButton('fileInputOptionsButton', label = "Hide File Input Options", class = "toggleButton fa fa-angle-up"),
				wellPanel(id = "fileInputPanel", 
				radioButtons('chooseInput',
    			label = "Choose Input Type",
    			choices = c(
    				"Upload File" = 'fileUpload',
						"Example File" = 'example'),
    			selected = 'example'),
				
				conditionalPanel(condition = "input.chooseInput == 'fileUpload'", 
    			fileInput("file", label = "Upload Distance Matrix File"))), 
				
				actionButton('colourOptionsButton', label = "Hide Colour Options", class = "toggleButton fa fa-angle-up"),
				wellPanel(id = "colourPanel", 
					selectInput('colour', label = "Colour Scheme", selectize = FALSE,
						choices = c(
							"Rainbow" = 'rainbow', 
							"Topo" = 'topo', 
							"Custom" = 'custom'), 
						selected = 'custom'), 
					
					tags$div(id = 'colourSection', 
						radioButtons('customVars', 
							label = "Number of Colour Variables", 
							choices = c(
								"3 (low, middle, high)" = 'custom3',
								"2 (low, high)" = 'custom2'), 
							selected = 'custom3'), 
						
						jscolourInput("lowColour", label = "Colour for low numbers", value = "#FF0000"),
						
						conditionalPanel(condition = "input.customVars == 'custom3'", 
							jscolourInput("midColour", label = "Colour for middle numbers")),
						
						jscolourInput("highColour", label = "Colour for high numbers", value = "#23B000"))), 
				
				
				actionButton('labelOptionsButton', label = "Hide Label Options", class = "toggleButton fa fa-angle-up"),
				wellPanel(id = "labelPanel", 
					textInput('title', label = "Title", value = ""),
				
					textInput('xlab', label = "X Axis Label", value = ""),
				
					textInput('ylab', label = "Y Axis Label", value = "")	
				),
				
				actionButton('downloadOptionsButton', label = "Hide Download Options", class = "toggleButton fa fa-angle-up"),
				wellPanel(id = "downloadPanel", 
					tags$strong("Download Image of Plot"), tags$br(),
					downloadButton('download', label = "Download Plot"))
				
    	), 
			mainPanel(
				tabsetPanel(id = "tabSelections", type = "tabs", 
					tabPanel(title = "Plot", tags$br(), 
						plotOutput("map", height = 500)),
					tabPanel(title = "Interactive", tags$br(), tags$br(), d3heatmapOutput("d3map", height = 500)),
					tabPanel(title = "Table", tags$br(), dataTableOutput("table"))
					))),	
	singleton(includeScript("www/js/active.js"))
	))