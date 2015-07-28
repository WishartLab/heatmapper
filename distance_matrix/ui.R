library(jscolourR)
library(d3heatmap)

shinyUI(fluidPage(
	includeHTML("www/navbar.html"),
	tags$style(".toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}
		#lowColour, #midColour, #highColour {width:100%;}"),
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
				
				
					selectInput('colourScheme', label = "Colour Scheme", selectize = FALSE,
						choices = c(
							"Rainbow" = 'rainbow', 
							"Topo" = 'topo', 
							"Custom" = 'custom'), 
						selected = 'custom'), 
			conditionalPanel(condition = "input.colourScheme == 'custom'",	
				fluidRow(
	  			column(4,jscolourInput("lowColour", label = "Low Colour", value = "#FF0000")), 
	  			column(4, jscolourInput("midColour", label = "Mid Colour")),
	  			column(4, jscolourInput("highColour", label = "High Colour", value = "#23B000")))
	 		),
				
			downloadButton('download', label = "Download Plot"),
				
				
				tags$br(), tags$br(),
				actionButton('advancedOptionsButton', label = "Show Advanced Options", class = "toggleButton fa fa-angle-down"),
	  		conditionalPanel(condition = "input.advancedOptionsButton%2",
				wellPanel(id = "advancedPanel", 
					textInput('title', label = "Title", value = ""),
				
					textInput('xlab', label = "X Axis Label", value = ""),
				
					textInput('ylab', label = "Y Axis Label", value = "")
				
				
				))
				
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