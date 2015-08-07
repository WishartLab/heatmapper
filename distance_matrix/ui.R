library(spin)
library(shinyBS)
library(jscolourR)
library(d3heatmap)
source("../strings.R")

shinyUI(fluidPage(
	includeHTML("www/navbar.html"),
	tags$style(".toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}
		#lowColour, #midColour, #highColour {width:100%;}
		#file_progress {height:0;}"),
	
	div(class = "busy", absolutePanel(width = "50px", height = "100px",
		fixed = TRUE, left = "50%", top = "40%", 
		h5("Loading"), tags$br(), spin())),
	
		sidebarLayout(
			sidebarPanel(
				radioButtons('chooseInput',
					inline = TRUE,
    			label = FILE_UPLOAD,
    			choices = c(
    				"Upload File" = 'fileUpload',
						"Example File" = 'example'),
    			selected = 'example'),
				
				conditionalPanel(condition = "input.chooseInput == 'fileUpload'",
	  			fluidRow(
	  				column(8, fileInput('file', label = NULL)), 
	  				column(4, HTML("<button id='clearFile' class='action-button' style='display:inline;float:right;'>Clear File</button>"))
	  			)
	  		),
				
					selectInput('colourScheme', label = "Colour Scheme", selectize = FALSE,
						choices = c(
							"Rainbow" = 'rainbow', 
							"Topo" = 'topo', 
							"Custom" = 'custom'), 
						selected = 'custom'), 
			sliderInput('numShades', label = BIN_NUMBER, min = 3, max = 299, value = 160),
			conditionalPanel(condition = "input.colourScheme == 'custom'",	
				fluidRow(
	  			column(4,jscolourInput("lowColour", label = "Low Colour", value = "#FF0000")), 
	  			column(4, jscolourInput("midColour", label = "Mid Colour")),
	  			column(4, jscolourInput("highColour", label = "High Colour", value = "#23B000")))
	 		),
			textInput('title', label = "Title", value = ""),
				
			textInput('xlab', label = "X Axis Label", value = ""),
				
			textInput('ylab', label = "Y Axis Label", value = ""),
				
			downloadButton('download', label = DOWNLOAD_PLOT),
				
				
				tags$br(), tags$br(),
				actionButton('advancedOptionsButton', label = "Show Advanced Options", class = "toggleButton fa fa-angle-down"),
	  		conditionalPanel(condition = "input.advancedOptionsButton%2",
				wellPanel(id = "advancedPanel", 
	  			
				checkboxInput('asp', label = "Set Aspect Ratio = 1", value = TRUE),
	  		sliderInput('plotWidth', label = WIDTH, min = 400, max = 2000, value = 600),
				sliderInput('plotHeight', label = HEIGHT, min = 400, max = 2000, value = 500),
					
				selectInput('downloadPlotFormat', label = "Plot download file type", 
					choices = c(
						"JPEG" = 'jpg',
						"PDF" = 'pdf',
						"PNG" = 'png'
					), 
					selected = 'png')	
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