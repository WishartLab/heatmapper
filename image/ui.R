library(spin)
library(shinyBS)
library(jscolourR)

shinyUI(fluidPage(
	includeHTML("www/navbar.html"),
	
	tags$head(
		HTML("<link rel=\"stylesheet\" href=\"//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css\">"),
		tags$style(".toggleButton{width:100%;} .fa-angle-down:before{float:right;} .fa-angle-up:before{float:right;}")),
	
	div(class = "busy", absolutePanel(width = "50px", height = "100px",
		fixed = TRUE, left = "50%", top = "40%", 
		h5("Loading"), tags$br(), spin())),
	
	sidebarLayout(
	  sidebarPanel(
	  	
	  	actionButton('fileInputOptionsButton', label = "Hide File Options", class = "toggleButton fa fa-angle-up"),
			wellPanel(id = "fileInputPanel",
	  		fileInput('imageFile', label = "Upload image here"),
				checkboxInput('stretchImage', label = strong("Stretch image to fit grid"), value = TRUE)
			),
	  	
	  	actionButton('editOptionsButton', label = "Hide Editing Options", class = "toggleButton fa fa-angle-up"),
			wellPanel(id = "editPanel",
	  		uiOutput("clickInfo"),
	  		tableOutput("clickTable"),
				numericInput('numInput', label = "Edit value here", value = "", min = 0, max = 1000), 
				actionButton('submit', label = "Submit")
			),
	  	
	  	actionButton('plotOptionsButton', label = "Hide Plot Options", class = "toggleButton fa fa-angle-up"),
			wellPanel(id = "plotPanel", 
				sliderInput('numGridRows', label = "Number of rows", min = 3, max = 40, step = 1, value = 10),
		  	bsTooltip(id = "numGridRows", 
					title = "Warning: any changes to values will be lost after changing the number of rows",
					placement = "bottom"),
		  	
		  	
		  	checkboxInput('showImage', label = strong("Show background image"), value = TRUE),
		  	checkboxInput('showContour', label = strong("Show contour lines"), value = TRUE),
		  	checkboxInput('showFill', label = strong("Show contour fill"), value = TRUE),
		  	checkboxInput('showPoints', label = strong("Show points"), value = TRUE),
		  	
		  	selectInput('pointType', label = "Select point type", 
		  		choices = c(
		  			"circle - hollow" = 1, 
		  			"circle - filled" = 16, 
		  			"square - hollow" = 0, 
		  			"square - filled" = 15
		  			), 
		  		selected = 1)
				),
	  	
	  	actionButton('downloadOptionsButton', label = "Hide Download Options", class = "toggleButton fa fa-angle-up"),
			wellPanel(id = "downloadPanel")	  	
		),
		mainPanel(
			tabsetPanel(
				tabPanel("Plot",
					plotOutput("ggplotMap", click = "plot_click", width = 600, height = 500)
				),
				tabPanel("Table", 
					tags$br(),
					dataTableOutput("table")
				)
			)
		)
	),
	singleton(includeScript("www/js/active.js"))
))
