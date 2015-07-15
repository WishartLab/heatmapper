library(spin)
library(shinyBS)
shinyUI(fluidPage(
	includeHTML("navbar.html"),
	
	div(class = "busy", absolutePanel(width = "50px", height = "100px",
	fixed = TRUE, left = "50%", top = "40%", 
	h5("Loading"), tags$br(), spin())),
	
	sidebarLayout(
  sidebarPanel(
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
  		selected = 1),
  	
  	uiOutput("info"),
  	tableOutput("hoverInfo"),
  	numericInput('numInput', label = "Edit value here", value = "", min = 0, max = 1000), 
  	actionButton('submit', label = "Submit"),
		tags$br(), tags$br(),
  	tableOutput("all")
	),
	mainPanel(
		plotOutput("ggplotMap", click = "plot_click", hover = "plot_hover", width = 500, height = 500)
	)))
)
