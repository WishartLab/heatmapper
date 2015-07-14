library(leaflet)
library(spin)
shinyUI(fluidPage(
	includeHTML("navbar.html"),
	
	div(class = "busy", absolutePanel(width = "50px", height = "100px",
	fixed = TRUE, left = "50%", top = "40%", 
	h5("Loading"), tags$br(), spin())),
	
	sidebarLayout(
  sidebarPanel(
  	sliderInput('numGridRows', label = "Number of rows", min = 3, max = 40, step = 1, value = 3),  
  	checkboxInput('showPoints', label = strong("Show points"), value = TRUE),
  	uiOutput("info"),
  	numericInput('numInput', label = "Edit value here", value = "", min = 0, max = 1000), 
  	actionButton('submit', label = "Submit"),
		tags$br(), tags$br(),
  	tableOutput("all")
	),
	mainPanel(
		plotOutput("ggplotMap", click = "plot_click", width = 500, height = 500)
	)))
)
