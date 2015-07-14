library(leaflet)
library(spin)
shinyUI(fluidPage(
	includeHTML("navbar.html"),
	
	div(class = "busy", absolutePanel(width = "50px", height = "100px",
	fixed = TRUE, left = "50%", top = "40%", 
	h5("Loading"), tags$br(), spin())),
	
	sidebarLayout(
  sidebarPanel(
  	uiOutput("info"),
  	numericInput("numInput", label = "Edit value here", value = "", min = 0, max = 1000), 
  	actionButton("submit", label = "Submit"),
		tags$br(), tags$br(),
  	tableOutput("all")
	),
	mainPanel(
		plotOutput("ggplotMap", click = "plot_click", width = 500, height = 500)
		)	
	
	))
)
