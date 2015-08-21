library(shiny)
source("../global_about.R")

server <- function(input, output) {
	output$instructions <- renderUI({
		includeHTML(paste0("www/", input$navlistPanel, ".html"))
	})
}

ui <- list(NAVBAR("#aboutTab"), fluidPage(
	tags$head(tags$style("table, th, td { border: 1px solid black;} th {text-align:center;} ul {margin:0.5em;}")),
	sidebarLayout(
		sidebarPanel(id = "sidebarPanel",
			NAVLIST_PANEL()
		),
		mainPanel(id = "mainPanel",
			titlePanel("Instructions"),
			tags$br(),
			uiOutput("instructions")
		)
	)
))

shinyApp(ui = ui, server = server)