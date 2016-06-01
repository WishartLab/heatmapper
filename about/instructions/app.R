library(shiny)
source("../global_about.R")

server <- function(input, output) {
	output$fileInputTab <- renderUI({
		includeHTML(paste0("www/", input$navlistPanel, ".html"))
	})
	output$featuresTab <- renderUI({
		includeHTML(paste0("www/features/", input$navlistPanel, ".html"))
	})
}

ui <- list(tags$head(includeScript("../../www/js/google-analytics.js")),
						NAVBAR("#aboutTab"), fluidPage(
	
	tags$head(tags$style("table, th, td { border: 1px solid black;} th {text-align:center;} ul {margin:0.5em;}")),
	sidebarLayout(
		sidebarPanel(id = "sidebarPanel",
			titlePanel("Instructions"),
			NAVLIST_PANEL()
		),
		mainPanel(id = "mainPanel",
			
			tabsetPanel(
				tabPanel("File Input", 
					tags$br(),
					uiOutput("fileInputTab")
				),
				tabPanel("Features",
					tags$br(),
					uiOutput("featuresTab")
				)
			)
		)
	)
))

shinyApp(ui = ui, server = server)