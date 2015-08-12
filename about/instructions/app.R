library(shiny)
source("../global_about.R")
server <- function(input, output) {
	output$instructions <- renderUI({
		includeHTML(paste0("www/", input$navlistPanel, ".html"))
	})
}

ui <- fluidPage(
	NAVBAR("#aboutTab"),

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
)

shinyApp(ui = ui, server = server)