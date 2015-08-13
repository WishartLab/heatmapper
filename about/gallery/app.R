library(shiny)
source("../global_about.R")

server <- function(input, output) {
	output$gallery <- renderUI({
		includeHTML(paste0("www/", input$navlistPanel, ".html"))
	})
}

ui <- list(NAVBAR("#aboutTab"), fluidPage(
	sidebarLayout(
		sidebarPanel(id = "sidebarPanel",
			NAVLIST_PANEL()
		),
		mainPanel(id = "mainPanel",
			titlePanel("Gallery"),
			tags$br(),
			uiOutput("gallery")
		)
	)
))

shinyApp(ui = ui, server = server)