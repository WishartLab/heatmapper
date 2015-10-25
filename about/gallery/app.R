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
            titlePanel("Gallery"),
			NAVLIST_PANEL()
		),
		mainPanel(id = "mainPanel",
			uiOutput("gallery")
		)
	)
))

shinyApp(ui = ui, server = server)