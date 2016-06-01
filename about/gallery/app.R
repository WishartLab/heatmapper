library(shiny)
source("../global_about.R")

server <- function(input, output) {
	output$gallery <- renderUI({
		includeHTML(paste0("www/", input$navlistPanelForGallery, ".html"))
	})
}

ui <- list(tags$head(includeScript("../../www/js/google-analytics.js")),
					NAVBAR("#aboutTab"), fluidPage(
# 	tags$head(includeScript("../../../www/js/google-analytics.js")),
	sidebarLayout(
		sidebarPanel(id = "sidebarPanel",
            titlePanel("Gallery"),
			NAVLIST_PANEL_FOR_GALLERY()
		),
		mainPanel(id = "mainPanel",
			uiOutput("gallery")
		)
	)
))

shinyApp(ui = ui, server = server)