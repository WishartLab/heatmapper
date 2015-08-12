library(shiny)
shinyServer(function(input, output){	
	output$instructions <- renderUI({
		list(
			tags$script(paste0("$('instructionsTab').addClass('active');")), 
			includeHTML(paste0("www/", input$navlistPanel, ".html"))
		)
	})
})