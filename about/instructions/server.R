library(shiny)
shinyServer(function(input, output){	
	output$instructions <- renderUI({
		includeHTML(paste0("www/", input$navlistPanel, ".html"))
	})
})