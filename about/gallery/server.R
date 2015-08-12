library(shiny)
shinyServer(function(input, output){	
	output$gallery <- renderUI({
		includeHTML(paste0("www/", input$navlistPanel, ".html"))
		
	})
})