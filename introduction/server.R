library(shiny)
shinyServer(function(input, output, session){
	output$instructions <- renderUI({
		includeHTML(paste0("www/instructions/", input$navlistPanel, ".html"))
	})
	
	output$gallery <- renderUI({
		includeHTML(paste0("www/gallery/", input$navlistPanel, ".html"))
	})
	
})