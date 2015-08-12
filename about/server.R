library(shiny)
shinyServer(function(input, output, session){
	output$instructions <- renderUI({
		list(
			tags$script(paste0("$('instructionsTab').addClass('active');")), 
			includeHTML(paste0("www/instructions/", input$navlistPanel, ".html"))
		)
	})
	
	output$gallery <- renderUI({
		list(
			tags$script(paste0("$('galleryTab').addClass('active');")), 
			includeHTML(paste0("www/gallery/", input$navlistPanel, ".html"))
		)
	})
	
})