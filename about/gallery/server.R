library(shiny)
shinyServer(function(input, output){	
	output$gallery <- renderUI({
		list(
			tags$script(paste0("$('galleryTab').addClass('active');")), 
			includeHTML(paste0("www/", input$navlistPanel, ".html"))
		)
	})
})