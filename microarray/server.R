library(shiny)
shinyServer(function(input, output, session){
	output$map <- renderPlot({
		x <- matrix(nrow = 10, ncol = 10, data = seq(from = 1, to = 100))
		print(x)
		heatmap(x)
	})
})