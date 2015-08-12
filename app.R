library(shiny)
server <- function(input, output) {}

ui <- fluidPage(
	includeHTML("www/navbar.html"),
	includeHTML("www/index.html")
)

shinyApp(ui = ui, server = server)