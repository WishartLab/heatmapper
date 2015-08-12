library(shiny)
source("../global_about.R")
server <- function(input, output) {}

ui <- fluidPage(
	NAVBAR("#aboutTab"),
	titlePanel("Contact"),
	includeHTML("www/contact.html")
)

shinyApp(ui = ui, server = server)