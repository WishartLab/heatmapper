library(shiny)
source("../global_about.R")
server <- function(input, output) {}

ui <- list(NAVBAR("#aboutTab"),fluidPage(
	titlePanel("Contact"),
	includeHTML("www/contact.html")
))

shinyApp(ui = ui, server = server)