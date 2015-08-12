source("../global_about.R")

shinyUI(fluidPage(
	NAVBAR("#aboutTab"),
	titlePanel("Contact"),
	tags$br(),
	includeHTML("www/contact.html")
))