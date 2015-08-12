source("../global_about.R")

shinyUI(fluidPage(
	NAVBAR("#aboutTab"),
	tags$script(paste0("$('contactTab').addClass('active');")), 
	includeHTML("www/contact.html")
))